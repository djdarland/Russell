#include <stdio.h>
#include "parm.h"

#include "stree/ststructs.mh"

#include "sigs.h"

# define SIZE_SIZE 8
# define DUMMY -2
# define is_dummy(id) ((id) -> id_str_table_index == DUMMY)

extern FILE * unparse_file;

extern int next_pre;

extern int yynerrs;

boolean replace_arg();
NODE * mknode();

extern unsigned indx_subscr;

extern NODE * var_Void;

extern NODE * diff_p, * diff_q; /* set by comp_st to point to difference in   */
				/* trees being compared.  See pass3/comp_st.c */

/*
 *  Return initial_args, with additional arguments appended to make it
 * the same length as params.  It is assumed that initial_args can
 * be extended to match params.  If this is false, either NIL or some
 * garbage argument list (of the same length as params) is returned.
 *  The missing arguments are filled in by either unifying corresponding
 * arguments and parameters (provided they have non-type signatures)
 * or by building an identifier node pointing to void_decl
 * for a var Void argument.  If oper is an identifier named ".",
 * if it has 3 parameters with val signature, and if there are 2 arguments
 * specified, both of which are strings of length < MAXINTLEN, then the third
 * argument is inferred to be the length of the second string.
 *  It is assumed that signatures for all expressions on the initial
 * argument list are known.  It is guaranteed that the same is true
 * for the final argument list.
 *  It is assumed that the argument list is initially no longer,
 * and presumably shorter than the parameter list.
 */

NODE *
infer_args(initial_args, params, void_decl, oper)
NODE * initial_args;
NODE * params;
NODE * void_decl;
NODE * oper;
{
    int n_params = length(params);
    int n_args = length(initial_args);  /* Number of known arguments */
    NODE * new_args;
    NODE * Void_id;
    int i;
    NODE * s_params;  /* parameters after substitution */
    boolean failed = FALSE;
    boolean is_decimal_point; /* third argument should be length of 2nd one */

#   ifdef DEBUG
      if (nargs() != 4) {
	dbgmsg("infer_args: wrong number of arguments\n");
      }
#   endif
    if (n_args == n_params) return(initial_args);
    new_args = lock(copylist(initial_args));
    /* determine whether the length of the second arg should be inserted */
      is_decimal_point = n_params >= 3 && n_args == 2 &&
		         oper -> kind == OPRID &&
		         oper -> id_str_table_index == indx_subscr;
      if (is_decimal_point) {
	int i = 0;
	extern boolean Gflag;
	extern NODE * val_Integer;

        maplist(s, new_args, {
          if (s -> kind != UQSTR) {
            is_decimal_point = FALSE;
	  } else if (!Gflag && strlen(s -> str_string) > MAXINTLEN
		     || Gflag && strlen(s -> str_string) > GMAXINTLEN) {
	    if (comp_st(s -> signature,
			val_Integer, NIL, NIL) == 0) {
		fprintf(stderr, "Warning: possible floating pt const too long\n");
	    }
          }
        });
        maplist (s, params, {
          i++;
          if (i <= 3 && s -> par_signature -> kind != VALSIGNATURE) {
            is_decimal_point = FALSE;
          }
        });
      }
    if (is_decimal_point) {
      /* add third argument */
        char * new_str_string = (char *) malloc(SIZE_SIZE);
        NODE * new_str;
	sprintf(new_str_string, "%d",
		strlen(last(new_args) -> str_string));
        new_str = mknode(UQSTR, new_str_string);
        new_str -> vlineno = oper -> vlineno;
	new_str -> str_use_list = first(new_args) -> str_use_list;
	if (findsig(new_str,FALSE) != SUCCESS) {
	  errmsg0(oper, "Can`t find signature for length arg of \".\" ");
          new_str -> signature = ERR_SIG;
          new_str -> sig_done = SIG_DONE;
	  return(NIL);
        }
        addright(new_args, new_str);
        n_args++;
    }
    /* new_args := new_args with parameter identifiers     */
    /* inserted for missing arguments.			   */
      i = 1;
      maplist(s, params, {
        if (i > n_args) {
          /* Add dummy node */
	    NODE * dummy_id;
	    dummy_id = mknode(LETTERID, DUMMY);
	    dummy_id -> id_last_definition = s;
	    dummy_id -> id_def_found = TRUE;
	    dummy_id -> signature = NIL;
	    addright(new_args, dummy_id);
	}
	i++;
      });
    /* add var Void arguments */
      if (void_decl != NIL) {
        Void_id = lock(mknode(LETTERID, 0));

#	ifdef DEBUG
	  if (void_decl -> kind != PARAMETER) {
            dbgmsg("infer_args: funny Void variable declaration: 0x%X, %s\n",
                   void_decl, kindname(void_decl -> kind));
            abort();
	  }
#	endif
	Void_id -> id_last_definition = void_decl;
        Void_id -> id_def_found = TRUE;
        Void_id -> vlineno = oper -> vlineno;
        initfld(&(Void_id -> signature), var_Void);
	Void_id -> sig_done = SIG_DONE;
	maplist(s, params, {
	    if (comp_st(s -> par_signature, var_Void, NIL, NIL) == 0) {
                boolean success = replace_arg(new_args, s, Void_id);
                if (success /* arg was missing */) {
                    n_args++;
		}
	    }
	});
	vfree(unlock(Void_id));
      }
    /* add arguments derivable by unification of non-type sigs */
      while (!failed && n_args < n_params) {
	s_params = subst(params, params, new_args);
	begin_map2lists(s, s_params, r, new_args) {
	    if ((r -> kind == LETTERID || r -> kind == OPRID) && is_dummy(r)) {
              /* Should have found missing arguments by now */
	      failed = TRUE;
	      break; /* out of map2lists */
	    }
	    /* real argument */
	    if (s -> par_signature -> kind != TYPESIGNATURE
		&& r -> signature != ERR_SIG) {
              NODE * par_to_unify = s -> par_signature;
              NODE * arg_to_unify = r -> signature;
            
              /* Adjust for possible coercions */
                if (par_to_unify -> kind == VALSIGNATURE) {
                    if (arg_to_unify -> kind == VARSIGNATURE) {
                        arg_to_unify = arg_to_unify -> var_denotation;
                        par_to_unify = par_to_unify -> val_denotation;
                    } else if (arg_to_unify -> kind == FUNCSIGNATURE
                               && is_empty(arg_to_unify -> fsig_param_list)) {
                        arg_to_unify = arg_to_unify -> fsig_result_sig;
                    }
                }

	      /* try to unify argument and parameter signatures */
                if (comp_st(par_to_unify,
			    arg_to_unify, NIL, NIL) != 0) {
		    NODE * sav_diff_p = diff_p;
		    NODE * sav_diff_q = diff_q;

		    if(sav_diff_p != NIL && is_dummy(sav_diff_p) &&
		       findsig(sav_diff_q, FALSE) == SUCCESS &&
		       replace_arg(new_args, sav_diff_p -> id_last_definition,
				   unshare(sav_diff_q))) {
			/* differed because of missing argument */
			n_args++;
			break;
		    } else {
			failed = TRUE;
		    }
		}
	    }
	} end_map2lists;
      }
    unlock(new_args);
    if (n_args != n_params) {
	vfree(new_args);
	return(NIL);
    } else {
        return(new_args);
    }
}

/*
 *  Replace the dummy node in list args which is declared by the node old
 * with the expression tree new.  Returns TRUE if such a node was found,
 * FALSE otherwise.
 */
boolean replace_arg(args, old, new)
NODE * args, * old, * new;
{
    int i = 1;  /* index of the element old */
    int j;
    boolean found_it = FALSE;
    
    mapinslist(s, args, {
	if (s != NIL && 
            s -> kind == LETTERID && is_dummy(s) &&
            is_declared_by(s, old)) {
		found_it = TRUE;
		DELETE;
		break;
	}
	i++;
    });
    if (!found_it) return(FALSE);

    /* Insert new node before position i.  This is a separate loop */
    /* only because INSERT followed by DELETE is not guaranteed to */
    /* work.							   */
	j = 1;
	mapinslist(s, args, {
	    if (j == i) {
		INSERT(new);
	    }
            j++;
	});
    return(TRUE);
}
