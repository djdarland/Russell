# define DEBUG

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

# define TRACE
# undef TRACE

# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "sigs.h"

extern int comp_index;

extern NODE * declerr;

extern boolean may_fail;

extern FILE * unparse_file;

NODE * finddecl1();

NODE * prev_def();

boolean def_match();

NODE * unshare();

NODE * subst();

/*
 * finddecl(id)
 *
 *  Find the appropriate declaration for id.  If id_appl is non-NIL
 * use the fact that it is being used as an operator with the given
 * argument list.  If signature is non-NIL use the fact that an explicit
 * signature was given.
 * "Finding the declaration" involves setting the
 * sel_type and last_definition fields to their final value (if possible)
 * On success the def_found field is set.  (Nothing whatsoever is
 * done if this field was originally set.)
 *  It is assumed that the signatures of all surrounding use list types
 * are known.
 *  Returns SUCCESS or a node pointer to indicate cyclic dependencies
 * in the same way as findsig does.
 *  It is assumed that the signatures of the arguments (if any) are known.
 */

/* check for multiple declarations later.                  */

NODE *
finddecl(id)
NODE * id;
{
    NODE * q;

    if (id -> id_appl == NIL) {
	return(finddecl1(id, id -> id_appl, TRUE));
    } else {
        /* Try for an exact match first */
	    q = finddecl1(id, id -> id_appl, TRUE);
        if (q != SUCCESS || id -> id_def_found)
            return(q);
        /* Now try for a match requiring coercions */
	    return(finddecl1(id, id -> id_appl, FALSE));
    }
}

/* 
 *  Identical to the above except:
 * Any arguments to which id may have been applied are passed explicitly.
 * If exact is TRUE only definitions giving exact argument parameter
 * matches are allowed.  Otherwise the effect of possible coercions is
 * anticipated.
 */

NODE *
finddecl1(id, appl, exact)
NODE * id, * appl;
boolean exact;
{
    NODE * usl;     /* use list currently being searched for identifier */
    NODE * q;
    NODE * curr_defn;   /* definition currently being examined  */
    NODE * prev_defn;   /* For partial uniqueness check         */
    NODE * args;

    if (id -> id_def_found) return(SUCCESS);
    if (id -> sel_type == NIL) {
	curr_defn = id -> id_last_definition;
	while( curr_defn != NIL ) {
	    q = declsig(curr_defn);
	    if (declerr != SUCCESS) {
                if (may_fail || prev_def(curr_defn) != NIL) {
		  return(declerr);
		} else {
		  /* may as well assume it's this one */
		  id -> id_last_definition = curr_defn;
		  id -> id_def_found = TRUE;
		  return(SUCCESS);
		}
            }
	    if(def_match(q, id -> signature, appl, id, exact)) {
		id -> id_last_definition = curr_defn;
		id -> id_def_found = TRUE;
#               ifdef TRACE
		    unparse_file = stdout;
		    printf("Found definition for ");
		    unparse(id);
		    printf("\n");
#               endif
		/* Make a partial check for uniqueness of declaration */
		  if (curr_defn -> kind == DECLARATION
		      && (prev_defn = prev_def(curr_defn)) != NIL
		      && prev_defn -> kind == DECLARATION
		      && curr_defn -> decl_scope == prev_defn -> decl_scope) {
		      q = declsig(prev_defn);
		      if (declerr == SUCCESS && q != ERR_SIG &&
			  def_match(q, id -> signature, appl, id, exact)) {
			extern int yynerrs;
			errmsg1(id, "Ambiguous reference to %s", 
				getname(id -> id_str_table_index));
		      }
		  }
                return(SUCCESS);
            } else {
#               ifdef TRACE
		    unparse_file = stdout;
		    printf("Def_match failed for ");
		    unparse(id);
		    printf("\nDeclaration sig:");
		    unparse(q);
		    printf("\n");
#               endif
		curr_defn = prev_def(curr_defn);
            }
	}
	if (appl != NIL) {
	  /* Try to infer a selection from an argument type */
	    args = appl -> ap_args;
#           ifdef DEBUG
		if (appl -> kind != APPLICATION) {
		    dbgmsg("finndecl1: bad application\n");
		    abort();
		}
#           endif
	    begin_maplist(p, args) {
		NODE * arg_sig = p -> signature; /* signature used in infering */
						 /* arg "type"                 */
		NODE * arg_type;

		if (arg_sig == ERR_SIG) continue;
		IFDEBUG(
		    if (arg_sig == NIL) {
			dbgmsg("finddecl: unknown arg signature\n");
			abort();
		    }
		)
		if (arg_sig -> kind == FUNCSIGNATURE) {
		    arg_sig = arg_sig -> fsig_result_sig;
		    if (arg_sig == ERR_SIG) continue;
		}
		if (arg_sig -> kind == VALSIGNATURE) {
		    arg_type = arg_sig -> val_denotation;
		} else if (arg_sig -> kind == VARSIGNATURE) {
		    arg_type = arg_sig -> var_denotation;
		} else {
		    continue;
                }
                /* ignore forgetting coercions */
                  if(arg_type -> kind == MODPRIMARY
                     && arg_type -> mp_type_modifier == NIL) {
                      arg_type = arg_type -> mp_primary;
                  }
                if((q = findsig(arg_type, FALSE)) != SUCCESS) {
		  if (!may_fail ||     /* <-- */
		      (length(args) == 1 ||
		       comp_st(first(args) -> signature, last(args) -> signature,
			      NIL, NIL) == 0)) {
		    /* May as well guess this type (This is silly, but ...) */
		    initfld(&(id -> sel_type), unshare(arg_type));
		    id -> id_def_found = TRUE;
		    return(SUCCESS);
		  } else {
		    return(q);
		  }
		}
		IFDEBUG(
		    if(arg_type -> signature == NIL) {
			dbgmsg("finddecl: no type signature\n");
			prtree(arg_type);
			printf("sig_done = %d\n", arg_type -> sig_done);
		    }
		)
		if (arg_type -> signature == ERR_SIG) {
		    continue;
		}
		if (arg_type -> signature -> kind == TYPESIGNATURE) {
		    q = getcomp(arg_type -> signature,
				id, 
				arg_type,
				id -> signature, NIL,
				appl, exact);
		} else {
		    /* leave error for checksigs to find */
			q = NIL;
		}
		if (q != NIL) {
		    initsig(id, q);
		    initfld(&(id -> sel_type), unshare(arg_type));
		    id -> id_def_found = TRUE;
		    id -> sel_index = comp_index;
		    id -> sig_done = SIG_DONE;
		    return(SUCCESS);
		}
	    } end_maplist;
	}
	/* Try to infer a selection from a use list type */
            usl = id -> id_use_list;
            while(usl != NIL) {
#               ifdef DEBUG
                    if (usl -> kind != USELIST) {
                        dbgmsg("finddecl: bad use list\n");
                    }
#               endif
		maplist(p, usl -> usl_type_list, {
		    IFDEBUG(
			if (p -> signature == NIL) {
                            dbgmsg("finddecl: use list type without sig\n");
                            prtree(p);
			}
		    )
		    if (p -> signature == ERR_SIG 
			|| p -> signature -> kind == TYPESIGNATURE)
                        q = getcomp(p -> signature,
				    id,
				    p,
				    id -> signature, NIL,
				    appl, exact);
                    else
                        /* leave error for checksigs to find */
                        q = NIL;
		    if (q != NIL) {
                        initsig(id, q);
			initfld(&(id -> sel_type), unshare(p));
                        id -> id_def_found = TRUE;
			id -> sel_index = comp_index;
			id -> sig_done = SIG_DONE;
                        return(SUCCESS);
                    }
                });
                usl = usl -> usl_previous_list;
	    }
	/* Didn't find it.  Return SUCCESS anyway to indicate no problem */
	/* with signatures.                                              */
	    return(SUCCESS);
    } else {
	id -> id_def_found = TRUE;
	return(SUCCESS);
    }
}

/*
 *  findstdecl(string_pointer)
 *
 * analogous to the above except deals with strings rather than identifiers.
 * Checks that explicitly specified type (if any) is appropriate.
 * Leaves (or sets) selection type to NIL if nothing appropriate is found.
 */
NODE *
findstdecl(string)
NODE * string;
{
    NODE * usl;
    boolean found_it = 0;
    NODE * q;

    if (string -> sel_type == NIL) {
        /* Try to infer a selection from a use list type */
	    usl = string -> str_use_list;
            while(usl != NIL && !found_it) {
#               ifdef DEBUG
                    if (usl -> kind != USELIST) {
                        dbgmsg("finddecl: bad use list\n");
                    }
#               endif
		maplist(p, usl -> usl_type_list, {
		    IFDEBUG(
			if (p -> signature == NIL) {
                            dbgmsg("finddecl: use list type without sig\n");
                        }
		    )
                    if (p -> signature == ERR_SIG) {
                        found_it = TRUE;
                    } else if (p -> signature -> kind == TYPESIGNATURE) {
			found_it = hasstring(p -> signature, string);
                    } else {
                        /* leave error for checksigs to find */
                        found_it = FALSE;
                    }
                    if(found_it) {
			initfld(&(string -> sel_type), unshare(p));
			break;
		    }
                });
                usl = usl -> usl_previous_list;
            }
    } else /* type explicitly specified */ {
	if ((q = findsig(string -> sel_type, FALSE)) != SUCCESS) {
	    return(q);
	}
	if (string -> sel_type -> signature == ERR_SIG) {
	    return(SUCCESS);
	}
	if (!hasstring(string -> sel_type -> signature, string)) {
	    chgfld(&(string -> sel_type), NIL);
	}
    }
    return(SUCCESS);
}

/*
 *  prev_def(def_pointer)
 *
 *  Returns the value of the previous definition field in the node pointed
 * to by def_pointer.
 */

NODE *
prev_def(def)
NODE * def;
{
    switch(def -> kind) {
        case DECLARATION:
            return(def -> decl_previous_definition);
        case PARAMETER:
            return(def -> par_previous_definition);
        case TYPESIGNATURE:
            return(def -> ts_previous_definition);
        case PRODCONSTRUCTION:
        case UNIONCONSTRUCTION:
            return(def -> prod_previous_definition);
        case RECORDCONSTRUCTION:
            return(def -> rec_previous_definition);
        case MODPRIMARY:
            switch (def -> mp_type_modifier -> kind) {
                case WITHLIST:
                    return(def -> mp_type_modifier -> wl_previous_definition);
                case EXPORTLIST:
                case HIDELIST:
                    return(def -> mp_type_modifier -> el_previous_definition);
#               ifdef DEBUG
                    default:
                        dbgmsg("prev_def: bad type modifier\n");
#               endif
            }
#       ifdef DEBUG
            default:
                dbgmsg("prev_def: bad definition field\n");
#       endif
    }
}

/*
 * sig_structure(sig)
 *
 *  Return a signature equivalent to sig that is not an identifier
 *  Assumes that a validity check was performed earlier.
 */
NODE * sig_structure(sig)
NODE * sig;
{
    NODE * result = sig;

    while (result -> kind == LETTERID || result -> kind == OPRID
	   && result -> id_last_definition -> kind == DECLARATION
	   && result -> id_last_definition -> decl_sig_transp) {
	result = result -> id_last_definition -> decl_denotation;
    }
    return(result);
}


/*
 * def_match( definition_signature, given_signature,
 *            application, op, exact )
 *
 * returns TRUE iff definition_signature is the same as given_signature
 * (if any) and - if application is given - is a function signature
 * with parameter signatures which match the argument list.
 *  The operator op is sometimes need to infer missing arguments.
 */ 


boolean
def_match( sig, sig2, appl, op, exact )
NODE * sig, * sig2, * appl;
NODE * op;
boolean exact;
{
    register NODE * arg_sig;
    NODE * args;
    NODE * void_decl;
    NODE * arg_type;
    NODE * new_args;
    int num_args, num_params;

    if (appl != NIL) {
#       ifdef DEBUG
	    if (appl -> kind != APPLICATION) {
		dbgmsg("def_match: bad application\n");
		abort();
	    }
#       endif
	args = appl -> ap_args;
	void_decl = appl -> ap_void_decl;
    } else {
	args = NIL;
	void_decl = NIL;
    }
    if (sig == ERR_SIG)
	return(TRUE);
    if (sig -> kind == LETTERID || sig -> kind == OPRID) {
#       ifdef TRACE
	    printf("Replacing signature transparent identifier\n");
#       endif
	sig = sig_structure(sig);
    }
    if (sig2 != NIL && sig2 != ERR_SIG && comp_st(sig, sig2, NIL, NIL) != 0) {
#       ifdef TRACE
	    printf("Failed to match explicit signature\n");
#       endif
        return(FALSE);
    }
    if (args == NIL)
	return(TRUE);
    if (sig -> kind != FUNCSIGNATURE) {
#       ifdef TRACE
	    printf("Non-function id with a specified application\n");
#       endif
        return(FALSE);
    }
    num_args = length(args);
    num_params = length(sig -> fsig_param_list);
    if (num_args < num_params) {
        new_args = infer_args(args,
			      sig -> fsig_param_list,
			      void_decl, op);
#       ifdef TRACE
	    printf("Inferred arguments\n");
#       endif
    } else {
	new_args = args;
    }
    if (num_args > num_params ||
	num_args < num_params && new_args == NIL) {
#       ifdef TRACE
	    printf("Incorrect number of arguments\n");
#       endif
        return(FALSE);
    }
    begin_map2lists(p, new_args, q, sig -> fsig_param_list) {
	NODE * par_sig = q -> par_signature;
	NODE * s_par_sig;

	s_par_sig = subst(par_sig,
			  sig -> fsig_param_list,
			  new_args);
	arg_sig = p -> signature;
	if (arg_sig == ERR_SIG || s_par_sig == ERR_SIG) {
	    if (s_par_sig != NIL && s_par_sig != ERR_SIG) {
		vfree(s_par_sig);
	    }
	    return(TRUE);
	}
	/* If we're looking for an approximate match,  */
	/* make sure s_par_sig is an identifier only   */
	/* if it's a parameter identifier.             */
	  if (!exact) {
	    if (s_par_sig -> kind == LETTERID || s_par_sig -> kind == OPRID) {
		   s_par_sig = sig_structure(s_par_sig);
#                  ifdef TRACE
		     unparse_file = stdout;
		     printf("Changed parameter signature to ");
		     unparse(s_par_sig);
		     printf("\n");
#                  endif
	    }
	  }
	lock(s_par_sig);
	if (exact || s_par_sig -> kind != VALSIGNATURE) {
            if (!amatch(arg_sig, s_par_sig)) {
		vfree(unlock(s_par_sig));
#               ifdef TRACE
		    printf("Failed exact match\n");
#               endif
                return(FALSE);
     	    }
	} else {
	  if (arg_sig -> kind == LETTERID || arg_sig -> kind == OPRID) {
		arg_sig = sig_structure(arg_sig);
#               ifdef TRACE
		  unparse_file = stdout;
		  printf("Changed argument signature to ");
		  unparse(arg_sig);
		  printf("\n");
#               endif
	  }
	  switch (arg_sig -> kind) {
	    case TYPESIGNATURE:
		vfree(unlock(s_par_sig));
#               ifdef TRACE
		    printf("Attempted val-type match\n");
#               endif
		return(FALSE);
	    case FUNCSIGNATURE:
                if (!is_empty(arg_sig -> fsig_param_list)) {
		    vfree(unlock(s_par_sig));
#                   ifdef TRACE
			printf("Application coercion failed - args\n");
#                   endif
		    return(FALSE);
		}
		if (comp_st(arg_sig -> fsig_result_sig,
			    s_par_sig, NIL, NIL) != 0) {
		    vfree(unlock(s_par_sig));
#                   ifdef TRACE
			printf("Application coercion failed - result\n");
#                   endif
		    return(FALSE);
		}
		break;
	    case VARSIGNATURE:
		if (comp_st(arg_sig -> var_denotation,
			    s_par_sig -> val_denotation, NIL, NIL) != 0) {
		    vfree(unlock(s_par_sig));
#                   ifdef TRACE
			printf("ValueOf coercion failed\n");
#                   endif
		    return(FALSE);
		}
		break;
	    case VALSIGNATURE:
		if (comp_st(arg_sig -> val_denotation,
			    s_par_sig -> val_denotation, NIL, NIL) != 0) {
		    vfree(unlock(s_par_sig));
#                   ifdef TRACE
		      {
			extern NODE * diff_p, * diff_q;
			printf("Val signature comparison failed with diff_p = ");
			unparse_file = stdout;
			unparse(diff_p);
			printf("; diff_q = ");
			unparse(diff_q);
			printf("\n");
		      }
#                   endif
		    return(FALSE);
		}
		break;
	    IFDEBUG(
		default:
		    dbgmsg("def_match: bad argument signature:%X\n", arg_sig);
		    prtree(p);
		    abort();
	    )
	  }
	}
	vfree(unlock(s_par_sig));
    }end_map2lists;
    return(TRUE);
}
