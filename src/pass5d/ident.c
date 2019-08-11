# define DEBUG
# define OBSOLETE
# undef OBSOLETE
/* Here we assume that reference counts are not being maintained */

/* 
 *   Compile code to fetch the value associated with an identifier.
 *  This is nontrivial due to the variety of closure styles,
 *  activation record styles, and the possibility of erroneous
 *  forward references in declaration blocks.
 *
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

FILE * Goutfile;

extern int yydebug;
extern int yynerrs;

extern boolean Vflag;  /* Optimization info */

extern boolean Oflag;  /* Enable optimization */

extern boolean Fflag;  /* parameters copied on entry */

extern boolean sl_available;  /* Static link available in      */
			      /* current function construction */

extern int avail_loc;     /* next available location number */

extern char str_code_buf[];       /* used for strings, "special" fns */
                                  /* and by find_inline              */
                                  /* and for object file names       */
				  /* and to assemble labels          */

extern NODE * Gcurrent;      /* Current function construction */

extern int Glevel;           /* Static nesting level of current function */

extern boolean copied_globals;

extern int n_globals;

extern boolean is_int_const();

char * Gnewlabel();

extern FILE * unparse_file;

/* Return either NIL or an expression q such that p is */
/* equivalent to q. P is different from q only if p is */
/* a (possibly selected) identifier.  The goal is to   */
/* make q as informative as possible.                  */
/* P and q are interchangeable only in that if q is a  */
/* constant, then p is the same constant.  If q is a   */
/* function, then its optimization information (except */
/* for static link availability) also applies to p.    */
/* This can't be done earlier, since we take constant  */
/* parameters into consideration.                      */

/* q may have type components that have been forgotten */
/* in p.                                               */
NODE * equiv_expr(p)
NODE * p;
{
    NODE * d;
    NODE * q;
    while (p -> kind == LETTERID || p -> kind == OPRID) {
      if (p -> sel_type == NIL ) {
	d = p -> id_last_definition;
	switch(d -> kind) {
	    case DECLARATION:
		q = d -> decl_denotation;
		break;
	    case PARAMETER:
		if (is_real_def(d -> par_only_def)) {
		    q = d -> par_only_def;
		} else {
		    return(p);
		}
		break;
	    default:
		return(p);
	}
	if (q -> kind != OPRID && q -> kind != LETTERID
	    || q -> sel_type != NIL
	    || q -> id_last_definition -> pre_num < d -> pre_num) {
	    /* Replacing p by q will not lead to a cycle */
	    p = q;
	} else {
	    return(q);
	}
      } else /* p -> sel_type != NIL */ {
	NODE * nt = equiv_expr(p -> sel_type);
	NODE * new_id;
    
	if (p -> signature -> kind == FUNCSIGNATURE
	    && (p -> signature -> fsig_construction != NIL
		|| p -> signature -> fsig_inline_code != NIL)) {
	    /* P already has good information; no reason to continue */
	    return(p);
	}
	nt = equiv_expr(p -> sel_type);
	if (nt == p -> sel_type) {
	    return(p);
	} else {
	    if (!is_unique(nt -> signature, p -> id_str_table_index)) {
		/* Too hard to find the correct component   */
		/* since signatures may have changed due to */
		/* substitution.                            */
		return(p);
	    }
	    new_id = copynode(p);
	    new_id -> sel_type = nt;
	    new_id -> signature = getcomp(nt -> signature, new_id,
					  p -> sel_type, NIL, NIL, NIL, TRUE);
	    new_id -> sig_done = SIG_DONE;
	    if (new_id -> signature == NIL) {
		dbgmsg("equiv_expr: getcomp failed\n");
		return(p);
	    } else {
		new_id -> signature = sig_structure(new_id -> signature);
		return(new_id);
	    }
	}
      }
    }
    return(p);
}

Gident(p, rloc)
NODE *p;
int rloc;
{
    register NODE * v;
    int display_reg; /* location used for a.r. pointer */
		    
    if (Oflag? is_int_const(equiv_expr(p)) : is_int_const(p)) {
	extern long int_const_val;

	gen2(LDN, int_const_val, rloc);
	return;
    }
    v = p -> id_last_definition;
    if (p -> sel_type == NIL) {
	ASSERT2 (v != NIL, "ident: id %s not declared\n", 
		 getname(p -> id_str_table_index)
	);
	ASSERT2 (v -> kind == DECLARATION 
		 || v -> kind == PARAMETER
		 || v -> kind == MODPRIMARY
		    && v -> mp_type_modifier -> kind == WITHLIST,
		 "ident: id %x not declaration or parameter\n",v);
	ASSERT (v -> kind != DECLARATION
		|| !(v -> decl_special & VAR_IN_REG),
		"Taking address of register variable\n");
	if (v -> kind == DECLARATION && (v -> decl_special & ID_IN_REG)) {
#           ifdef DEBUG
	    if(v -> level < Gcurrent -> ar_static_level) {
		dbgmsg("Nonlocal register access\n");
		fprintf(stderr,
			"- to %s declared at level %d from level %d; declaration:\n",
			getname(p -> id_str_table_index), v -> level, Glevel);
		unparse_file = stderr;
		if (v -> kind == DECLARATION) {
		    unparse(v -> decl_denotation);
		} else {
		    unparse(v);
		}
		fprintf(stderr, "\n");
		errmsg0(p, "Nonlocal access");
		abort(p, v);
	    }
#           endif
	    gen2(MOV, v -> displacement, rloc);
	} else if (sl_available || v -> level == 0) {
	  if (!copied_globals || v -> level == 0 || v -> level == Glevel) {
	    ASSERT2(v -> level == 0 || v -> level >= Gcurrent -> ar_static_level
		   || (Gcurrent -> fc_complexity & SL_ACC),
		   "Indirecting through nonexistent static link for %s\n",
		   getname(p -> id_str_table_index));
#           ifdef DEBUG
		if (v -> level > Glevel) {
		    dbgmsg("Accessing identifier %s at nesting level %d from level %d\n",
			   getname(p -> id_str_table_index), v -> level, Glevel);
		    abort();
		}
#           endif
	    DISPLAY ( display_reg, v -> level);
	    if (v -> kind == DECLARATION
		&& (v -> decl_special & VAR_ON_STACK)) {
		switch(v -> displacement) {
		  case 1:
		    gen3(ADP, display_reg, C1, rloc);
		    break;
		  case 2:
		    gen3(ADP, display_reg, C2, rloc);
		    break;
		  case 3:
		    gen3(ADP, display_reg, C3, rloc);
		    break;
		  default:
		    gen2(LDN, v -> displacement, T1);
		    gen3(ADP, display_reg, T1, rloc);
		    gen1(UDC, T1);
		}
	    } else {
		gen3 (LDI, display_reg, v->displacement, rloc);
	    }
	    UNDISPLAY ( display_reg );
	  } else /* access to a nonlocal id that has been copied */ {
	    switch(n_globals) {
		case 0:
		    dbgmsg("ident: unexpected non-local: %s\n",
			   getname(p -> id_str_table_index));
		    fprintf(stderr, "Current function: %s\n",
			    Gcurrent -> fc_code_label);
		    abort();
		case 1:
		    /* Identifier binding is stored in "static link" */
		    gen3(LDI, AR, 0, rloc);
		    break;
		default:
		    /* *AR contains pointer to closure.  Binding */
		    /* is stored at offset 2 + index in list     */
		    {
			int cl_ptr = avail_loc++;
			int index;   /* Index in list of free ids */

			index = 1;  /* For first non-local */
			maplist(s, Gcurrent -> fc_free_vars, {
			   if (s -> id_last_definition -> pre_num
			       == v -> pre_num) {
				break;
			    }
			    index ++;
			});
#			ifdef DEBUG
			    if(index > n_globals) {
				dbgmsg("ident: couldn't find non-local id %s\n",
				       getname(p -> id_str_table_index));
				printf("Free variables for %s are:\n",
				       Gcurrent -> fc_code_label);
				maplist(s, Gcurrent -> fc_free_vars, {
				    printf("\t%s\n",
				           getname(s -> id_str_table_index));
				});
				abort("couldnt find free var");
			    }
#			endif    
			gen2(DCL, cl_ptr, DCL_ADDR);
			gen3(LDI, AR, 0, cl_ptr);
			gen3(LDI, cl_ptr, 2+index, rloc);
			gen1(UDC, cl_ptr);
		    }
	    }
	  }
	} else /* no static link, no real activation record */ {
	    extern int first_param_loc;
	    /* Must be be parameter */
#           ifdef OBSOLETE
		gen3(LDI, FP, v->displacement+1, rloc);
#           else
		if (first_param_loc != 0) {
		    /* arguments are actually in temporaries */
		    int loc = first_param_loc + v -> displacement - 1;

		    ASSERT(Fflag || Gcurrent -> fc_complexity & DIR_REC,
			   "ident: bad first_param_loc\n");
		    gen2(MOV, loc, rloc);
		} else {
		    gen2(GAR, v -> displacement, rloc);
		}
#           endif
	}
    } else {
	int type_loc = avail_loc++;

	gen2(DCL, type_loc, DCL_ADDR);
	Gexpression (p -> sel_type, type_loc, FALSE);
	gen3(LDI, type_loc, p -> sel_index, rloc);
	gen1(UDC, type_loc);
    }
    if (p -> id_forward_ref) {
      if (p -> signature -> kind == VALSIGNATURE) {
	NODE * type_den;
	NODE * type_sig;
	NODE * new_sig;
	extern NODE * id_New;
	/* May not be represented as a pointer */
	/* May not be possible to check for    */
	/* undefined value                     */
	/* Try to find out whether it is a ptr: */
	type_den = p -> signature -> val_denotation;
	tl_findsig(type_den, FALSE);
	type_sig = type_den -> signature;
	if (type_sig == ERR_SIG) {
	    exit(1);
	    /* Nearly impossible */
	}
	new_sig = getcomp(type_sig, id_New, NIL,
			  NIL, NIL, NIL, FALSE);
#       ifdef VERBOSE
	    unparse_file = stdout;
	    printf("Trying to find rep of:\n");
	    unparse(type_den);
	    printf("\nWith signature:\n");
	    unparse(type_sig);
	    printf("\n");
	    if (new_sig == NIL) {
		printf("No New function\n");
	    } else {
		printf("Special: %X\n", new_sig -> fsig_special);
	    }
#       endif
	if (new_sig == NIL
	    || new_sig -> kind != FUNCSIGNATURE
	    || special_tp(new_sig -> fsig_special) != PTR_NEW) {
	  errmsg1(p, "Warning: unchecked possible forward reference to %s",
		  getname(p -> id_str_table_index));
	  /* Only a warning */
	    yynerrs--;
	} else {
	  goto check_undef;
	}
      } else {
	char * tmp_lbl;
check_undef:
	/* Check that the value is defined */
	  tmp_lbl = Gnewlabel("L");
	  gen2(HINT, OPT, 4);
	  gen3(EQI, rloc, UN, TL);
	  genl(BRF, tmp_lbl);
	  genl(ERR, "_forward_error");
	  genl(LBL, tmp_lbl);
      }
    }
}
