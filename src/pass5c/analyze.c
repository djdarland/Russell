#define DEBUG

#define VERBOSE
#undef VERBOSE
/*
 *   This uses a simple iterative data-flow analysis algorithm to
 * set the NO_SL, NO_PUT, NO_CALLCC, and NO_CONSTR bits of all
 * fc_complexity fields in a program.  The "setup" routine
 * initially traverses the tree.  It computes approximations
 * to the fc_complexity fields based only on intraprocedural
 * approximation, and enters all function constructions into
 * a call graph.  The "solve" routine then finds fc_complexity
 * values such that the fc_complexity value at each procedure is
 * the bitwise and of its local value and of those for the procedures
 * it calls.
 *   We frequently use conservative approximations, especially
 * in the presence of higher order functions.
 *   These routines are designed to be called with and/or without
 * previously computed information about variable allocation and
 * declaration accessibility.  The NO_SL information will be less conservative
 * with such information.
 *   Nflag is ignored everywhere except in the top level routine "callc_callcc".
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codeutil.h"
# include "pass3/is_local.h"

# define RELEVANT (NO_SL | NO_PUT | NO_CALLCC | NO_CONSTR)

# ifdef VERBOSE
#   define IFVERBOSE(x) x
# else
#   define IFVERBOSE(x)
# endif

extern FILE * unparse_file;

/* Is p of the form id $ id $ ... $ id? */
boolean is_selected_id(p)
NODE *p;
{
    if (p -> kind != LETTERID && p -> kind != OPRID) {
	return(FALSE);
    } else if (p -> sel_type == NIL) {
	return(TRUE);
    } else {
	return(is_selected_id(p -> sel_type));
    }
}

# define setup(p) get_complexity(p, TRUE)

extern boolean Gflag; /* Generating intermediate code */
extern boolean Vflag; /* Print optimization info      */
extern boolean Oflag;
extern boolean hflag; /* No stack allocation.  Thus don't set NO_SL */
extern boolean Fflag; /* Simple functions cant access GF */
extern boolean Nflag; /* No Call/cc calls. */

extern boolean finished_accessible;
		      /* Finished accessibility and static link */
		      /* analysis, as well as the allocation    */
		      /* pass.                                  */

/* List of called functions */
struct cf {
    NODE * cf_fc;
    struct cf * cf_next;
};

/* List of all function constructions                        */
/* Note that separately compiled functions dont appear here, */
/* though they may appear in the list of called functions    */
struct fcs {
    NODE * fcs_fc;
    struct cf * fcs_called; /* list of called functions */
    struct fcs * fcs_next;
} * all_fcs, * all_fcs_tail;

NODE * whole_program;

NODE * outer_fc = NIL;  /* Outermost function construction, excl. */
			/* main function.                         */

void solve();

void print_fcs();

void free_fcs();

analyze(p)
NODE *p;
{
    whole_program = p;
    /* Initialize function list */
	all_fcs = all_fcs_tail = NIL;
    (void) setup(p);
#   ifdef VERBOSE
	printf("INITIAL CALL GRAPH INFO:\n");
	print_fcs();
	printf("\nFINAL CALL GRAPH INFO:\n");
#   endif
    solve();
#   ifdef VERBOSE
	print_fcs();
#   else
	if (Vflag) {
	    print_fcs();
	}
#   endif
    /* Deallocate table */
	free_fcs();
}

/* Determine whether an expression results in a call to put */
/* Assumes that analyze has previously been run.            */
boolean calls_put(p)
NODE * p;
{
    return((get_complexity(p, FALSE) & NO_PUT) == 0);
}

/* The same thing for call/cc */
boolean calls_callcc(p)
NODE * p;
{
    if (Nflag) {
	return(FALSE);
    } else {
	return((get_complexity(p, FALSE) & NO_CALLCC) == 0);
    }
}

/* Add a function to the end of the all_fcs list  */
/* Return a pointer to the newly created fcs node */
struct fcs * new_fc(p)
NODE *p;    /* Better be a function construction */
{
    struct fcs * result = (struct fcs *) malloc(sizeof (struct fcs));

    result -> fcs_fc = p;
    result -> fcs_next = NIL;
    result -> fcs_called = NIL;
    if (all_fcs == NIL)  {
	all_fcs = all_fcs_tail = result;
    } else {
	all_fcs_tail -> fcs_next = result;
	all_fcs_tail = result;
    }
    return(result);
}

/* Add a call from the function pointed to by fcs node p to the */
/* function construction q.                                     */
void add_call(p, q)
struct fcs *p;
NODE * q;
{
    struct cf * t = (struct cf *) malloc(sizeof (struct cf));
    
    t -> cf_fc = q;
    t -> cf_next = p -> fcs_called;
    p -> fcs_called = t;
}

/* Reevaluate the fc_complexity field of p, based on other fc_complexity */
/* information.  Return TRUE if it changed.                              */
boolean reeval(p)
struct fcs *p;
{
    int old_compl = (p -> fcs_fc -> fc_complexity) & RELEVANT;
    int new_compl = old_compl;
    struct cf * q;

    for (q = p -> fcs_called; q != NIL; q = q -> cf_next) {
	new_compl &= (q -> cf_fc -> fc_complexity) & RELEVANT;
    }                                                                         
    /* Preserve NO_CONSTR bit.  Preserve NO_SL bit if SL_ACC was used */      
    /* to determine its original value.                               */      
      if (Gflag && finished_accessible) {                                     
	new_compl |= (old_compl & (NO_CONSTR | NO_SL));                       
      } else {                                                                
	new_compl |= (old_compl & NO_CONSTR);
      }
    p -> fcs_fc -> fc_complexity &= ~RELEVANT;
    p -> fcs_fc -> fc_complexity |= new_compl;
    return(new_compl != old_compl);
}


/* Iteratively adjust various fc_complexity fields */
void solve()
{
    boolean changed = TRUE;
    struct fcs *p;

    while (changed) {
	changed = FALSE;
	for (p = all_fcs; p != NIL; p = p -> fcs_next) {
	    changed |= reeval(p);
	}
    }
}


/* Print the call graph and fc_complexity fields */
void print_fcs()
{
    struct fcs *p;
    struct cf *q;

    for (p = all_fcs; p != NIL; p = p -> fcs_next) {
	NODE * f = p -> fcs_fc;
	int compl = f -> fc_complexity;

	printf("function %s:\n", p -> fcs_fc -> fc_code_label);
	if (compl & NO_SL) {
	    printf(" - no real activ. record");
	}
	if (compl & NO_PUT) {
	    printf(" - no put call");
	}
	if (compl & NO_CALLCC) {
	    printf(" - no callcc call");
	}
	if (compl & NO_CONSTR) {
	    printf(" - no nested constrs");
	}
	printf("\n");
#       ifndef VERBOSE
	if (!Gflag)  /* -GV prints this stuff anyway */
#       endif
	  if (p -> fcs_called != NIL) {
	    printf("- known to call:\n");
	    for (q = p -> fcs_called; q != NIL; q = q -> cf_next) {
		printf("\t%s\n", q -> cf_fc -> fc_code_label);
	    }
	  }
    }
}

/* Free the call graph data structure */
void free_fcs()
{
    struct fcs *p;
    struct cf *q;

    p = all_fcs;
    while (p != NIL) {
	struct fcs * Op = p;

	/* Free called function nodes */
	  q = p -> fcs_called;
	  while (q != NIL) {
	      struct cf * Oq = q;
 
	      q = q -> cf_next;
	      free(Oq);
	  }

	/* Free this node */
	  p = p -> fcs_next;
	  free(Op);
    }
}


struct fcs * current_fcs;  /* fcs node for current function construction */

/* Determine whether a with list has a component matching given name */
boolean wl_has_comp(wl, id)
NODE * wl;
NODE * id;
{
    maplist(s, wl, { 
	if (s -> decl_id -> id_str_table_index
	    == id -> id_str_table_index) {
		IFVERBOSE(
		    printf("Found it as wlc\n");
		)
		return(TRUE);
	}
    });
#   ifdef VERBOSE
	printf("Didnt find it as wlc\n");
#   endif
    return(FALSE);
}

/* Return a type expression t1, s.t. (t1[...]...[...])$id is    */
/* identical to t$id.                                           */
/* Attempt to make t1 as informative as possible.               */
NODE * get_type_def(t, id)
NODE * t;
NODE * id;
{
    NODE * t1 = t;
    NODE * def;

    for (;;) {
#       ifdef VERBOSE
	    printf("get_type_def: type = ");
	    unparse_file = stdout;
	    unparse(t1);
	    printf("\n");
#       endif
	switch(t1 -> kind) {
	    case LETTERID:
	    case OPRID:
		if (t1 -> sel_type != NIL) {
#                   ifdef VERBOSE
			printf("type selected from type\n");
#                   endif
		    return(t1);
		}
		def = t1 -> id_last_definition;
		switch(def -> kind) {
		    case DECLARATION:
			if (def -> post_num < t1 -> pre_num) {
			    t1 = def -> decl_denotation;
			    continue;
			} else {
#                           ifdef VERBOSE
				printf("Possible cycle\n");
#                           endif
			    /* There may be a cycle */
			    return(t1);
			}
		    case MODPRIMARY:
			t1 = def;
			continue;
		    default:
#                       ifdef VERBOSE
			    printf("Definition defies analysis:");
			    unparse_file = stdout;
			    unparse(t1);
			    printf("\n");
#                       endif
			return(t1);
		}
	    case EXTENSION:
		t1 = t1 -> ext_denotation;
		continue;
	    case APPLICATION:
		t1 = t1 -> ap_operator;
		continue;
	    case MODPRIMARY:
		{
		    NODE * tm = t1 -> mp_type_modifier;

		    if (tm == NIL
			|| tm -> kind == HIDELIST
			|| tm -> kind == EXPORTLIST
			|| tm -> kind == WITHLIST
			   && !wl_has_comp(tm -> wl_component_list,id)){
			t1 = t1 -> mp_primary;
#                       ifdef VERBOSE
			    printf("Looking at modified type\n");
#                       endif
			continue;
		    } else {
#                       ifdef VERBOSE
			    printf("Complicated type mod:");
			    unparse_file = stdout;
			    unparse(t1);
			    printf("\n");
#                       endif
			return(t1);
		    }
		}
	    default:
#               ifdef VERBOSE
		    printf("Type expression defies analysis:");
		    unparse_file = stdout;
		    unparse(t1);
		    printf("\n");
#               endif
		return(t1);
	}
    }
}

/*
 * Determine whether the operator p is a, possible repeated, application
 * of, or selection from, a primitive type or function.  Accordingly,
 * return some combination of IS_PUT, IS_CALLCC, and IS_PRIMITIVE.
 * For our purposes, a function is primitive if it is not a construction,
 * and if we can tell if it is "put" or "Callcc".
 * Is_primitive may err on the side of treating a primitive as a
 * non-primitive.
 */
# define IS_PRIMITIVE 1
# define IS_PUT 2
# define IS_CALLCC 4
int is_primitive(p)
NODE *p;
{
    int i = 0; /* current approximation to result */
    int stp;
    NODE * def;
    int n_appls = 0;

    for (;;) {
	/* If there is "special" info, use it. */
	    if (p -> signature -> kind == FUNCSIGNATURE) {
		stp = special_tp(p -> signature -> fsig_special);
	    } else {
		stp = NOT_SPECIAL;
	    }
	    switch (stp) {
		case NOT_SPECIAL:
		  break;
		case STD_PUT:
		  return (IS_PUT | IS_PRIMITIVE);
		case STD_CALLCC:
		  return (IS_CALLCC | IS_PRIMITIVE);
		case PROD_PROJ:
		case UNION_PROJ:
		  /* Function itself is benign, but repeated */
		  /* application may cause problems          */
		  if (n_appls == 0) {
		    return(IS_PRIMITIVE);
		  } else {
		    return(0);
		  }
		default:
		  return(IS_PRIMITIVE);
	    } 
      switch (p -> kind) {
	case LETTERID:
	case OPRID:
	    if (p -> sel_type != NIL) {
		NODE * real_tp = get_type_def(p -> sel_type, p);
		extern long indx_put;

		if (real_tp -> kind == LETTERID) {
		    NODE * def = real_tp -> id_last_definition;
		    if (real_tp -> sel_type != NIL) {
			return(0);
		    } else if (def -> kind == PARAMETER
			       && def -> par_scope == whole_program) {
			i = IS_PRIMITIVE;
			if (p -> id_str_table_index == indx_put) {
			    i |= IS_PUT;
			}
			return(i);
		    } else {
			return(0);
		    }
		} else {
		    return(0);
		}
	    }
	    def = p -> id_last_definition;
	    switch(def -> kind) {
		case DECLARATION:
		    if (def -> post_num < p -> pre_num) {
			p = def -> decl_denotation;
			continue;
		    } else {
			/* There may be a cycle */
			return(0);
		    }
		case PARAMETER:
		    if (def -> par_scope == whole_program) {
			return (i | IS_PRIMITIVE);
		    } else {
			/* No idea where it came from */
			return(0);
		    }
		case RECORDCONSTRUCTION:
		case UNIONCONSTRUCTION:
		case PRODCONSTRUCTION:
		    /* Hopefully will never get here */
		    dbgmsg("Constr comp not marked special\n");
		    return(0);
		case MODPRIMARY:
		    /* Should usually be picked up elsewhere, */
		    /* either by special info or should be    */
		    /* construction.                          */
#                   ifdef VERBOSE
			printf("is_primitive saw MODPRIMARY\n");
#                   endif
		    return(0);
		case TYPESIGNATURE:
		default:
		    dbgmsg("is_primitive saw bad definition\n");
	    }

	case APPLICATION:
	    p = p -> ap_operator;
	    n_appls++;
	    continue;

	case FUNCCONSTR:
	    if (p -> fc_body -> kind == EXTERNDEF) {
		return(IS_PRIMITIVE | IS_PUT);
		    /* Assume it doesn't call Callcc */
	    }

	case REXTERNDEF:
	    /* Shouldn't matter.  Typically this will either be marked */
	    /* special, or we will have had a pointer to a dummy       */
	    /* construction enetered in the call graph.                */
	    return(0);

	default:
	    return(0);
      }
    }
}

/*
 *  Return the fc_complexity value appropriate for the subexpression p.
 *  If initial is true, then enter nested function constructions and
 *  calls into list;  if not, assume fc_complexity values are final.
 *  If initial is FALSE, the NO_SL bit will be very conservative, since
 *  we do not assume that we have correct context info.
 */
long get_complexity(p, initial)
boolean initial;
register NODE * p;
{
    NODE * v;
    int i;
    boolean is_global_id;
#   define SIMPLE (NO_SL | NO_PUT | NO_CALLCC | NO_CONSTR)

    if (p == NIL) return;

    if (p -> signature -> kind == SIGNATURESIG) {
	/* signatures are trivial to evaluate. */
	return(SIMPLE);
    }

    switch ( p -> kind ) {
	case LETTERID:
	case OPRID:
	    if (p -> sel_type != NIL) {
		return(get_complexity(p -> sel_type, initial));
	    }
	    /* Determine whether the identifier is a global that can be */
	    /* accessed through the global frame pointer.               */
	    /* Note that there may be LET activation records outside    */
	    /* the outermost function.  We can be much more precise     */
	    /* after allocation.                                        */
	    is_global_id = 
		(!Fflag && initial
		 && (finished_accessible 
		     && p -> id_last_definition -> level == 0
		    || p -> id_last_definition -> kind == PARAMETER
		       && (outer_fc == NIL || !is_local(p, outer_fc))));
	    if (is_global_id
		|| (initial && is_local(p, current_fcs -> fcs_fc))
		|| (Oflag && is_int_const(p))) {
#               ifdef VERBOSE
		    printf("Found simple id: %s, outer_fc = %X, scope:\n",
			   getname(p -> id_str_table_index),
			   outer_fc);
		    unparse_file = stdout;
		    if (p -> id_last_definition -> kind == PARAMETER) {
			unparse(p -> id_last_definition -> par_scope);
			printf("\n");
			if (initial) {
			    unparse(current_fcs -> fcs_fc);
			} else {
			    printf("unknown");
			}
			printf("\n");
		    }
#               endif
		return(SIMPLE);
	    } else {
#               ifdef VERBOSE
		    extern FILE * unparse_file;
		    printf("Found complex id: %s, outer_fc = %X, scope:\n",
			   getname(p -> id_str_table_index),
			   outer_fc);
		    unparse_file = stdout;
		    if (p -> id_last_definition -> kind == PARAMETER) {
			unparse(p -> id_last_definition -> par_scope);
			printf("\n");
			if (initial) {
			    unparse(current_fcs -> fcs_fc);
			} else {
			    printf("unknown");
			}
			printf("\n");
		    }
#               endif
		return(NO_PUT | NO_CALLCC | NO_CONSTR);
	    }

	case BLOCKDENOTATION :
		i = SIMPLE;
		maplist(s, p -> bld_declaration_list, {
    
		    ASSERT (s -> kind == DECLARATION,
			    "analyze: decl expected");
		    if (!Gflag) { i &= ~NO_SL; }
		    if (!finished_accessible) {
			i &= ~NO_SL;
			i &= get_complexity(s -> decl_denotation, initial);
		    } else if (s -> decl_needed) {
			if (!(s -> decl_special & (VAR_IN_REG | ID_IN_REG))) {
			  /* Need activation record space for identifier */
			  i &= ~NO_SL;
			}
			i &= get_complexity(s -> decl_denotation, initial);
		    } else {
                        /* Declaration is not executed.            */
			/* Thus r.h.s is irrelevant, unless it is  */
			/* a function called from within the block,*/
			/* in which case we deal with it at the    */
			/* call site.                              */
			if (initial) {
			    (void) get_complexity(s -> decl_denotation, TRUE);
			}
		    }
		});
		maplist (v,p->bld_den_seq, {
		    i &= get_complexity(v, initial);
		});
		return(i);

	case USELIST:
		i = SIMPLE;
		maplist(s, p -> usl_den_seq, {
		    i &= get_complexity(s, initial);
		});
		return(i);

		
	case APPLICATION:
		{
		  NODE * op_sig = p -> ap_operator -> signature;
		  boolean has_inline, inline_usable;
		  extern boolean is_id();
					
		  has_inline = (op_sig -> fsig_inline_code != NIL)
			       && is_id(p -> ap_operator);
			       /* operator has no side effects */
		  inline_usable =
			has_inline &&
			(Gflag || index(op_sig -> fsig_inline_code, '%') == 0);
			/* Can be used in Fcodegen environment */
		  i = SIMPLE;
		  /* Adjust for operator */
		    if ((has_inline && !inline_usable) ||
			(!has_inline)) {
			if (!Gflag || op_sig -> fsig_construction == NIL
			    || (!finished_accessible)
			    || (op_sig -> fsig_construction -> fc_complexity
				& SL_ACC)) {
			    i &= ~NO_SL;
			}
		    }
		    if (op_sig -> fsig_construction == NIL) {
			int is_prim = is_primitive(p -> ap_operator);
			if (is_prim & IS_PRIMITIVE) {
			    if (is_prim & IS_PUT) {
				i &= ~NO_PUT;
			    } else if (is_prim & IS_CALLCC) {
				i &= ~NO_CALLCC;
			    }
			} else {
			    i &= ~NO_PUT;
			    i &= ~NO_CALLCC;
			}
		    } else {
			if (initial) {
			  /* Add the call to call graph */
			    add_call(current_fcs,
				     op_sig -> fsig_construction);
			    if (is_descendant(op_sig -> fsig_construction,
					      current_fcs -> fcs_fc)) {
				/* Callee may be inside otherwise */
				/* unevaluated rhs of declaration */
				i &= ~NO_CONSTR;
			    }
			} else {
			    i &= op_sig -> fsig_construction
					-> fc_complexity;
			}
		    }
		  maplist(s, p -> ap_args, {
		    i &= get_complexity(s, initial);
		  });
		  if (!inline_usable
		      && (op_sig -> fsig_construction == NIL
			  || !(op_sig -> fsig_slink_known)
			  || !is_selected_id(p -> ap_operator) )) {
		    i &= get_complexity(p -> ap_operator, initial);
		  }
#                 ifdef VERBOSE
		    printf("Application (%x): ", i);
		    unparse_file = stdout;
		    unparse(p);
		    printf("\n");
#                 endif
		  return(i);
		}  

        case LOOPDENOTATION:
	case GUARDEDLIST:
		i = SIMPLE;
		if (!Gflag && p -> kind == LOOPDENOTATION) {
		    i &= ~NO_SL;
		    /* Simple code generator doesn't understand loops */
		}
		maplist(v,p->gl_list, {
		    i &= get_complexity(v, initial);
		});
#               ifdef VERBOSE
		    printf("Guarded commands (%x): ", i);
		    unparse_file = stdout;
		    unparse(p);
		    printf("\n");
#               endif
		return(i);

	case GUARDEDELEMENT:
		return(get_complexity(p -> ge_guard, initial)
		       & get_complexity(p -> ge_element, initial));
		break;

	case FUNCCONSTR:
		{
		    NODE * old_outer_fc = outer_fc;
		    struct fcs * old_fcs = current_fcs;
		    int f_compl;

		    if (initial) {
		      /* Fill in fc_code_label */
			if (!Gflag && p -> fc_body -> kind == EXTERNDEF) {
			  p -> fc_code_label = p -> fc_body -> ext_name;
			   /* The Vax code generator needs this so that   */
			   /* the correct routine can be called directly. */
			   /* The -G code generator generates a stub      */
			   /* named by the old fc_code_label field.       */
			}
#                       ifdef DEBUG
			  if (p -> fc_code_label == NIL) {
			    dbgmsg("get_complexity: Missing fc_code_label\n");
			  }
#                       endif
		    }

		    i = NO_PUT | NO_CALLCC;
		     
		    if (initial) {
		      if (p != whole_program && outer_fc == NIL) {
			outer_fc = p;
		      }
		      current_fcs = new_fc(p);
		      f_compl = get_complexity(p -> fc_body, initial);
		      p -> fc_complexity &= ~RELEVANT;
		      p -> fc_complexity |= f_compl;
		      if (hflag) {
			p -> fc_complexity &= ~NO_SL;
		      }
#                     ifdef DEBUG
			if (f_compl & (~RELEVANT)) {
			    dbgmsg("analyze: irrelevant bits set\n");
			    abort(f_compl);
			}
#                     endif
		      current_fcs = old_fcs;
		      outer_fc = old_outer_fc;
		    }

		    return(i);
		}

	case MODPRIMARY:
		i = SIMPLE;
		if (!Gflag) {
		    i &= ~NO_SL;
		}
		i &= get_complexity(p -> mp_primary, initial);
		if (p -> mp_type_modifier != NIL
		    && p -> mp_type_modifier -> kind == WITHLIST) {
		    i &= ~NO_SL;  /* Needs activation record space */
		    maplist (q, p -> mp_type_modifier -> wl_component_list, {
			i &= get_complexity(q -> decl_denotation, initial);
		    });
		}
		return(i);

	case ENUMERATION:
	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
		/* Subexpressions are not evaluated */
		return(SIMPLE & ~NO_SL);

	case QSTR:
	case UQSTR:
		{
		    NODE * tsig = p -> sel_type -> signature;
		    int maxlen;

		    ASSERT(tsig -> kind == TYPESIGNATURE,
			   "setup: bad string type");
		    if (tsig -> ts_string_max == -1) {
			maxlen = MAXSTRLEN;
		    } else {
			maxlen = tsig -> ts_string_max;
		    }
		    if (tsig -> ts_string_code != NIL
			&& tsig -> ts_element_code != NIL
			&& strlen(p -> str_string) <= maxlen) {
			    return(SIMPLE);
			    /* May be dubious on VAX ? */
		    } else {
			if (Gflag) {
			    return(get_complexity(p -> str_expansion, initial));
			} else {
			    return(get_complexity(p -> str_expansion, initial)
				   & (~NO_SL));
			}
		    }
		}

	case WORDELSE:
		return(SIMPLE);

	case EXTERNDEF:
		/* Setting NO_SL here is counterproductive, since */
		/* the resulting simple function is unlikely to   */
		/* ever get called.				  */
		return(NO_CALLCC);

	case REXTERNDEF:
		{
		    NODE * sig = p -> signature;

		    ASSERT(sig -> kind == FUNCSIGNATURE,
			   "setup: funny REXTERN");
		    /* Worry about it in calling context */
		    return(SIMPLE);
		}

	case RECORDCONSTRUCTION:
		i = SIMPLE & ~NO_SL;
                maplist(s, p -> rec_component_list, {
		  i &= get_complexity(s -> re_denotation, initial);
                });
		return(i);

        case EXTENSION:
		return(get_complexity(p -> ext_denotation, initial) & ~NO_SL);

        case RECORDELEMENT:
	case DECLARATION:
	case PARAMETER:
        case FUNCSIGNATURE:
	case LISTHEADER: /* should never get here */
	case VARSIGNATURE:
	case VALSIGNATURE:
	case TYPESIGNATURE:
	case TSCOMPONENT:
        case DEFCHARSIGS:
	case WITHLIST:
        case EXPORTLIST:
        case EXPORTELEMENT:
        case ALLCONSTANTS:
	case HIDELIST:
	case WORDCAND:
	case WORDCOR:
	default:
		dbgmsg("setup: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}
