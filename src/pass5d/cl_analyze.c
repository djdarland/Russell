#define DEBUG

#define VERBOSE
#undef VERBOSE

#define MAXFREEIDS 6  /* Maximum number of non-local (& non-global) */
		      /* free variables that can appear inside a    */
		      /* closure containing copied identifier       */
		      /* bindings rather than a static link.        */

/*
 *  Set the CP_GLOBALS, NO_AR_REFS, and DIR_REC bits in FUNCCONSTR
 * nodes.  (See stree/streedefs.h for definition.)  Build the fc_free_vars
 * lists in those function constructions that will use closures
 * with copied globals.
 *  Clear NO_SL bit if Oflag is clear and cflag is set.  Thus code size is
 *  minimized.
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "pass3/is_local.h"
# include "codegen.h"

extern FILE * unparse_file;

extern boolean fflag;

extern boolean Vflag;

extern boolean Oflag;

extern boolean cflag;

static int nestlevel;     /* Static nesting level of current function */

static NODE * current_fc;  /* Innermost construction currently being */
			   /* analyzed.                              */

static NODE * current_free_ids;   /* List of free identifiers for current */
				  /* function construction.               */

static int num_free_ids;    /* length(current_free_ids) */

static boolean ar_refs;     /* Pointer to current activation record */
			    /* may be needed.                       */

static boolean fast_calls;  /* Function contains calls to functions */
			    /* that do not have closures allocated  */

static boolean forward_refs;  /* current_fc contains forward references */
			      /* to non_local ids.                      */

/* Test whether                                                        */
/* compiling subexpression p might result in a reference to a nonlocal */
/* that has no previous nonlocal references,  and thus may not be      */
/* accessible, e.g. because it is in a register.                       */
boolean needs_new_nonl(p)
NODE *p;
{
    boolean result;

    switch(p -> kind) {
	case LETTERID:
	case OPRID:
	    if (p -> sel_type == NIL) {
		if (p -> id_last_definition -> kind == DECLARATION
		    && !(p -> id_last_definition -> decl_special 
			 & ID_IMPORTED)
		    && p -> id_last_definition -> level < nestlevel) {
		    result = TRUE;
		} else {
		    result = FALSE;
		}
	    } else {
		result = needs_new_nonl(p -> sel_type);
	    }
	    break;
	default:
	    result = TRUE;
    }
#   ifdef VERBOSE
	printf("needs_new_nonl(");
	unparse_file = stdout;
	unparse(p);
	printf(") returning %d level = %d, id level = %d\n",
	       result, nestlevel,
	       p -> id_last_definition? p -> id_last_definition -> level : 0);
#   endif
    return(result);
}

/* Add a (non-slected) identifier to current_free_vars, if there is  */
/* any chance the list will be needed.                               */
/* Do nothing if we can determine that the acces can't really occur. */
add_id_fv(id)
NODE *id;
{
    boolean already_there;

    if (id -> id_last_definition -> kind == DECLARATION
	&& !(id -> id_last_definition -> decl_special & ID_IMPORTED)) {
	/* The accessibility check decided that this access was impossible; */
	/* We'll believe it.                                                */
#           ifdef VERBOSE
		printf("Discarding bogus global reference.\n");
#           endif
	    return;
    }
    if (((current_fc -> fc_complexity) & NO_CONSTR) != 0
	&& num_free_ids <= MAXFREEIDS
	&& ! forward_refs) {
      /* Globals should potentially be copied.  */
      /* Add p to list of free variables.       */
	already_there = FALSE;
	maplist(s, current_free_ids, {
	    if (s -> id_last_definition -> pre_num
		== id -> id_last_definition -> pre_num)
		/* s and id are really the same */ {
		already_there = TRUE;
		break;
	    }
	});
	if (!already_there) {
#         ifdef VERBOSE
	    printf("Adding non-local %s for fn %s\n",
		   getname(id -> id_str_table_index),
		   current_fc -> fc_code_label);
#         endif
	  addright(current_free_ids, id);
	  num_free_ids++;
	}
    }
}


/*
 *  Set fields in the tree headed by p.  Lst is true if p is the
 * last subexpression executed as part of the current function.
 */
cl_analyze(p, lst)
register NODE * p;
boolean lst;
{
    NODE * v;
    int i;

    if (p == NIL) return;

    if (p -> signature -> kind == SIGNATURESIG) {
	/* signature evaluation doesn't reference anything. */
	return;
    }

    switch ( p -> kind ) {
	case LETTERID:
	case OPRID:
		if (p -> sel_type != NIL) {
		    cl_analyze(p -> sel_type, FALSE);
		} else {
		    if (!is_local(p, current_fc) &&
			(p -> id_last_definition -> level != 0)) {
			add_id_fv(p);
			/* Is this a forward reference? */
			    if (current_fc -> ar_static_level
				> (p -> id_last_definition -> level) + 1) {
				/* There is an intervening function */
				/* construction.  This implies      */
				/* that id_forward_ref is accurate  */
				/* even in the presence of copied   */
				/* bindings.  (see decl_sort.c)     */
				    if (p -> id_forward_ref) {
					forward_refs = TRUE;
				    }
			    } else {
				/* Use a simple, very conservative test: */
				if (p -> id_last_definition -> post_num
				    > current_fc -> post_num) {
				    forward_refs = TRUE;
				}
			    }
		    }
		}
		break;

	case BLOCKDENOTATION :
		maplist (v, p->bld_declaration_list, {
		    ASSERT (v->kind == DECLARATION,
			    "cl_analyze: decl expected");
		    cl_analyze(v-> decl_denotation, FALSE);
		});
		maplist (v,p->bld_den_seq, {
		    if (v == last(p -> bld_den_seq)) {
			cl_analyze(v, lst);
		    } else {
			cl_analyze(v, FALSE);
		    }
		});
		break;

	case USELIST:
		maplist(s, p -> usl_den_seq, {
		    if (s == last(p -> usl_den_seq)) {
			cl_analyze(s, lst);
		    } else {
			cl_analyze(s, FALSE);
		    }
		});
		break;
		
	case APPLICATION:
		{
		  NODE * constr = p -> ap_operator -> signature
				      -> fsig_construction;
		  /* Set DIR_REC bit, if appropriate */
		    if (lst && constr != NIL
			&& constr -> pre_num == current_fc -> pre_num
			&& constr -> pre_num != 0 /* bogus, e.g. external */) {
			current_fc -> fc_complexity |= DIR_REC;
#                       ifdef VERBOSE
			    printf("Directly recursive:\n");
			    unparse_file = stdout;
			    unparse(p);
			    printf("\nSetting recursion bit in %s (%d)\n",
				   current_fc -> fc_code_label,
				   current_fc -> pre_num);
#                       endif
		    }
		  /* Check if this call could require operator value */
		  /* cl_analyze the operator if necessary.           */
		    {
		      extern boolean is_id();
		      NODE * op_sig = p -> ap_operator -> signature;
		      boolean is_ident = is_id(p -> ap_operator);
		      boolean no_op_val =
			     is_ident
			     && (op_sig -> fsig_inline_code != NIL
				 || (constr != NIL &&
				     (constr -> ar_static_level == 1
				      || constr -> fc_complexity & NO_SL)));
		      if (!no_op_val) {
			  if (is_ident
			      && constr != NIL
			      && op_sig -> fsig_slink_known) {
			    if (constr -> ar_static_level > 1
				&& (constr -> fc_complexity & SL_ACC)
				&& !(constr -> fc_complexity & NO_SL)) {
				/* Call code requires static link */
				if(!(constr -> fc_complexity & NEED_CL)
				   || needs_new_nonl(p -> ap_operator)) {
				  /* Already made choices that make it        */
				  /* impossible to compute closure for callee.*/
				  /* If we copied globals, we couldn't get    */
				  /* static link, thus there would be no way  */
				  /* to compile the call.                     */
#                                 ifdef VERBOSE
				    unparse_file = stdout;
				    printf("\nfast call: ");
				    unparse(p);
				    printf("\n");
#                                 endif
				  fast_calls = TRUE;
				} else {
				  /* I do need the binding */
				    cl_analyze(p -> ap_operator, FALSE);
				}
			    }  /* Otherwise I don't need the binding */
			  } else {
			      cl_analyze(p -> ap_operator, FALSE);
			  }
		      }
		    }
		}
		maplist(s, p -> ap_args, {
		    cl_analyze(s, FALSE);
		});
		break;

        case LOOPDENOTATION:
		maplist(v,p->gl_list, {
		    cl_analyze(v, FALSE);
		});
		break;

	case GUARDEDLIST:
		maplist(v,p->gl_list, {
		    cl_analyze(v, lst);
		});
		break;

	case GUARDEDELEMENT:
		cl_analyze(p -> ge_guard, FALSE);
		cl_analyze(p -> ge_element, lst);
		break;

	case FUNCCONSTR:
		{
		    NODE * old_fc = current_fc;
		    NODE * old_free_ids = current_free_ids;
		    int old_num_free_ids = num_free_ids;
		    boolean old_ar_refs = ar_refs;
		    boolean old_forward_refs = forward_refs;
		    boolean old_fast_calls = fast_calls;

		    boolean need_closure;           /* new function */
		    boolean cp_globals;             /* new function */

		    need_closure = ((p -> fc_complexity & NEED_CL) != 0);

		    current_fc = p;
		    nestlevel = current_fc -> ar_static_level;
		    current_free_ids = lock(emptylist());
		    num_free_ids = 0;
		    ar_refs = FALSE;
		    forward_refs = FALSE;
		    fast_calls = FALSE;

		    cl_analyze(p -> fc_body, TRUE);

		    cp_globals = ((p -> fc_complexity & NO_CONSTR)
				  && (num_free_ids < MAXFREEIDS)
				  && need_closure
				  && !(p -> fc_complexity
				       & (DIR_CALL | NESTED_AR_BLOCK))
				  && !forward_refs
				  && !fast_calls
				  && p -> ar_static_level != 0);
		    if (Vflag && forward_refs) {
			printf("%s contains embedded forward references\n",
			       p -> fc_code_label);
		    }
		    if (Vflag && fast_calls) {
			printf("%s contains calls requiring static link\n",
			       p -> fc_code_label);
		    }
		    if (cp_globals) {
			p -> fc_complexity |= CP_GLOBALS;
			p -> fc_free_vars = current_free_ids;
				/* not ref counted */
			if (Vflag) {
			    printf("%s closure contains copies of %d non-locals\n",
				   p -> fc_code_label,
				   length(current_free_ids));
			}
			/* Make sure that none of the non-locals are vars */
			/* allocated directly in activation record.       */
			maplist(s, current_free_ids, {
			    if (s -> id_last_definition -> kind == DECLARATION
				&& (s -> id_last_definition -> decl_special
				    & VAR_ON_STACK)) {
				s -> id_last_definition -> decl_special
					&= ~VAR_ON_STACK;
				if (Vflag) {
				    printf("\tForcing heap allocation of %s\n",
					   getname(s -> id_str_table_index));
				}
			    }
			});
		    } else {
			if (current_free_ids != NIL) {
			    vfree(unlock(current_free_ids));
			}
		    }
		    if (!ar_refs) {
			p -> fc_complexity |= NO_AR_REFS;
		    }

		    if (cflag && !Oflag) {
			p -> fc_complexity &= (~NO_SL);
			/* Otherwise we would probably generate code twice */
		    }

		    if (cp_globals) {
			ar_refs = old_ar_refs;
		    } else if (!need_closure) {
			ar_refs |= old_ar_refs;
		    } else {
			if (Vflag && old_fc != NIL) {
			    printf("%s may need a.r. pointer for %s\n",
				   p -> fc_code_label,
				   old_fc -> fc_code_label);
			}
			ar_refs = TRUE;
		    }

		    if (Vflag && !(p -> fc_complexity & SL_ACC)) {
			printf("%s contains no indirections through slink\n",
			       p -> fc_code_label);
		    }

		    current_fc = old_fc;
		    if (current_fc != NIL) {
		      nestlevel = current_fc -> ar_static_level;
		    }
		    current_free_ids = old_free_ids;
		    num_free_ids = old_num_free_ids;
		    forward_refs = old_forward_refs;
		    fast_calls = old_fast_calls;
		    break;
		}

	case MODPRIMARY:
		cl_analyze(p -> mp_primary, FALSE);
		if (p -> mp_type_modifier != NIL
		    && p -> mp_type_modifier -> kind == WITHLIST) {
		    maplist (q, p -> mp_type_modifier -> wl_component_list, {
			cl_analyze(q -> decl_denotation, FALSE);
		    });
		}
		break;

	case ENUMERATION:
	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
		/* Subexpressions are not evaluated */
		break;

	case QSTR:
	case UQSTR:
		{
		    NODE * tsig = p -> sel_type -> signature;

		    ASSERT(tsig -> kind == TYPESIGNATURE,
			   "cl_analyze: bad string type");
		    if (tsig -> ts_string_code != NIL
			&& tsig -> ts_element_code != NIL
			&& strlen(p -> str_string) <= MAXSTRLEN) {
			    break;
			    /* May be dubious on VAX ? */
		    } else {
			cl_analyze(p -> str_expansion, lst);
		    }
		    break;
		}

	case WORDELSE:
		break;

	case EXTERNDEF:
		break;

	case REXTERNDEF:
		break;

	case RECORDCONSTRUCTION:
		maplist(s, p -> rec_component_list, {
		    cl_analyze(s -> re_denotation, FALSE);
                });
		break;

        case EXTENSION:
		cl_analyze(p -> ext_denotation, FALSE);
		break;

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
		dbgmsg("cl_analyze: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}
