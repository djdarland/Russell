/* Fill in INSIDE_LOOP and CONTAINS_CLOSURE bits for blocks */
/* These are used by allocate to decide whether to promote  */
/* declarations to surrounding blocks, and whether to reuse */
/* activation record slots.                                 */
/* ASSUMES GC instead of reference counting.                */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "../pass3/is_local.h"

extern FILE * unparse_file;

extern boolean Vflag;
extern boolean Oflag;
extern boolean Nflag;

extern NODE * insrtptr;     /* Root of user program */

static boolean surr_loop = FALSE;       /* There is a surrounding loop, */
					/* w/o intervening function or  */
                                        /* block.                       */
            /* Note that for our purposes, string selection types are in */
            /* a loop, since theyt are represented only once, but may    */
            /* be repeatedly evaluated.                                  */


static boolean callcc_call = FALSE;     /* Call to current function may save */
					/* continuation.                     */

/* Fill in the information for tree rooted at *p.  Return true iff a */
/* reference to the environment can escape.                          */
/* We pass a reference to a node pointer, so that we can slightly    */
/* modify *p to make this easier.  We only replace MODPRIMARY        */
/* nodes.                                                            */
boolean bld_analyze(arg)
NODE **arg;
{
    NODE * p = *arg;
    boolean r = FALSE;

    /* We claim that p points to an expression node */
    if (p -> signature -> kind == SIGNATURESIG) return(FALSE);

    switch(p -> kind) {
	case BLOCKDENOTATION:
	    {
		boolean Osurr_loop = surr_loop;
		boolean nontr_decl = FALSE;
		boolean contains_closure = FALSE;
		extern boolean calls_callcc();

		maplist(s, p -> bld_declaration_list, {
		    if (s -> decl_needed) {
			/* Will need either act. record or register space */
			nontr_decl = TRUE;
		    }
		});
		if (nontr_decl) {
		    /* This one's for real ... */
		    if (surr_loop || callcc_call) {
			p -> bld_flags |= INSIDE_LOOP;
		    }
		    surr_loop = FALSE;
		}
		maplist(v, p -> bld_declaration_list, {
		    contains_closure |= bld_analyze(&(v -> decl_denotation));
		});
		maplistp(v, p -> bld_den_seq, {
		    contains_closure |= bld_analyze(v);
		});
		if (contains_closure) {
		    p -> bld_flags |= CONTAINS_CLOSURE;
		}
		if (callcc_call) {
		    if (Oflag) {
			if (calls_callcc(p)) {
			    p -> bld_flags |= CALLCC_CALL;
			}
		    } else if (!Nflag) {
			p -> bld_flags |= CALLCC_CALL;
		    }
		}
		if ((p -> bld_flags & (CONTAINS_CLOSURE | CALLCC_CALL))
		     && (p -> bld_flags & INSIDE_LOOP)
		     && is_descendant(p, insrtptr)) {
		    p -> bld_flags |= REQUIRES_AR;
		    /* We claim that NO_SL cannot ever be set for the */
		    /* surrounding function, since there is either a  */
		    /* nested closure, or we cannot determine that    */
		    /* there is no Call/cc call.                      */
		    if (Vflag) {
			findvl(p -> vlineno);
			printf("Block requires a.r. (file: %s, line: %d)\n",
			       getname(getfn()), getrl());
			if (Osurr_loop) {
			    printf("\t- explicit surrounding loop\n");
			}
			if (callcc_call) {
			    printf("\t- possible Callcc call in function may create loop\n");
			}
			if (p -> bld_flags & CONTAINS_CLOSURE) {
			    printf("\t- embedded closure\n");
			}
			if (p -> bld_flags & CALLCC_CALL) {
			    printf("\t- possible embedded Callcc call\n");
			}
		    }
		}
		r |= contains_closure;
		surr_loop = Osurr_loop;
	    }
	    break;

	case MODPRIMARY:
	    if (p -> mp_type_modifier != NIL
		&& p -> mp_type_modifier -> kind == WITHLIST) {
		boolean Osurr_loop = surr_loop;
		boolean captures_cont;

		surr_loop = FALSE;
		maplist(v, p -> mp_type_modifier -> wl_component_list, {
		    r |= bld_analyze(&(v -> decl_denotation));
		});
		if (callcc_call) {
		    if (Oflag) {
			captures_cont = calls_callcc(p);
		    } else {
			if (Nflag) {
			    captures_cont = FALSE;
			} else {
			    captures_cont = TRUE;
			}
		    }
		} else {
		    captures_cont = FALSE;
		}
		if (Osurr_loop || callcc_call) {
		    /* Introduce a surrounding block with its own */
		    /* activation record to hold the reference    */
		    /* to the new type.                           */
		    NODE * block = mknode(BLOCKDENOTATION,
					  emptylist(), mklist(p, -1));

		    block -> vlineno = p -> vlineno;
		    block -> signature = p -> signature;
		    block -> sig_done = SIG_DONE;
		    if ((r || captures_cont) && p -> mp_needed) {
			block -> bld_flags = INSIDE_LOOP;
			if (is_descendant(p, insrtptr)) {
			  block -> bld_flags |= REQUIRES_AR;
			  if (Vflag) {
			    findvl(p -> vlineno);
			    printf("Introducing block with a.r. for type modification (file: %s, line: %d)\n",
				   getname(getfn()), getrl());
			    printf("\t - type modification: ");
			    unparse_file = stdout;
			    unparse(p);
			    printf("\n");
			    if (Osurr_loop) {
				printf("\t- explicit surrounding loop\n");
			    }
			    if (callcc_call) {
				printf("\t- possible Callcc call in function may create loop\n");
			    }
			    if (r) {
				printf("\t- embedded closure\n");
			    }
			    if (captures_cont) {
				printf("\t- possible embedded Callcc call\n");
			    }
			  }
			}
			if (r) {
			    block -> bld_flags |= CONTAINS_CLOSURE;
			}
			if (captures_cont) {
			    block -> bld_flags |= CALLCC_CALL;
			}
		    }
		    *arg = block;
		}
		surr_loop = Osurr_loop;
	    }
	    r |= bld_analyze(&(p -> mp_primary));
	    break;

	case APPLICATION:
	    r = bld_analyze(&(p -> ap_operator));
	    maplistp(v, p -> ap_args, r |= bld_analyze(v));
	    break;

	case LOOPDENOTATION:
	    {
		boolean Osurr_loop = surr_loop;

		surr_loop = TRUE;
		maplist(v, p -> gl_list, {
		    r |= bld_analyze(&(v -> ge_guard));
		    r |= bld_analyze(&(v -> ge_element));
		});
		surr_loop = Osurr_loop;
	    }
            break;

        case QSTR:
        case UQSTR:
            {
                boolean Osurr_loop = surr_loop;

                surr_loop = TRUE;
                bld_analyze(&(p -> sel_type));
                surr_loop = Osurr_loop;
            }
            break;

	case GUARDEDLIST:
	    maplist(v, p-> gl_list, {
		r |= bld_analyze(&(v -> ge_guard));
		r |= bld_analyze(&(v -> ge_element));
	    });
	    break;

	case OPRID:
	case LETTERID:
	    if (p -> sel_type != NIL) {
		r |= bld_analyze(&(p -> sel_type));
	    }
	    break;

	case FUNCCONSTR:
	    if (p -> fc_body -> kind == EXTERNDEF) return;
	    {
		boolean Ocallcc_call = callcc_call;
		boolean Osurr_loop = surr_loop;

		callcc_call = !(p -> fc_complexity & NO_CALLCC);
		if (Nflag) callcc_call = FALSE;
		surr_loop = FALSE;

		r |= (p -> fc_complexity & NEED_CL);
		r |= bld_analyze(&(p -> fc_body));

		callcc_call = Ocallcc_call;
		surr_loop = Osurr_loop;
	    }
	    break;

	case REXTERNDEF:
	    break;

	case USELIST:
	    maplistp(q, p -> usl_den_seq, r |= bld_analyze(q));
	    break;

	case EXTENSION:
	    r = bld_analyze(&(p -> ext_denotation));
	    break;

	case RECORDCONSTRUCTION:
	    maplist(s, p -> rec_component_list, {
		r |= bld_analyze(&(s -> re_denotation));
	    });
	    break;

	case WORDELSE:
	    break;

	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
	case ENUMERATION:
	    /* No evaluated subexpressions. */
	    break;

	default:
	    dbgmsg("bld_analyze: Bad kind: %d\n", p -> kind);
    }
    return(r);
}
