# define DEBUG

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "sigs.h"

# include "stree/is_ptr.h"

NODE * curr_tsig = NIL;  /* signature of anonymous local type ids */

boolean in_sig;  /* checking inside a signature */

# define ERR_NODE_DEFINED
NODE * err_node = NIL;  /* node to be used for error message in lieu */
			/* of current node.  Used by errmsg macros   */
char * err_msg;         /* message to be used in lieu of usual one   */

extern NODE * val_Boolean,
	    * val_Void,
	    * var_Void;

extern NODE * sig_Signature;

extern int yynerrs;

extern FILE * unparse_file;

/*
 *  checksigs(p,dont_coerce)
 *
 *  Do signature checking on the expression subtree headed by p
 *  Dont_coerce indicates that that the expression occurs in a context
 * in which the result will be discarded anyway.
 *  It is also insured that all signature and optimization info
 * needed to generate code for p will be attached to the nodes in the
 * subtree.
 */


checksigs(p,dont_coerce)
NODE * p;
{
    register int knd;
    NODE * op;  /* operator in application */

    if (p == ERR_SIG) return;
    knd = p -> kind;

    switch(knd) {
        case OPRID:
	case LETTERID:
	    if (p -> sel_type != NIL) {
		/* find type signature first.  This guarantees that */
		/* sel_index will be filled in                      */
		checksigs(p -> sel_type, FALSE);
	    }
	    tl_findsig(p,dont_coerce);
	    if (p -> sel_type != NIL && in_sig
                && p -> sel_type -> signature != ERR_SIG) {
              /* check for ambiguous selection */
		if (!is_unique(p -> sel_type -> signature,
			       p -> id_str_table_index)) {
		    errmsg0(p, "ambiguous selection in signature");
		}
	    }
	    break;

	case QSTR:
	case UQSTR:
	    {
		NODE * sig;
		int maxlen;
		tl_findsig(p, dont_coerce);
		if (p -> signature != ERR_SIG) {
		  sig = p -> sel_type -> signature;
		  if (sig -> ts_string_max == -1) {
		      maxlen = MAXSTRLEN;
		  } else {
		      maxlen = sig -> ts_string_max;
		  }
		  if (sig -> ts_string_code == NIL
		      || sig -> ts_element_code == NIL
		      || strlen(p -> str_string) > maxlen
		      || p -> sel_type -> kind != LETTERID
		      || p -> sel_type -> sel_type != NIL) {
		    checksigs(p -> str_expansion, dont_coerce);
		  } else {
		    checksigs(p -> sel_type, FALSE);
		  }
		}
	    }
            break;

        case FUNCCONSTR:
          {
            boolean is_val_Void =
                      (comp_st(val_Void,
                               p -> signature -> fsig_result_sig,
                               NIL, NIL) == 0);
            checksigs(p -> fc_body, is_val_Void);
	    tl_findsig(p,dont_coerce);
	    if (!in_sig &&
		p -> signature -> fsig_result_sig != ERR_SIG) {
		/* find signatures of var parameter types     */
		/* and of result type for code generator      */
		/* code generator                             */
		  in_sig = TRUE;
		  if (p -> signature -> fsig_result_sig -> kind
		      == VALSIGNATURE) {
			tl_findsig(p -> signature -> fsig_result_sig
				     -> val_denotation, FALSE);
		  }
		  maplist(q, p -> signature -> fsig_param_list, {
		      NODE * sig = q -> par_signature;

		      if (sig -> kind == VARSIGNATURE) {
			tl_findsig(sig -> var_denotation, FALSE);
		      }
		  });
		  in_sig = FALSE;
	    }
	    if (p -> fc_body -> kind == EXTERNDEF) {
	      /* check that last parameter has var void sig */
		NODE * psig;

		if (is_empty(p -> signature -> fsig_param_list)) {
		    psig = NIL;
		} else {
		    psig = last(p -> signature -> fsig_param_list) -> par_signature;
		}
		if (comp_st(psig, var_Void, NIL, NIL) != 0) {
		    errmsg0(p, "Warning - last parameter of extern should be var Void");
		    yynerrs --; /* Only a warning */
		}
	    } else {
	      /* check that result sig matches body */
                if (!is_val_Void &&
                    !amatch(p -> fc_body -> signature, 
			    p -> signature -> fsig_result_sig)) {
		    errmsg0(p, "Function result signature mismatch");
		}
	    }
            break;
          }

        case REXTERNDEF:
            break;

        case APPLICATION:
            /* insert coercions first */
                tl_findsig(p, dont_coerce);
            maplist(q, p -> ap_args, {
		checksigs(q,FALSE);
	    });
	    op = p -> ap_operator;
	    if (op -> kind == OPRID || op -> kind == LETTERID) {
		/* use argument list to find right decl. */
		    op -> id_appl = p;
	    }
            checksigs(op, FALSE);
	    tl_findsig(p,dont_coerce);
            if (p -> signature != ERR_SIG 
                && op -> signature != ERR_SIG /* not implied by prec cond */) {
		NODE * op_sig = op -> signature;
		NODE * new_args;
		int num_args;
		int num_params;

		/* This guarantees operator has func signature */
      
                /*  printf("Checksigs looking at application(%X):",p);
                    unparse_file = stdout;
                    unparse(p);
                    printf("\n");
                */
                if (!in_sig) {
                  /* Try to fill in in-line code, if not already there */
                    if (op -> signature -> fsig_inline_code == NIL
                        && op -> signature -> fsig_construction != NIL) {
                        op -> signature -> fsig_inline_code =
                            op -> signature -> fsig_construction
                               -> signature -> fsig_inline_code;
                    }
                  /* compute signature of result type (for code generator) */
		    in_sig = TRUE;
		    err_node = p;
                    err_msg = "signature of result type is bad";
                    switch(p -> signature -> kind) {
		      case VALSIGNATURE:
			tl_findsig(p -> signature -> val_denotation, TRUE);
		      case VARSIGNATURE:
			tl_findsig(p -> signature -> var_denotation, TRUE);
		    }
		    in_sig = FALSE;
		    err_node = NIL;
		}

		/* check argument parameter matching, and compute signature */
		/* of any var parameter types     (for code generator)      */

#                   ifdef DEBUG
			if (!is_ptr(op_sig)) {
                            dbgmsg("checksigs: bad operator signature\n");
                            unparse_file = stdout;
                            unparse(p);
                            printf("\n");
                            prtree(p);
                            printf("Signature:\n");
                            prtree(p -> signature);
                            abort();
			}
			if (!is_ptr(op_sig -> fsig_param_list)
			    || !is_list(op_sig -> fsig_param_list)) {
                            dbgmsg("checksigs: bad parameter list\n");
                            abort();
			}
			if (!is_ptr(p -> ap_args) || !is_list(p -> ap_args)) {
                            dbgmsg("checksigs: bad argument list\n");
                            abort();
			}
#                   endif
		    num_args = length(p -> ap_args);
		    num_params = length(op_sig -> fsig_param_list);
		    if (num_args != num_params) {
			errmsg0(p, "wrong number of arguments");
		    } else {
			NODE * par_sig;

			begin_map2lists (s, p -> ap_args,
					 r, op_sig -> fsig_param_list) {
			    NODE * sig = s -> signature;
			    NODE * den;

			    if (has_sig(s) &&
				(sig -> kind == VARSIGNATURE)
				&& !in_sig) {
				/* find signature for code generator */
				    in_sig = TRUE;
				    err_node = s;
				    err_msg = "bad argument type signature";
				    if (sig -> kind == VALSIGNATURE) {
					den = sig -> val_denotation;
				    } else {
					den = sig -> var_denotation;
				    }
				    tl_findsig(den, TRUE);
				    err_node = NIL;
				    in_sig = FALSE;
			    }
			    par_sig = subst(r -> par_signature,
				            op_sig -> fsig_param_list,
				            p -> ap_args);
			    lock(par_sig);
			    if (!amatch(s -> signature, par_sig)) {
				extern NODE * failed_comp; /* set by amatch */

				errmsg0(p, "Argument parameter mismatch");
                                unparse_file = stderr;
                                fprintf(stderr, "\tArgument:\n\t");
                                unparse(s);
                                fprintf(stderr,
                                   "\n\tParameter signature after substitution:\n\t");
				unparse(par_sig);
				if (failed_comp != NIL) {
				  if (failed_comp -> kind == DEFCHARSIGS) {
				    fprintf(stderr, "\n\tMissing Constant");
				  } else {
				    /* TSCOMPONENT */
				    fprintf(stderr,
					    "\n\tOffending parameter component: ");
				    unparse(failed_comp -> tsc_id);
				    fprintf(stderr, ":");
				    unparse(failed_comp -> tsc_signature);
				  }
				}
                                fprintf(stderr,
                                   "\n\tArgument signature:\n\t");
                                unparse(s -> signature);
                                fprintf(stderr, "\n");
                            }
			    vfree(unlock(par_sig));
			} end_map2lists;
		    }
	    }
	    break;

        case GUARDEDLIST:
        case LOOPDENOTATION:
            maplist(q, p -> gl_list, {
		checksigs(q -> ge_guard, FALSE);
		checksigs(q -> ge_element, knd==LOOPDENOTATION || dont_coerce);
            });
	    tl_findsig(p,dont_coerce);
	    /* check that all guards have Boolean signature */
	      maplist(q, p -> gl_list, {
		if (q -> ge_guard -> signature != ERR_SIG &&
		    comp_st(q -> ge_guard -> signature, 
			    val_Boolean, NIL,NIL) != 0)  { 
		    errmsg0(q -> ge_guard, "Guard has inappropriate signature");
                    unparse_file = stderr;
                    fprintf(stderr, "\tGuard signature:\n\t");
                    unparse(q -> ge_guard -> signature);
                    fprintf(stderr, "\n");
                }
	      });
	    if(!dont_coerce && knd == GUARDEDLIST
	       && p -> signature != ERR_SIG) {
		/* Signatures should all be the same - make sure they are */
		    maplist(q, p -> gl_list, {
			if (q -> ge_element -> signature != ERR_SIG
			    && comp_st(q -> ge_element -> signature,
				       p -> signature, NIL, NIL) != 0) {
			    errmsg0(q -> ge_element, 
				"Guarded expression has incorrect signature:");
			    unparse_file = stderr;
			    fprintf(stderr, "\t");
			    unparse(q -> ge_element -> signature);
			    fprintf(stderr, "\n\tShould be:\n\t");
			    unparse(p -> signature);
			    fprintf(stderr, "\n");
			}
		    });
	    }
	    break;

	case BLOCKDENOTATION:
	    maplist(q, p -> bld_declaration_list, {
		checksigs(q -> decl_denotation,FALSE);
		if (q -> decl_signature != NIL &&
		    q -> decl_denotation -> signature != ERR_SIG &&
                    comp_st(q -> decl_signature,
                            q -> decl_denotation -> signature, NIL, NIL) != 0) {
                    errmsg0(q, "Signature does not match declaration");
                    unparse_file = stderr;
                    fprintf(stderr, "\texplicit signature:\n\t");
                    unparse(q -> decl_signature);
                    fprintf(stderr, "\n\texpression signature:\n\t");
                    unparse(q -> decl_denotation -> signature);
                    fprintf(stderr, "\n");
		}
		if (q -> decl_signature == NIL) {
		    q -> decl_signature = q -> decl_denotation -> signature;
		    q -> decl_sig_done = SIG_DONE;
		}
	    });
	    maplist(q, p -> bld_den_seq, {
		if (q == last(p -> bld_den_seq)) {
		    checksigs(q,dont_coerce);
		} else {
		    checksigs(q,TRUE);
		}
            });
            tl_findsig(p,dont_coerce);
	    break;

        case USELIST:
            maplist(q, p -> usl_type_list, {
		checksigs(q,FALSE);
		if (q -> signature != ERR_SIG) {
		    if (q -> signature -> kind != TYPESIGNATURE) {
			chgfld(&(q -> signature),
			       sig_structure(q -> signature));
			if (q -> signature -> kind != TYPESIGNATURE) {
			  errmsg0(q, "Non-type appears in use type list");
			}
		    }
		}
            });
            maplist(q, p -> usl_den_seq, {
		if (q == last(p -> usl_den_seq)) {
		    checksigs(q,dont_coerce);
		} else {
		    checksigs(q,TRUE);
		}
	    });
            tl_findsig(p,dont_coerce);
	    break;

        case WORDELSE:
            tl_findsig(p,dont_coerce);
	    break;

	case MODPRIMARY:
	    checksigs(p -> mp_primary, FALSE);
	    /* Make sure with list components are traversed in their */
            /* final order, so that they don't get reordered in the  */
	    /* middle of this.                                       */
	      {
		NODE * q;

		if((q = findsig(p, dont_coerce)) != SUCCESS) {
		   errmsg0(p, "Can't find signature of with list");
		   fprintf(stderr,"\tOffending expression:\n\t");
		   unparse_file = stderr;
		   unparse(q);
		   fprintf(stderr, "\n");
		   p -> sig_done = SIG_DONE;
		   p -> signature = ERR_SIG;
		   break;
		};
	      }
	    if (!in_sig && has_sig(p -> mp_primary)) {
              /* check for ambiguous selections etc. in orig. type */
                err_node = p;
                err_msg = 
                   "bad type signature before modification";
		checksigs(p -> mp_primary -> signature, FALSE);
		err_node = NIL;
	    }
	    if (p -> mp_type_modifier != NIL
		&& p -> mp_type_modifier -> kind == WITHLIST) {
		maplist(q, p -> mp_type_modifier -> wl_component_list, {
		    IFDEBUG(
			if (q -> kind != DECLARATION) {
			    dbgmsg("checksigs: bad with list\n");
			}
		    )
		    checksigs(q -> decl_denotation, dont_coerce);
		    if (q -> decl_signature != NIL &&
			q -> decl_denotation -> signature != ERR_SIG &&
                        comp_st(q -> decl_signature,
                                q -> decl_denotation -> signature,
                                NIL, NIL) != 0) {
			errmsg0(q, "Signature does not match WITH declaration");
		    }
		});
		tl_findsig(p, dont_coerce);
		if (!in_sig && has_sig(p)) {
		  /* check for export rule violations after hiding */
		  /* but before adding new components.             */
		    NODE * sig_after_hiding =
		      lock(delcomp(p -> mp_primary -> signature,
				   p -> mp_delete_v));
    
		    err_node = p;
		    err_msg = "bad signature after hiding";
		    checksigs(sig_after_hiding, FALSE);
                    vfree(unlock(sig_after_hiding));

                  /* Check for ambiguous selections after component addition */
                    err_msg = "bad signature after adding components";
                    checksigs(p -> signature, FALSE);
		    err_node = NIL;
                }
	      } else {
		tl_findsig(p, dont_coerce);
		/* check for export rule violations */
		  if (!in_sig && has_sig(p)) {
		    err_node = p;
		    err_msg = "bad signature after hiding";
		    checksigs(p -> signature, FALSE);
		    err_node = NIL;
		  }
	      }
	    break;

	case RECORDCONSTRUCTION:
	    tl_findsig(p,dont_coerce);
	    maplist(q, p -> rec_component_list, {
		tl_findsig(q -> re_denotation);
		if (q -> re_denotation -> signature != ERR_SIG
		    && q -> re_denotation -> signature -> kind
		       != TYPESIGNATURE) {
                    errmsg0(q, "Non-type expression in record");
		}
	    });
            break;

	case UNIONCONSTRUCTION:
	case PRODCONSTRUCTION:
	    tl_findsig(p, dont_coerce);
	    maplist(q, p -> prod_components, {
                if (q -> par_signature -> kind == VARSIGNATURE) {
                  switch(p -> kind) {
                   case PRODCONSTRUCTION:
		    errmsg0(q, "Product shouldn't have var component");
                   case UNIONCONSTRUCTION:
                    errmsg0(q, "Union shouldn't have var component");
                  }
		}
	    });
	    break;

        case ENUMERATION:
	    tl_findsig(p, dont_coerce);
	    break;

	case EXTENSION:
	    checksigs(p -> ext_denotation);
            tl_findsig(p, dont_coerce);
            break;

	case TYPESIGNATURE:
	    {
		boolean old_in_sig = in_sig;
		NODE * old_curr_tsig = curr_tsig;

		if (p -> signature == NIL) {
		    initfld(&(p -> signature), sig_Signature);
		}
		p -> sig_done = SIG_DONE;
		in_sig = TRUE;
		curr_tsig = p;
		/* check nested signatures */
		  maplist(s, p -> ts_clist, {
		    if (s -> kind == TSCOMPONENT) {
		      checksigs(s -> tsc_signature, FALSE);
		    }
		  });
		in_sig = old_in_sig;
		curr_tsig = old_curr_tsig;
		break;
	    }

	case FUNCSIGNATURE:
	    /* should occur only when checking signatures of type     */
	    /* modifications and when signatures occur as expressions */
	    if (p -> signature == NIL) {
		initfld(&(p -> signature), sig_Signature);
	    }
	    p -> sig_done = SIG_DONE;
	    maplist(s, p -> fsig_param_list, {
		IFDEBUG(
		  if (s -> kind != PARAMETER || !is_ptr(s -> par_signature)) {
		    dbgmsg("checksigs: bad parameter\n");
		    prtree(s);
		    abort();
		  }
		)
		checksigs(s -> par_signature, FALSE);
	    });
	    checksigs(p -> fsig_result_sig);
	    break;

	case VALSIGNATURE:
	    if (p -> signature == NIL) {
		initfld(&(p -> signature), sig_Signature);
	    }
	    p -> sig_done = SIG_DONE;
	    checksigs(p -> val_denotation, FALSE);
	    break;

	case VARSIGNATURE:
	    if (p -> signature == NIL) {
		initfld(&(p -> signature), sig_Signature);
	    }
	    p -> sig_done = SIG_DONE;
	    checksigs(p -> var_denotation, FALSE);
	    break;

	case SIGNATURESIG:
	    if (p -> signature == NIL) {
		initfld(&(p -> signature), sig_Signature);
	    }
	    p -> sig_done = SIG_DONE;
	    break;

	case EXTERNDEF:
	    break;

        default:
	    dbgmsg("checksigs: bad kind: %s\n",kindname(p -> kind));
	    abort();
    }
#   ifdef DEBUG
	if (p -> kind != TYPESIGNATURE && p -> kind != FUNCSIGNATURE
	    && p -> kind != VALSIGNATURE && p -> kind != VARSIGNATURE
	    && p -> kind != EXTERNDEF && p -> sig_done == SIG_IN_PROGRESS) {
	    dbgmsg("Checksigs: findsig blew it on %s\n", kindname(p -> kind));
	}
#   endif
}


/*
 * Top level driver routine for the above.  Also calls import to check 
 * for import and export rule violations.
 */

tl_checksigs(p)
NODE * p;
{
    checksigs(p, FALSE);
    import(p, NIL);
}

