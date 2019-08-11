# define TRACE
# undef TRACE
# define DEBUG

# define TRACE2
# undef TRACE2

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

# include <stdio.h>
# include "parm.h"
# include "arith.h"

# include "stree/ststructs.mh"
# ifdef DEBUG
#   include "stree/is_ptr.h"
# endif

# include "sigs.h"

# include "stree/Array.h"

/* Needed to construct type signatures for constructions */
# include "pass1/stt/sttdefs.h"

extern FILE * unparse_file;

extern boolean Gflag;

extern boolean Nflag;  /* No Call/cc calls */

# define UNDEFNAME ((sttrelptr) 0)   /* the name of an anonymous identifier */

extern sttrelptr indx_New,
		 indx_ValueOf,
		 indx_assign,
		 indx_equals,
		 indx_ne,
		 indx_Mk,
		 indx_First,
		 indx_Last,
		 indx_Pred,
		 indx_Succ,
		 indx_Ord,
		 indx_OrdInv,
                 indx_Card,
                 indx_In,
                 indx_Out;

extern NODE * sig_New,
	    * sig_ValueOf,
	    * sig_assign,
	    * sig_equals,
	    * sig_const,
	    * val_Boolean,
	    * val_Integer,
	    * val_Void,
	    * sig_Signature;

/* Given a type sig comp corr to a function, set the inline code to match */
/* the special field							  */
# define init_inline(r) \
	(r) -> tsc_signature -> fsig_inline_code = \
	(*spcl_to_inline)((r) -> tsc_signature -> fsig_special);

extern int yynerrs;

extern int next_pre; /* needed so that comparison for declarations will */
		     /* continue to work.                               */

# define ERR_NODE_DEFINED
extern NODE * err_node;        /* node to be used for error message in lieu */
			       /* of current node.  Used by errmsg macros   */
extern char * err_msg;         /* message to be used in lieu of usual one   */

extern NODE * curr_tsig; 
	   /* type signature corresponding to local type identifier */

NODE * declerr;     /* declsig failure indication */

NODE * substerr;    /* subst error indication                       */
		    /* Set to something other than SUCCESS if       */
		    /* subst is asked to substitute an incompletely */
		    /* expanded expression, as indicated by         */
		    /* dontsubst                                    */

extern int match_len;      /* length of argument type.  Set by amatch. */
extern unsigned * match_delv;  /* bitvector indicating necessary deletions */
			       /* set by amatch.                           */

extern NODE * failed_asig;  /* Set by amatch to indicate last failure */
extern NODE * failed_psig;  /* Used as a hint in error message        */
extern NODE * failed_comp;

struct cn * dontsubst = NIL; /* list of incompletely expanded nodes  */
			      /* which should not be substituted into */
			      /* signatures.                          */

extern int comp_index;

# ifdef VAX
    int nargs();
# endif

void find_inline();

void Gfind_inline();

boolean may_fail = FALSE; /* current signature deduction may fail */
			  /* without dire consequences.           */

/*
 *    findsig(p,dont_coerce) finds the signature corresponding to the expression
 * tree rooted at p and of all its subexpressions.
 *    If the sig_done field of p is already SIG_IN_PROGRESS it fails by 
 * returning the pointer p.
 *    If it is SIG_DONE findsig immediately returns SUCCESS. 
 *    If sig_done is originally NIL it is set to SIG_IN_PROGRESS
 * and begins the (usually recursive) signature computation.
 * If any of the subsidiary computations fail by running into a computation
 * which is already IN_PROGRESS the offending
 * node pointer is returned.
 *    If findsig succeeds in determining the signature it stores it in the
 * signature field and returns SUCCESS.
 *    IN_PROGRESS is thus used to detect and abort non-terminating
 * signature calculations.
 *    The special value ERR_SIG is used to avoid avalanche errors.  It is
 * treated as an acceptable signature in all contexts.
 *    Note that it is not acceptable to immediately set signatures
 * to ERR_SIG once a cycle is encountered.  It is quite possible
 * that the cycle is encountered as a result of using one particular
 * branch of a conditional to evaluate its signature, and that
 * backtracking and using another branch will still result in success.
 *    Dont_coerce indicates that the expression is used in a context
 * in which the result signature doesnt matter.  Thus no attempt
 * need be made to coerce the branches of a conditional to match.
 * Note that the value of this argument only matters the first time
 * findsig is called on a given node.
 *    The sig_done field is set to SIG_DONE whenever signature computation
 * for that node is completed.  The one exception are anonymous local
 * type identifiers.  In their case the signature must be recomputed
 * each time the node is encountered.
 */

		    /* Finish up after recognizing a separately reported */
		    /* error.                                            */
# define err_return \
    p -> signature = ERR_SIG; \
    p -> sig_done = SIG_DONE; \
    return(SUCCESS);            /* dealt with it here */

# ifdef DEBUG
    NODE * return_val;
#   define return(p) { return_val = p; goto findsig_out; }
# endif

NODE * findsig(p, dont_coerce)
register NODE * p;
boolean dont_coerce;
{
    register int knd;
    register int status = p -> sig_done;
    NODE * q, * r;
    boolean all_ok;     /* signature evaluation for all subexpressions  */
			/* was successful.                              */

#   ifdef DEBUG
#     ifdef VAX
	if(nargs() != 2) {
	    dbgmsg("findsig: wrong number of args\n");
	    abort();
	}
#    endif
#   endif
#   ifdef DEBUG
	if (p == (NODE *)0x40404040) abort();
		/* use for "conditional breakpoint" */
#   endif
    if (status == SIG_DONE)
	return(SUCCESS);
    if (status == SIG_IN_PROGRESS /* infinite signature computation */) {
	return(p);
    }
    knd = p -> kind;
#   ifdef DEBUG
	if ( p -> signature != NIL
	    && knd != OPRID && knd != LETTERID && knd != FUNCCONSTR ) {
	    dbgmsg("findsig: bad sig_done value, knd = %s, sig:\n",
		    kindname(knd));
	    prtree(p);
	    prtree(p -> signature);
	    abort();
	}
#   endif
    /* compute new signature */
	p -> sig_done = SIG_IN_PROGRESS;
	switch(knd) {
	    case OPRID:
	    case LETTERID:
		return(findidsig(p));

	    case VALSIGNATURE:
	    case VARSIGNATURE:
	    case FUNCSIGNATURE:
	    case TYPESIGNATURE:
	    case SIGNATURESIG:
		initsig(p, sig_Signature);
		p -> sig_done = SIG_DONE;
		return(SUCCESS);

	    case FUNCCONSTR:
#               ifdef DEBUG
		  if(p -> signature == NIL) {
                    /* not possible: function signatures must be at least */
                    /* partially specified.                               */
                    dbgmsg("NIL FUNCCONSTR signature\n");
                    abort();
                  }
#               endif
		q = p -> signature -> fsig_result_sig;
		if (q == NIL /* need to fill in result signature */) {
		    if ((r = findsig(p -> fc_body,FALSE)) != SUCCESS) {
			p -> sig_done = SIG_UNKNOWN;
                        return(r);
		    }
		    if(p -> fc_body -> signature != ERR_SIG) {
			initfld(&(p -> signature -> fsig_result_sig),
				p -> fc_body -> signature);
			if (Gflag) {
			  Gfind_inline(p);
			} else {
			  find_inline(p);
			}
		    } else {
			p -> signature -> fsig_result_sig = ERR_SIG;
		    }
		    p -> sig_done = SIG_DONE;
                    return(SUCCESS);
		} else {
		    /* find signature of body if possible */
			boolean old_may_fail = may_fail;
	                boolean is_void = comp_st(val_Void, q, NIL, NIL) == 0;

			if (p -> fc_body -> kind == EXTERNDEF) {
			    p -> sig_done = SIG_DONE;
			    if (Gflag) {
			      Gfind_inline(p);
			    } else {
			      find_inline(p);
			    }
			    return(SUCCESS);
			}
			may_fail = TRUE;    /* This has low priority */
			r = findsig(p -> fc_body, is_void);
			may_fail = old_may_fail;
			if (r != SUCCESS) {
			    dontsubst = cn_cons(p, dontsubst);
			    p -> sig_done = SIG_DONE;
			    return(SUCCESS);
                        }
                    /* q is still the result signature */
		    if (q == ERR_SIG
			|| p -> fc_body -> signature == ERR_SIG) {
			p -> sig_done = SIG_DONE;
			return(SUCCESS);
		    }
		    if (q -> kind == VALSIGNATURE && !is_void
			   && p -> fc_body -> signature -> kind
			      != VALSIGNATURE) {
			/* attempt coercion */
			    NODE * nresult;

			    nresult = coerce(p -> fc_body);
			    if ((r = findsig(nresult,FALSE)) != SUCCESS) {
				p -> sig_done = SIG_UNKNOWN;
				vfree(nresult);
				return(r);
			    }
			    chgfld(&(p -> fc_body), nresult);
                    }
                    if (q -> kind == TYPESIGNATURE
                        && amatch(p -> fc_body -> signature,q)
                        && match_delv != NIL) {  /* forget components */
                        NODE * nresult = mknode(MODPRIMARY,
                                                p -> fc_body,
                                                NIL,
                                                match_delv
                                               );

                        nresult -> mp_orig_length = match_len;
			initfld(&(nresult -> signature), q);
			nresult -> sig_done = SIG_DONE;
                        chgfld(&(p -> fc_body), nresult);
		    }
		    /* replace result signature to take advantage of */
                    /* optimization information                      */
                    if (comp_st(p -> fc_body -> signature,
                                p -> signature -> fsig_result_sig,
                                NIL, NIL) == 0) {
                        chgfld(&(p -> signature -> fsig_result_sig),
                               p -> fc_body -> signature);
		    }
		    if (Gflag) {
		      Gfind_inline(p);
		    } else {
		      find_inline(p);
		    }
		    p -> sig_done = SIG_DONE;
		    return(SUCCESS);
		}

	    case APPLICATION:
		return(findapplsig(p));

            case GUARDEDLIST:
		{
		    NODE * good_element;    /* element which can be used to */
					    /* determine signature          */
		    boolean kinds_differ;   /* signatures of guarded        */
					    /* elements have different kind */
                                            /* fields                       */
                    boolean coerce_guards;  /* Guards need to be coerced    */
		    int prev_kind = -1;     /* previous element kind        */
		    int curr_kind;
		    boolean old_may_fail = may_fail;

                    good_element = NIL;
		    all_ok = TRUE;
                    kinds_differ = FALSE;
                    coerce_guards = FALSE;
		    /* find signatures of guarded expressions where possible */
			maplist(q, p -> gl_list, {
			    IFDEBUG(
				if(q -> kind != GUARDEDELEMENT) {
				    dbgmsg("findsig: Bad guarded element\n");
				}
			    )
			    if(findsig(q -> ge_element,dont_coerce) == SUCCESS) {
				good_element = q;
				if (q -> ge_element -> signature != ERR_SIG) {
				    curr_kind = q -> ge_element -> signature
						  -> kind;
				    if (prev_kind != -1 
					&& curr_kind != prev_kind) {
					kinds_differ = TRUE;
				    }
				    prev_kind = curr_kind;
				}
			    } else {
				all_ok = FALSE;
			    }
			});
                    may_fail = TRUE;
                    /* Now find signatures of guards */
			begin_maplist(s, p -> gl_list) {
			    if((q = findsig(s -> ge_guard,FALSE)) != SUCCESS) {
#                               ifdef TRACE
				    printf("Couldn't find guard signature\n");
#                               endif
                                all_ok = FALSE;
			    } else if (s -> ge_guard -> signature != ERR_SIG
				       && s -> ge_guard -> signature -> kind
					  != VALSIGNATURE) {
#                               ifdef TRACE
				    printf("Must coerce guards\n");
#                               endif
                                coerce_guards = TRUE;
                            }
			} end_maplist;
		    may_fail = old_may_fail;
		    if (good_element == NIL || (may_fail && !all_ok)) {
			/* cant determine signature of any subexpression */
                        /* or its not worth going on partial info        */
			p -> sig_done = SIG_UNKNOWN;
#                       ifdef TRACE
			    printf("Giving up\n");
#                       endif
			return(p);
                    }
                    if ( (kinds_differ || coerce_guards)
			 /* && (all_ok || !may_fail) */) {
                        /* add omitted V and constant application coercions */
                        /* where possible                                   */
                            if ((!dont_coerce) && kinds_differ) {
                              /* May be impossible to find result sig */
                              /* after coercion                       */
                                good_element = NIL;
                            }
			    begin_maplist(s, p -> gl_list) {
                                if (!dont_coerce && kinds_differ
                                    && s -> ge_element -> signature != NIL) {
				    NODE * nelement = coerce(s -> ge_element);
			     
				    if (nelement -> sig_done == SIG_UNKNOWN) {
				      if ((q = findsig(nelement, FALSE)) 
					== SUCCESS) {
					good_element = s;
					chgfld(&(s -> ge_element), nelement);
				      } else {
					all_ok = FALSE;
					vfree(nelement);
					if (may_fail) {
					    p -> sig_done = SIG_UNKNOWN;
					    return(s);
					}
				      }
                                    } else {
                                      good_element = s;
                                    }
				}
                                if (coerce_guards
                                    && s -> ge_guard -> signature != NIL) {
				    NODE * nguard = lock(coerce(s -> ge_guard));

#                                   ifdef TRACE
					printf("Coercing guard\n");
#                                   endif
				    if (nguard -> sig_done == SIG_UNKNOWN) {
				      if ((q = findsig(nguard, FALSE)) 
					== SUCCESS) {
#                                       ifdef TRACE
					    printf("Changed ");
					    unparse_file = stdout;
					    unparse(s -> ge_guard);
					    printf(" to ");
					    unparse(nguard);
					    printf(" with signature ");
					    unparse(nguard -> signature);
					    printf("and refcount %d\n", nguard -> refcount);
#                                       endif
					chgfld(&(s -> ge_guard), nguard);
					unlock(nguard);
                                      } else {
					vfree(unlock(nguard));
					p -> sig_done = SIG_UNKNOWN;
					return(s);
				      }
				    }
				}
			    }end_maplist;
		    }
		    /* At least one element had val or func signature */
		    /* It must have been possible to determine its    */
		    /* signature after coercion. Thus good_element    */
		    /* != NIL                                         */
#                   ifdef DEBUG
			if (good_element == NIL) {
			    dbgmsg("findsig: bad good_element\n");
			}
#                   endif
		    if(!all_ok) {
			/* signature is known, but subexpressions havent  */
			/* been completely expanded.                      */
			dontsubst = cn_cons(p, dontsubst);
		    }
		    initsig(p, good_element -> ge_element -> signature);
		    if (!dont_coerce /* value matters */) 
		      /* Fix up code generator information */
			maplist(q, p -> gl_list, {
			    r = fixhints(p -> signature,
					 q -> ge_element -> signature);
			    if (r != p -> signature) {
				chgsig(p, r);
			    }
			});
		    p -> sig_done = SIG_DONE;
                    return(SUCCESS);
		}

            case BLOCKDENOTATION:
                {
		    NODE * last_den = last(p -> bld_den_seq);
		    boolean old_may_fail = may_fail;

		    if((q = findsig(last_den,FALSE)) != SUCCESS) {
			p -> sig_done = SIG_UNKNOWN;
                        return(q);
		    }
		    all_ok = TRUE;
		    may_fail = TRUE;
		    /* Now find signatures of all subexpressions */
			maplist(s, p -> bld_declaration_list, {
			    if((q = findsig(s -> decl_denotation,FALSE)) != SUCCESS) {
				all_ok = FALSE;
			    }
			});
			maplist(s, p -> bld_den_seq, {
			    if((q = findsig(s,TRUE)) != SUCCESS) {
				all_ok = FALSE;
			    }
			});
		    may_fail = old_may_fail;
		    initsig(p, last_den -> signature);
		    if (!Nflag || !(p -> bld_flags & NO_SURR_LOOP)) {
			/* May need a.r. for this block */
			clear_slink_known(p -> signature);
		    }
		    if (!all_ok)
			dontsubst = cn_cons(p, dontsubst);
		    p -> sig_done = SIG_DONE;
		    return(SUCCESS);
		}

            case USELIST:
                {
		    NODE * last_den = last(p -> usl_den_seq);
		    boolean old_may_fail = may_fail;

		    /* find signatures of types */
			maplist(s, p -> usl_type_list, {
			    if((q = findsig(s,FALSE)) != SUCCESS) {
				p -> sig_done = SIG_UNKNOWN;
				return(q);
			    } else {
				if (s -> signature != ERR_SIG
				    && (s -> signature -> kind == LETTERID
					|| s -> signature -> kind == OPRID)) {
				    chgfld(&(s -> signature),
					   sig_structure(s -> signature));
				}
			    }
			});
		    if((q = findsig(last_den,FALSE)) != SUCCESS) {
			p -> sig_done = SIG_UNKNOWN;
                        return(q);
		    }
		    /* Now find signatures of all subexpressions */
			may_fail = TRUE;
			all_ok = TRUE;
			maplist(s, p -> usl_den_seq, {
			    if((q = findsig(s,TRUE)) != SUCCESS) {
				all_ok = FALSE;
			    }
			});
			may_fail = old_may_fail;
		    initsig(p, last_den -> signature);
		    if (!all_ok)
			dontsubst = cn_cons(p, dontsubst);
		    p -> sig_done = SIG_DONE;
                    return(SUCCESS);
		}

            case WORDELSE:
		initfld(&(p -> signature), val_Boolean);
		p -> sig_done = SIG_DONE;
		return(SUCCESS);

	    case LOOPDENOTATION:
		{
		  /* Find signatures of all subexpressions */
		    boolean old_may_fail = may_fail;

		    all_ok = TRUE;
		    may_fail = TRUE;
		    maplist(s, p -> gl_list, {
			if((q = findsig(s -> ge_guard,FALSE)) != SUCCESS) {
			    all_ok = FALSE;
			} else {
			  /* coerce if necessary */
			    NODE * nguard = coerce(s -> ge_guard);

			    if (nguard -> sig_done == SIG_UNKNOWN) {
			      if ((q = findsig(nguard, FALSE)) 
				== SUCCESS) {
				chgfld(&(s -> ge_guard), nguard);
			      } else {
				vfree(nguard);
				p -> sig_done = SIG_UNKNOWN;
				may_fail = old_may_fail;
				return(s);
			      }
			    }
			}
			if((q = findsig(s -> ge_element,TRUE)) != SUCCESS) {
			    all_ok = FALSE;
			}
		    });
		    may_fail = old_may_fail;
		  initfld(&(p -> signature), val_Void);
		  if (!all_ok)
		    dontsubst = cn_cons(p, dontsubst);
		  p -> sig_done = SIG_DONE;
		  return(SUCCESS);
		}

#           ifdef DEBUG
              case WORDCAND:
              case WORDCOR:
                dbgmsg("findsig: cand or cor in syntax tree\n");
                return(SUCCESS);
#           endif
	    case QSTR:
	    case UQSTR:
		if ((q = findstdecl(p)) != SUCCESS) {
		    p -> sig_done = SIG_UNKNOWN;
		    return(q);
		}
		if (p -> sel_type == NIL) {
		    switch(p -> kind) {
			case QSTR:  
			    errmsg1(p,
				    "No appropriate type for \"%s\"",
				    p -> str_string);
			    break;
                        case UQSTR:
                            errmsg1(p,
				    "No appropriate type for %s",
				    p -> str_string);
			    break;
		    }
		    p -> signature = ERR_SIG;
		    p -> sig_done = SIG_DONE;
		    return(SUCCESS);
                }
                if ((q = findsig(p -> sel_type, dont_coerce)) != SUCCESS) {
		    p -> sig_done = SIG_UNKNOWN;
                    return(q);
                }
                {
                    NODE * sel_sig = p -> sel_type -> signature;
		    NODE * r;
		    int maxlen;  /* Maximum length for validity of */
				 /* ts_string_code                 */

		    if (sel_sig == ERR_SIG) {
			p -> signature = ERR_SIG;
			p -> sig_done = SIG_DONE;
			return(SUCCESS);
		    }
		    if (sel_sig -> ts_string_max == -1) {
			maxlen = MAXSTRLEN;
		    } else {
			maxlen = sel_sig -> ts_string_max;
		    }
                    if (sel_sig -> ts_string_code != NIL
			&& sel_sig -> ts_element_code != NIL
			&& strlen(p -> str_string) <= maxlen
			&& p -> sel_type -> kind == LETTERID
			&& p -> sel_type -> sel_type == NIL) {
                        if ((r = on_dontsubst(p -> sel_type)) != NIL) {
                            p -> sig_done = SIG_UNKNOWN;
                            return(r);
                        }
                        /* Safe to take a shortcut */
                        initsig(p, mknode(VALSIGNATURE, p -> sel_type));
                        p -> sig_done = SIG_DONE;
                        return(SUCCESS);
                    }
		}
		if (p -> str_expansion == NIL) {
		    initfld(&(p->str_expansion), expand_str(p));
		}
		if ((q = findsig(p -> str_expansion, dont_coerce)) == SUCCESS) {
		    initsig(p, p -> str_expansion -> signature);
		    p -> sig_done = SIG_DONE;
		    return(SUCCESS);
		} else {
		    p -> sig_done = SIG_UNKNOWN;
		    return(q);
		}

	    case MODPRIMARY:
		return(findmpsig(p));

	    case PRODCONSTRUCTION:
		{
		    NODE * par_list = p -> prod_components;
		    NODE * arg_list;  /* fake argument list, used */
				      /* for substitutions        */
		    NODE * comp_list = lock(emptylist());
		    NODE * local_id = p -> prod_local_type_id == NIL?
					mknode(LETTERID, UNDEFNAME)
				      : copynode(p -> prod_local_type_id);
		    NODE * self = mknode(VALSIGNATURE, local_id);
		    boolean simple_type = TRUE;
		    int len = length(par_list);
		    int i;

		    /* Compute product size */
			if (Gflag) {
			    len = 0;
			    maplist(s, par_list, {
				if (!vacuous_arg(s -> par_signature)) {
				    len++;
				}
			    });
			} else {
			    len = length(par_list);
			}
		    /* Put New, ValueOf and := in component list */
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_New),
				   copynode(sig_New));
			r -> tsc_signature -> fsig_special = 
			    special(PROD_NEW, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_ValueOf),
				   copynode(sig_ValueOf));
			r -> tsc_signature -> fsig_special = 
			    special(PROD_VALUEOF, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_assign),
				   copynode(sig_assign));
			r -> tsc_signature -> fsig_special = 
			    special(PROD_ASSIGN, len);
			init_inline(r);
			addright(comp_list, r);

		    /* add Mk function */
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_Mk),
				   mknode(FUNCSIGNATURE, NIL,
					  par_list,
					  self
					 )
				  );
			r -> tsc_signature -> fsig_special = 
			    special(PROD_MK, len);
			init_inline(r);
			addright(comp_list, r);

		    {/* add projection functions                       */ 
		      NODE * par = emptylist(); /* parameter list for */
                                                /* func signature     */
                      NODE * arg = emptylist();  /* list containing      */
                                                 /* argument in function */
						 /* applications         */
		      NODE * t;

		      /* fix up par and arg */
                        addright(par, t = mknode(PARAMETER, NIL, self));
                        t -> pre_num = next_pre++;
                            /* so copies can be recognized */
			addright(arg, t = mknode(LETTERID, UNDEFNAME));
                        t -> id_last_definition = first(par);
			t -> id_def_found = TRUE;

		      /* build arg_list */
			arg_list = lock(emptylist());
			maplist(s, par_list, {
			  r = copynode(s -> par_id);
			  chgfld(&(r -> sel_type), local_id);
			  r -> id_last_definition = NIL;
			  r -> id_def_found = TRUE;
			  r = mknode(APPLICATION, r, arg);
			  addright(arg_list,r);
			});
		      i = 0;
		      maplist(s, par_list, {
			boolean vac = Gflag && vacuous_arg(s -> par_signature);
				    /* Not represented in product */

			r = mknode(FUNCSIGNATURE,
				   NIL /* SHOULD BE INLINE CODE */,
				   par,
				   subst(s -> par_signature,
					 par_list,
					 arg_list)
				  );
			if (!vac) { 
			    r -> fsig_special = special(PROD_PROJ, i);
			} else {
			    r -> fsig_special = special(UNDEF_CONST, 0);
			}
			IFDEBUG(
			  if(substerr != NIL) {
			    dbgmsg("findsig: unexpected substerr\n");
			  }
			)
			if (s -> par_id == NIL) {
			    errmsg0(s, "Product component not named");
			}
			r = mknode(TSCOMPONENT, s -> par_id, r);
			init_inline(r);
			addright(comp_list, r);
			if (!vac) {
			    i++;
			}
		      });
		    }
		    initsig(p, mknode(TYPESIGNATURE,
				      local_id,
				      comp_list,
				      NIL, NIL, NIL));
		    p -> signature -> pre_num = next_pre++;
				/* so copies can be recognized */
		    tsig_order(p -> signature);
		    /* Make references to product local type id point to */
		    /* signature instead                                 */
		      /* Make sure tsubst doesn't get confused */
		        local_id -> id_def_found = TRUE;
		        local_id -> id_last_definition = p -> signature;
		      chgfld(&(p -> signature), tsubst(p -> signature,
						       p,
						       local_id,
						       FALSE));
		    /* Fix up local_id */
		      local_id -> id_def_found = TRUE;
		      local_id -> id_last_definition = p -> signature;
		    p -> sig_done = SIG_DONE;
		    /* Compute whether resulting type is "simple" */
			maplist(s, par_list, {
			  switch(s -> par_signature -> kind) {
			    case FUNCSIGNATURE:
			    case TYPESIGNATURE:
				simple_type = FALSE;
				break;
			    case VALSIGNATURE:
				if(findsig(s -> par_signature
					     -> val_denotation, FALSE)
					  != SUCCESS) {
				    simple_type = FALSE;
				} else {
				  NODE * den_sig;
				  den_sig = s -> par_signature
					      -> val_denotation
					      -> signature;
				  if (den_sig != ERR_SIG
				      && den_sig -> kind == TYPESIGNATURE) {
				      simple_type = simple_type &&
						    den_sig -> ts_simple_type;
				  }
				}
			    /* checksigs will complain about VARSIGNATUREs */
			  }
			});
		    p -> signature -> ts_simple_type = simple_type;
		    unlock(comp_list);
		    unlock(arg_list);
		    vfree(arg_list);
		    return(SUCCESS);
		}

	    case UNIONCONSTRUCTION:
		{
                    NODE * field_list = p -> prod_components;
		    NODE * comp_list = lock(emptylist());
                    NODE * local_id = p -> prod_local_type_id == NIL?
					mknode(LETTERID, UNDEFNAME)
                                      : copynode(p -> prod_local_type_id);
		    NODE * self = mknode(VALSIGNATURE, local_id);
		    int len = length(field_list);
		    int i;
		    boolean simple_type;

		    /* Put New, ValueOf and := in component list */
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_New),
				   copynode(sig_New));
			r -> tsc_signature -> fsig_special = 
			    special(UNION_NEW, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_ValueOf),
				   copynode(sig_ValueOf));
			r -> tsc_signature -> fsig_special = 
			    special(UNION_VALUEOF, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_assign),
				   copynode(sig_assign));
			r -> tsc_signature -> fsig_special = 
			    special(UNION_ASSIGN, len);
			init_inline(r);
			addright(comp_list, r);

		    {/* add projection, injection and inquiry functions */ 
		      NODE * par = emptylist(); /* parameter list for */
						 /* func signature     */
		      NODE * t;

		      /* fix up par */
                        addright(par, t = mknode(PARAMETER, NIL, self));
                        t -> pre_num = next_pre++;

		      i = 0;
		      begin_maplist(s, field_list) {
			boolean vac = Gflag && vacuous_arg(s -> par_signature);
				      /* This field is not really represented */

			IFDEBUG(
			    if (s -> kind != PARAMETER) {
                                dbgmsg("findsig: bad union component\n");
			    }
			)
			if (s -> par_id -> kind != LETTERID) {
			  errmsg1(s, "Bad union field name: %s",
				  getname(s -> par_id -> id_str_table_index));
			}
			/* Projection */
			  r = mknode(FUNCSIGNATURE,
				     NIL /* SHOULD BE INLINE CODE */,
				     par,
				     s -> par_signature
				    );
			  if (!vac) {
			      r -> fsig_special = special(UNION_PROJ, i);
			  } else {
			      r -> fsig_special = special(UNDEF_CONST, 0);
			  }
			  r -> fsig_inline_code =
			    (*spcl_to_inline)(r -> fsig_special);
			  addright(comp_list,
				   mknode(TSCOMPONENT,
					  prefix ("to_", s -> par_id),
					  r)
				  );
			/* Inquiry */
			  r = mknode(FUNCSIGNATURE,
				     NIL /* SHOULD BE INLINE CODE */,
				     par,
				     val_Boolean
				    );
			  r -> fsig_special = special(UNION_INQ, i);
			  r -> fsig_inline_code =
			    (*spcl_to_inline)(r -> fsig_special);
			  addright(comp_list,
				   mknode(TSCOMPONENT,
					  prefix ("is_", s -> par_id),
					  r)
				  );
			/* Injection */
			  r = mknode(FUNCSIGNATURE,
				     NIL,
				     mklist( mknode(PARAMETER,
						    NIL,
						    s -> par_signature),
					     -1
					   ),
				     self
				    );
			  if (!vac) {
			      r -> fsig_special = special(UNION_INJ, i);
			  } else {
			      r -> fsig_special = special(UNION_INJ0, i);
			  }
			  r -> fsig_inline_code =
			    (*spcl_to_inline)(r -> fsig_special);
			  addright(comp_list,
				   mknode(TSCOMPONENT,
					  prefix ("from_", s -> par_id),
					  r)
				  );
			i++;
		      } end_maplist;
		    }
		    initsig(p, mknode(TYPESIGNATURE, NIL,
				      comp_list,
				      NIL, NIL, NIL));
		    p -> signature -> pre_num = next_pre++;
				/* so copies can be recognized */
                    tsig_order(p -> signature);
                    /* Make references to union local type id point to   */
		    /* signature instead                                 */
		      /* Make sure tsubst doesn't get confused */
		        local_id -> id_def_found = TRUE;
		        local_id -> id_last_definition = p -> signature;
		      chgfld(&(p -> signature), tsubst(p -> signature,
						       p,
						       local_id,
						       FALSE));
		    /* Fix up local_id */
                      local_id -> id_last_definition = p -> signature;
                    p -> sig_done = SIG_DONE;
		    /* Compute whether resulting type is "simple" */
                        maplist(s, field_list, {
			  switch(s -> par_signature -> kind) {
			    case FUNCSIGNATURE:
			    case TYPESIGNATURE:
				simple_type = FALSE;
				break;
			    case VALSIGNATURE:
				if(findsig(s -> par_signature
					     -> val_denotation, FALSE)
					  != SUCCESS) {
				    simple_type = FALSE;
				} else {
				  NODE * den_sig;
				  den_sig = s -> par_signature
					      -> val_denotation
					      -> signature;
				  if (den_sig != ERR_SIG
				      && den_sig -> kind == TYPESIGNATURE) {
				      simple_type = simple_type &&
						    den_sig -> ts_simple_type;
				  }
				}
			    /* checksigs will complain about VARSIGNATUREs */
			  }
			});
                    p -> signature -> ts_simple_type = simple_type;
                    vfree(comp_list);
		    return(SUCCESS);
		}

	    case ENUMERATION:
		{
		    NODE * id_list = p -> enum_id_list;
		    NODE * comp_list = lock(emptylist());
		    NODE * local_id = mknode(LETTERID, -1);
		    NODE * self = mknode(VALSIGNATURE, local_id);
		    NODE * self_param = mknode(PARAMETER, NIL, self);
		    NODE * self_plist = mklist(self_param, -1);
		    NODE * Int_param = mknode(PARAMETER, NIL, val_Integer);
		    NODE * Int_plist = mklist(Int_param, -1);
		    int len = length(id_list);
                    int i;

		    /* Put New, ValueOf and := in component list */
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_New),
				   copynode(sig_New));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_NEW, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_ValueOf),
				   copynode(sig_ValueOf));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_VALUEOF, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_assign),
				   copynode(sig_assign));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_ASSIGN, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_equals),
				   copynode(sig_equals));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_EQ, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_ne),
				   copynode(sig_equals));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_NE, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_First),
				   copynode(sig_const));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_ELEMENT, 0);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_Last),
				   copynode(sig_const));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_ELEMENT, len-1);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_Pred),
				   mknode(FUNCSIGNATURE,
					  NIL, self_plist, self));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_PRED, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_Succ),
				   mknode(FUNCSIGNATURE,
					  NIL, self_plist, self));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_SUCC, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_Ord),
				   mknode(FUNCSIGNATURE,
					  NIL, self_plist, val_Integer));
                        r -> tsc_signature -> fsig_special = 
			    special(IDENTITY, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_OrdInv),
				   mknode(FUNCSIGNATURE,
					  NIL, Int_plist, self));
			r -> tsc_signature -> fsig_special = 
			    special(IDENTITY, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
				   mkcompnm(indx_Card),
				   mknode(FUNCSIGNATURE,
					  NIL, emptylist(), val_Integer));
			r -> tsc_signature -> fsig_special = 
			    special(ENUM_CARD, len);
			init_inline(r);
			addright(comp_list, r);

		    {/* add constant functions */ 

		      i = 0;
		      maplist(s, id_list, {
			  IFDEBUG(
			    if (s -> kind != LETTERID && s -> kind != OPRID) {
				dbgmsg("findsig: bad enumeration element\n");
			    }
			  )
			  r = copynode(sig_const);
			  r -> fsig_special = special(ENUM_ELEMENT, i);
			  r -> fsig_inline_code =
			    (*spcl_to_inline)(r -> fsig_special);
			  addright(comp_list,
				   mknode(TSCOMPONENT, s, r)
				  );
			i++;
		      });
		    }
		    initsig(p, mknode(TYPESIGNATURE, NIL,
				      comp_list,
                                      NIL, NIL, NIL));
                    unlock(comp_list);
		    p -> signature -> pre_num = next_pre++;
		    p -> sig_done = SIG_DONE;
				/* so copies can be recognized */
		    tsig_order(p -> signature);
		    p -> signature -> ts_simple_type = TRUE;
		    return(SUCCESS);
		}

            case EXTENSION:
                {
                    NODE * In_sig;
                    NODE * Out_sig;
                    NODE * sig; 
                    NODE * par_list;
                    NODE * orig_sig;
		    NODE * new_sig;
		    NODE * local_id;
                    NODE * id_In, * id_Out;

                    if ((q = findsig(p -> ext_denotation, FALSE)) != SUCCESS) {
                        p -> sig_done = SIG_UNKNOWN;
                        return(q);
                    }
                    if (p -> ext_denotation -> signature == ERR_SIG) {
                        p -> signature = ERR_SIG;
                        p -> sig_done = SIG_DONE;
                        return(SUCCESS);
                    }
		    /* sig := copy of argument signature */
		      sig = lock(copynode(p -> ext_denotation -> signature));
                      if (sig -> kind != TYPESIGNATURE) {
                          errmsg0(p -> ext_denotation,
                                  "Extension argument not a type");
                          p -> signature = ERR_SIG;
                          p -> sig_done = SIG_DONE;
                          return(SUCCESS);
		      }
		      if (sig -> ts_local_type_id != NIL) {
			local_id = lock(copynode(sig -> ts_local_type_id));
		      } else {
			local_id = mknode(LETTERID, UNDEFNAME);
		      }
		      sig -> pre_num = next_pre++;
			    /* Make sure we recognize it as different from */
			    /* the old one.                                */
		      chgfld(&(sig -> ts_clist), copylist(sig -> ts_clist));
		      /* Fix up references to local type id of sig */
		      {
			NODE * Osig = sig;

			local_id -> id_last_definition = sig;
			local_id -> id_def_found = TRUE;
			sig = lock(tsubst(sig,
					  p -> ext_denotation -> signature,
					  local_id,
					  TRUE));
			local_id -> id_last_definition = sig;
			vfree(unlock(Osig));
		      }
		    /* Build id nodes for In and Out */
                      id_In = mknode(LETTERID, indx_In);
                      id_Out = mknode(LETTERID, indx_Out);
                    /* Build signatures of In and Out */
                      /* check that it's safe to put arg into signature */
                        if (on_dontsubst(p -> ext_denotation) != NIL) {
                            p -> sig_done = SIG_UNKNOWN;
                            return(on_dontsubst(p -> ext_denotation));
                        }
                      orig_sig = mknode(VALSIGNATURE, p -> ext_denotation);
                      new_sig = mknode(VALSIGNATURE, mknode(LETTERID, -1));
                      par_list = mklist(mknode(PARAMETER, NIL, orig_sig), -1);
                      In_sig = mknode(FUNCSIGNATURE,
                                      NIL,
                                      par_list,
                                      new_sig);
                      par_list = mklist(mknode(PARAMETER, NIL, new_sig), -1);
                      Out_sig = mknode(FUNCSIGNATURE,
                                       NIL,
                                       par_list,
                                       orig_sig);
                      In_sig -> fsig_special = special(IDENTITY, 0);
                      In_sig -> fsig_inline_code =
			    (*spcl_to_inline)(In_sig -> fsig_special);
                      Out_sig -> fsig_special = special(IDENTITY, 0);
                      Out_sig -> fsig_inline_code =
			    (*spcl_to_inline)(Out_sig -> fsig_special);
                    /* Check that In and Out are not already present */
                    /* It's not clear that they can be, but rather   */
                    /* than trying to prove that, ...                */
                      if (getcomp(sig, id_In, NIL, In_sig, sig, NIL, TRUE)
                          != NIL) {
                        errmsg0(p, "In occurs in extend argument sig");
                      }
                      if (getcomp(sig, id_Out, NIL, Out_sig, sig, NIL, TRUE)
                          != NIL) {
                        errmsg0(p, "Out occurs in extend argument sig");
                      }
                    /* Add In and Out, initialize indicees */
                      inscomp(sig, id_In, In_sig, NIL);
                      p -> In_index = comp_index;
                      inscomp(sig, id_Out, Out_sig, NIL);
                      p -> Out_index = comp_index;
                    initsig(p, sig);
                    p -> sig_done = SIG_DONE;
                    /* No identifier references to sig, thus pre_num is */
                    /* irrelevant.  OK to use old optimization info     */
                    return(SUCCESS);
                }

            case RECORDCONSTRUCTION:
		{
                    NODE * field_list = p -> enum_id_list;
		    NODE * comp_list = lock(emptylist());
		    NODE * local_id = mknode(LETTERID, -1);
		    NODE * self = mknode(VALSIGNATURE, local_id);
		    NODE * self_param = mknode(PARAMETER, NIL, self);
                    NODE * self_plist = mklist(self_param, -1);
                    NODE * Mk_param_list = emptylist();
                    NODE * id_New = mkcompnm(indx_New);
                    NODE * id_ValueOf = mkcompnm(indx_ValueOf);
                    NODE * id_assign = mkcompnm(indx_assign);
                    NODE * re_den_sig;
                    int len = length(field_list);
		    int i;

                    /* Find signatures of type expressions,  make sure */
                    /* they can be incorporated into signatures, and   */
                    /* that they have the right components             */
		      begin_maplist(s, p -> rec_component_list) {
			if ((q = findsig(s -> re_denotation, FALSE))
			    != SUCCESS) {
			  p -> sig_done = SIG_UNKNOWN;
			  return(q);
			}
			re_den_sig = s -> re_denotation -> signature;
			if (re_den_sig == ERR_SIG) {
			  err_return;
			}
			if (re_den_sig -> kind != TYPESIGNATURE) {
			    errmsg0(s,
			      "Non-type expression in record construction");
			    err_return;
			}
			if (on_dontsubst(s -> re_denotation) != NIL) {
			    p -> sig_done = SIG_UNKNOWN;
			    return(on_dontsubst(s -> re_denotation));
			}
			if (getcomp(re_den_sig,
				    id_ValueOf, NIL, sig_ValueOf, re_den_sig,
				    NIL, TRUE) == NIL) {
			  errmsg0(s, "No V operation in record component");
			}
			s -> re_ValueOf_index = comp_index;
			if (getcomp(re_den_sig,
				    id_New, NIL, sig_New, re_den_sig,
				    NIL, TRUE) == NIL) {
			  errmsg0(s, "No New operation in record component");
			}
                        s -> re_New_index = comp_index;
			if (getcomp(re_den_sig,
				    id_assign, NIL, sig_assign, re_den_sig,
				    NIL, TRUE) == NIL) {
			  errmsg0(s, "No := operation in record component");
			}
			s -> re_assign_index = comp_index;
		      } end_maplist;
		    /* Put New, ValueOf and := in component list */
			r = mknode(TSCOMPONENT,
                                   id_New,
				   copynode(sig_New));
			r -> tsc_signature -> fsig_special = 
                            special(RECORD_NEW, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
                                   id_ValueOf,
				   copynode(sig_ValueOf));
			r -> tsc_signature -> fsig_special = 
                            special(RECORD_VALUEOF, len);
			init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
                                   id_assign,
				   copynode(sig_assign));
			r -> tsc_signature -> fsig_special = 
                            special(RECORD_ASSIGN, len);
			init_inline(r);
			addright(comp_list, r);
                    /* Put Mk in comp_list */
                      maplist(s, p -> rec_component_list, {
                        r = mknode(VALSIGNATURE, s -> re_denotation);
                        r = mknode(PARAMETER, NIL, r);
                        addright(Mk_param_list, r);
                      });
                      r = mknode(FUNCSIGNATURE, NIL,
                                 Mk_param_list, self);
                      r -> fsig_special = special(RECORD_MK, len);
                      r = mknode(TSCOMPONENT,
                                 mkcompnm(indx_Mk),
                                 r);
                      init_inline(r);
                      addright(comp_list, r);
                    /* Add fields          */
                      i = 0;   /* component number */
		      begin_maplist(s, p -> rec_component_list) {
                        NODE * vl_field_sig;
                        NODE * vr_field_sig;

                        vl_field_sig =
                             mknode(FUNCSIGNATURE,
                                    NIL,  /* in-line code */
                                    self_plist,
                                    mknode(VALSIGNATURE,
                                           s -> re_denotation)); 
                        r = mklist(mknode(PARAMETER,
                                          NIL,
                                          mknode(VARSIGNATURE,local_id)),
                                   -1);
                        vr_field_sig =
                             mknode(FUNCSIGNATURE,
                                    NIL,
                                    r,
                                    mknode(VARSIGNATURE,
                                           s -> re_denotation));
                        vl_field_sig -> fsig_special =
                            special(RECORD_VAL_FIELD, i);
                        vr_field_sig -> fsig_special =
                            special(RECORD_VAR_FIELD, i);
			r = mknode(TSCOMPONENT,
                                   s -> re_id,
                                   vl_field_sig);
                        init_inline(r);
			addright(comp_list, r);
			r = mknode(TSCOMPONENT,
                                   s -> re_id,
                                   vr_field_sig);
                        init_inline(r);
                        addright(comp_list, r);
                        i++;
		      } end_maplist;
                    initsig(p, mknode(TYPESIGNATURE, NIL,
				      comp_list,
                                      NIL, NIL, NIL));
                    unlock(comp_list);
		    p -> signature -> pre_num = next_pre++;
		    p -> sig_done = SIG_DONE;
				/* so copies can be recognized */
                    tsig_order(p -> signature);
                    /* fill in ts_simple type */
                      p -> signature -> ts_simple_type = TRUE;
                      maplist(s, p -> rec_component_list, {
                        if (s -> re_denotation -> signature
                              -> ts_simple_type == FALSE) {
                            p -> signature -> ts_simple_type = FALSE;
                        }
                      });
                    return(SUCCESS);
                }

            case REXTERNDEF:
                dbgmsg("findsig: REXTERNDEF without signature\n");
                abort();

            default:
                dbgmsg("findsig: unknown expression kind\n");
		abort();
	}
#   ifdef DEBUG
	findsig_out:
#           ifdef TRACE2
		printf("findsig: %X ", p);
		unparse_file = stdout;
		unparse(p);
		printf(" returning: %X\n", return_val);
		fflush(stdout);
		if (return_val != SUCCESS) {
		    unparse(return_val);
		    printf("\n");
		}
#           endif
	    if (!is_ptr(return_val)) {
		dbgmsg("findsig returning bogus value: 0x%X\n", return_val);
		unparse_file = stdout;
		unparse(p);
		printf("\n");
		abort();
	    }
#           undef return
	    return(return_val);
#   endif
}


/*
 * NODE * declsig(p)
 *  Returns the signature of the identifier declared by the declaration p
 * Sets declerr to either SUCCESS or a pointer to the offending node.
 */
NODE *
declsig(p)
NODE * p;
{
    boolean old_may_fail;
    NODE * q;
    switch (p -> kind) {
	case DECLARATION:
	    if (p -> decl_sig_done == SIG_DONE) {
		declerr = SUCCESS;
		return(p -> decl_signature);
	    }
	    old_may_fail = may_fail;
	    may_fail = (may_fail || p -> decl_signature != NIL);
	    /* Signature of right side should be found if at all possible */
	    /* It's useful for optimization info, if nothing else.        */
            declerr = findsig(p -> decl_denotation,FALSE);
#           ifdef TRACE
              printf("declsig: p = %X, declerr = %X\n", p, declerr);
#           endif
	    if (declerr != SUCCESS) {
	      if (p -> decl_signature != NIL) {
		declerr = SUCCESS;
                may_fail = old_may_fail;
                return(p -> decl_signature);
	      }
              /* try to get declerr to point to an identifier */
                if(declerr -> kind != LETTERID && declerr -> kind != OPRID) {
                    declerr = p -> decl_id;
#                   ifdef TRACE
                      printf("Changing decl_err to %X\n", declerr);
#                   endif
		}
	    } else {
		p -> decl_sig_done = SIG_DONE;
                if (p -> decl_denotation -> signature == ERR_SIG) {
		  p -> decl_signature = ERR_SIG;
		} else {
		  chgfld(&(p -> decl_signature),
			 p -> decl_denotation -> signature);
		}
	    }
            may_fail = old_may_fail;
            return(p -> decl_signature);
        case PARAMETER:
            declerr = SUCCESS;
            return(p -> par_signature);
        case TYPESIGNATURE:
            declerr = SUCCESS;
            return(p);
        case PRODCONSTRUCTION:
        case RECORDCONSTRUCTION:
	case UNIONCONSTRUCTION:
        case MODPRIMARY:
            declerr = findsig(p,FALSE);
            return(p -> signature);
#       ifdef DEBUG
	    default:
		dbgmsg("declsig: Bad declaration pointer: %x, kind=%s\n",
		       p, kindname(p->kind));
		abort();
#       endif
    }
}


/* top level version of findsig. Reports errors */
void
tl_findsig(p,dont_coerce)
NODE *p;
boolean dont_coerce;
{
    NODE * q;

    /* Redo any node which may still require coercion */
      switch (p -> kind) {
        case GUARDEDLIST:
	case LOOPDENOTATION:
	    chgsig(p, NIL);
	    /* and clear sig_done: */
	case FUNCCONSTR:
	    p -> sig_done = SIG_UNKNOWN;
      }
    q = findsig(p,dont_coerce);
    if (q != SUCCESS) {
	if (q -> kind == LETTERID || q -> kind == OPRID) {
	    errmsg1(q,"circular signature dependency involving %s",
		    getname(q -> id_str_table_index));
	} else {
	    errmsg0(q,"Circular signature dependency");
	}
	if (p -> kind == FUNCCONSTR) {
	    p -> signature -> fsig_result_sig = ERR_SIG;
	} else {
	    p -> signature = ERR_SIG;
	}
	p -> sig_done = SIG_DONE;
    }
    /* clear dontsubst list */
	while (dontsubst != NIL) {
	    dontsubst = cn_del_hd(dontsubst);
	}
}
