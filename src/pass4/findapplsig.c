/* Perform findsigs task in the case of applications.  We insure that the */
/* final operator signature is not an ID node.                            */
# define TRACE
# undef TRACE
# define DEBUG

# define TRACE2
# undef TRACE2
# include <stdio.h>
# include "parm.h"
# include "arith.h"

# ifdef TRACE
#   define IFTRACE(x) x
# else
#   define IFTRACE(x)
# endif

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

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

extern int yynerrs;

# define ERR_NODE_DEFINED
extern NODE * err_node;        /* node to be used for error message in lieu */
                   /* of current node.  Used by errmsg macros   */
extern char * err_msg;         /* message to be used in lieu of usual one   */

NODE * declerr;     /* declsig failure indication */

NODE * substerr;    /* subst error indication           */
        /* Set to something other than SUCCESS if       */
        /* subst is asked to substitute an incompletely */
        /* expanded expression, as indicated by         */
        /* dontsubst                                    */

extern int match_len;      /* length of argument type.  Set by amatch. */
extern unsigned * match_delv;  /* bitvector indicating necessary deletions */
                   /* set by amatch.                           */

# ifdef VAX
    int nargs();
# endif

void find_inline();

void Gfind_inline();

NODE * coerce();

NODE * infer_args();

NODE * declsig();

NODE * fixhints();

char * (* spcl_to_inline)();

boolean may_fail; /* current signature deduction may fail */
		  /* without dire consequences.           */

NODE * findapplsig(p)
register NODE * p;
{
        NODE * args = p -> ap_args;
	NODE * op = p -> ap_operator;
	NODE * r;
	NODE * q;

#       ifdef TRACE
            unparse_file = stdout;
            printf("Finding signature of application:");
            unparse(p);
            printf("\n");
#       endif
        /* find signatures of the arguments */
            maplist(q, args, {
                if ((r = findsig(q,FALSE)) != SUCCESS) {
                    p -> sig_done = SIG_UNKNOWN;
                    return(r);
                }
                if (q -> signature == ERR_SIG) {
                    p -> signature = ERR_SIG;
                    p -> sig_done = SIG_DONE;
                    return(SUCCESS);
                }
            });
       /* find signature of the operator */
            if (op -> kind == OPRID || op -> kind == LETTERID) {
                /* use argument list to find right decl. */
                    op -> id_appl = p;
            }
            if ((r = findsig(op,FALSE)) != SUCCESS) {
                p -> sig_done = SIG_UNKNOWN;
                return(r);
            }
            if (op -> signature == ERR_SIG) {
                p -> signature = ERR_SIG;
                p -> sig_done = SIG_DONE;
                return(SUCCESS);
	    }
	    /* Make sure operator signature is not an identifier. */
		while (op -> signature -> kind == OPRID
		       || op -> signature -> kind == LETTERID) {
		    NODE * decl = op -> signature -> id_last_definition;

		    if (decl -> kind == DECLARATION
			&& decl -> decl_sig_transp) {
			NODE * den = decl -> decl_denotation;

			if (den -> kind == LETTERID
			    || den -> kind == OPRID
			    || den -> kind == FUNCSIGNATURE) {
			    chgsig(op, den);
			} else {
			    break;
			}
#                   ifdef DEBUG
		      } else {
			  dbgmsg("Findapplsig: bad op sig\n");
			  abort(op, decl);
#                   endif
		    }
		}
	    if (op -> signature -> kind != FUNCSIGNATURE) {
		errmsg0(
                    op,
                    "Error - non-function used as operator"
                );
                p -> signature = ERR_SIG;
                p -> sig_done = SIG_DONE;
                return(SUCCESS);  /* i.e. this routine already */
                                  /* dealt with the error.     */
            }
        /* insert Valueof and constant applications, */
        /* omitted arguments,
        /* and type coercions (forgetting)           */
          {
            NODE * op_sig = op -> signature;
            NODE * ext_args;

            if (length(op_sig -> fsig_param_list) >= length(args)) {
                NODE * par_sig;
                NODE * nargs = emptylist();  /* modifed version  */
                                             /* of argument list */

		begin_map2lists (s, args,
				 r, op_sig -> fsig_param_list) {
		    par_sig = r -> par_signature;
		    while (par_sig -> kind == LETTERID
			   || par_sig -> kind == OPRID) {
			par_sig = par_sig -> id_last_definition
					  -> decl_denotation;
#                       ifdef TRACE
			  unparse_file = stdout;
			  printf("applsig: Changed parameter signature to ");
			  unparse(par_sig);
			  printf("\n");
#                       endif
		    }
		    if (s -> signature != ERR_SIG &&
                        par_sig -> kind == VALSIGNATURE &&
                        s -> signature -> kind != VALSIGNATURE) {
                        NODE * narg = coerce(s);

                        if((q = findsig(narg,FALSE)) != SUCCESS) {
                            p -> signature = NIL;
                            vfree(nargs);
                            p -> sig_done = SIG_UNKNOWN;
                            return(q);
                        }
                        addright(nargs, narg);
                    } else {
                        addright(nargs, s);
                    }
		} end_map2lists;
                if (length(op_sig -> fsig_param_list)
                    > length(args)) {
                  /* Add any missing arguments */
                    ext_args = infer_args(nargs,
                                          op_sig -> fsig_param_list,
                                          p -> ap_void_decl, op);
                    if (ext_args == NIL) {
                        p -> signature = ERR_SIG;
                        p -> sig_done = SIG_DONE;
                        errmsg0(p, "Can't infer missing arguments");
                        return(SUCCESS);
                    } else {
#                       ifdef TRACE
                          printf("Added args\n");
#                       endif
                        nargs = ext_args;
                    }
                }
                args = nargs;
                nargs = emptylist();
		begin_map2lists (s, args,
				 r, op_sig -> fsig_param_list) {
		  par_sig = r -> par_signature;
		  if (par_sig -> kind == LETTERID
		      || par_sig -> kind == OPRID) {
		    par_sig = sig_structure(par_sig);
		  }
		  if (par_sig -> kind == TYPESIGNATURE) {
		      NODE * s_par_sig;

		      substerr = SUCCESS;
		      s_par_sig = subst(par_sig,
					op_sig -> fsig_param_list,
					args);
		      lock(s_par_sig);
		      if (substerr == SUCCESS
			  && amatch(s -> signature, s_par_sig)
			  && match_delv != NIL) {
			  /* components need to be forgotten */
			  NODE * narg = mknode( MODPRIMARY,
						s,
						NIL,
						match_delv
					      );

			  IFTRACE(
			    printf("Adding forgetting node to:");
			    unparse_file = stdout;
			    unparse(s);
			    printf("\n");
			  )
			  narg -> mp_orig_length = match_len;
			  initfld(&(narg->signature),
				  delcomp(s -> signature,
					  match_delv));
			      /* s_par_sig would be OK, exc. */
			      /* for opt. info.              */
			  narg -> sig_done = SIG_DONE;
			  addright(nargs, narg);
		      } else {
			if (substerr != SUCCESS) {
			  /* Cant  decide whether and how to do */
			  /* coercion yet.                      */
			  p -> sig_done = SIG_UNKNOWN;
			  return(substerr);
			}
			addright(nargs, s);
		      }
		      vfree(unlock(s_par_sig));
		  } else {
		      addright(nargs, s);
		  }
		} end_map2lists;
                chgfld(&(p -> ap_args), nargs);
                args = nargs;
            }
          }
        /* compute result signature */
#           ifdef DEBUG
                if (op -> signature -> fsig_result_sig == NIL) {
                    dbgmsg("findsig:APPL: missing result sig\n");
                }
#           endif
            substerr = SUCCESS; /* reinitialize error indication */
            initsig(p,
		    subst(op -> signature -> fsig_result_sig,
                          op -> signature -> fsig_param_list,
                          args
                    )
            );
            /* Add special info for arrays */
              if (special_tp(op -> signature -> fsig_special)
                  == STD_ARRAY) {
                  fix_array_sig(p -> signature,
                                first(args), second(args));
              }
            if (substerr != SUCCESS) {
              /* clear signature field */
                chgsig(p, NIL);
                p -> sig_done = SIG_UNKNOWN;
            } else {
                p -> sig_done = SIG_DONE;
#               ifdef DEBUG
                    if (p -> signature == NIL) {
                        dbgmsg("findsig produced bad APPL sig\n");
                    }
#               endif
            }
            if (has_sig(p)
                && (p -> signature -> kind == FUNCSIGNATURE
                    || p -> signature -> kind == TYPESIGNATURE)) {
                /* leaving environment which might be needed */
                /* to evaluate a function produced by this   */
                /* application.                              */
		clear_slink_known(p -> signature);
                    /* We should really copy the signature  */
                    /* rather than zapping it in place, but */
                    /* this is safe, and it rarely matters. */
            }
            return(substerr);
}
