/* Perform the task of findsig for identifier nodes. */
# define TRACE
# undef TRACE
# define DEBUG
# undef DEBUG
# define TRACE2
# undef TRACE2
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

# define UNDEFNAME ((sttrelptr) 0)   /* the name of an anonymous identifier */

extern int yynerrs;

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


extern NODE * failed_asig;  /* Set by amatch to indicate last failure */
extern NODE * failed_psig;  /* Used as a hint in error message        */
extern NODE * failed_comp;

extern struct cn * dontsubst; /* list of incompletely expanded nodes  */
                  /* which should not be substituted into */
                  /* signatures.                          */

extern int comp_index;

# ifdef VAX
    int nargs();
# endif

boolean may_fail;         /* current signature deduction may fail */
			  /* without dire consequences.           */

NODE * findidsig(p)
register NODE * p;
{
    NODE * q;

    if(p -> id_str_table_index == -1) {
        /* anonymous local type identifier */
        chgsig(p, curr_tsig);
        p -> sig_done = SIG_UNKNOWN;
                 /* must be recomputed next time */
        return(SUCCESS);
    }
#   ifdef TRACE
      printf("Finding signature of %s, may_fail = %d\n",
             getname(p -> id_str_table_index), may_fail);
#   endif
    /* find the right instance */
        /* clear earlier failure indication */
          if (failed_asig != NIL) {
            vfree(unlock(failed_asig));
          }
          failed_asig = NIL;
        if ((q = finddecl(p)) != SUCCESS) {
            p -> sig_done = SIG_UNKNOWN;
#           ifdef TRACE
              printf("Can't find declaration for\n");
              unparse_file = stdout;
              unparse(p);
              printf("\n");
#           endif
            return(q);
        }
    /* Check if finddecl already found signature */
        if (p -> sig_done == SIG_DONE) {
            return(SUCCESS);
        }
    if (!(p -> id_def_found)) {
      if(may_fail) {
        /* May get another chance at this identifier */
        /* with arguments.                           */
	p -> sig_done = SIG_UNKNOWN;
#       ifdef TRACE
          printf("Didn't find:\n");
          unparse_file = stdout;
          unparse(p);
          printf("\n");
#       endif
        return(p);
      } else {
        errmsg1(
            p,
            "No declaration with appropriate signature for %s",
            getname(p -> id_str_table_index)
        );
        if (p -> id_appl != NIL) {
            unparse_file = stderr;
            maplist(s, p -> id_appl -> ap_args, {
                fprintf(stderr, "\tArgument: ");
                unparse(s);
                fprintf(stderr, "\n\tArgument signature: ");
                unparse(s -> signature);
                fprintf(stderr, "\n");
            });
        }
        if (failed_asig != NIL) {
            fprintf(stderr, "\tAttempted to match:\n\t");
            unparse_file = stderr;
            unparse(failed_asig);
            fprintf(stderr, "\n\tagainst:\n\t");
            unparse(failed_psig);
            fprintf(stderr, "\n");
            if (failed_comp != NIL) {
              if(failed_comp -> kind == TSCOMPONENT) {
                fprintf(stderr,
                        "\t\tOffending parameter component:\n\t\t");
                unparse(failed_comp -> tsc_id);
                fprintf(stderr, ":");
                unparse(failed_comp -> tsc_signature);
                fprintf(stderr, "\n");
              } else {
                fprintf(stderr, "\t\tMissing constant\n");
              }
            }
        }
        if (p -> signature != NIL) {
            unparse_file = stderr;
            fprintf(stderr, "\tSpecified signature: ");
            unparse(p -> signature);
            fprintf(stderr, "\n");
        }
        p -> signature = ERR_SIG;
        p -> sig_done = SIG_DONE;
        return(SUCCESS);  /* i.e. this routine already */
                          /* handled the error.        */
      }
    }
    if (p -> sel_type != NIL) {
      NODE * tsig;
      boolean sel_index_correct = TRUE;

      /* get signature of type component */
        if( (q = findsig(p -> sel_type,FALSE)) != SUCCESS ) {
            NODE * curr_type = p -> sel_type;
            NODE * curr_decl;

#           ifdef TRACE
              printf("Didn't find sel type signature for %X\n", p);
#           endif
            sel_index_correct = FALSE;
            if (!trivial(p -> sel_type)) {
              dontsubst = cn_cons(p -> sel_type, dontsubst);
            }

            /* Now try to find this component anyway */

            /* First get at the heart of the matter by    */
            /* skipping through irrelevant modifications  */
            /* etc.                                       */

              for(;;) {
#               ifdef TRACE
                  printf("node = %X, curr_type = %X\n",p, curr_type);
                  unparse_file = stdout;
                  unparse(curr_type);
                  printf("\n");
#               endif
                switch(curr_type -> kind) {
                  case LETTERID:
                  case OPRID:
                    if (curr_type -> id_str_table_index == -1) {
                      tsig = curr_tsig;
                      goto found_tsig;
                    }
                    /* find the right instance */
                      if (finddecl(curr_type) != SUCCESS
                          || !(curr_type -> id_def_found)) {
                        p -> sig_done = SIG_UNKNOWN;
                        return(q);
                      }
                    if (curr_type -> sel_type != NIL) {
                      if((q = findsig(curr_type, FALSE)) != SUCCESS) {
                        p -> sig_done = SIG_UNKNOWN;
                        return(q);
                      }
                      tsig = curr_type -> signature;
                      goto found_tsig;
                    }
                    curr_decl = curr_type -> id_last_definition;
                    switch (curr_decl -> kind) {
                      case DECLARATION:
                        if (curr_decl -> decl_signature != NIL) {
                          tsig = curr_decl -> decl_signature;
                          goto found_tsig;
                        }
                        curr_type = curr_decl -> decl_denotation;
                        break;
                      case PARAMETER:
                        tsig = curr_decl -> par_signature;
                        goto found_tsig;
                      case TYPESIGNATURE:
                        tsig = curr_decl;
                        goto found_tsig;
                      default:
                        curr_type = curr_decl;
                    }
                    break;
                  case MODPRIMARY:
                    {
                      NODE * tm = curr_type -> mp_type_modifier;
                      switch (tm -> kind) {
                        case EXPORTLIST:
                        case HIDELIST:
                          curr_type = curr_type -> mp_primary;
                          break;
                        case WITHLIST:
			  begin_maplist(s, tm -> wl_component_list) {
                            if (s -> decl_id -> id_str_table_index
                                == p -> id_str_table_index) {
                              NODE * prim = curr_type -> mp_primary;
                              NODE * r = findsig(prim, FALSE);

#                             ifdef TRACE
                                printf("Found with list component\n");
#                             endif
                              if (r != SUCCESS ||
                                  prim -> signature == ERR_SIG ||
                                  prim -> signature -> kind !=
                                    TYPESIGNATURE ||
                                  hascomp(prim -> signature,
                                       p -> id_str_table_index)) {
                                /* Dont know whether this is the */
                                /* right instance                */
#                               ifdef TRACE
				  printf("Also primary component\n");
#                               endif
                                p -> sig_done = SIG_UNKNOWN;
                                return(q);
                              } else {
                                /* known to be in with list */
                                NODE * t = declsig(s);
                                NODE * subst_sig;
                   
#                               ifdef TRACE
                                  printf("Occurs only in with list\n");
#                               endif
                                if (declerr != SUCCESS) {
                                    p -> sig_done = SIG_UNKNOWN;
                                    return(q);
                                } else {
                                    substerr = NIL;
                                    subst_sig = tsubst(t, curr_type, p -> sel_type, FALSE);
                                    if (substerr != NIL) {
#                                     ifdef TRACE
                                        printf("substitution error\n");
#                                     endif
                                      p -> sig_done = SIG_UNKNOWN;
                                      return(q);
                                    }
                                    if (!def_match(subst_sig, NIL, p -> id_appl, p, FALSE)) {
                                        continue;  /* Try rest of with list */
                                    }
                                    initsig(p, subst_sig);
                                    p -> sig_done = SIG_UNKNOWN;
                                        /* sel_index is wrong &   */
                                        /* needs to be calculated */
                                        /* later.                 */
#                                   ifdef TRACE
                                      printf("success\n");
#                                   endif
                                    return(SUCCESS);
                                }
                              }
                            }
			  } end_maplist;
                          /* doesnt appear in with list */
#                           ifdef TRACE
			      printf("Not in with list\n");
#                           endif
                          curr_type = curr_type -> mp_primary;
                          break;
                      }
                    }
                    break;
                  default:
                    if((q = findsig(curr_type, FALSE)) != SUCCESS) {
                      p -> sig_done = SIG_UNKNOWN;
                      return(q);
                    }
                    tsig = curr_type -> signature;
                    goto found_tsig;
                }
              }
        } else {
          tsig = p -> sel_type -> signature;
        }
      found_tsig:
	if (tsig != ERR_SIG && tsig -> kind != TYPESIGNATURE) {
	  tsig = sig_structure(tsig);
	  if (tsig -> kind != TYPESIGNATURE) {
	    errmsg1(
                p,
                "Identifier %s selected from non-type",
                getname(p -> id_str_table_index)
            );
            p -> sig_done = SIG_DONE;
            chgsig(p, ERR_SIG);
	    return(SUCCESS); /* error has been dealt with */
	  }
        }
        substerr = SUCCESS;
        if( (q = getcomp(tsig,
                         p,
                         p -> sel_type,
                         p -> signature, NIL,
                         p -> id_appl, TRUE)) == NIL
         && (q = getcomp(tsig,
                         p,
                         p -> sel_type,
                         p -> signature, NIL,
                         p -> id_appl, FALSE)) == NIL ) {
            /* type has no such component */
            errmsg1(
                p,
                "No appropriate type component %s",
                getname(p -> id_str_table_index)
            );
            if (p -> id_appl != NIL) {
              unparse_file = stderr;
              maplist(s, p -> id_appl -> ap_args, {
                fprintf(stderr, "\tArgument: ");
                unparse(s);
                fprintf(stderr, "\n\tArgument signature: ");
                unparse(s -> signature);
                fprintf(stderr, "\n");
              });
            }
            if (p -> signature != NIL) {
              unparse_file = stderr;
              fprintf(stderr, "\tSpecified signature: ");
              unparse(p -> signature);
              fprintf(stderr, "\n");
            }
            if ((q = getcomp(tsig,
                             p,
                             p -> sel_type,
                             NIL, NIL,
                             NIL, TRUE)) != NIL
                 && is_unique(tsig, p -> id_str_table_index) ) {
              fprintf(stderr, "\tActual component signature: ");
              unparse_file = stderr;
	      unparse(q);
	      fprintf(stderr, "\n");
	      if (q -> kind == FUNCSIGNATURE
		  && ! is_empty(q -> fsig_param_list)
		  && p -> id_appl != NIL
		  && ! is_empty (p -> id_appl -> ap_args)) {
		  if (amatch( first(p -> id_appl -> ap_args) -> signature,
			      first(q -> fsig_param_list) -> par_signature)) {
		    fprintf(stderr, "\tFirst argument signature matches\n");
		  } else {
		    extern NODE * diff_p, * diff_q;
		    if (diff_p != NIL) {
			fprintf(stderr, "\tFirst arg match failed at ");
			unparse(diff_p);
			fprintf(stderr, " and ");
			unparse(diff_q);
			fprintf(stderr, "\n");
		    } else {
			fprintf(stderr, "\tMatch of first argument failed\n");
		    }
		  }
	      }
            } else {
              fprintf(stderr, "\tActual type signature: ");
              unparse_file = stderr;
              unparse(tsig);
              fprintf(stderr, "\n");
            }
            p -> sig_done = SIG_DONE;
            chgsig(p, ERR_SIG);
            return(SUCCESS);  /* i.e. this routine already */
                              /* caught the error.         */
        }
        if (sel_index_correct) {
#         ifdef TRACE
            printf("Setting selection index for %X(%s) to %d\n",
                   p, getname(p -> id_str_table_index),comp_index);
#         endif
          p -> sel_index = comp_index;
        }
        if (substerr == SUCCESS) {
            chgsig(p, q);
            p -> sig_done = sel_index_correct? SIG_DONE
                                             : SIG_UNKNOWN;
        } else {
#           ifdef TRACE
              printf("bad substitution\n");
              printf("Returning substerr: 0x%X\n");
#           endif
            p -> sig_done = SIG_UNKNOWN;
        }
        return(substerr);
    }
    /* not selection */
    q = declsig(p -> id_last_definition);
    if (declerr == SUCCESS) {
        chgsig(p, q);
        p -> sig_done = SIG_DONE;
    } else {
        p -> sig_done = SIG_UNKNOWN;
    }
#     ifdef TRACE
        if (declerr != SUCCESS) {
            printf("Returning declerr: 0x%X\n", declerr);
        }
#     endif
    return(declerr);
}
