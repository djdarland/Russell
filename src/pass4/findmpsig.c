/* Perform findsig's job for MODPRIMARY nodes with currently unknown */
/* signature.                                        */
# define TRACE
# undef TRACE
# define DEBUG

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

extern boolean Nflag;

# define UNDEFNAME ((sttrelptr) 0)   /* the name of an anonymous identifier */

extern int yynerrs;

extern int next_pre; /* needed so that comparison for declarations will */
		     /* continue to work.                               */

# define ERR_NODE_DEFINED
extern NODE * err_node;        /* node to be used for error message in lieu */
			       /* of current node.  Used by errmsg macros   */
extern char * err_msg;         /* message to be used in lieu of usual one   */

NODE * declerr;     /* declsig failure indication */

NODE * substerr;    /* subst error indication       */
		    /* Set to something other than SUCCESS if       */
		    /* subst is asked to substitute an incompletely */
		    /* expanded expression, as indicated by         */
		    /* dontsubst                                    */

extern int match_len;      /* length of argument type.  Set by amatch. */
extern unsigned * match_delv;  /* bitvector indicating necessary deletions */
			       /* set by amatch.                           */

struct cn * dontsubst; /* list of incompletely expanded nodes  */
		       /* which should not be substituted into */
		       /* signatures.                          */

extern int comp_index;

# ifdef VAX
    int nargs();
# endif

void find_inline();

void Gfind_inline();

NODE * declsig();

boolean is_const();

int comp_wlc();

boolean may_fail;         /* current signature deduction may fail */
			  /* without dire consequences.           */

static NODE * current_type; /* used as implicit parameter to comp_wlc */

NODE * findmpsig(p)
NODE * p;
{
    NODE * sig; /* current approximation to result signature */
    NODE * tm = p -> mp_type_modifier;
    NODE * q;
    Array *a;
    int a_len;
    unsigned * delv;    /* bit vector indicating deleted */
                        /* components.                   */
    extern boolean changed_strings;
                        /* Set by delcomp. indicates that    */
                        /* either constants or concatenation */
                        /* were replaced.                    */
    int i;


    /* find signature of original type */
        if ((q = findsig(p -> mp_primary, FALSE)) != SUCCESS) {
#           ifdef TRACE
              printf("Can't find signature of primary:\n");
              unparse_file = stdout;
              unparse(p -> mp_primary);
              printf("\n");
#           endif
            p -> sig_done = SIG_UNKNOWN;
            return(q);
        }
        sig = p -> mp_primary -> signature;
#       ifdef TRACE
          printf("orig_sig = %X\n", sig);
          if (sig != ERR_SIG) {
            unparse_file = stdout;
            unparse(sig);
            printf("\nrefcount = %X\n", sig -> refcount);
          }
#       endif
        if (sig == ERR_SIG) {
            p -> signature = ERR_SIG;
            p -> sig_done = SIG_DONE;
            return(SUCCESS);
        }
        if (sig -> kind != TYPESIGNATURE) {
            errmsg0(p, "Non-type used in type modification");
            p -> signature = ERR_SIG;
            p -> sig_done = SIG_DONE;
            return(SUCCESS);
        }
#   ifdef DEBUG
        if (tm == NIL) {
            dbgmsg("findsig: NIL type modifier\n");
            abort();
        }
#   endif
    switch(tm -> kind) {
      case WITHLIST:
        /* create array version of list */
            a = list_to_array(tm -> wl_component_list);
            a_len = a -> a_size;
        /* find signatures of all components */
            for (i = 0; i < a_len; i++) {
#               ifdef DEBUG
                    if(a->a_body[i] -> kind != DECLARATION) {
                        dbgmsg("findsig: bad wl\n");
                    }
#               endif
                (void) declsig(a->a_body[i]);
                if (declerr != SUCCESS) {
                    p -> sig_done = SIG_UNKNOWN;
#                   ifdef TRACE
                      printf("Can't find with list component signature:\n");
                      unparse_file = stdout;
                      unparse(a -> a_body[i] -> decl_id);
                      printf("\n");
#                   endif
                    free_array(a);
                    return(declerr);
                }
                if (a -> a_body[i]
                      -> decl_signature != ERR_SIG) {
                  if(a -> a_body[i] -> decl_signature
                         -> kind == VARSIGNATURE) {
                    free_array(a);
                    errmsg0(a -> a_body[i],
			    "Variable in with list");
                    p -> sig_done = SIG_DONE;
                    p -> signature = ERR_SIG;
                    return(SUCCESS);
                  }
                  /* Clear ARRAY_SIZE etc. indication, since */
                  /* it now applies to the wrong type        */
                  {
		    int tp;
		    NODE * sig = a -> a_body[i] -> decl_signature;

		    if (sig -> kind == FUNCSIGNATURE) {
		      tp = special_tp(sig -> fsig_special);
		      if (tp == ARRAY_SIZE
			  || tp == ARRAY_STD_NEW
			  || tp == ARRAY_PTR_NEW) {
			  sig -> fsig_special = 0;
		      }
		    }
                  }
                }
            }
        /* sort them */
	    current_type = p;
	    qsort(&a->a_body[0], a_len,
                  (sizeof (NODE *)), comp_wlc);
        /* check for duplicates */
            for (i = 0; i < a_len-1; i++) {
                if (comp_wlc(&(a->a_body[i]),
                    &(a->a_body[i+1])) == 0) {
                    errmsg1(tm, "Duplicate declaration of %s in with",
                            getname(a->a_body[i]->decl_id
                                    ->id_str_table_index));
		}
	    }
	/* construct deletion vector */
          {
            int delv_len; /* length in bytes */
            int delv_len_w; /* length in words */
            unsigned *t;

            p -> mp_orig_length = tsig_length(sig);
            delv_len = roundup(p -> mp_orig_length,
                               WORDLENGTH) >> 3;
            delv_len_w = delv_len >> (LOGWL - 3);
            if (delv_len > 0) {
                delv = (unsigned *)malloc(delv_len);
            } else {
                delv = 0;
            }
            for (t = delv; t < delv + delv_len_w; t++) {
                *t = 0;
            }
            for (i = 0; i < a_len; i++) {
                if(getcomp(sig,
                           a -> a_body[i] -> decl_id,
                           NIL,
                           a -> a_body[i] -> decl_signature, p,
                           NIL,
                           TRUE) != NIL) {
                    t = delv + (comp_index >> LOGWL);
                    *t |= 1 << (WORDLENGTH-1 - mod(comp_index,
                                                   WORDLENGTH));
                }
            }
          }
        /* delete components from signature */
          sig = lock(delcomp(sig, delv));
          if (changed_strings) {
            sig -> ts_string_code = NIL;
            sig -> ts_element_code = NIL;
          }
#         ifdef TRACE
            printf("sig after deletion = %X\n", sig);
            printf("refcount = %X\n", sig -> refcount);
#         endif
        /* add new values */
          for (i = 0; i < a_len; i++) {
            inscomp(sig,
                    a->a_body[i] -> decl_id,
                    a->a_body[i] -> decl_signature, p);
#           ifdef TRACE
              printf("sig after insertion = %X\n", sig);
              printf("refcount = %X\n", sig -> refcount);
#           endif
            a->a_body[i] -> decl_sel_index = comp_index;
          }
        /* replace with list with a sorted version */
          chgfld(&(tm -> wl_component_list), emptylist());
          for (i = 0; i < a_len; i++) {
            addright(tm -> wl_component_list, a->a_body[i]);
          }
        /* deallocate the array version */
          free_array(a);
          break;

      case HIDELIST:
      case EXPORTLIST:
        /* construct deletion vector */
          {
            int delv_len;  /* length in bytes */
            int delv_len_w;  /* length in words */
            unsigned *t;

            p -> mp_orig_length = tsig_length(sig);
            delv_len = roundup(p -> mp_orig_length,
                               WORDLENGTH
                              ) >> 3;
            delv_len_w = delv_len >> (LOGWL - 3);
            delv = (unsigned *)malloc(delv_len);
#           ifdef TRACE
                printf("findsig:export: delv_len_w = %d\n",
                       delv_len_w);
#           endif
            for (t = delv; t < delv + delv_len_w; t++) {
                *t = 0;
            }
	    begin_maplist(s, tm -> el_export_element_list) {
	      switch(s -> kind) {

              case EXPORTELEMENT:
                if (s -> ee_export_list != NIL) {
                  errmsg0(s,
                      "nested export lists not implemented");
                }
                if(getcomp(sig,
                           s -> ee_id,
			   (s -> ee_signature == NIL /* only an optimization */
			    || tm -> el_local_type_id != NIL
					       /* don't subst if id is given */
			    ? NIL : p -> mp_primary),
			   s -> ee_signature,
			   tm -> el_local_type_id == NIL? NIL : p,
                           NIL,
                           TRUE) != NIL) {
#                   ifdef TRACE
                        printf("found comp w/ index %d\n",
                               comp_index);
#                   endif
                    t = delv + (comp_index >> LOGWL);
                    *t |= 1 << (WORDLENGTH-1 - mod(comp_index,
                                                   WORDLENGTH));
                  if (s -> ee_signature == NIL &&
                      !is_unique(sig, s -> ee_id
                                      -> id_str_table_index)) {
                      errmsg1(s, "ambiguous export element: %s",
                              getname(s -> ee_id
                                        -> id_str_table_index));
                      unparse_file = stderr;
                      fprintf(stderr, "\ttype signature is: ");
                      unparse(sig);
                      fprintf(stderr, "\n");
                  }
                } else {
                  errmsg1(s,
                          "Missing export/hide list element: %s",
                          getname(s -> ee_id
                                    -> id_str_table_index));
                }
                break;
              case ALLCONSTANTS:
                {
                  int nconstants;  /* # of consts in orig. type */
                  int i;
                  NODE * dcs_node = first(sig -> ts_clist);
                  unsigned * base;

#                 ifdef DEBUG
                    if (dcs_node -> kind != DEFCHARSIGS) {
                      dbgmsg("findsig: bad DEFCHARSIGS");
                      abort();
                    }
#                 endif
#                 ifdef TRACE
                    printf("processing all constants\n");
#                 endif
                  base = &(dcs_node -> dcs_0);
                  /* Calculate # of constants */
                    nconstants = 0;
                    for(i = 0; i < NVECTORS; i++) {
                      nconstants += bitcnt(base[i]);
                    }
                  /* Fill in whole words */
                    for (t = delv;
                         nconstants >= WORDLENGTH;) {
                        *t = -1;
                        nconstants -= WORDLENGTH;
                        t++;
                    }
                  /* Fill in remaining bits */
                    for (i = 0; i < nconstants; i++) {
                      *t |= 1 << (WORDLENGTH-1
                                  - mod(i,WORDLENGTH));
                    }
                  break;
                }

#             ifdef DEBUG
                default:
                    dbgmsg("findsig: bad export list\n");
                    abort();
#             endif

              } /* end switch */
	    } end_maplist;
            if (tm -> kind == EXPORTLIST) {
              /* complement deletion vector */
#             ifdef TRACE
                printf("complementing deletion vector\n");
#             endif
                for (t = delv; t < delv + delv_len_w; t++) {
                  *t = ~(*t);
                }
            }
          }
        /* delete components from signature */
          sig = lock(delcomp(sig, delv));
          if (changed_strings) {
            sig -> ts_string_code = NIL;
            sig -> ts_element_code = NIL;
          }
#         ifdef TRACE
            printf("sig after hide deletion = %X\n", sig);
            unparse_file = stdout;
            unparse(sig);
            printf("\nrefcount = %X\n", sig -> refcount);
#         endif
        break;

#     ifdef DEBUG
        default:
            dbgmsg("findsig: bad type modifier\n");
#     endif
    }
    p -> mp_delete_v = (char *)delv;
    /* Fix up local type ids */
      {
        NODE * old_sig = sig;
        NODE * new_id;

#       ifdef TRACE
          printf("Fixing local id references\n");
#       endif
        if (sig -> ts_local_type_id == NIL) {
            new_id = mknode(LETTERID, UNDEFNAME);
        } else {
            new_id = copynode(sig -> ts_local_type_id);
        }
        new_id -> id_last_definition = sig;
        new_id -> id_def_found = TRUE;
        sig = tsubst(old_sig, sig, new_id, TRUE);
        vfree(unlock(old_sig));
#       ifdef TRACE
          printf("sig after substitution = %X\n", sig);
          unparse_file = stdout;
          unparse(sig);
          printf("\nrefcount = %X\n", sig -> refcount);
#       endif
        new_id -> id_last_definition = sig;
      }
    if (!Nflag || !(p -> mp_no_surr_loop)) {
	/* May need to introduce separate block and a.r. later */
	clear_slink_known(sig);
    }
    initsig(p, sig);
#   ifdef TRACE
      printf("final sig = %X\n", p -> signature);
      printf("refcount = %X\n", p -> signature -> refcount);
      printf("primary sig = %X\n", p -> mp_primary -> signature);
      unparse_file = stdout;
      unparse(p -> mp_primary -> signature);
      printf("\nrefcount = %X\n",
             p -> mp_primary -> signature -> refcount);
#   endif
    p -> sig_done = SIG_DONE;
    return(SUCCESS);
}


/* compare 2 with list components */
int comp_wlc(p, q)
NODE **p, **q;

{
    register int i;
    char *s, *t;
    boolean p_is_const, q_is_const;
    NODE * p_sig, * q_sig;

    s = (char *)getname((*p) -> decl_id -> id_str_table_index);
    t = (char *)getname((*q) -> decl_id -> id_str_table_index);
    i = strcmp(s, t);

    p_sig = (*p) -> decl_signature;
    q_sig = (*q) -> decl_signature;

    p_is_const = s[0] == '\'' && s[2] == '\'';
    if (p_is_const) {
	p_is_const = is_const(p_sig, current_type);
#       ifdef TRACE
	    printf("character: %c, p_is_const: %d, current_type: %X\n",
		   s[1], p_is_const, current_type);
	    unparse_file = stdout;
	    unparse(p_sig);
	    printf("\n");
	    unparse(current_type);
	    printf("\n");
#       endif
    }
    q_is_const = t[0] == '\'' && t[2] == '\'';
    if (q_is_const) {
	q_is_const = is_const(q_sig, current_type);
#       ifdef TRACE
	    printf("character: %c, q_is_const: %d, current_type: %X\n",
		   t[1], q_is_const, current_type);
	    unparse_file = stdout;
	    unparse(q_sig);
	    printf("\n");
	    unparse(current_type);
	    printf("\n");
#       endif
    }

    if (p_is_const && !q_is_const) return(-1);
    if (q_is_const && !p_is_const) return(1);

    if (i == 0)
	return(comp_st(p_sig, q_sig,
		       current_type, current_type));
    else
	return(i);
}

