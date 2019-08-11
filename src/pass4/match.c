# include <stdio.h>
# include "parm.h"
# include "arith.h"

# include "stree/ststructs.mh"

# include "sigs.h"

# include "pass3/decl_pairs.h"

extern FILE * unparse_file;

/* Knows about implementation of lists.  There doesn't seem to be a clean */
/* and efficient way to merge two lists otherwise.                        */

extern unsigned stplinks[];

/*
 * amatch(arg_sig, par_sig)
 *
 *  Asymmetric match of argument signature against parameter
 * signature.  If both are type signatures, both forgetting
 * and reordering may be applied to par_sig.  (Reordering is
 * trivial since it is assumed that all type signatures
 * are in canonical form.)
 *  Note however
 * that it is assumed that par_sig has already been expanded by
 * substituting arguments for parameters.
 *  Returns TRUE or FALSE depending on whether the arguments match.
 *  Sets match_delv to point to a bit vector describing wich type
 * components need to be forgotten when a type argument is passed.
 * NIL indicates no forgetting is necessary.  If the argument signature
 * is a type signature match_len is set to the length of the type 
 * representation.
 *  If the match fails, but was "close", failed_asig and failed_psig are set.
 * If both signatures are type signatures,
 * failed_comp is set to the component of the signature responsible
 * for the failure.
 */

unsigned * match_delv;
int match_len;
static int delv_len;

NODE * failed_comp;
NODE * failed_asig;
NODE * failed_psig;

/* Initialize delv to an appropriate block of memory    */
/* if not done already.                                 */
/* assumes that match_len has been properly initialized */
#define init_delv() \
    if (match_delv == NIL) { \
	delv_len = roundup(match_len, WORDLENGTH) >> 3; \
	/* allocate deletion vector */ \
            match_delv = (unsigned *)malloc(delv_len); \
            delv_len >>= LOGWL - 3; /* Convert to words */\
	    for (s = match_delv; \
		s < match_delv + delv_len; s++) { \
		*s = 0; \
	    } \
    }

amatch(asig,psig)
NODE * asig,*psig;
{
    int asig_ind = 0;   /* current component number in asig */
    int i, j, k;
    unsigned *s, *t;

    if (failed_asig != NIL) {
	vfree(unlock(failed_asig));
    }
    if (failed_psig != NIL) {
	vfree(unlock(failed_psig));
    }
    if (failed_comp != NIL) {
	vfree(unlock(failed_comp));
    }
    match_delv = NIL;
    failed_comp = failed_asig = failed_psig = NIL;
    if (asig == ERR_SIG || psig == ERR_SIG) return(TRUE);
    /* Replace identifier signatures by their bindings           */
    /* This is safe because we checked for circularity in sigids */
      if (asig -> kind == LETTERID || asig -> kind == OPRID) {
	asig = sig_structure(asig);
      }
      if (psig -> kind == LETTERID || psig -> kind == OPRID) {
	psig = sig_structure(psig);
#       ifdef TRACE
	  printf("Match: Replaced psig by (%X) ", psig);
	  unparse_file = stdout;
	  unparse(psig);
	  printf("\n");
#       endif
      }
    if (asig -> kind != psig -> kind) return(FALSE);
    if (asig -> kind != TYPESIGNATURE) {
	if (comp_st(asig, psig, NIL, NIL) != 0) {
	    failed_asig = lock(asig);
	    failed_psig = lock(psig);
	    return(FALSE);
	} else {
	    return(TRUE);
	}
    }
    /* Both are type signatures */
    {
	struct cn *acomps, *pcomps; /* pointers to lists of type components */
				    /* remaining to be examined.            */
	NODE * acomp, * pcomp;      /* current type components              */
	unsigned * p, * q;
	int i;

	match_len = tsig_length(asig);

        acomps = asig -> ts_clist -> lh_first;
	pcomps = psig -> ts_clist -> lh_first;
#       ifdef DEBUG
	    if (acomps == NIL) {
		dbgmsg("amatch: NIL type sig\n");
	    }
#       endif
	pcomp = (NODE *)cn_head(pcomps);
	while (acomps != NIL) {
	    acomp = (NODE *)cn_head(acomps);
	    switch(acomp -> kind) {
		case DEFCHARSIGS:
#                   ifdef DEBUG
			if (pcomp -> kind != DEFCHARSIGS) {
			    dbgmsg("amatch: non normal form type sig\n");
			}
#                   endif
		    p = &(acomp -> dcs_0);
		    q = &(pcomp -> dcs_0);
		    for (i = 0; i < NVECTORS; i++) {
			if (((*q) & ~(*p)) != 0) {
			    failed_asig = lock(asig);
			    failed_psig = lock(psig);
			    failed_comp = lock(pcomp);
			    return(FALSE);
			} else if (((*p) & ~(*q)) != 0) {
			    /* forgetting required */
			    init_delv();
			    j = *p;
			    k = *q;
			    while (j != 0 /* argument components left */) {
				if (j < 0) {
				    /* argument component in this position */
				    if (k >= 0) {
				      /* delete this component */
					t = match_delv
                                            + (asig_ind >> LOGWL);
					*t |=  1 << (WORDLENGTH-1 -
						     mod(asig_ind,WORDLENGTH));
				    }
				    asig_ind++;
				}
				j <<= 1; k <<= 1;
			    }
			} else {
			    asig_ind += bitcnt(*p);
			}
			p++; q++;
		    }
		    pcomps = cn_tail(pcomps);
		    if (pcomps != NIL) {
			pcomp = (NODE *)cn_head(pcomps);
		    }
		    break;
		case TSCOMPONENT:
		    if ( pcomps == NIL ||
			 acomp -> tsc_id -> id_str_table_index !=
			 pcomp -> tsc_id -> id_str_table_index ||
			 ( acomp -> tsc_signature != ERR_SIG
			   && pcomp -> tsc_signature != ERR_SIG
                           && comp_st(acomp -> tsc_signature,
				       pcomp -> tsc_signature,
				       asig, psig, FALSE) != 0 )) {
			/* no matching parameter component */
			init_delv();
                        t = match_delv + (asig_ind >> LOGWL);
			*t |= 1 << (WORDLENGTH-1 - mod(asig_ind,WORDLENGTH));
		    } else {
			/* pcomp matches */
			if (pcomps != NIL)
			    pcomps = cn_tail(pcomps);
			if (pcomps != NIL) 
			    pcomp = (NODE *)cn_head(pcomps);
		    }
		    asig_ind++;
		    break;
#           ifdef DEBUG
		default:
		    dbgmsg("amatch: bad type signature component\n");
#           endif
	    }
	    acomps = cn_tail(acomps);
	}
	if (pcomps != NIL) {
	    failed_asig = lock(asig);
	    failed_psig = lock(psig);
	    failed_comp = lock(pcomp);
	    return(FALSE);
	} else {
	    return(TRUE);
	}
    }
}


