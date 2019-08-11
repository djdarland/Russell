# include "parm.h"

# include "stree/ststructs.mh"

# include "sigs.h"

extern int indx_ValueOf;

/*
 *  Return an expression which corresponds to p coerced to a value 
 * signature (if remotely possible).  If this is clearly impossible
 * just return p.  It is assumed that p's signature is known.
 *  Note that the only correctness claim for this procedure is
 * that if the coercion is possible it is done correctly.  No
 * attempt is made to determine whether the resulting expression
 * will be signature correct.  This is left up to checksigs.
 * The resulting expression may no longer have known signature.
 *  The expression tree p may be destructively updated in order
 * to propagate coercions into LET blocks.  This is necessary
 * in that it reduces the likelihood of bogus import rule violations.
 * It also enhances the effectiveness of allocate.c in allocating
 * variables to registers.
 *  We claim that there exists a subtle argument that shows that the
 * destructive update cannot update signatures, and is thus safe.
 */
NODE * coerce(p)
NODE * p;
{
    register NODE * nappl;  /* New application node */

    if(p -> signature == ERR_SIG) return(p);

    if (p -> kind == BLOCKDENOTATION) {
	(void) replace_last(p -> bld_den_seq, coerce(last(p -> bld_den_seq)));
	chgfld(&(p -> signature), NIL);
	p -> sig_done = SIG_UNKNOWN;
	return(p);
    } else if (p -> kind == USELIST) {
	(void) replace_last(p -> usl_den_seq, coerce(last(p -> usl_den_seq)));
	chgfld(&(p -> signature), NIL);
	p -> sig_done = SIG_UNKNOWN;
	return(p);
    }

    switch(p -> signature -> kind) {
	case VALSIGNATURE:
	    return(p);
	    break;

	case VARSIGNATURE:
	    {
		NODE * nop; /* new operator node */

		nop = mknode(LETTERID, indx_ValueOf);
		initfld(&(nop -> sel_type), p -> signature -> var_denotation);
		nop -> id_def_found = TRUE;
		nappl = mknode(APPLICATION, nop, mklist(p, -1));
		nappl -> vlineno = nop -> vlineno = p -> vlineno;
		nappl -> pre_num = nop -> pre_num = p -> pre_num;
		nappl -> post_num = nop -> post_num = p -> post_num;
		return(nappl);
	    }
	    break;

	case FUNCSIGNATURE:
	    if(length(p->signature->fsig_param_list) == 0) {
		nappl = mknode(APPLICATION, p, emptylist());
		nappl -> vlineno = p -> vlineno;
		nappl -> pre_num = p -> pre_num;
		nappl -> post_num = p -> post_num;
		return(nappl);
	    } else {
		/* clearly hopeless */
		return(p);
	    }
	    break;

	default:
	    /* again hopeless */
	    return(p);
    }
}
