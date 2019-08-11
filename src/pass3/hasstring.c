# include "parm.h"

# include "stree/ststructs.mh"

# include "is_local.h"

extern int indx_sconc, indx_pconc, indx_empty;

/*
 *  hasstring(type_signature, string)
 * 
 *  Returns either TRUE or FALSE depending on whether the given type
 * signature has all components required for expansion of the given
 * string.  String is a pointer to a QSTR or UQSTR node.
 *  It is assumed that the type signature has been converted to
 * normal form.
 */

hasstring(sig,string)
NODE * sig;
NODE * string;
{
    register NODE * p;
    register char * curr_char;  /* character currently being checked for */
    boolean has_empty;
    NODE * const_node;

    int i;

#   ifdef DEBUG
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("hasstring: bad type signature\n");
            abort();
	}
#   endif

    switch(string -> kind) {
	case QSTR:
	    if(!hascomp(sig, indx_sconc)) {
		return(FALSE);
	    }
	    /* check whether sig has a constant '' */
		has_empty = FALSE;
		maplist(p, sig -> ts_clist, {
		    if (p -> kind == TSCOMPONENT) {
			if (p -> tsc_id -> id_str_table_index == indx_empty) {
			     if (is_const(p->tsc_signature,sig))
				has_empty = TRUE;
			}
		    }
		});
		if (!has_empty) return(FALSE);
	    break;
	case UQSTR:
	    if(!hascomp(sig, indx_pconc)) {
		return(FALSE);
	    }
	    break;
#       ifdef DEBUG
	    default:
		dbgmsg("hasstring: bad string\n");
#       endif
    }

    /* check that all characters are present as constants in the type */
	const_node = first(sig -> ts_clist);
#       ifdef DEBUG
	    if(const_node -> kind != DEFCHARSIGS) {
		dbgmsg("hasstring: Unnormalized type signature\n");
	    }
#       endif
	for (curr_char = string -> str_string; *curr_char != '\0'; curr_char++) {
	    /* Check whether the character is present in const_node */
		unsigned word;
		int bitno;
		int wordno;
		unsigned * base = &(const_node -> dcs_0);

		wordno = ((int) *curr_char) / WORDLENGTH;
		word = *(base + wordno);
		bitno = ((int) *curr_char) - wordno * WORDLENGTH;

		if ( (((int) word) << bitno) >= 0) {
		    /* constant does not appear */
			return(FALSE);
		}
	}
    return(TRUE);
}

