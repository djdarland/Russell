# include "parm.h"

# include "stree/ststructs.mh"

# include "decl_pairs.h"

/*
 * Routines to manipulate decl_list, the list of declaration pairs
 * used in expression comparisons and substitutions of various kinds.
 */

/*
 * Return TRUE iff p and q are corresponding declarations
 */
boolean
dl_match(p,q)
NODE * p;
NODE * q;
{
	struct p_pair * hd;
	ConsNode * tl;

	for(tl = decl_list; tl != NIL; tl = cn_tail(tl)) {
		hd = (struct p_pair *) cn_head(tl);
                if (hd -> pp_1 -> pre_num == p -> pre_num
                    && hd -> pp_2 -> pre_num == q -> pre_num) {
                    return(TRUE);
                }
	}
	return(FALSE);
}

/*
 * Return the second element of the pair whose first element is the
 * declaration for the identifier p.
 * Return the declaration for p if no such pair exists.
 */
NODE *
dl_new_decl(p)
NODE * p;
{
	struct p_pair * hd;
	ConsNode * tl;

#       ifdef DEBUG
	  if (p -> kind != LETTERID) {
	    dbgmsg("dl_new_decl: bad arg\n");
	  }
#       endif
	for(tl = decl_list; tl != NIL; tl = cn_tail(tl)) {
		hd = (struct p_pair *) cn_head(tl);
		if (is_declared_by(p, hd -> pp_1)) return(hd -> pp_2);
	}
	return(p -> id_last_definition);
}
