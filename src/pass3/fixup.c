# include "parm.h"

# include "stree/ststructs.mh"

/*
 *  Pass3 driver.  Call various small fix up passes in succession.
 */

void fixup(p)
NODE * p;
{
	number(p);  /* pre- and postorder numbers */
	reorder(p); /* reorder type signatures and guarded commands */
	sigids(p);  /* fix up symbol table for certain kinds of ids */
}
