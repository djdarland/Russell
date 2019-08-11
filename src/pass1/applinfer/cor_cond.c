# include "parm.h"
# include "stree/ststructs.mh"

extern NODE * sel_true;     /* syntax tree coresponding to Boolean$True */

/*
 * cor_cond(d1, d2)
 *
 * input: d1,d2 are syntax trees coresponding to some denotation.
 *        They should be lists with a single element.
 *
 * output: the syntax tree corresponding to (d1 cor d2), i.e.
 *         corresponding to  IF d1 ==> Boolean$true[] # ELSE ==> d2 FI.
 */

NODE * cor_cond(d1, d2)
NODE *d1, *d2;
{
    NODE * gl, *ge1, *ge2;

    if (is_empty(d1) || is_empty(d2)) {
	return(mknode(GUARDEDLIST, emptylist()));
    }
    ge1 = mknode(GUARDEDELEMENT, first(d1), sel_true);
    ge2 = mknode(GUARDEDELEMENT, mknode(WORDELSE), first(d2));
    gl = mknode(GUARDEDLIST, mklist(ge1, ge2, -1));
    return( gl );
}
