/* Reorder type signatures, conditionals, and loops.  Reordering of   */
/* type signatures is essential since subsequent passes assume a      */
/* normal form.  Reordering of conditionals and loops generalizes     */
/* signature matching and makes sure the programmer doesn't rely      */
/* on sequential evaluation of guarded commands.  It also makes life  */
/* easier for the code generator, since else guards will be permuted  */
/* to the end.                                                        */

# include "parm.h"

# include "stree/ststructs.mh"

extern int stplinks[];



/* Fix up the subtree headed by p */
reorder(p)
NODE * p;
{
    register int * q;   /* pointer to next field of p to be recursively     */
                        /* examined.                                        */
    register int v;     /* bit vector specifying primary link fields of *p  */
                        /* shifted so that the most significant bit         */
                        /* corresponds to q.                                */

    if (p == NIL) return;
    /* recursively examine subtrees */
        if (is_list(p)) {
            maplist(e, p, {
                reorder(e);
            });
        } else {
            v = stplinks[p -> kind];
            q = (int *) p;
            while ( v != 0 ) {
                if ( v < 0 /* msb is set */) {
                    reorder(*q);
                }
                q++;
                v <<= 1;
            }
	}
    /* Now massage the subtree in question */
        switch( p -> kind ) {
            case GUARDEDLIST:
            case LOOPDENOTATION:
                /* reorder list of guarded denotations */
                    gden_order(p);
                break;
            case TYPESIGNATURE:
                /* reorder type signature */
                    tsig_order(p);
                break;
        }
}

