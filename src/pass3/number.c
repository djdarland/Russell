/* assign pre- and post-order numbers to each node in the syntax tree */

# include "parm.h"

# include "stree/ststructs.mh"

# include "stree/stplinks.mh"

int next_pre; next_post;   /* next pre- and post-order numbers to be */
                                  /* assigned.                              */

/* Number the subtree headed by p */
number(p)
NODE * p;
{
    register int * q;   /* pointer to next field of p to be recursively     */
                        /* examined.                                        */
    register int v;     /* bit vector specifying primary link fields of *p  */
                        /* shifted so that the most significant bit         */
                        /* corresponds to q.                                */
    if ( p == NIL ) return;
    p -> pre_num = next_pre++;
    /* recursively examine subtrees */
        if (is_list(p)) {
            maplist(e, p, {
                number(e);
            });
        } else {
            v = stplinks[p -> kind];
            q = (int *) p;
            while ( v != 0 ) {
                if ( v < 0 /* msb is set */) {
                    number(*q);
                }
                q++;
                v <<= 1;
            }
        }
    p -> post_num = next_post++;
}

