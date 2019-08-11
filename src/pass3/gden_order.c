# include "parm.h"

# include "stree/ststructs.mh"

# include "stree/Array.h"

int comp_gden();

Array * list_to_array();


/*
 *  gden_order(guarded_list_or_loop)
 *
 *  Sort the list of guarded commands into some arbitrary but well-defined
 * order.  All but one else clause is deleted, and that clause appears at
 * the end.
 */

gden_order(gden)
NODE * gden;
{
    Array * a;  /* array representation of the list of guarded denotations */
    int a_len;  /* number of pointers in a */
    register int i,j;
    unsigned *p, *q;
    NODE * r;

#   ifdef DEBUG
	if (gden->kind != LOOPDENOTATION && gden->kind != GUARDEDLIST) {
	    dbgmsg("gden_order: Improper argument: %x\n", gden);
	    abort();
	}
#   endif
    a = list_to_array(gden -> gl_list);
    a_len = a -> a_size;

    /*
	printf("#%x,%d,",a,a_len);
	for(i = 0; i < a_len; i++) printf("%x,",a->a_body[i]);
	printf("\n");
    */
    /* Sort the array */
	qsort(&a->a_body[0], a_len, (sizeof (NODE *)), comp_gden);

    /*
       printf("#%x,%d,",a,a_len);
       for(i = 0; i < a_len; i++) printf("%x,",a->a_body[i]);
       printf("\n");
    */
    /* Put everything back into the loop or conditional node */
	chgfld(&(gden -> gl_list), emptylist());
        for (i = 0; i < a_len; i++) {
	    addright(gden -> gl_list, a->a_body[i]);
            /* stop after the first else clause */
		if(a->a_body[i] -> ge_guard -> kind == WORDELSE) break;
        }
        free_array(a);
}

/*
 * compare two guarded denotation nodes, imposing an arbitrary order on the
 * guards, except that anything with a WORDELSE guard is infinitely large.
 */
comp_gden(p, q)
NODE **p, **q;
{
    register int i;

    i = comp_st((*p) -> ge_guard, (*q) -> ge_guard, 
		NIL, NIL /* may be wrong, but no -1 stt indicees
			    are ever generated within conditionals,
			    so it doesn't matter */);
    if (i == 0) {
	i = comp_st((*p) -> ge_element, (*q) -> ge_element, NIL, NIL);
    }
    return(i);
}
