/* 
 *   Primitive list operations.
 *   These and the roiutines in mknode.c should be the only routines
 *   that know about the implementation of lists.
 */

# include "parm.h"

# include <stdio.h>

# include <varargs.h>

# include "ststructs.mh"

# include "Array.h"

# ifdef BAD
    extern boolean BADflag;
# endif

extern int stsize[];
extern int stptrs[];
extern int stmkfields[];


/*
 * add node e at beginning of list l.
 * if e is NIL do nothing
 * return l
 */
NODE * addleft(l,e)
NODE *l,*e;
{
    register ConsNode * p;
    if (e == NIL) return(l);

#   ifdef DEBUG
        if( !is_list(l) ) {
	    dbgmsg("addleft: bad arg: kind = %s\n", kindname(l->kind));
            abort();
        }
#   endif

    if( is_empty(l) ) {
        p = cn_cons(lock(e),NIL);
        l->lh_first = p;
        l->lh_last = p;
    } else {
        l -> lh_first = cn_cons(lock(e), l -> lh_first);
    }
#   ifdef BAD
        if (BADflag)
            flcheck(0,0);
        else
            flcheck(0,1);
#   endif
    return ( l );
}

/*
 * Add node e at end of list l.
 * If e is NIL do nothing.
 */
LIST addright(l,e)
LIST l;
NODE *e;
{
    register ConsNode * p;

    if ( e == NIL ) return(l);

#   ifdef DEBUG
        if( !is_list(l) ) {
            dbgmsg("\naddright: bad arg: kind = %s\n", kindname(l -> kind));
            abort();
        }
#   endif

    p = cn_cons(lock(e), NIL);
    if( is_empty(l) ) {
        l -> lh_first = p;
        l -> lh_last = p;
    } else {
        cn_settail( l -> lh_last, p );
        l -> lh_last = p;
    }
#   ifdef BAD
        if (BADflag)
            flcheck(0,0);
        else
            flcheck(0,1);
#   endif
    return ( l );
}

/*
 * mklist( e1, ..., ek, -1 )
 *
 * Paste e1 ... ek together into a list and return it.
 * If ei is NIL, it is ignored.
 */

LIST mklist( va_alist )
va_dcl
{
va_list lp;
register unsigned p;
register NODE *l;

    va_start(lp);
    p = va_arg(lp, unsigned);
    l = emptylist();
    while( p != -1 ) {
        addright( l, p );
        p = va_arg(lp, unsigned);
    }
    va_end(lp);
    return ( l );
}

/*
 * length(l)
 *
 * assuming l is a list
 *   return the number of elements in it
 */
int length(l)
NODE *l;
{
register int count = 0;

#   ifdef DEBUG
        if ( !is_list(l) ) {
	    dbgmsg("length: nonlist arg\n");
            abort();
        }
#   endif
    maplist(p, l, count++);
    return ( count );
}



/*
 * list_to_array(l)
 *
 * input: A list l.
 *
 * output: An array containing pointers to the elements of l.
 *         The elements of l are locked.
 *         l is vfreed (but unaffected if it has nonzero reference count).
 */
Array * list_to_array(l)
NODE * l;
{
    Array * a;      /* the array */
    int len;        /* size of the list */

#   ifdef DEBUG
	if(!is_list(l)) {
	    dbgmsg("list_to_array: bad arg: %x\n",l);
	}
#   endif
    len = length(l);
    a = (Array *)salloc( sizeof(Array) + len * sizeof(NODE *) );
#   ifdef BAD
        if(BADflag) {
            diagmsg("list_to_array: addr= 0x%x, size(bytes) = %d\n", a, sizeof(Array) + len * sizeof(NODE *) );
        }
#   endif
    a->a_size = len;

    /* Fill the array with pointers to elements, adjusting ref counts.     */
    { register NODE **q;

        q = a->a_body;
        maplist(p, l, *q++ = lock(p) );
    }

    vfree(l);
    return ( a );
}


/*
 * free_array(a)
 *
 * The array a is reclaimed. Its elements are assumed
 * to point to nodes, so their ref counts are decremented
 * and an attempt is made to reclaim them.
 */

free_array(a)
Array *a;
{
    NODE ** p;

    for (p = a->a_body; p < &a->a_body[a->a_size]; p++)
        vfree( unlock(*p) );

#   ifdef BAD
        if(BADflag) {
            diagmsg("free_array: arg = 0x%x, size = %d\n", a, a -> a_size);
            diagmsg("  calling sfree with size = %d\n", sizeof(Array) + a->a_size * sizeof(NODE *) );
            flcheck(0,0);
        } else {
            flcheck(0,1);
        }
#   endif

    free( a );

#   ifdef BAD
        if(BADflag)
            flcheck(0,0);
        else
            flcheck(0,1);
#   endif

}


/*
 *  split(kind, id_list, other args ...)
 *
 *  input: Similar to vertex, where id_list is a list of ids.
 *
 *  output: A list of "kind" nodes, with one of the ids on the id_list
 *          in the id field of each node, and the other args replicated
 *          for each node. 
 */
LIST split(kindno, id_list, arg1, arg2, arg3, arg4, arg5)
int kindno;
NODE * id_list;
NODE * arg1, *arg2, *arg3, *arg4, *arg5;
{
    NODE * vrtx;
    NODE * result;

    result = emptylist();
    maplist (p, id_list, {
        /* Build a node with the id pointed to by p in the id field */
        /* and add it to the result.                                 */
            switch( bitcnt(stmkfields[kindno]) ) {
                case 2: vrtx = mknode(kindno, p, arg1); break;
                case 3: vrtx = mknode(kindno, p, arg1, arg2); break;
                case 4: vrtx = mknode(kindno, p, arg1, arg2, arg3); break;
                case 5: vrtx = mknode(kindno, p, arg1, arg2, arg3, arg4); break;
                case 6: vrtx = mknode(kindno, p, arg1, arg2, arg3, arg4, arg5); break;
                default: dbgmsg("split: bad node size: %d\n", stsize[kindno]);
            }
            addright(result, vrtx);
    } );

    return ( result );
}

/* 
 *     conc(l1,l2)
 *
 *  Destructively appends l1 to l2 and returns the result.
 *  l1 becomes the result. l2 is destroyed.
 *
 */
NODE * conc(l1,l2)
NODE * l1, *l2;
{
#   ifdef DEBUG
        if ( !is_list(l1) || !is_list(l2)) {
            dbgmsg("\nconc: args are not lists: l1=%x, l2=%x\n",l1,l2);
            abort();
        }
        if ( is_refd(l1) || is_refd(l2) ) {
            dbgmsg("\nconc: args have non-zero ref counts: l1=%x,l2=%x\n",l1,l2);
            abort();
        }
#   endif

    if ( is_empty(l1) ) { vfree(l1); return(l2); }
    if ( !is_empty(l2) ) {
        cn_settail( l1 -> lh_last, l2 -> lh_first );
        (l1 -> lh_last) = (l2 -> lh_last);
    }
    /* free l2. Note that vfree cannot be used without clearing the first */
    /* last fields since the cons nodes are being reused. */ 
        (l2 -> lh_last) = (l2 -> lh_first) = NIL;
        vfree(l2);
    return ( l1 );
}

/*
 *  replace_last(l,p)
 * Destructively change the last element of the list l to be p
 */
NODE * replace_last(l,p)
NODE *l, *p;
{
    NODE * q = last(l);

    cn_sethead(l -> lh_last, lock(p));
    vfree(unlock(q));
    return(l);
}
