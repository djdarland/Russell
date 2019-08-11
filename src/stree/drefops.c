/*
 * Full versions of chgfld, initfld, lock, and unlock
 */
# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "is_ptr.h"

# define MINPTR 10     /* minimum "real" pointer value */

int retaddr();

# ifdef UNDEFINED
NODE * lock();

/*
 *  chgfld(pp,v)
 *
 *  *pp is a field in a node, v is the new value for that field.
 *  If *pp is not NIL, decrement refcount of **pp.
 *  Stick v into *pp, and inc refcount of *v if not NIL.
 *  Free the old **pp if appropriate.
 *  Assumes that the new and old field values are different.
 */
chgfld(pp, v)
NODE **pp;
register NODE *v;
{   register NODE **rpp = pp;

    /* Make sure that rpp is a pointer */
#       ifdef DEBUG
            if (!is_ptr(rpp)) {
                dbgmsg("chgfld: argument not a pointer: value=0x%x, retaddr=0x%x\n",
                        rpp, retaddr());
                abort();
            }
#       endif
    /* If old value for *pp is not NIL, decrement refcount and free if zero. */
        if ( *rpp != NIL) {
            unlock(*rpp);
	    if ( ((*rpp)->refcount) <= 0 && (v != *rpp) )
                vfree(*rpp);
        }

    /* Put new value into field and if v is not NIL etc., inc *v's refcount. */
        if ( (*rpp = v) > MINPTR ) {
            /* Check if *v has been freed. */
#             ifdef DEBUG
		if ( *v > 10000 ) {
                    dbgmsg("chgfld: new field value already freed: nvalue=0x%x, kind=%s, retaddr=0x%x\n",
                            *v, kindname(v->kind), retaddr());
                    abort();
                }
#             endif
            (void) lock(v);
        }
}

/*
 *  initfld(pp,v)
 *
 *  *pp is a field in a node, v is its new value.
 *  Stick v into *pp, then if v is not NIL, inc *v's refcount.
 */
initfld(pp,v)
NODE **pp;
register NODE *v;
{   register NODE * rpp = pp;

    /* Check that rpp is a pointer */
#     ifdef DEBUG
        if (!is_ptr(rpp)) {
            dbgmsg("initfld: argument not a pointer: value=0x%x, retaddr=0x%x\n",
                   rpp, retaddr());
            abort();
        }
#     endif
    if ( (*rpp = v) > MINPTR) {
        (void) lock(v);
    }
}


/*
 * lock(p)
 *
 * p is a node. If p is not NIL, increment its ref count.
 * Return p.
 */
NODE *
lock(p)
NODE *p;
{   register NODE *rp = p;

    if ( rp != NIL ) {
#     ifdef DEBUG
        /* Make sure that rp is a pointer */
            if (!is_ptr(rp)) {
                dbgmsg("lock: argument not a pointer: value=0x%x, retaddr=0x%x\n",
                        rp, retaddr());
                abort();
            }
        /* Check if rp has been freed. */
            if ( *rp != 0 && is_ptr(*rp) ) {
                dbgmsg("lock: node has been freed: node addr=0x%x, kind=%s, retaddr=0x%x\n",
                        rp, kindname(p->kind), retaddr());
                abort();
            }
        /* Make sure the reference count is reasonable */
            if (((unsigned)(rp -> refcount)) > 1000) {
                dbgmsg("lock: node has bad refcount: node addr=0x%x, refcount=%d, kind=%s, retaddr=0x%x\n",
                        rp, p -> refcount, kindname(p->kind), retaddr());
                fflush(stdout); fflush(stderr); prtree(rp);
                abort();
            }
#     endif
      ++(rp->refcount);
    }
    return(rp);
}                      

/*
 * unlock(p)
 *
 * *p is a node. Decrement its refcount.
 */
unlock(p)
NODE *p;
{   register NODE * rp = p;

    /* reserves space to add debugging instruction */
#     ifdef DEBUG
        if (p == 0xffffffff) {
	    printf("sleeping\n");
	    sleep(3);
        }
#     endif
    if ( rp != NIL ) {
#     ifdef DEBUG
        /* Make sure that rp is a pointer */
            if (!is_ptr(rp)) {
                dbgmsg("unlock: argument not a pointer: value=0x%x, retaddr=0x%x\n",
                        rp, retaddr());
                abort();
            }
        /* Check if rp has been freed. */
            if ( *rp != 0 && is_ptr(*rp) ) {
                dbgmsg("unlock: node has been freed: node addr=0x%x, kind=%s, retaddr=0x%x\n",
                        rp, kindname(p->kind), retaddr());
                abort();
            }
        /* Make sure the reference count is reasonable */
            if (((unsigned)(rp -> refcount)) > 1000) {
                dbgmsg("unlock: node has bad refcount: node addr=0x%x, refcount=%d, kind=%s, retaddr=0x%x\n",
                        rp, p -> refcount, kindname(p->kind), retaddr());
                abort();
            }
#     endif
      --(rp->refcount);
    }
    return(rp);
}

#endif
/* The following are included only so main doesn't have to include */
/* streedefs.h                                                     */

NODE * bld_den_seq_f(b)
NODE * b;
{
    return(b -> bld_den_seq);
}

NODE * signature_f(x)
NODE * x;
{
    return(x -> signature);
}

NODE * first_elmnt(x)
NODE * x;
{
    return(first(x));
}

