/* tree building routines for declaration tree */
# include "parm.h"

# include <stdio.h>

# include <varargs.h>

# include "ststructs.mh"

# include "stsizes.mh"

# include "stptrs.mh"

# include "stmkfields.mh"

extern char tokenbuf[];  /* temporary string space, shared with scanner */

/*
 *  the BAD flag (Bad Allocation Debug) is used to catch usage of previously
 *  freed nodes.
 */
#ifdef BAD
    extern boolean BADflag;
#endif

#ifndef BAD

# ifdef UNDEFINED
#   define MINPTR 10
  /* Faster version of initfld. */
#   define initfld(pp,v) \
        if ( (*pp = v) > MINPTR) { \
            (void) lock(v); \
        }

  /* Faster version of lock */
#   define lock(p) \
        (((p) -> refcount)++, p)

  /* Faster version of unlock */
#   define unlock(p) \
	(((p) -> refcount)--, p)
# endif

#endif

int retaddr();

extern int yyvline;

/*
 *    Construct a structure with kind field strkind. Set the vlineno field
 * to the current value of yyvline. Fields with the corresponding bit in 
 * stmkfields set are set to the values of the remaining arguments.
 * Other fields are initialized to NIL.
 *
 * The fundamental MACHINE DEPENDENCY here is that all fields must be the
 * same length -- the length of a C "unsigned".
 */

NODE *mknode( va_alist )
va_dcl
{
    va_list ap;
    int strkind;
    register unsigned *p;
    unsigned q;
    register int v; /* bit vector identifying pointer fields in strkind */
    register int i; /* bit vector identifying fields to be initialized  */
                    /* to the argument values. */
    NODE *result;

#   ifdef BAD
        if (BADflag)
            flcheck (0,0);  /* Do a complete free list check */
        else
            flcheck (0,1);  /* Do a quick free list check */
#   endif

    va_start(ap);
    strkind = va_arg(ap, int);

    p = (unsigned *)salloc(stsize[strkind] * (sizeof (NODE *)));
    result = (NODE *)p;

    v = stptrs[strkind];
    i = stmkfields[strkind];
    /* p -> refcount = 0; */
    result -> kind = strkind;
    result -> vlineno = yyvline;
    /* adjust p, i & v to skip standard prefix. For efficiency only */
	p += SZSTANDARDPREFIX;
	i <<= SZSTANDARDPREFIX;
	v <<= SZSTANDARDPREFIX;


    q = va_arg(ap, int);
    while( p - (unsigned *)result < stsize[strkind] ) {
        if ( i < 0 ) {
            /* initialize this field */
                if( v < 0 ) {
                    /* store a pointer field */
                        initfld( p, q );
                } else {
                    /* store a non-pointer field */
                        *p = q;
                }
                q = va_arg(ap, int);
        } else {
            *p = NIL;
        }
	i <<= 1;
	v <<= 1;
        p++;
    }
    va_end(ap);
    
#   ifdef BAD
        if (BADflag) {
            diagmsg("mknode: addr of new node=0%x, kind=%s\n",
                     result, kindname(strkind));
        }
#   endif

    return( result );
}

# ifdef UNDEFINED
/*
 * vfree(p)
 *
 * if *p's reference count is zero,
 *   free *p
 *   decrement reference counts of descendants
 *   call self recursively on descendants
 */

# define MIN_PTR 10   /* lower limit for real pointers. lower values */
		      /* have special significance.                  */

vfree(p)
int *p;
{
    register int size;
    register int v; /* bit vector identifying pointers */
    register unsigned *q;

#   ifdef BAD
        if (BADflag)
            flcheck (0,0);  /* Do a complete free list check */
        else
            flcheck (0,1);  /* Do a quick free list check */
#   endif

    if ( p == NIL ) return;

    if ( !is_refd(p) ) {
        /* call vfree for each child */
            if (( p -> kind) == LISTHEADER ) {
                /* traverse list of descendants */
                    maplist(e, p, {
                        if( !is_refd(unlock(e)) )
                            vfree(e);
                    } );
                /* now free the cons nodes */
                    {   ConsNode *c;
                        for ( c = p -> lh_first; c != NIL; c = cn_del_hd(c) );
                    }
            }
            else {  /* use stptrs to find the children */
                v = stptrs[p -> kind];
                q = p;
                while(v) {
		    if(v < 0 /* *q is a pointer */ && *q > MIN_PTR) {
			if( !is_refd( unlock((NODE *) *q) ) )
			    vfree((NODE *) *q);
                    }
		    v <<= 1;
                    q++;
                }
            }
        /* deallocate storage */
            size = stsize[p -> kind];
#           ifdef BAD
                if (BADflag) {
                    diagmsg("vfree: addr of freed node=0%x, kind=%s, retaddr=0%x\n",
                        p, kindname(p->kind), retaddr());
                }
#           endif
            sfree(p, size * sizeof(NODE *));
    }
}
# endif

/*
 * copynode(np)
 *
 * copy the node np;
 * don't copy the children, but increment their reference counts.
 * Return a pointer to the copy.
 */

NODE * copynode(p)
register NODE *p;
{   int size;
    register int v;   /* bit vector giving pointer fields */
    register unsigned *q; /* pointer to middle of new node      */
    register unsigned *r; /* pointer to middle of original node */
    NODE * result;

    if ( is_list(p) ) 
        return( copylist(p) );
    size = stsize[p -> kind];
    v = stptrs[p -> kind];
    q = (unsigned *)salloc(size * sizeof(NODE *));
    result = (NODE *) q;
    /* copy and adjust reference counts where necessary */
	/* q->refcount = 0; */
        result->kind = p->kind;
	result->vlineno = p->vlineno;
	result->pre_num = p -> pre_num;
	result->post_num = p -> post_num;
	q += SZSTANDARDPREFIX;
	r = (unsigned *)p + SZSTANDARDPREFIX;
	size -= SZSTANDARDPREFIX;
	v <<= SZSTANDARDPREFIX;
        while( size-- ) {
            if( v < 0 ) {
                /* copy a pointer */
                initfld( q, *r );
                q++; r++;
            } else {
                /* copy a nonpointer */
                *q++ = *r++;
            }
	    v <<= 1;
        }
#       ifdef BAD
            if (BADflag) {
                diagmsg("copynode: addr of new node=0%x, kind=%s, retaddr=0%x\n",
                        result, kindname(result->kind), retaddr());
            }
#       endif
    return( result );
}

/*
 * copylist(l)
 *
 * Make a new list which has on it the same elements (not copies) that 
 * are on l.
 */
LIST copylist(l)
LIST l;
{   ConsNode *p;                /* The first element of the constructed list. */
    register ConsNode *q;       /* The last element of the constructed list. */

    p = q = NIL;

#   ifdef DEBUG
        if ( !is_list(l) ) {
            dbgmsg("\ncopylist: arg not a list: l=%x\n",l);
            abort();
        }
#   endif

    maplist(v, l, 
            {   if ( v == first(l) ) 
                    p = q = cn_cons(lock(v),NIL);
                else {
                    cn_settail(q, cn_cons(lock(v), NIL));
                    q = cn_tail(q);
                }
            } );
    return( (LIST) mknode(LISTHEADER, p, q) );
}


/*
 * Used by maprlist below
 */
static void maprl1(l,fn)
ConsNode * l;
void (*fn)();
{
    if (l != NIL) {
      maprl1(cn_tail(l),fn);
      (*fn) (cn_head(l));
    }
}

/*
 *  Apply fn to each element of the list l in reverse order
 */
void maprlist(l, fn)
LIST l;
void (*fn)();
{
    maprl1(l -> lh_first, fn);
}

/* 
 *  Return a pointer to a new LETTERID node with name fldname (a string table
 * index).  Set the id_def_found field.
 */
NODE * mkcompnm(sttptr)
unsigned sttptr;
{
    register NODE * result;
    result = mknode(LETTERID, sttptr);
    result -> id_def_found = TRUE;
    return(result);
}

/*
 * return an identifier node for an identifer with name
 * consisting of string concatenated with the name of id.
 * Id may not be an OPRID.
 * The returned id has id_def_found set.  (It is presumed to be a
 * a field name.)
 */
NODE * prefix(string, id)
char * string;
NODE * id;
{
#   ifdef DEBUG
	if (id -> kind != LETTERID) {
            dbgmsg("prefix: bad identifier node\n");
	}
#   endif
    strcpy(tokenbuf, string);
    strcat(tokenbuf, getname(id -> id_str_table_index));
    return(mkcompnm(stt_enter(tokenbuf, strlen(tokenbuf)+1)));
}

