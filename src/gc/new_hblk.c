/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991,1992 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to copy this garbage collector for any purpose,
 * provided the above notices are retained on all copies.
 *
 * This file contains the functions:
 *	ptr_t GC_build_flXXX(h, old_fl)
 *	void GC_new_hblk(n)
 */


# include <stdio.h>
# include "gc_private.h"

/*
 * Build a free list for size 1 objects inside hblk h.  Set the last link to
 * be ofl.  Return a pointer tpo the first free list entry.
 */
ptr_t GC_build_fl1(h, ofl)
struct hblk *h;
ptr_t ofl;
{
    register word * p = (word *)h;
    register word * lim = (word *)(h + 1);
    
    p[0] = (word)ofl;
    p[1] = (word)(p);
    p[2] = (word)(p+1);
    p[3] = (word)(p+2);
    p += 4;
    for (; p < lim; p += 4) {
        p[0] = (word)(p-1);
        p[1] = (word)(p);
        p[2] = (word)(p+1);
        p[3] = (word)(p+2);
    };
    return((ptr_t)(p-1));
}

/* The same for size 2 cleared objects */
ptr_t GC_build_fl_clear2(h, ofl)
struct hblk *h;
ptr_t ofl;
{
    register word * p = (word *)h;
    register word * lim = (word *)(h + 1);
    
    p[0] = (word)ofl;
    p[1] = 0;
    p[2] = (word)p;
    p[3] = 0;
    p += 4;
    for (; p < lim; p += 4) {
        p[0] = (word)(p-2);
        p[1] = 0;
        p[2] = (word)p;
        p[3] = 0;
    };
    return((ptr_t)(p-2));
}

/* The same for size 3 cleared objects */
ptr_t GC_build_fl_clear3(h, ofl)
struct hblk *h;
ptr_t ofl;
{
    register word * p = (word *)h;
    register word * lim = (word *)(h + 1) - 2;
    
    p[0] = (word)ofl;
    p[1] = 0;
    p[2] = 0;
    p += 3;
    for (; p < lim; p += 3) {
        p[0] = (word)(p-3);
        p[1] = 0;
        p[2] = 0;
    };
    return((ptr_t)(p-3));
}

/* The same for size 4 cleared objects */
ptr_t GC_build_fl_clear4(h, ofl)
struct hblk *h;
ptr_t ofl;
{
    register word * p = (word *)h;
    register word * lim = (word *)(h + 1);
    
    p[0] = (word)ofl;
    p[1] = 0;
    p[2] = 0;
    p[3] = 0;
    p += 4;
    for (; p < lim; p += 4) {
        p[0] = (word)(p-4);
        p[1] = 0;
        p[2] = 0;
        p[3] = 0;
    };
    return((ptr_t)(p-4));
}

/* The same for size 2 uncleared objects */
ptr_t GC_build_fl2(h, ofl)
struct hblk *h;
ptr_t ofl;
{
    register word * p = (word *)h;
    register word * lim = (word *)(h + 1);
    
    p[0] = (word)ofl;
    p[2] = (word)p;
    p += 4;
    for (; p < lim; p += 4) {
        p[0] = (word)(p-2);
        p[2] = (word)p;
    };
    return((ptr_t)(p-2));
}

/* The same for size 4 uncleared objects */
ptr_t GC_build_fl4(h, ofl)
struct hblk *h;
ptr_t ofl;
{
    register word * p = (word *)h;
    register word * lim = (word *)(h + 1);
    
    p[0] = (word)ofl;
    p[4] = (word)p;
    p += 8;
    for (; p < lim; p += 8) {
        p[0] = (word)(p-4);
        p[4] = (word)p;
    };
    return((ptr_t)(p-4));
}


/*
 * Allocate a new heapblock for small objects of size n.
 * Add all of the heapblock's objects to the free list for objects
 * of that size.  Will fail to do anything if we are out of memory.
 */
void GC_new_hblk(sz, kind)
register word sz;
int kind;
{
    register word *p,
		  *prev;
    word *last_object;		/* points to last object in new hblk	*/
    register struct hblk *h;	/* the new heap block			*/
    register bool clear = GC_obj_kinds[kind].ok_init;

#   ifdef PRINTSTATS
	if ((sizeof (struct hblk)) > HBLKSIZE) {
	    abort("HBLK SZ inconsistency");
        }
#   endif

  /* Allocate a new heap block */
    h = GC_allochblk(sz, kind);
    if (h == 0) return;

  /* Handle small objects sizes more efficiently.  For larger objects 	*/
  /* the difference is less significant.				*/
    switch (sz) {
        case 1: GC_obj_kinds[kind].ok_freelist[1] =
        	  GC_build_fl1(h, GC_obj_kinds[kind].ok_freelist[1]);
        	return;
        case 2: if (clear) {
         	    GC_obj_kinds[kind].ok_freelist[2] =
        	      GC_build_fl_clear2(h, GC_obj_kinds[kind].ok_freelist[2]);
        	} else {
        	    GC_obj_kinds[kind].ok_freelist[2] =
        	      GC_build_fl2(h, GC_obj_kinds[kind].ok_freelist[2]);
        	}
        	return;
        case 3: if (clear) {
         	    GC_obj_kinds[kind].ok_freelist[3] =
        	      GC_build_fl_clear3(h, GC_obj_kinds[kind].ok_freelist[3]);
        	    return;
        	} else {
        	    /* It's messy to do better than the default here. */
        	    break;
        	}
        case 4: if (clear) {
        	    GC_obj_kinds[kind].ok_freelist[4] =
        	      GC_build_fl_clear4(h, GC_obj_kinds[kind].ok_freelist[4]);
        	} else {
        	    GC_obj_kinds[kind].ok_freelist[4] =
        	      GC_build_fl4(h, GC_obj_kinds[kind].ok_freelist[4]);
        	}
        	return;
        default:
        	break;
    }
    
  /* Clear the page if necessary. */
    if (clear) bzero((char *)h, (int)HBLKSIZE);
    
  /* Add objects to free list */
    p = &(h -> hb_body[sz]);	/* second object in *h	*/
    prev = &(h -> hb_body[0]);       	/* One object behind p	*/
    last_object = ((word *)((char *)h + HBLKSIZE)) - sz;
			    /* Last place for last object to start */

  /* make a list of all objects in *h with head as last object */
    while (p <= last_object) {
      /* current object's link points to last object */
        obj_link(p) = (ptr_t)prev;
	prev = p;
	p += sz;
    }
    p -= sz;			/* p now points to last object */

  /*
   * put p (which is now head of list of objects in *h) as first
   * pointer in the appropriate free list for this size.
   */
      obj_link(h -> hb_body) = GC_obj_kinds[kind].ok_freelist[sz];
      GC_obj_kinds[kind].ok_freelist[sz] = ((ptr_t)p);
}

