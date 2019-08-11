#include <stdio.h>
#include "runtime.h"
#define GATHERSTATS
#undef GATHERSTATS
#define PRINTSTATS
#undef PRINTSTATS
#define DEBUG
#undef DEBUG

long mem_found = 0;     /* Number of longwords of memory reclaimed     */

long composite_in_use;  /* Number of longwords in accessible composite */
			/* objects.                                    */

long atomic_in_use;     /* Number of longwords in accessible atomic */
			/* objects.                                 */

/*
 * reclaim phase
 *
 */

reclaim()
{
register struct hblk *hbp;	/* ptr to current heap block		*/
register int word_no;		/* Number of word in block		*/
register long i;
register word *p;		/* pointer to current word in block	*/
register int mb;		/* mark bit of current word		*/
int sz;				/* size of objects in current block	*/
word *plim;
struct hblk **nexthbp;		/* ptr to ptr to current heap block	*/
int nonempty;			/* nonempty ^ done with block => block empty*/
struct obj *list;		/* used to build list of free words in block*/
register int is_atomic;         /* => current block contains atomic objs */

#   ifdef DEBUG_RT
	fprintf(stderr,"clearing all between %x and %x, %x and %x\n",
		objfreelist, &objfreelist[MAXOBJSZ+1],
		aobjfreelist,&aobjfreelist[MAXAOBJSZ+1]);
#   endif
    { register struct obj **fop;
	for( fop = objfreelist; fop < &objfreelist[MAXOBJSZ+1]; fop++ ) {
	    *fop = (struct obj *)0;
	}
	for( fop = aobjfreelist; fop < &aobjfreelist[MAXAOBJSZ+1]; fop++ ) {
	    *fop = (struct obj *)0;
	}
    }
    
    atomic_in_use = 0;
    composite_in_use = 0;

#   ifdef PRINTSTATS
	fprintf(stderr, "reclaim: current block sizes:\n");
#   endif

  /* go through all heap blocks (in hblklist) and reclaim unmarked objects */
    nexthbp = hblklist;

    while( nexthbp < last_hblk ) {
	hbp = *nexthbp++;

	nonempty = FALSE;
	sz = hbp -> hb_sz;
	is_atomic = 0;
	if (sz < 0) {
	    sz = -sz;
	    is_atomic = 1;		/* this block contains atomic objs */
	}
#	ifdef PRINTSTATS
	    fprintf(stderr, "%d(%c",sz, (is_atomic)? 'a' : 'c');
#	endif

	if( sz > (is_atomic? MAXAOBJSZ : MAXOBJSZ) ) {  /* 1 big object */
	    mb = mark_bit(hbp, (hbp -> hb_body) - ((word *)(hbp)));
	    if( mb ) {
#               ifdef GATHERSTATS
		    if (is_atomic) {
			atomic_in_use += sz;
		    } else {
			composite_in_use += sz;
		    }
#               endif
		nonempty = TRUE;
	    } else {
		mem_found += sz;
	    }
	} else {				/* group of smaller objects */
	    p = (word *)(hbp->hb_body);
	    word_no = ((word *)p) - ((word *)hbp);
	    plim = (word *)((((unsigned)hbp) + HBLKSIZE)
		       - WORDS_TO_BYTES(sz));

	    list = (is_atomic) ? aobjfreelist[sz] : objfreelist[sz];

	  /* go through all words in block */
	    while( p <= plim )  {
		mb = mark_bit(hbp, word_no);

		if( mb ) {
#                   ifdef GATHERSTATS
			if (is_atomic) atomic_in_use += sz;
			else           composite_in_use += sz;
#                   endif
#		    ifdef DEBUG_RT
			fprintf(stderr,"found a reachable obj\n");
#		    endif
		    nonempty = TRUE;
		    p += sz;
		} else {
		  mem_found += sz;
		  /* word is available - put on list */
		    ((struct obj *)p)->obj_link = list;
		    list = ((struct obj *)p);
		  if (is_atomic) {
		    p += sz;
		  } else {
		    /* Clear object, advance p to next object in the process */
			i = (long)(p + sz);
			p++; /* Skip link field */
			while (p < (word *)i) {
			    *p++ = 0;
			}
		  }
		}
		word_no += sz;
	    }

	  /*
	   * if block has reachable words in it, we can't reclaim the
	   * whole thing so put list of free words in block back on
	   * free list for this size.
	   */
	    if( nonempty ) {
		if ( is_atomic )	aobjfreelist[sz] = list;
		else			objfreelist[sz] = list;
	    }
	} 

#	ifdef PRINTSTATS
	    fprintf(stderr, "%c),", nonempty ? 'n' : 'e' );
#	endif
	if (!nonempty) {
	    if (!is_atomic && sz < MAXOBJSZ) {
		/* Clear words at beginning of objects */
		/* Since most of it is already cleared */
		  p = (word *)(hbp->hb_body);
		  plim = (word *)((((unsigned)hbp) + HBLKSIZE)
			 - WORDS_TO_BYTES(sz));
		  while (p <= plim) {
		    *p = 0;
		    p += sz;
		  }
		hbp -> hb_uninit = 0;
	    } else {
		/* Mark it as being uninitialized */
		hbp -> hb_uninit = 1;
	    }

	  /* remove this block from list of active blocks */
	    del_hblklist(hbp);	

	    /* This entry in hblklist just got replaced; look at it again  */
	    /* This admittedly depends on the internals of del_hblklist... */
	    nexthbp--;

	    freehblk(hbp);
	}  /* end if (one big object...) */
    } /* end while (nexthbp ...) */

#   ifdef PRINTSTATS
	fprintf(stderr, "\n");
#   endif
}
