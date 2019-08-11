/* A table containing free/allocated information for temporaries. */
/* Represented as a single bit vector.                            */

# include <stdio.h>
# include "../parm.h"
# include "tables.h"

# define MAX_TMPS 4096
# define BITS_PER_LONG 32
# define LONGS_TO_BITS(n) ((n) << 5)
# define BITS_TO_LONGS(n) ((n) >> 5)
# define ALL_ONES (-1L)

# define SZ_TMPS_ALLOCD ((MAX_TMPS + BITS_PER_LONG)/BITS_PER_LONG)

long tmps_allocd[SZ_TMPS_ALLOCD];       /* 1 bit ==> allocated.        */
					/* bits numbered right to left */
					/* within a long.              */
					/* Temporaries are numbered    */
					/* starting at 0.              */
int max_tmp = -1;    /* Highest numbered temporary allocated */

/* Return the number of the lowest numbered available temporary */
/* and mark it as allocated.                                    */
int get_next_free()
{
    register int i,j;
    register long w;

    for (i = 0; (w = tmps_allocd[i]) == ALL_ONES; i++) {
	if (i > SZ_TMPS_ALLOCD) {
	    fprintf(stderr, "Too many temporaries\n");
	    exit(1);
	}
    }
    for (j = 0; w & 1; w >>= 1) j++;
    tmps_allocd[i] |= (1 << j);
    j += LONGS_TO_BITS(i);
    if (j > max_tmp) max_tmp = j;
    return(j);
}

/* Mark temporary i as no longer being in use.  */
void free_tmp(i)
int i;
{
    int word_no = BITS_TO_LONGS(i);
    int bit_no = i - LONGS_TO_BITS(word_no);

    tmps_allocd[word_no] &= ~(1 << bit_no);
}

/* Make sure no temporaries are still in use, and reset max_tmp */
void reset_tmps()
{
    register int i,lim;
    register long w;
    int j;

    lim = BITS_TO_LONGS(max_tmp + 1);
    for (i = 0; i < lim; i++) {
      if ((w = tmps_allocd[i]) != 0) {
	for (j = 0; !(w & 1); w >>= 1) j++;
	fprintf(stderr, "Temporary %d not deallocated\n",
		LONGS_TO_BITS(i) + j);
	tmps_allocd[i] = 0;
      }
    }
    max_tmp = -1;
}

/* Return the name associated with the ith temporary */
char * tmp_name(i)
long i;
{
    register char * result;

    if (i < 100) {
	/* A fast version for the common case: */
	result = (char *) GC_malloc_atomic(6);
	result[0] = 't';
	result[1] = 'm';
	result[2] = 'p';
	result[3] = i / 10 + '0';
	result[4] = i % 10 + '0';
	result[5] = '\0';
    } else if (i & ARG_FLAG) {
	result = (char *) GC_malloc_atomic(12);
	sprintf(result, "arg%d", i & ~ARG_FLAG);
    } else {
	result = (char *) GC_malloc_atomic(12);
	sprintf(result, "tmp%d", i);
    }
    return(result);
}
 


typedef struct tmp_list {
    long tl_no;
    struct tmp_list * tl_next;
} tl;
#define TL_NIL ((tl *) 0)

tl * rmd_tmps;   /* List of temporaries that correspond to dead virtual regs */
		 /* but that may still appear inside remembered exprs.       */

/* Free all temporaries on rmd_tmps list */
void rem_tmps()
{
    register tl * p;
    
    for (p = rmd_tmps; p  != TL_NIL; p = p -> tl_next) {
	free_tmp(p -> tl_no);
    }
    rmd_tmps = TL_NIL;
}

/* Add an entry to a list of dead temporaries */
void dead_tmp(n)
long n;
{
    tl * result = (tl *) GC_malloc(sizeof (tl));

    result -> tl_no = n;
    result -> tl_next = rmd_tmps;
    rmd_tmps = result;
}

