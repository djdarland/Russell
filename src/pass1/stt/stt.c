#
/*
 * Russell
 *  String table routines
 */

#include "parm.h"
#include "sttdefs.h"
#include "arith.h"
#include <stdio.h>

/*
 *    sttindx(blkno) - maps block numbers into their address in core.
 *    sttfirstfree   - relative position of the first free byte in the table.
 *    STTHDRSZ       - number of hash table headers.
 *    stthdr(hash)   - maps the hash of a string into a chain of entries with
 *                     the same hash.
 */
sttblock *(sttindx[STTNBLKS]);
sttrelptr sttfirstfree;
#define STTHDRSZ 128
sttrelptr stthdr[STTHDRSZ];


/*
 * stt_abs(rpt)
 *
 * convert relative pointer into string table
 *   to absolute pointer to string table entry.
 */
Sttentry *stt_abs(rpt)
sttrelptr rpt;
{
    return(
        (Sttentry *)(&(sttindx[STTBLKNO(rpt)]->sttb_data[STTOFFSET(rpt)]))
    );
}

/*
 * getname( rpt )
 * 
 * convert relative string table pointer to absolute
 *   pointer to string.
 */
char *getname( rpt )
sttrelptr rpt;
{
    if (rpt == (sttrelptr)(-1)) {
	return("(local type id)");
    } else {
	return( stt_abs(rpt) -> stte_str );
    }
}


/*
 *  stt_balloc(blkno)
 *  Allocate block number blkno, if possible.
 */
stt_balloc(blkno)
unsigned blkno;
{
    if (blkno >= STTNBLKS) {
	yyperror("Too many identifiers");
        exit(4);
    }
    sttindx[blkno] = (sttblock *)gc_malloc_atomic( sizeof(sttblock) );
}


/*
 * stt_alloc( nbytes )
 *   
 * find nbytes space in string table
 * and return address( relative to start of string table ).
 */
sttrelptr stt_alloc( nbytes )
unsigned nbytes;
{
    register sttrelptr firstfree = sttfirstfree;
    unsigned blkno;

    /* Check for requests that are too large. */
	if (nbytes > sizeof(sttblock)) {
	    fprintf(stderr,"stt_alloc: identifier too large: %u\n",nbytes);
            abort();
        }

    /* If firstfree's block has not been allocated (due to roundup), do so. */
        if ( sttindx[ STTBLKNO(firstfree) ] == 0 )
            stt_balloc( STTBLKNO(firstfree) );

    /* Get a new block if there is no space on the current one. */
        if ( STTOFFSET(firstfree) + nbytes > sizeof(sttblock) ) {
            firstfree = roundup(firstfree,sizeof(sttblock));
            stt_balloc( STTBLKNO(firstfree) );
        }

    /* Compute new value for sttfirstfree (round up to a multiple of a word). */
        sttfirstfree = roundup(firstfree + nbytes, sizeof(int));


    return( firstfree );
}

/*
 * rp = stt_enter( str, lgth )
 *
 * enter given string (of given length) into string table,
 *   return its relative position.
 * the length counts the trailing null
 *   therefore, length is at least 2.
 */
sttrelptr stt_enter( str, lgth )
char *str; int lgth;
{   sttrelptr p; 
    Sttentry *pabs;
    static boolean firstentry = TRUE;   /* True on first call of routine. */
    unsigned hashval = ( (unsigned) ((13 * str[0]) + (5 * str[lgth-2]) + lgth) )
                       % STTHDRSZ;

    if( firstentry ) {
        /* Initialize hash table headers and sttfirstfree. */
            register int i;
            for( i = 0; i < STTHDRSZ; i++ )
                stthdr[i] = STT_NO_NEXT_ENTRY;
            stt_balloc(0);
            sttfirstfree = 0;
            sttnstrings = 0;
            firstentry = FALSE;
    }
    

    /* Search through entries chained off of hash header for one with */
    /* the same name as str. If found, return its relative position.  */
        for(p = stthdr[hashval]; p != STT_NO_NEXT_ENTRY; p = pabs->stte_next) {
            pabs = stt_abs( p );
            if( strcmp( pabs->stte_str, str ) == 0 )
                return( p );
        }

    sttnstrings++;

    /* Add a new entry to the head of the chain. */
        p = stt_alloc( STTENTRYSZ(lgth) );
        pabs = stt_abs( p );
        pabs->stte_next = stthdr[hashval];
        stthdr[hashval] = p;
        strcpy( pabs->stte_str, str );
        return( p );
}

/*
 * build_Idtable()
 * 
 * Build Idtable, a mapping between strings and values used to build the
 * symbol table.
 */
# include "stree/ststructs.mh"
# include "pass2/Idtable.h"

/* used to be stt_write( sttfildes ) */
build_Idtable()
{
    register int i;
    register sttblock *p;
    register Identry *Ide;   /* First free Idtable entry. */
    extern int Idcompare();

    /* Allocate Idtable, then go down each hash header chain, entering */
    /* strings and and setting their initial Idtable value to NIL.     */
        Idtable = (Identry *)alloc( sttnstrings * sizeof(Identry) );
        Ide = &Idtable[0];
        for (i = 0; i < STTHDRSZ; i++) {
            sttrelptr e;
            for (e = stthdr[i]; e != STT_NO_NEXT_ENTRY; e = (stt_abs(e)->stte_next)) {
                Ide->i_sttindx = e;
                Ide->i_value = NIL;
                Ide++;
            }
        }
#       ifdef DEBUG
            if (Ide != Idtable + sttnstrings) {
                dbgmsg( "stt_write: error in identifier count\n" );
                abort();
            }
#       endif
    /* Sort Idtable by i_sttindx. */
        qsort(Idtable, sttnstrings, sizeof(Identry), Idcompare);

}

/*
 *  Idcompare (e1, e2)
 *  Identry *e1, *e2;
 *
 *  Compare the two Identries and return an integer less than, equal to, or
 *  greater than 0 according as e1->i_sttindx is <, ==, or > e2->i_sttindx.
 */
Idcompare(e1, e2)
Identry *e1, *e2;
{
    return ( (int) (e1->i_sttindx - e2->i_sttindx) );
}
