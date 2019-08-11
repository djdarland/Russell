
/*
 *	retrieve(key)
 *
 *	Return a pointer to the Identry with i_sttindx key.
 */

# include "parm.h"
# include "stree/ststructs.mh"
# include "pass1/stt/sttdefs.h"
# include "Idtable.h"
# define DEBUG yes

/* int nprobes = 0; */
int nkeys = 0;

Identry * retrieve(key)
sttrelptr key;
{
	nkeys++;
	return ( &Idtable[retr1(key, 0, sttnstrings-1)] );
}

/*
 *	retr1(key, f, l)
 *
 *	Hidden routine to search Idtable between the bounds f and l for the entry
 *	with i_sttindx equal to indx. An interpolation search is used.
 */
retr1(key, f, l)
register sttrelptr key;
register int f, l;					/* first, last indices				*/
{	register int c; 				/* "current" probe index			*/

	for(;;) {

		/* nprobes++; */

    	if ( f > l || Idtable[f].i_sttindx > key || Idtable[l].i_sttindx < key) {
    		dbgmsg("retr1: Can't find name!\n");
    		/* abort(); */ return;
    	}
    
    	c = f;
		if( l != f ) {
		  long r;
		  int t;

		  /*c +=
				( ((long)(l-f)) * ((long)(key-Idtable[f].i_sttindx)) ) /
					( (int) (Idtable[l].i_sttindx - Idtable[f].i_sttindx) ); */

			r= ( ((long)(l-f)) * ((long)(key-Idtable[f].i_sttindx)) ); 
			t=		( (int) (Idtable[l].i_sttindx - Idtable[f].i_sttindx) );
			r += t >> 1;
			c += r / t;
		}
/*		printf("f=%d, l=%d, c=%d\n", f, l, c); */
		if ( Idtable[c].i_sttindx == key )
			return(c);
    	else if ( key < Idtable[c].i_sttindx ) {
    		f += 1; l = c-1;
    	} else {
    		f = c+1; l -= 1;
		}
	}
}
