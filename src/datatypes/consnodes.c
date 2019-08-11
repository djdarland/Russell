/* 
 * These are some C routines to manipulate LISP style lists.
 */

# include "parm.h"
# include "consnodes.h"

/*
 * The BAD flag traces allocs and frees.
 */
# ifdef BAD
	extern boolean BADflag;
# endif


ConsNode * cn_cons(hd,tl)
ANYTHING * hd;
ConsNode * tl;
{	register ConsNode *p;

#	ifdef BAD
		if (BADflag)
			flcheck (0,0);	/* Do a complete free list check */
							/* for the salloc free list 	 */
		else
			flcheck (0,1);	/* Do a quick free list check */
#	endif

        p = (ConsNode *)malloc(sizeof (ConsNode) );
	cn_sethead( p, hd );
	cn_settail( p, tl );
#	ifdef BAD
		if (BADflag) {
			diagmsg("cn_cons: addr of new node=0%o, kind=ConsNode, retaddr=0%o\n",
					p, retaddr());
		}
#	endif
	return(p);
}

ConsNode * cn_del_hd(l)
		/* return the tail of l after freeing the first cons node */
			  /* of l */
ConsNode *l;
{	register ConsNode *p;
#	ifdef BAD
		if (BADflag) {
			diagmsg("cn_del_hd: addr of freed node=0%o, kind=ConsNode, retaddr=0%o\n",
					l, retaddr() );
		}
#	endif

#	ifdef BAD
		if (BADflag)
			flcheck (0,0);	/* Do a complete free list check */
		else
			flcheck (0,1);	/* Do a quick free list check */
#	endif

	p = cn_tail(l);
	free(l);
	return(p);
}



