# define DEBUG
# undef DEBUG
# include "parm.h"

# include <stdio.h>

# include "stree/ststructs.mh"

extern FILE * unparse_file;

# ifdef BAD
	extern boolean BADflag;
# endif

NODE * cor_cond();

NODE * cand_cond();

NODE ** outermost_op();

/*
 *  mkappl2(h,t)
 *
 *  input:  A denotation represented as an array of pointers to primaries.
 *          h points to the start of the array, t to the end. The array
 *          should not be empty.
 *
 *  output: a tree of applications and conditionals (which reflect the
 *          precedence of operations in the denotation), in the form of
 *          an arg list for a higher level application.
 *          The outermost node always has a ref count of zero.
 */

NODE * mkappl2(h,t)
NODE    ** h,
        ** t;
{
    NODE ** op,        /* "outermost" operator (i.e. the operation performed
                          last) in the denotation                          */
         * result,
         * left_arg,   /* a tree of applications which is the left arg of op */
	 * right_arg,
	 ** rest = t+1; /* start of arguments for next applications  */


#   ifdef DEBUG
        /* test for empty array */
            if ( h == t + 1 ) {
                dbgmsg("\nmkappl2: empty array - %o, %o\n", h, t);
                abort();
            }
#   endif


    /* If there is just a single primary, convert it into
       an arg list and return.                           */
        if ( h == t ) {
            /* is it already a list? */
		if ( is_list(*h) ) {
		    /* Copy it and return the copy. */
                        return ( copylist(*h) );
		} else {
		    return ( mklist(*h, -1) );
		}
        }


    /* Find the "outermost" operator and assume its args are
       the sequences of primaries to its right and left.
       Build a tree representing the application of that operator
       to it args.                                          */

        /* Find the outermost operator */
	    op = outermost_op( h,t );

	if( op == NIL ) {
	    /* Couldn't find a reasonable one */
	    if (!is_list(*h)) {
		/* Interpret it as curried application */
		    rest = h+2;
		    right_arg = mkappl2( h+1, h+1);
		    lock(right_arg);
		    left_arg = emptylist();
		    lock(left_arg);
		    op = h;
	    } else {
		/* no way to deal with this mess */
		yyperror("Improperly bracketed expression");
		return( mklist(emptylist(), -1) );
	    }
	} else {

	  /* Construct the left argument tree */
            if ( op == h )
                left_arg = emptylist();
            else
                left_arg = mkappl2( h, op-1 );
            lock(left_arg);

	  /* Construct the right argument tree */
            if ( op == t )
                right_arg = emptylist();
            else
                right_arg = mkappl2( op+1, t );
	    lock(right_arg);
	}

	/* Construct the application of op to args,
	    or conditional in the case of "cand" and "cor" */
            switch ( (*op) -> kind ) {
                case WORDCAND:
                    /* Check that there are only two args */
                        single_arg(left_arg); single_arg(right_arg);
                    result = cand_cond(left_arg, right_arg);
                    vfree(unlock(left_arg));  vfree(unlock(right_arg));
                    break;
                case WORDCOR:
                    /* Check that there are only two args */
                        single_arg(left_arg); single_arg(right_arg);
                    result = cor_cond(left_arg, right_arg);
                    vfree(unlock(left_arg));  vfree(unlock(right_arg));
                    break;
                default:
                    unlock(left_arg); unlock(right_arg);
		    result = mknode(APPLICATION, *op, conc(left_arg,right_arg));
		    /* add curried applications */
			while (rest <= t) {
			    result = mknode(APPLICATION, result,
					    mkappl2( rest, rest ));
			    rest++;
			}
            }


#           ifdef BAD
		if (BADflag)
		    flcheck(0,0);
		else
		    flcheck(0,1);
#           endif
        /* Make it into an arg list and return it */
            return ( mklist(result, -1) );
}
