# include "parm.h"

# include "stree/ststructs.mh"

# include "stree/Array.h"

# ifdef BAD
    extern boolean BADflag;
# endif

/*
 * mkappl(l)
 *
 * input: A denotation in the form of a list of primaries.
 *
 * output: The denotation in the form of a tree of applications
 *         and conditionals. A heuristic (described elsewhere)
 *         is applied to decide how to parse the list if args
 *         are not fully bracketed.
 *         The input list l is destroyed and replaced by the
 *         empty list.
 */

NODE * mkappl(l)
NODE * l;
{   Array *  array;      /* array of pointers to primaries */
    NODE *  tree,       /* the result tree */
         *  sl;         /* a singleton list containing the tree */

#   ifdef DEBUG
        if ( !is_list(l) || is_empty(l) ) {
            dbgmsg("\nmkappl: arg not a list: %o\n", l);
            abort();
        }
#   endif


    /* Convert l into an array. */
        array = list_to_array(l);

    /* Construct a singleton list containing (hopefully) the tree of
       applications and conditionals.                               */
        sl = mkappl2( array->a_body, &array->a_body[array->a_size-1] );
        lock(sl);

    /* Return the tree, freeing the array and the list. */
	if (is_empty(sl)) {
	    yyperror("Empty argument list without operator");
	    tree = lock(sl);
	} else {
	    tree = lock( first(sl) );
	}

#       ifdef BAD
            if (BADflag)
                flcheck(0,0);
            else
                flcheck(0,1);
#       endif

        free_array(array);  vfree( unlock(sl) );

#       ifdef BAD
            if (BADflag)
                flcheck(0,0);
            else
                flcheck(0,1);
#       endif

        return ( unlock(tree) );

}
