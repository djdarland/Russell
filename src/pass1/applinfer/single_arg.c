# include "parm.h"
# include "stree/ststructs.mh"

# define DEBUG DEBUG

/*
 * Check that x is a legal arg list.
 *
 */
single_arg(x)
NODE * x;
{
#   ifdef DEBUG
        if ( !is_list(x) ) {
            dbgmsg("\nsimple_arg: Improper argument list: %o\n",x);
            abort();
        }
#   endif

    if ( is_empty(x) || first(x) != last(x) ) {
	yyperror("Argument list not meaningful");
    }
}

