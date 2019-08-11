#include "types.h"

extern struct obj * global_ar;

#define MAX_STACK_AR 16

/* Call a 0 argument Russell Routine from C */
struct obj * call_russell0(f)
register struct funcobj *f;
{
    word ar_block[MAX_STACK_AR];
    register struct ar * ar_ptr;
    register int len;

    if (((unsigned)(len = f -> fo_arlgth)) > MAX_STACK_AR) {
	/* either negative, i.e. requires heap, or too big */
	    if ( len < 0 ) { len = -len; }
	    ar_ptr = (struct ar *)ralloc_comp(len);
    } else {
	ar_ptr = (struct ar *)ar_block;
    }
    ar_ptr -> ar_static_link = (struct ar *)(f -> fo_ep);
    return((*(f -> fo_ip))(ar_ptr));
}


/* Call a 1 argument Russell Routine from C */
struct obj * call_russell1(f, x)
register struct funcobj *f;
struct obj * x;
{
    word ar_block[MAX_STACK_AR];
    register struct ar * ar_ptr;
    register int len;

    if (((unsigned)(len = f -> fo_arlgth)) > MAX_STACK_AR) {
	/* either negative, i.e. requires heap, or too big */
	    if ( len < 0 ) { len = -len; }
	    ar_ptr = (struct ar *)ralloc_comp(len);
    } else {
	ar_ptr = (struct ar *)ar_block;
    }
    ar_ptr -> ar_static_link = (struct ar *)(f -> fo_ep);
    ar_ptr -> ar_arg1 = x;
    return((*(f -> fo_ip))(ar_ptr));
}


/* Call a 2 argument Russell Routine from C */
struct obj * call_russell2(f, x, y)
register struct funcobj *f;
struct obj * x;
struct obj * y;
{
    word ar_block[MAX_STACK_AR];
    register struct ar * ar_ptr;
    register int len;

    if (((unsigned)(len = f -> fo_arlgth)) > MAX_STACK_AR) {
	/* either negative, i.e. requires heap, or too big */
	    if ( len < 0 ) { len = -len; }
	    ar_ptr = (struct ar *)ralloc_comp(len);
    } else {
	ar_ptr = (struct ar *)ar_block;
    }
    ar_ptr -> ar_static_link = (struct ar *)(f -> fo_ep);
    ar_ptr -> ar_arg1 = x;
    ar_ptr -> ar_arg2 = y;
    return((*(f -> fo_ip))(ar_ptr));
}
