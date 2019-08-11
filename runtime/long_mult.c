#define DEBUG
#undef DEBUG
/* Some multiplication routines for long.c.  These confuse the SUN 3.x */
/* optimizer.                                                          */

#include "types.h"
#include "BigNum/h/BigNum.h"

# ifndef M68K
#   define add32(x,c,y,z)  /* x := y + z, c := carry */ \
	{ \
	    unsigned long low_sum = ((y) & 0xffff) + ((z) & 0xffff); \
	    unsigned long high_sum = (((unsigned)(y)) >> 16) \
				     + (((unsigned)(z)) >> 16) \
				     + (low_sum >> 16); \
 \
	    (c) = high_sum >> 16; \
	    (x) = (high_sum << 16) + (low_sum & 0xffff); \
	}

#   define mul32(x,c,y,z)  /* x := lsw(y * z), c := msw(y * z) */ \
	{ \
	    unsigned long low_y = ((y) & 0xffff); \
	    unsigned long low_z = ((z) & 0xffff); \
	    unsigned long high_y = (((unsigned) y) >> 16); \
	    unsigned long high_z = (((unsigned) z) >> 16); \
	    unsigned long low_prod = low_y * low_z; \
	    unsigned long mid_prod1 = low_y * high_z; \
	    unsigned long mid_prod2 = high_y * low_z; \
	    unsigned long high_prod = high_y * high_z; \
	    unsigned long c1; \
 \
	    add32((x), c1, low_prod, (mid_prod1 << 16)); \
	    add32((x), (c), (x), (mid_prod2 << 16)); \
	    (c) += high_prod + (mid_prod1 >> 16) \
			     + (mid_prod2 >> 16) + c1; \
	}
#endif
struct obj * Long_cp();

# define sign_bit(x) (((long)(x)) < 0 ? -1 : 0)

/* Long_Mult for intermediate range values, assumes positive arguments */
struct obj * Long_basic_Mult(opx,opy)
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    register long lenx = (long) (opx -> obj_component[0]);
    register long leny = (long) (opy -> obj_component[0]);
    long nlen = lenx + leny;
    extern long Long_0[];
    struct obj * result = (struct obj *) Long_cp(Long_0,nlen-1);
 
    /* Switch x and y if necessary */
      if (lenx < leny) {
	{
	    struct obj * tmp;

	    tmp = opx;
	    opx = opy;
	    opy = tmp;
	}
	{
	    long tmp;
	    
	    tmp = lenx;
	    lenx = leny;
	    leny = tmp;
	}
      }
    /* Fill in result */
      BnnMultiply (&(result->obj_component[1]), nlen,
		  &(opx->obj_component[1]), lenx,
		  &(opy->obj_component[1]), leny);

    /* Renormalize result to avoid unnecessary growth */
      while (nlen > 1
	     && BnnGetDigit(&(result->obj_component[nlen]))
		== sign_bit(BnnGetDigit(&(result -> obj_component[nlen-1])))) {
	nlen--;
      }
    result -> obj_component[0] = nlen;
    return( result );
}

/* Long_Mult for single word arguments */
struct obj * Long_quick_Mult(opx,opy)
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    register unsigned long D7;
    register unsigned long D6;
    register struct obj *result;    /* val Long */
#   ifndef M68K
	int sign;
	extern struct obj * Long_Neg();
#   endif

    result = ralloc(3);

    D6 = opx -> obj_component[1];
    D7 = opy -> obj_component[1];
#   ifdef M68K
	asm("mulsl d7,d7:d6");
#   else
	sign = (((long)D6) < 0) ^ (((long)D7) < 0);
	D6 = ((((long)D6) < 0) ? -D6 : D6);
	D7 = ((((long)D7) < 0) ? -D7 : D7);
	mul32(D6,D7,D6,D7);
#   endif
    result -> obj_component[1] = D6;    /* lsw    */
    result -> obj_component[2] = D7;    /* msw    */
    /* Renormalize result to avoid unnecessary growth */
      if (result -> obj_component[2] == 0 &&
	     ((long) (result -> obj_component[1])) >= 0
	  || ((long)(result -> obj_component[2])) == -1 &&
	       ((long) (result -> obj_component[1])) < 0) {
	  result -> obj_component[0] = 1;     /* length */
      } else {
	  result -> obj_component[0] = 2;     /* length */
      }
#   ifndef M68K
	if (sign) {
	    result = Long_Neg(result);
	}
#   endif
    return(result);
}

