#define DEBUG
#undef DEBUG
#define VERBOSE
#undef VERBOSE
/**/
/* (almost) unlimited length integer data type
/* Values are represented as a length (in words) of the 2nd fld, followed
/* by the number, stored in two's complement form, with msbs in the high
/* numbered word.  The length does not necessarily correspond to the
/* size of the block allocated to the number.
/* Variables are represented as a single word pointing to a value.
/* This assumes overflow traps are disabled.
/* The specified length is always minimal.
/**/

/*
 * This implementation is based on the lowest level of the DEC/INRIA
 * BigNum package.  We rely on the fact that digits are 32 bits long.
 */ 

#include "types.h"
#include "BigNum/h/BigNum.h"

/* A redefinition of BnGetDigit as a macro, to avoid all sorts of casts */
#ifdef BnGetDigit
#  undef BnGetDigit
#endif

#define BnGetDigit(x,y) ((x)->obj_component[y])

#define LONGVARSZ 1

long Long_m1[] = {1, -1};
long Long_0[] = {1,0};
long Long_1[] = {1,1};
long Long_2[] = {1,2};
long Long_3[] = {1,3};
long Long_4[] = {1,4};
long Long_5[] = {1,5};
long Long_6[] = {1,6};
long Long_7[] = {1,7};
long Long_8[] = {1,8};
long Long_9[] = {1,9};
long Long_10[] = {1,10};

static void fail(s)
char * s;
{
    russell_error(s);
}

# ifdef DEBUG
   int long_level = 0;  /* Level of operation nesting */
#endif

/* Long_New: func[] var Long */

MkIP(Long_New())
{
register struct obj *op;    /* var Long */

    op = ralloc_comp(LONGVARSZ);
    op->obj_component[0] = UNINIT;
    return(op);
}

MkFVAL0(Long_New);

/* Long_init_New: func[val Long] var Long */

MkIP(Long_init_New(x))
struct obj * x;
{
register struct obj *op;    /* var Long */

    op = ralloc_comp(LONGVARSZ);
    op->obj_component[0] = (word)x;
    return(op);
}

MkFVAL1(Long_init_New);


/* Long_Assign: func[var Long; val Long] val Long */

MkIP(Long_Assign(lop,rop))
struct obj *lop;    /* var Long */
struct obj *rop;    /* val Long */
{
    lop->obj_component[0] = (word)rop;
    return(rop);
}

MkFVAL2(Long_Assign);


/* Long_ValueOf: func[var Long] val Long */

MkIP(Long_ValueOf(aop))
struct obj *aop;    /* var Long */
{
    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(Long_ValueOf);

# define sign_bit(x) (((long)(x)) < 0 ? -1 : 0)

/* Long_cp: return a copy of the argument, sign extending with n longwords */
struct obj * Long_cp(x,n)
struct obj * x;
unsigned long n;
{
    unsigned long len = x -> obj_component[0] + 1;
    register unsigned long nlen = len + n;
    register struct obj *op;

    op = ralloc(nlen);
    {
	register word * p = (word *) op;
	register word * q = (word *) x;
	register word * lim = p + len;
	register long sign = sign_bit(q[len-1]);

	for (; p < lim; *p++ = *q++);
	for (lim = ((word *)op) + nlen; p < lim; *p++ = sign);
    }
    return(op);
}

/* Long_cp1: As above, but don't include size field in result, 0 fill */
/* rather than sign extend.					      */
struct obj * Long_cp1(x,n)
struct obj * x;
unsigned long n;
{
    unsigned long len = x -> obj_component[0];
    register unsigned long nlen = len + n;
    register struct obj *op;

    op = ralloc(nlen);
    {
	register word * p = (word *) op;
	register word * q = (word *) x;
	register word * lim = p + len;

	for (q++ /* skip sign */; p < lim; *p++ = *q++);
	for (lim = ((word *)op) + nlen; p < lim; *p++ = 0);
    }
    return(op);
}

/* Long_Add: func[x,y: val Long] val Long */

struct obj * Long_Add();
struct obj * Long_Sub();
struct obj * Long_Sub1();
struct obj * Long_Put();
struct obj * Long_Puts();

/* First a quick version to take care of the case in which both      */
/* opx and opy have length 1                                         */
/* We clobber a0, a1  and d0                                         */
# ifdef M68K
    asm(".globl _Long_Add");
    asm("_Long_Add:");
    /* a0 := opx; d0 := length(opx) */
	asm("movl sp@(4),a0");
	asm("movl a0@,d0");
    /* if (d0 != 1) return(Long_Add1(opx,opy)) */
	asm("cmpl #1,d0");
	asm("jne  _Long_Add1");
    /* d0 := value(opx) */
	asm("movl a0@(4),d0");
    /* a0 := opy  */
	asm("movl sp@(8),a0");
    /* if (length(a0) != 1) return(Long_Add1(opx,opy)) */
	asm("cmpl #1,a0@");
	asm("jne  _Long_Add1");
    /* d0 += value(a0) */
	asm("addl a0@(4),d0");
    /* if (overflow) return(Long_Add1(opx,opy)) */
	asm("jvs _Long_Add1");
    /* a0 := new atomic object of size 2 */
	asm("movl #_aobjfreelist+8,a1");
	asm("movl a1@,a0");
	asm("tstl a0");
	asm("jne  1f");
	/* save d0 */
	    asm("movl d0,sp@-");
	asm("movl #2,sp@-");
	asm("jbsr _allocaobj");
	asm("movl d0,a0");
	/* restore d0 */
	    asm("movl sp@(4),d0");
	asm("addl #8,sp");
	asm("1: movl a0@,a1@");
    /* Fill in new object */
	asm("movl #1,a0@");
	asm("movl d0,a0@(4)");
    /* return(a0) */
	asm("movl a0,d0");
	asm("rts");
# else
  /*
   * currently do simple add for VAX and other machines
   */
    MkIP(Long_Add(opx,opy))
    struct obj * opx;
    struct obj * opy;
    {
	MkIP(Long_Add1());		/* declare Long_Add1's type */

	return(Long_Add1(opx,opy));
    }
# endif

/*
 * and now the general case add:
 */
struct obj * Long_Add1(opx,opy)
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    register long lenx;
    register long leny;
    register long lenr;
    register struct obj * result;
    struct obj * new_y;
    long sign_y;

  /* set lenx and leny to lengths of opx and opy, resp. */
    lenx = (long) opx -> obj_component[0];
    leny = (long) opy -> obj_component[0];

  /* Allocate sufficient length result */
    if (leny > lenx) {
        result = Long_cp(opx, 1 + leny - lenx);
	lenr = leny + 1;
    } else {
	result = Long_cp(opx, 1);
	lenr = lenx + 1;
    }
#   ifdef DEBUG
          result -> obj_component[0] = lenr;
	  long_level++;
          if (Long_cmp(result, opx) != 0) {
	    MSG("Add: first operand comparison failure");
	    Long_Put(result); MSG("");
	    Long_Put(opx); MSG("");
	    fail("Add: first operand comparison failure");
          }
	  long_level--;
#   endif

    sign_y = BnGetDigit(opy, leny);

  /* Sign extend y if necessary */
    if (sign_y < 0) {
        new_y = Long_cp(opy, lenr - leny);
	leny = lenr;
#       ifdef DEBUG
	  long_level++;
          new_y -> obj_component[0] = leny;
          if (Long_cmp(new_y, opy) != 0) {
	    fail("Add: comparison failure");
          }
	  long_level--;
#       endif
    } else {
	new_y = opy;
    }


  /* Fill in result */
    (void) BnnAdd(&(result->obj_component[1]), lenr,
		  &(new_y->obj_component[1]), leny, 0);
    while (lenr > 1
           && BnGetDigit(result, lenr) 
              == sign_bit(BnGetDigit(result, lenr-1))) {
	lenr--;
    }
    result -> obj_component[0] = lenr;
    if (new_y != opy) {
	rfree(new_y);
    }
#   ifdef DEBUG
	if (long_level == 0) {
	  long_level++;
	  if (Long_cmp(Long_Sub1(result, opy), opx) != 0) {
	    MSG("Long_Add: incorrect result");
	    Long_Put(opx); MSG("");
	    Long_Put(opy); MSG("");
	    Long_Put(result); MSG("");
	    Long_Put(Long_Sub1(result,opy)); MSG("");
	    ABORT("Long_Add: incorrect result\n");
	  }
	  long_level--;
	}
#   endif
    return( result );
}

MkFVAL2(Long_Add);


/* Long_Sub: func[x,y: val Long] val Long */

/* First a quick version to take care of the case in which whic both */
/* opx and opy have length 1                                         */
/* We clobber a0, a1 and d0                                          */
# ifdef M68K
    asm(".globl _Long_Sub");
    asm("_Long_Sub:");
    /* a0 := opx; d0 := length(opx) */
	asm("movl sp@(4),a0");
	asm("movl a0@,d0");
    /* if (d0 != 1) return(Long_Sub1(opx,opy)) */
	asm("cmpl #1,d0");
	asm("jne  _Long_Sub1");
    /* d0 := value(opx) */
	asm("movl a0@(4),d0");
    /* a0 := opy  */
	asm("movl sp@(8),a0");
    /* if (length(a0) != 1) return(Long_Sub1(opx,opy)) */
	asm("cmpl #1,a0@");
	asm("jne  _Long_Sub1");
    /* d0 -= value(a0) */
	asm("subl a0@(4),d0");
    /* if (overflow) return(Long_Sub1(opx,opy)) */
	asm("jvs _Long_Sub1");
    /* a0 := new atomic object of size 2 */
	asm("movl #_aobjfreelist+8,a1");
	asm("movl a1@,a0");
	asm("tstl a0");
	asm("jne  1f");
	/* save d0 */
	    asm("movl d0,sp@-");
	asm("movl #2,sp@-");
	asm("jbsr _allocaobj");
	asm("movl d0,a0");
	/* restore d0 */
	    asm("movl sp@(4),d0");
	asm("addl #8,sp");
	asm("1: movl a0@,a1@");
    /* Fill in new object */
	asm("movl #1,a0@");
	asm("movl d0,a0@(4)");
    /* return(a0) */
	asm("movl a0,d0");
	asm("rts");
# else /* any other machine */
    MkIP(Long_Sub(opx,opy))
    struct obj * opx;
    struct obj * opy;
    {
	MkIP(Long_Sub1());

	return(Long_Sub1(opx,opy));
    }
# endif

/*
 * and now the general case subtract:
 */
struct obj * Long_Sub1(opx,opy)
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    register long lenx;
    register long leny;
    register long lenr;
    register struct obj * result;
    struct obj * new_y;
    long sign_y;

  /* set lenx and leny to lengths of opx and opy, resp. */
    lenx = (long) opx -> obj_component[0];
    leny = (long) opy -> obj_component[0];

  /* Allocate sufficient length result */
    if (leny > lenx) {
        result = Long_cp(opx, 1 + leny - lenx);
	lenr = leny + 1;
    } else {
	result = Long_cp(opx, 1);
	lenr = lenx + 1;
    }
#   ifdef DEBUG
          result -> obj_component[0] = lenr;
	  long_level++;
          if (Long_cmp(result, opx) != 0) {
	    fail("Sub: first operand comparison failure");
          }
	  long_level--;
#   endif
    sign_y = BnGetDigit(opy, leny);

  /* Allow 1 word for carry in result */

  /* Sign extend y if necessary */
    if (sign_y < 0) {
        new_y = Long_cp(opy, lenr - leny);
	leny = lenr;
#       ifdef DEBUG
          new_y -> obj_component[0] = leny;
	  long_level++;
          if (Long_cmp(new_y, opy) != 0) {
	    fail("Sub: comparison failure");
          }
	  long_level--;
#       endif
    } else {
	new_y = opy;
    }

  /* Fill in result */
    (void) BnnSubtract(&(result->obj_component[1]), lenr,
		       &(new_y->obj_component[1]), leny, 1);
    while (lenr > 1
           && BnGetDigit(result, lenr)
              == sign_bit(BnGetDigit(result, lenr-1))) {
	lenr--;
    }
    result -> obj_component[0] = lenr;
    if (new_y != opy) {
	rfree(new_y);
    }
#   ifdef DEBUG
      if (long_level == 0) {
	long_level++;
	if (Long_cmp(Long_Add1(opy, result), opx) != 0) {
	    fail("Long_Sub: incorrect result");
	}
	long_level--;
      }
#   endif
    return( result );
}

MkFVAL2(Long_Sub);


/* Long_Neg: func[x: val Long] val Long */

MkIP(Long_Neg(opx))
struct obj *opx;    /* val Long */
{
    register long lenx = (long) opx -> obj_component[0]; 
    struct obj * result = Long_cp(opx, 1);

    lenx++;
    BnnComplement(&(result->obj_component[1]), lenx);
    BnnAddCarry(&(result->obj_component[1]), lenx, 1);
    while (lenx > 1
           && BnGetDigit(result, lenx)
              == sign_bit(BnGetDigit(result, lenx-1))) {
	lenx--;
    }
    result -> obj_component[0] = lenx;
#   ifdef DEBUG
      if (long_level == 0) {
	long_level++;
	if (Long_cmp(Long_Add1(opx, result), Long_0) != 0) {
	    fail("Long_Negate: incorrect result");
	}
	long_level--;
      }
#   endif
    return(result);
}

MkFVAL1(Long_Neg);


/* Long_Shift: func[op, nbits: val Long] val Long */
/* positive ==> left shift, neg ==> right         */

MkIP(Long_Shift(op, nbits))
struct obj *op;       /* val Long */
struct obj *nbits;    /* val Long */
{
    register long len = (long) op -> obj_component[0];
    register long nb = (long) nbits -> obj_component[1];
    long msw = op -> obj_component[len];
    long nlen;       /* length of result */
    long tmp;
    long add_words;  /* number of additional words needed by result */
    struct obj * result;

    /* Check for absurdly long shifts  */
      if ((tmp = (nbits -> obj_component[0])) != 1) {
	if ( ((long) nbits -> obj_component[tmp]) < 0) {
	    if (((long) op -> obj_component[len]) >= 0) {
		return((struct obj *) Long_0);
	    } else {
		return((struct obj *) Long_m1);
	    }
        } else {
          /* left shift */
            russell_error("Long$shift: Left shift too far\n");
        }
      }
    /* Calculate new length generously */
      add_words = (nb + WORDSZ -1) >> LOGWL;
      nlen = len + add_words;
      if (nlen <= 0) {
	  if (((long) op -> obj_component[len]) >= 0) {
              return((struct obj *) Long_0);
          } else {
              return((struct obj *) Long_m1);
          }
      }

    if (msw < 0) {
      /* Sign extend for an extra word to make sure 1s get shifted */
      /* into the most significant bits.			   */
      result = ralloc(nlen + 2);
      result -> obj_component[nlen+1] = -1;
    } else {
      result = ralloc(nlen + 1);
    }

    /* Fill in result */
      {
	register word *px, *pr;
	register word *t;
	word *rlim;

	/* Set result to op left shifted by add_words words */
	  if (add_words < 0) {
	    px = &(op -> obj_component[1 - add_words]);
	  } else {
	    px = &(op -> obj_component[1]);
  	  }
          if (add_words > 0) {
	    pr = &(result -> obj_component[1 + add_words]);
	  } else {
	    pr = &(result -> obj_component[1]);
	  }
	  rlim = &(result -> obj_component[nlen]); /* last word */
	  /* Clear least significant words of result */
	    for (t = &(result -> obj_component[1]); t < pr; t++) {
	       *t = 0;
	    }
	  /* Copy the relevant section of op */
	    while (pr <= rlim) {
		*pr++ = *px++;
	    }
	}
    /* Now right shift by the right number of bits */
      { 
	  long nb_in_w;  /* number of bits to shift each individual word */

	  nb_in_w = (-nb & (WORDSZ-1));
	  /* -nb = nb_in_w mod WORDSZ, 0 <= nb_in_w <= 31 */
	  if (add_words > 0) {
	      result->obj_component[add_words] = 
		BnnShiftRight(&(result->obj_component[add_words + 1]),
			      (msw < 0? len+1 : len),
			      nb_in_w);
	  } else {
	    /* discard remainder */
	      (void) BnnShiftRight(&(result->obj_component[1]),
				   (msw < 0? nlen+1 : nlen),
				   nb_in_w);
	  }
      }

    /* Renormalize result to avoid unnecessary growth */
      while (nlen > 1
             && BnGetDigit(result, nlen)
                == sign_bit(BnGetDigit(result, nlen-1))) {
	nlen--;
      }
    result -> obj_component[0] = nlen;
        
    return( result );
}

MkFVAL2(Long_Shift);


/* Long_Nbits: func[x: val Long] val Long           */
/* returns the number of bits needed to represent x */
/* It takes 1 bit to represent 0                    */

MkIP(Long_Nbits(op))
struct obj *op;       /* val Long */
{
    register long lenw = (long) op -> obj_component[0];
    register long msw = (long) op -> obj_component[lenw];
    long tmp_msw;
    long lenb;       /* no of unused bits in msw */
    register struct obj * result;
#   define RESULTSZ 2  /* length of result */

    result = ralloc(RESULTSZ);	

    /* Calculate lenb */
        lenb = 0;
        if (msw > 0) {
	  lenb = BnnNumLeadingZeroBitsInDigit(op-> obj_component[lenw]) - 1;
	} else if (msw < 0) {
	  tmp_msw = ~msw;
	  lenb = BnnNumLeadingZeroBitsInDigit(&tmp_msw) - 1;
        } else {
	  lenb = WORDSZ - 1/* needed for sign */;
        }

    /* Fill in result */
        result -> obj_component[0] = RESULTSZ - 1;
        result -> obj_component[1] = (lenw << LOGWL) - lenb;
        
    return( result );
#   undef RESULTSZ
}

MkFVAL1(Long_Nbits);


/* Long_Nwords: func[x: val Long] val Long               */
/* returns the number of longwords needed to represent x */
/* Faster than nbits.                                    */

MkIP(Long_Nwords(op))
struct obj *op;       /* val Long */
{
#   define RESULTSZ 2
    register struct obj * result;
    
    result = ralloc(RESULTSZ);
    result -> obj_component[0] = 1;
    result -> obj_component[1] = ((long) op -> obj_component[0]);
    return(result);
#   undef RESULTSZ
}

MkFVAL1(Long_Nwords);


/* Long_Mult for intermediate range values (in file long_mult.c) */
struct obj * Long_basic_Mult();

/* Long_Mult for single word arguments (in file long_mult.c) */
struct obj * Long_quick_Mult();

/* Destructively normalize x, x >= 0 */
Long_norm(x)
struct obj * x;
{
    unsigned long len = x -> obj_component[0];

    while(((long)(x -> obj_component[len])) == 0
	  && len > 1
	  && ((long)(x -> obj_component[len-1])) >= 0) {
	len--;
    }
    x -> obj_component[0] = len;
}

/* Left shift x by n words, n >= 0 */
struct obj * Long_ws(opx, n)
struct obj * opx;
long n;
{
    long lenx = (long) (opx -> obj_component[0]);
    struct obj * result = ralloc(lenx + n + 1);
    register int i;

    result -> obj_component[0] = lenx + n;
    for (i = 1; i <= n; i++) {
	result -> obj_component[i] = 0;
    }
    for (i = 1; i <= lenx; i++) {
	result -> obj_component[n+i] = opx -> obj_component[i];
    }
    return(result);
}

/* Long_Mult: func[x,y: val Long] val Long */
MkIP(Long_Mult(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
#   ifdef HARDWARE_MULT
#       define LONG_THRESHOLD 256
#   else
#       define LONG_THRESHOLD 24
#   endif
    long lenx = (long) (opx -> obj_component[0]);
    long leny = (long) (opy -> obj_component[0]);
    
    if (lenx == 1 && leny == 1) {
	return(Long_quick_Mult(opx,opy));
    } else {
	short sx = ((long) opx -> obj_component[lenx]) < 0;
	short sy = ((long) opy -> obj_component[leny]) < 0;
	short sr = sx ^ sy;   /* result is negative */
	struct obj * result;

	if (sx) {
	    opx = Long_Neg(opx);
	}
	if (sy) {
	    opy = Long_Neg(opy);
	}
	/* result := opx * opy; opx and opy are nonnegative */
	    if (lenx >= LONG_THRESHOLD && leny >= LONG_THRESHOLD) {
	      /* use n**1.58 algorithm */
		int i;
		long *tmp1, *tmp2, *tmp3, *tmp4;
		unsigned long split_pos = lenx > leny? (leny >> 1)
						     : (lenx >> 1);
		struct obj *low_prod, *high_prod, *comb_prod;
		struct obj *sum1, *sum2;

		/* Round up split_pos to multiple of 16 */
		    split_pos += 15;
		    split_pos &= ~15;

		/* Split x into tmp and tmp2 (most sign.) */
		  tmp1 = (long *) ralloc(split_pos+2);
		  tmp2 = (long *) ralloc(lenx - split_pos + 1);
		  for (i = 1; i <= split_pos; i++) {
		    tmp1[i] = opx -> obj_component[i];
		  }
		  tmp1[split_pos+1] = 0;  /* Make sure tmp1 is positive */
		  tmp1[0] = split_pos+1;  /* length */
		  Long_norm(tmp1);
		  for (i = split_pos + 1; i <= lenx; i++) {
		    tmp2[i-split_pos] = opx -> obj_component[i];
		  }
		  tmp2[0] = lenx - split_pos;
		/* Split y into tmp3 and tmp4 (most sign.) */
		  tmp3 = (long *) ralloc(split_pos+2);
		  tmp4 = (long *) ralloc(leny - split_pos + 1);
		  for (i = 1; i <= split_pos; i++) {
		    tmp3[i] = opy -> obj_component[i];
		  }
		  tmp3[split_pos+1] = 0;  /* Make sure tmp3 is positive */
		  tmp3[0] = split_pos+1;  /* length */
		  Long_norm(tmp3);
		  for (i = split_pos + 1; i <= leny; i++) {
		    tmp4[i-split_pos] = opy -> obj_component[i];
		  }
		  tmp4[0] = leny - split_pos;
		/* Compute three partial products */
		  sum1 = Long_Add((struct obj *) tmp1, (struct obj *) tmp2);
		  sum2 = Long_Add((struct obj *) tmp3, (struct obj *) tmp4);
		  if ((split_pos >> 1) > LONG_THRESHOLD) {
		    low_prod = Long_Mult((struct obj *) tmp1,
					 (struct obj *) tmp3);
		    high_prod = Long_Mult((struct obj *) tmp2,
					  (struct obj *) tmp4);
		    comb_prod = Long_Mult((struct obj *) sum1,
					  (struct obj *) sum2);
		  } else {
		    low_prod = Long_basic_Mult((struct obj *) tmp1,
					       (struct obj *) tmp3);
		    high_prod = Long_basic_Mult((struct obj *) tmp2,
						(struct obj *) tmp4);
		    comb_prod = Long_basic_Mult((struct obj *) sum1,
						(struct obj *) sum2);
		  }
		/* Compute sum of middle terms */
		  rfree(sum1); rfree(sum2);
		  sum1 = Long_Sub(comb_prod, high_prod);
		  sum2 = Long_Sub(sum1, low_prod);
		  rfree(comb_prod);
		  rfree(sum1);
		  sum1 = Long_ws(sum2, split_pos);
		  rfree(sum2);
		/* Add in the other two products */
		  sum2 = Long_ws(high_prod, split_pos << 1);
		  rfree(high_prod);
		  high_prod = sum2;
		  sum2 = Long_Add(sum1, high_prod);
		  rfree(sum1);
		  rfree(high_prod);
		  sum1 = Long_Add(sum2, low_prod);
		  rfree(sum2);
		  rfree(low_prod);
		  rfree(tmp1); rfree(tmp2); rfree(tmp3); rfree(tmp4);
		result = sum1;
	    } else {
	      /* use quadratic algorithm */
		result = Long_basic_Mult(opx, opy);
	    }
	/* Free negated operands */
	  if (sx) {
	    rfree(opx);
	  }
	  if (sy) {
	    rfree(opy);
	  }
	/* negate result if it is strictly negative */
	  if (sr) {
	    struct obj * Oresult = result;
	    result = Long_Neg(result);
	    rfree(Oresult);
	  }
	return(result);
    }
}


MkFVAL2(Long_Mult);


static long remainder;  /* used by Long_Div and Long_Put */

/* Long_Div: func[x: val Long, y: val Short] val Long */

MkIP(Long_Div(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Short */
{
    register long lenx = (long) (opx -> obj_component[0]);
    long divisor = (long) opy;				/* the divisor	    */
    long sx = ((long) opx -> obj_component[lenx]) < 0;	/* => dividend < 0  */
    long sy = (divisor < 0);				/* => divisor < 0   */
    long sr = sx ^ sy;					/* => result < 0    */
    long nlen = 0;					/* length of result */
    struct obj * result = (struct obj *) 0;		/* result	    */
 
    if (sx) { opx = Long_Neg(opx); }	/* make dividend positive */
    if (sy) { divisor = -divisor; }	/* make divisor positive  */

    if (divisor == 0) {
	russell_error("Long$/ : division by 0\n");
    }

    result = Long_cp1(opx, 1);

    /* Fill in result */
	remainder = 
	  BnnDivideDigit(&(result->obj_component[1]),
			 &(result->obj_component[0]), lenx+1, divisor);

    /* Adjust lenx to be the true length of the result */
        while (lenx > 1
               && BnGetDigit(result, lenx)
                  == sign_bit(BnGetDigit(result, lenx-1))) {
	     lenx--;
        }
	result -> obj_component[0] = lenx;

    /* Free negated dividend and negate remainder if appropriate*/
      if (sx) {
	rfree(opx);
	remainder = -remainder;
      }
    /* negate result if necessary */
      if (sr) {
	result = Long_Neg(result);
      }
        
    return( result );
}

MkFVAL2(Long_Div);


/* Long_LDiv: func[x: val Long, y: val Long] val Long */

MkIP(Long_LDiv(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    register long lenx = (long) (opx -> obj_component[0]);
    register long leny = (long) (opy -> obj_component[0]);
    long sx = ((long) opx -> obj_component[lenx]) < 0;	/* => dividend < 0  */
    long sy = ((long) opy -> obj_component[leny]) < 0;	/* => divisor <  0  */
    long sr = sx ^ sy;					/* => result < 0    */
    long nlen = 0;					/* length of result */
    struct obj * result = (struct obj *) 0;		/* result	    */
    struct obj * tmp_result = (struct obj *) 0;		/* result	    */
 
    if (sx) { opx = Long_Neg(opx); }	/* make dividend positive */
    if (sy) { opy = Long_Neg(opy); }	/* make divisor positive  */

    if (leny == 1 && opy -> obj_component[1] == 0) {
	russell_error("Long$/ : division by 0\n");
    }

    tmp_result = Long_cp1(opx, 1); /* Add extra 0 to make BnnDivide happy */

    if (lenx < leny) {
        /* Operands are normalized ==> result must be 0 */
	return((struct obj *)Long_0);
    }
    /* Make sure divisor has no leading zeroes */
        if (BnGetDigit(opy, leny) == 0) {
	    leny--;
	}
    /* Fill in result */
	BnnDivide(&(tmp_result->obj_component[0]), lenx+1,
		  &(opy->obj_component[1]), leny);

    /* Copy quotient out of the result */
	nlen = 1 + lenx - leny;
	result = (struct obj *)ralloc(nlen+1);
	{
	    register word *p, *q, *lim;

	    p = &(tmp_result -> obj_component[leny]);
	    lim = &(tmp_result -> obj_component[1 + lenx]);
	    q = &(result -> obj_component[1]);
	    while (p < lim) {
		*q++ = *p++;
	    }
	}
	
    /* Adjust nlen to be the true length of the result */
        while (nlen > 1
               && BnGetDigit(result, nlen)
                  == sign_bit(BnGetDigit(result, nlen-1))) {
	     nlen--;
        }
	result -> obj_component[0] = nlen;

    rfree(tmp_result);
    /* Free negated dividend and divisor if appropriate*/
      if (sx) {
	rfree(opx);
      }
      if (sy) {
	rfree(opy);
      }
    /* negate result if necessary */
      if (sr) {
	tmp_result = result;
	result = Long_Neg(result);
	rfree(tmp_result);
      }
        
    return( result );
}

MkFVAL2(Long_LDiv);


/* Long_LMod: func[x: val Long, y: val Long] val Long */

MkIP(Long_LMod(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    register long lenx = (long) (opx -> obj_component[0]);
    register long leny = (long) (opy -> obj_component[0]);
    long sx = ((long) opx -> obj_component[lenx]) < 0;	/* => dividend < 0  */
    long sy = ((long) opy -> obj_component[leny]) < 0;	/* => divisor <  0  */
    long nlen = 0;					/* length of result */
    struct obj * result = (struct obj *) 0;		/* result	    */
    struct obj * tmp_result = (struct obj *) 0;		/* result	    */
 
    if (sx) { opx = Long_Neg(opx); }	/* make dividend positive */
    if (sy) { opy = Long_Neg(opy); }	/* make divisor positive  */

    if (leny == 1 && opy -> obj_component[1] == 0) {
	russell_error("Long$% : division by 0\n");
    }


    if (lenx < leny) {
        /* Operands are normalized ==> result must be x */
	return((struct obj *)Long_cp(opx,0));
    }

    tmp_result = Long_cp1(opx, 1); /* Add extra 0 to make BnnDivide happy */

    /* Make sure divisor has no leading zeroes */
        if (BnGetDigit(opy, leny) == 0) {
	    leny--;
	}

    /* Fill in result */
	BnnDivide(&(tmp_result->obj_component[0]), lenx+1,
		  &(opy->obj_component[1]), leny);

    /* Copy quotient out of the result */
	nlen = leny;
	result = (struct obj *)ralloc(nlen+1);
	{
	    register word *p, *q, *lim;

	    p = &(tmp_result -> obj_component[0]);
	    lim = &(tmp_result -> obj_component[leny]);
	    q = &(result -> obj_component[1]);
	    while (p < lim) {
		*q++ = *p++;
	    }
	}
	
    /* Adjust nlen to be the true length of the result */
        while (nlen > 1
               && BnGetDigit(result, nlen)
                  == sign_bit(BnGetDigit(result, nlen-1))) {
	     nlen--;
        }
	result -> obj_component[0] = nlen;

    rfree(tmp_result);
    /* Free negated dividend and divisor if appropriate*/
      if (sx) {
	rfree(opx);
      }
      if (sy) {
	rfree(opy);
      }

    /* negate result if necessary */
      if (sx) {
	tmp_result = result;
	result = Long_Neg(result);
	rfree(tmp_result);
      }
        
    return( result );
}

MkFVAL2(Long_LMod);


/* Long_cmp: Compare two long integers and return a short     */
/* integer less than, equal to, or greater than 0             */
/* depending on whether the first is less than, equal to, or  */
/* greater than 0.                                            */
long Long_cmp(opx,opy)
struct obj * opx;
struct obj * opy;
{
    register long lenx = ((long)(opx -> obj_component[0]));
    register long leny = ((long)(opy -> obj_component[0]));
    long Long_cmp1();
    
#   ifdef DEBUG
      /* Debugging code may call these routines on non-normalized values */
      while (lenx > 1
           && BnGetDigit(opx, lenx) 
              == sign_bit(BnGetDigit(opx, lenx-1))) {
	if (long_level == 0) {
	    fail("Long_cmp: unnormalized operand\n");
	}
	lenx--;
      }
      while (leny > 1
           && BnGetDigit(opy, leny) 
              == sign_bit(BnGetDigit(opy, leny-1))) {
	if (long_level == 0) {
	    fail("Long_cmp: unnormalized operand\n");
	}
	leny--;
      }
#   endif
    if (lenx > leny) {
	/* Return a nonzero value with sign = sign of msw of x */
	return(((long)(opx -> obj_component[lenx])) | 1);
    } else if (lenx < leny) {
	return(-(((long)(opy -> obj_component[leny])) | 1));
	    /* Note that the preceding negation cannot overflow */
    } else /* equal length */ {
	if (((long)(opx -> obj_component[lenx]))
	    > ((long)(opy -> obj_component[leny]))) {
	    return(1);
	} else if (((long)(opx -> obj_component[lenx]))
		   < ((long)(opy -> obj_component[leny]))) {
	    return(-1);
	} else if (lenx == 1) {
	    return(0);
	} else {
            return(Long_cmp1(opx,opy,lenx));
        }
    }
}

/* Version of Long_cmp that assumes length and most */
/* significant words of opx and opy are the same.   */
/* Note that this implies opx and opy have the same */
/* sign.                                            */
long Long_cmp1(opx,opy,len)
struct obj *opx;
struct obj *opy;
{
    register unsigned long cwx;  /* word currently being examined */
    register unsigned long cwy;
    register long * opxp = (long *) &(opx -> obj_component[len-1]);
                                    /* current position in opx */
    register long * opyp = (long *) &(opy -> obj_component[len-1]);

    
        while (opxp > (long *)opx) {
            cwx = *opxp--;
            cwy = *opyp--;
            if (cwx != cwy) {
                if (cwx > cwy /* unsigned comparison */) {
                    return(1);
                } else {
                    return(-1);
                }
            }
        }
    /* Opx and opy are equal */
    return(0);
}


/* Long_Lt: func[x,y: val Long] val Bool */

MkIP(Long_Lt(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    return( (struct obj *)
	( Long_cmp(opx,opy) < 0 )
    );
}

MkFVAL2(Long_Lt);


/* Long_Le: func[x,y: val Long] val Bool */

MkIP(Long_Le(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    return( (struct obj *)
        ( Long_cmp(opx,opy) <= 0 )
    );
}

MkFVAL2(Long_Le);


/* Long_Eq: func[x,y: val Long] val Bool */

MkIP(Long_Eq(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    return( (struct obj *)
        ( Long_cmp(opx,opy) == 0 )
    );
}

MkFVAL2(Long_Eq);


/* Long_Ne: func[x,y: val Long] val Bool */

MkIP(Long_Ne(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    return( (struct obj *)
        ( Long_cmp(opx,opy) != 0 )
    );
}

MkFVAL2(Long_Ne);


/* Long_Ge: func[x,y: val Long] val Bool */

MkIP(Long_Ge(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    return( (struct obj *)
        ( Long_cmp(opx,opy) >= 0 )
    );
}

MkFVAL2(Long_Ge);


/* Long_Gt: func[x,y: val Long] val Bool */

MkIP(Long_Gt(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    return( (struct obj *)
	( Long_cmp(opx,opy) > 0 )
    );
}

MkFVAL2(Long_Gt);

/*  Long_0 thru Long_9: func[]: val Long */
/*  Value is encoded in ep                  */

MkIP(Long_Const(A))
struct ar * A;
{
    return((struct obj *)(A -> ar_static_link));
}

MkEPFVAL0(Long_0,Long_Const,Long_0);
MkEPFVAL0(Long_1,Long_Const,Long_1);
MkEPFVAL0(Long_2,Long_Const,Long_2);
MkEPFVAL0(Long_3,Long_Const,Long_3);
MkEPFVAL0(Long_4,Long_Const,Long_4);
MkEPFVAL0(Long_5,Long_Const,Long_5);
MkEPFVAL0(Long_6,Long_Const,Long_6);
MkEPFVAL0(Long_7,Long_Const,Long_7);
MkEPFVAL0(Long_8,Long_Const,Long_8);
MkEPFVAL0(Long_9,Long_Const,Long_9);


/* Long_Concat: func[x,y: val Long] val Long */

MkIP(Long_Concat(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Long */
{
    struct obj * product;
    struct obj * result;

    product = Long_Mult(opx, Long_10);
    result = Long_Add(opy, product);
    rfree(product);
    return(result);
}

MkFVAL2(Long_Concat);


/* Long_Mod: func[val Long; val Short] val Short */

MkIP(Long_Mod(opx,opy))
struct obj *opx;    /* val Long */
struct obj *opy;    /* val Short */
{
    register long lenx = (long) (opx -> obj_component[0]);
    long divisor = (long) opy;				/* the divisor	    */
    long sx = ((long) opx -> obj_component[lenx]) < 0;	/* => dividend < 0  */
    long sy = (divisor < 0);				/* => divisor < 0   */
    struct obj * quotient = (struct obj *) 0;		/* result	    */
 
    if (sx) { opx = Long_Neg(opx); }	/* make dividend positive */
    if (sy) { divisor = -divisor; }	/* make divisor positive  */

    if (divisor == 0) {
	russell_error("Long$% : division by 0\n");
    }

    quotient = Long_cp1(opx, 1);

    /* Compute remainder, filling in quotient as (useless) byproduct */
	remainder = 
	  BnnDivideDigit(&(quotient->obj_component[1]),
			 &(quotient->obj_component[0]), lenx+1, divisor);
	rfree(quotient);

    /* Free negated dividend and negate remainder if appropriate*/
      if (sx) {
	rfree(opx);
	remainder = -remainder;
      }
        
    return((struct obj *) remainder);
}

MkFVAL2(Long_Mod);


/* Long_Put: func[ val Long ] val Long                         */

MkIP(Long_Put(n))
struct obj *n;
{
    fputs((char *)Long_Puts(n), stdout);
    return(n);
}

MkFVAL1(Long_Put);


/* Long_Puts: func[ val Long ] val ChStr                         */

MkIP(Long_Puts(n))
struct obj *n;
{
#   define DECIMAL_EXPANSION 3  /* length increase as result of conv. */
#   define CREEP 4		/* repeated division may cause quotient */
				/* to creep forward in buffer by at     */
				/* most length/CREEP		        */
    register long i,j;
    register long remainder;
    char * result = (char *) 0;  /* conversion buffer */
				 /* should be found by garbage collector */
    char * bufptr;
    long t;
    struct obj * tmp;        /* number still to be converted  */
    long tmp_len;	     /* length of the number in tmp   */
    long tmp_pos;	     /* pos of first digit in tmp     */
    struct obj * tmp2;
    long divisor = 1000000000;

    /* allocate temporary space for conversion, and for result */
        tmp_len = n -> obj_component[0];
	result = (char *)ralloc(DECIMAL_EXPANSION * tmp_len + 1);

    /* add sign to buffer, tmp := abs(n) */
	if ( ((long) (n -> obj_component[tmp_len])) < 0 ) {
	    *result = '-';
	    bufptr = result + 1;
            tmp2 = Long_Neg(n);
	    tmp_len = tmp2 -> obj_component[0] + 1;
	    tmp = Long_cp1(tmp2, tmp_len/CREEP + 2);
	    rfree(tmp2);
        } else {
	    bufptr = result;
	    tmp_len++;  /* One extra digit for BnnDivideDigit */
            tmp = Long_cp1(n, tmp_len/CREEP + 2);
        }
	tmp_pos = 0;

    /* Put converted form (in reverse order) in new space     */
    /* For efficiency reasons this is done 9 digits at a time */
	for (i = 0; ;) {
#		ifdef DEBUG
		    if(BnGetDigit(tmp, tmp_pos + tmp_len - 1) != 0) {
			fail("Long$Puts invariant failed\n");
		    }
#		endif
		remainder = 
		  BnnDivideDigit(&(tmp -> obj_component[tmp_pos+1]),
				 &(tmp -> obj_component[tmp_pos]),
				 tmp_len, divisor);
		/* The tmp_len-1 st digit should always be 0 */
		/* Make sure that there is never more than one leading 0 */
		if (BnGetDigit(tmp, tmp_pos  + tmp_len - 1) == 0) {
		    tmp_len--;
		}
		tmp_pos++;
            if (tmp_len == 1 /* quotient was 0 */) {
                while(remainder != 0) {
                  bufptr[i++] = remainder%10 + '0';
                  remainder /= 10;
                }
                break;
            } else {
		for (j = 0; j < 9; j++) {
                  bufptr[i++] = remainder%10 + '0';
                  remainder /= 10;
                }
            }
	}
	bufptr[i] = '\0'; 
	if (i == 0) return((struct obj *) "0");
    /* Reverse buffer contents */
	j = 0; i--;
	/* j points to first digit, i to last */
	while (i > j) {
	    t = bufptr[j];
	    bufptr[j] = bufptr[i];
	    bufptr[i] = t;
	    j++; i--;
	}

    rfree(tmp);
    return((struct obj *) result);
}

MkFVAL1(Long_Puts);


/* Long_In: func[val Short] val Long */

MkIP(Long_In(x))
struct obj * x;
{
register struct obj *op;    /* var Long */

    op = ralloc(2);
    op->obj_component[0] = 1;  /* length */
    op->obj_component[1] = (long) x;  /* value */
    return(op);
}

MkFVAL1(Long_In);   


/* Long_Odd: func[val Long] val Boolean */
MkIP(Long_Odd(x))
struct obj * x;
{
    return((struct obj *)((x -> obj_component[1]) & 1));
}

MkFVAL1(Long_Odd);


/* Long_Out: func[val Long] val Short */

MkIP(Long_Out(x))
struct obj * x;
{
    register long val = (long) (x -> obj_component[1]);

    if(x->obj_component[0] != 1) {
	MSG("Long$Out: Argument too big:");
        Long_Put(x);
        MSG("");
        FLUSH_MSG;
        print_tr_stack();
        ABORT("Long$Out: Argument too big");
    }
    return((struct obj *) (long) val);
}

MkFVAL1(Long_Out);

/*  Long - the type value */

MkTVAL(Long) = {
    &FVAL(Long_0),
    &FVAL(Long_1),
    &FVAL(Long_2),
    &FVAL(Long_3),
    &FVAL(Long_4),
    &FVAL(Long_5),
    &FVAL(Long_6),
    &FVAL(Long_7),
    &FVAL(Long_8),
    &FVAL(Long_9),

    &FVAL(Long_Mod),
    &FVAL(Long_LMod),
    &FVAL(Long_Mult),
    &FVAL(Long_Add),
    &FVAL(Long_Neg),
    &FVAL(Long_Sub),
    &FVAL(Long_Div),
    &FVAL(Long_LDiv),
    &FVAL(Long_Assign),
    &FVAL(Long_Lt),
    &FVAL(Long_Le),
    &FVAL(Long_Ne),
    &FVAL(Long_Eq),
    &FVAL(Long_Gt),
    &FVAL(Long_Ge),
    &FVAL(Long_In),
    &FVAL(Long_New),
    &FVAL(Long_init_New),
    &FVAL(Long_Out),
    &FVAL(Long_ValueOf),
    &FVAL(Long_Concat),
    &FVAL(Long_Nbits),
    &FVAL(Long_Nwords),
    &FVAL(Long_Odd),
    &FVAL(Long_Put),
    &FVAL(Long_Puts),
    &FVAL(Long_Shift),
};
