#define DEBUG
/**/
/* Floating point data type.
/* This assumes IEEE standard 64 bit floating point, with Motorola
/* style byte numbering.
/* Values are represented by pointers to atomic objects 2 longwords long.
/* Variables are represented in the same way.
/* The ValueOf operation has to allocate a new object, but it is usually
/* possible to optimize away the ValueOf.
/**/

#include <math.h>

#include "types.h"

#define FLOATSZ 2

/* Float_New: func[] var Float */

MkIP(Float_New())
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    op->obj_component[0] = 0;
    op->obj_component[1] = 0;
    return(op);
}

MkFVAL0(Float_New);


/* Float_init_New: func[val Float] var Float */

MkIP(Float_init_New(x))
struct obj * x;
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    op->obj_component[0] = x -> obj_component[0];
    op->obj_component[1] = x -> obj_component[1];
    return(op);
}

MkFVAL1(Float_init_New);


/* Float_Assign: func[var Float; val Float] val Float */

MkIP(Float_Assign(lop,rop))
struct obj *lop;    /* var Short */
struct obj *rop;    /* val Short */
{
    lop->obj_component[0] = rop->obj_component[0];
    lop->obj_component[1] = rop->obj_component[1];
    return(rop);
}

MkFVAL2(Float_Assign);


/* Float_ValueOf: func[var Float] val Float */

MkIP(Float_ValueOf(aop))
struct obj *aop;    /* var Short */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    op -> obj_component[0] = aop -> obj_component[0];
    op -> obj_component[1] = aop -> obj_component[1];
    return(op);
}

MkFVAL1(Float_ValueOf);


/* Float_Add: func[x,y: val Float] val Float */

MkIP(Float_Add(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op = (* (double *)opx) + (* (double *)opy);
    return(op);
}

MkFVAL2(Float_Add);


/* Float_Sub: func[x,y: val Float] val Float */

MkIP(Float_Sub(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op = (* (double *)opx) - (* (double *)opy);
    return(op);
}

MkFVAL2(Float_Sub);


/* Float_Neg: func[val Float] val Float */

MkIP(Float_Neg(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op =  - (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Neg);


/* Float_Mult: func[x,y: val Float] val Float */

MkIP(Float_Mult(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op = (* (double *)opx) * (* (double *)opy);
    return(op);
}

MkFVAL2(Float_Mult);



/* Float_Div: func[x,y: val Float] val Float */

MkIP(Float_Div(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op = (* (double *)opx) / (* (double *)opy);
    return(op);
}

MkFVAL2(Float_Div);


/* Float_Lt: func[x,y: val Float] val Bool */

MkIP(Float_Lt(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
    return( (struct obj *)
	( (*(double *)opx) < (*(double *)opy) )
    );
}

MkFVAL2(Float_Lt);


/* Float_Le: func[x,y: val Float] val Bool */

MkIP(Float_Le(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
    return( (struct obj *)
	( (* (double *)opx) <= (* (double *)opy) )
    );
}

MkFVAL2(Float_Le);


/* Float_Eq: func[x,y: val Float] val Bool */

MkIP(Float_Eq(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
    return( (struct obj *)
	( (* (double *)opx) == (* (double *)opy) )
    );
}

MkFVAL2(Float_Eq);


/* Float_Ne: func[x,y: val Float] val Bool */

MkIP(Float_Ne(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
    return( (struct obj *)
	( (* (double *)opx) != (* (double *)opy) )
    );
}

MkFVAL2(Float_Ne);


/* Float_Ge: func[x,y: val Float] val Bool */

MkIP(Float_Ge(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
    return( (struct obj *)
	( (*(double *)opx) >= (* (double *)opy) )
    );
}

MkFVAL2(Float_Ge);


/* Float_Gt: func[x,y: val Float] val Bool */

MkIP(Float_Gt(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Float */
{
    return( (struct obj *)
	( (* (double *)opx) > (* (double *)opy) )
    );
}

MkFVAL2(Float_Gt);


/* Float_Dot: func[x,y: val Short; length_y: val Short] val Float */
/* Turns a whole number, a fraction part and the length of the    */
/* fraction part, and turns it into a Float.			  */

MkIP(Float_Dot(opx,opy,ly))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
struct obj *ly;     /* val Short */
{
struct obj *op;    /* var Float */
register struct obj **opp;
register int i;
double scale = 0.0;  /* Initialize so g.c. doesn't get confused */

    op = ralloc(FLOATSZ);
    switch((int) ly) {
        case 0:
            scale = 1.0;
            break;
        case 1:
            scale = 0.1;
            break;
        case 2:
            scale = 0.01;
            break;
        case 3:
            scale = 0.001;
            break;
        case 4:
            scale = 0.0001;
            break;
        default:
	    scale = 1.0;
	    for (i = 0; i < (int) ly; i++) { scale /= 10.0; }
    }
    * (double *) op = ((double) ((long) opx))
		      + (((double) ((long) opy))*scale);
    return(op);
}

MkFVAL3(Float_Dot);

/* Float_In: func[val Short] val Float */
/* Convert a Short to a Float          */

MkIP(Float_In(opx))
struct obj *opx;    /* val Short */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    * (double *) op = ((double) ((long) opx));
    return(op);
}

MkFVAL1(Float_In);

/* Float_Out: func[val Float] val Short */
/* Convert a Float to a Short           */

MkIP(Float_Out(opx))
struct obj *opx;    /* val Float */
{
register double x = * (double *) opx;

    if (x >= 2147483648.0 || x <= -2147483649.0) {
        russell_error("Float too big for conversion to Short\n");
    }
    return((struct obj *)((long) x));
}

MkFVAL1(Float_Out);


/* Float_Put: func[ val Float ] val Float */

MkIP(Float_Put(n))
struct obj *n;
{
    printf("%.12g",* (double *)n);
    return(n);
}

MkFVAL1(Float_Put);


/* Float_Get: func[ var Void ] val Float */

MkIP(Float_Get())
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    scanf(" %F", (double *)op);
    return(op);
}

MkFVAL0(Float_Get);


/* Float_Atan: func[val Float] val Float */

MkIP(Float_Atan(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op =  atan (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Atan);


/* Float_Cos: func[val Float] val Float */

MkIP(Float_Cos(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op =  cos (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Cos);


/* Float_Sin: func[val Float] val Float */

MkIP(Float_Sin(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op =  sin (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Sin);


/* Float_Sqrt: func[val Float] val Float */

MkIP(Float_Sqrt(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    if ((* (double *)opx) < 0.0) {
	russell_error("Square root of negative number\n");
    }
    *(double *)op =  sqrt (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Sqrt);


/* Float_Exp: func[val Float] val Float */

MkIP(Float_Exp(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    *(double *)op =  exp (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Exp);


/* Float_Ln: func[val Float] val Float */

MkIP(Float_Ln(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* var Float */

    op = ralloc(FLOATSZ);
    if ((* (double *)opx) < 0.0) {
	russell_error("Logarithm of negative number\n");
    }
    *(double *)op =  log (* (double *)opx);
    return(op);
}

MkFVAL1(Float_Ln);

/* Float_to_SFloat: func[val Float] val SFloat */
/* Convert to single precision.                */
typedef union {
    float   fl_as_float;
    word    fl_as_int;
} fi;

MkIP(Float_to_SFloat(opx))
struct obj * opx;   /* val Float */
{
    fi result;

    result.fl_as_float = *(double *)opx;
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(Float_to_SFloat);


/* Float_Exponent: func[val Float] val Short */

MkIP(Float_Exponent(opx))
struct obj *opx;    /* val Float */
{
    short msw = * (short *) opx;

#   ifdef VAX  /* VAX D floating */
	return((struct obj *) (((msw & 0x7f80) >> 7) - 128));
#   else  /* IEEE 64 bit */
	return((struct obj *) (((msw & 0x7ff0) >> 4) - 1022));
#   endif
}

MkFVAL1(Float_Exponent);


/* Float_Shift: func[val Float; val Short] val Float */

MkIP(Float_Shift(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Short */
{
register struct obj *op;    /* var Float */
register struct obj **opp;
short msw;
short biased_exp;
long new_exp;

    msw = * ((short *) opx);
#   ifdef VAX
	biased_exp = (msw & 0x7f80) >> 7;
#   else
	biased_exp = (msw & 0x7ff0) >> 4;
#   endif

    op = ralloc(FLOATSZ);
    new_exp = biased_exp + ((long) opy);
#   ifdef VAX
      if (new_exp >= 256) {
#   else
      if (new_exp >= 2047) {
#   endif
	russell_error("Floating point overflow in shift\n");
    }
#   ifdef VAX
      if (new_exp < 0) {
#   else
      if (new_exp <= 0) {
#   endif
	/* Underflow */
	    /* Could be smarter for IEEE ... */
	*(double *)op = 0.0;
    } else {
	*(double *)op =  * (double *)opx;
	/* Clear exponent field */
#           ifdef VAX
		(* (short *) op) &= 0x807f;
#           else
		(* (short *) op) &= 0x800f;
#           endif
	/* Put in new exponent */
#           ifdef VAX
		(* (short *) op) |= (new_exp << 7);
#           else
		(* (short *) op) |= (new_exp << 4);
#           endif
    }
    return(op);
}

MkFVAL2(Float_Shift);

		      
/*  Float_Puts: func[val Float] val ChStr */
 
MkIP(Float_Puts(opx))
struct obj *opx;    /* val Float */
{
register struct obj *op;    /* val ChStr */
register int lw;
int lgth;
char buf[40];

    sprintf(buf, "%.12g", (* (double *) opx));
    lgth = strlen(buf)+1;
    lw = (lgth + (sizeof (word)) - 1) / (sizeof (word));
 
    op = ralloc(lw);

    strcpy((char *)(op -> obj_component), buf);

    return(op);
}

MkFVAL1(Float_Puts);


/* Float_to_Long: func[val Float] val Long */
/* Hint: Read a description of VAX D floating point format */
/* and/or the IEEE floating point standard                 */
/* before you even try to understand this code.            */
extern struct obj * Long_Neg();
extern struct obj * Long_Shift();

/* Swap the high and low halves of a longword */
# define swap(x) \
    {   \
	register long t = (x) & 0xffff; \
	(x) >>= 16; \
	(x) |= (t << 16); \
    }

MkIP(Float_to_Long(x))
struct obj * x;
{
register int exponent = ((int) (Float_Exponent(x)));
long tmp_long[3];/* long version of x * 2**(52-exponent) */
		 /* (56-exponent for vax)                */
 		 /* i.e. x with fraction interpreted as  */
		 /* integer                              */
long shift_count_tmp[2];
			/* val Long representation of shift count */
register unsigned long tmp;

    /* Set up tmp_long */
	tmp_long[0] = 2; /* size */
	tmp = ((unsigned long *) x)[1];
#       ifdef VAX
	    swap(tmp);    /* Get word order to agree with longs */
#       endif
	tmp_long[1] = tmp;  /* least signif. bits */
	tmp = (* (unsigned long *) x);
#       ifdef VAX
	    swap(tmp);
	    tmp_long[2] = (tmp & 0x7fffff) | 0x800000;
			/* msbs, exponent removed, implied bit added */
#       else /* IEEE */
	    tmp_long[2] = (tmp & 0xfffff) | 0x100000;
			/* msbs, exponent removed, implied bit added */
#       endif

    /* Adjust for sign */
	if (tmp & 0x80000000) {
	    if (tmp_long[1] == 0) {
		/* No need to negate it. */
		tmp_long[2] = -tmp_long[2];
	    } else {
		tmp_long[1] = -tmp_long[1];
		tmp_long[2] = ~tmp_long[2];
	    }
	}

    /* Build Long representation of shift count */
	shift_count_tmp[0] = 1;
#       ifdef VAX
	    shift_count_tmp[1] = exponent - 56;
#       else
	    shift_count_tmp[1] = exponent - 53;
#       endif

    /* Shift to adjust for correct exponent.  This also results */
    /* in the result getting properly allocated on the heap     */
	return(Long_Shift(tmp_long, shift_count_tmp));

}

MkFVAL1(Float_to_Long);


/*  Float - the type value */

MkTVAL(Float) = {
    &FVAL(Float_Mult),
    &FVAL(Float_Add),
    &FVAL(Float_Neg),
    &FVAL(Float_Sub),
    &FVAL(Float_Dot),
    &FVAL(Float_Div),
    &FVAL(Float_Assign),
    &FVAL(Float_Lt),
    &FVAL(Float_Le),
    &FVAL(Float_Ne),
    &FVAL(Float_Eq),
    &FVAL(Float_Gt),
    &FVAL(Float_Ge),
    &FVAL(Float_In),
    &FVAL(Float_New),
    &FVAL(Float_init_New),
    &FVAL(Float_Out),
    &FVAL(Float_ValueOf),
    &FVAL(Float_Atan),
    &FVAL(Float_Cos),
    &FVAL(Float_Exp),
    &FVAL(Float_Exponent),
    &FVAL(Float_Get),
    &FVAL(Float_Ln),
    &FVAL(Float_Put),
    &FVAL(Float_Puts),
    &FVAL(Float_Shift),
    &FVAL(Float_Sin),
    &FVAL(Float_Sqrt),
    &FVAL(Float_to_Long),
    &FVAL(Float_to_SFloat),
};
