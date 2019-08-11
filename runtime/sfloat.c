#define DEBUG
/**/
/* Single precision floating point data type.
/* This assumes IEEE standard 32 bit floating point, with Motorola
/* style byte numbering.
/* Values are represented directly as 32 bit values.
/* Variables are represented as for most data types.
/**/

# define SFLOATSZ 1

#include <math.h>

#include "types.h"

# ifdef M68K
#   define M68881CODE
# endif

/* SFloat_New: func[] var SFloat */

MkIP(SFloat_New())
{
register struct obj *op;    /* var Float */

    op = ralloc(SFLOATSZ);
    op->obj_component[0] = 0;
    return(op);
}

MkFVAL0(SFloat_New);


/* SFloat_init_New: func[val SFloat] var SFloat */

MkIP(SFloat_init_New(x))
struct obj * x;
{
register struct obj *op;    /* var Float */

    op = ralloc(SFLOATSZ);
    op->obj_component[0] = (word) x;
    return(op);
}

MkFVAL1(SFloat_init_New);


/* SFloat_Assign: func[var SFloat; val SFloat] val SFloat */

MkIP(SFloat_Assign(lop,rop))
struct obj *lop;    /* var Short */
struct obj *rop;    /* val Short */
{
    lop->obj_component[0] = (word) rop;
    return(rop);
}

MkFVAL2(SFloat_Assign);


/* SFloat_ValueOf: func[var SFloat] val SFloat */

MkIP(SFloat_ValueOf(op))
struct obj *op;    /* var Short */
{
    return((struct obj *)(op -> obj_component[0]));
}

MkFVAL1(SFloat_ValueOf);


/* Union type to allow treatment of a floating point value as an integer */
/* Note that SFloat values must be returned as integers.  Otherwise C    */
/* returns a double precision value, which has a completley different    */
/* format.  This is probably not legal C, but ...                        */
typedef union {
    float   fl_as_float;
    word    fl_as_int;
} fi;


/* SFloat_Add: func[x,y: val SFloat] val SFloat */

# ifdef M68881CODE
struct obj * SFloat_Add();

    asm(".globl _SFloat_Add");
    asm("_SFloat_Add:");
    asm("fmoves sp@(4),fp0");
    asm("fadds  sp@(8),fp0");
    asm("fmoves fp0,d0");
    asm("rts");
# else
MkIP(SFloat_Add(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2, result;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    result.fl_as_float = arg1.fl_as_float + arg2.fl_as_float;
    return((struct obj *)(result.fl_as_int));
}
# endif

MkFVAL2(SFloat_Add);


/* SFloat_Sub: func[x,y: val SFloat] val SFloat */

# ifdef M68881CODE
struct obj * SFloat_Sub();
    asm(".globl _SFloat_Sub");
    asm("_SFloat_Sub:");
    asm("fmoves sp@(4),fp0");
    asm("fsubs  sp@(8),fp0");
    asm("fmoves fp0,d0");
    asm("rts");
# else
MkIP(SFloat_Sub(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2, result;
    
    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    result.fl_as_float = arg1.fl_as_float - arg2.fl_as_float;
    return((struct obj *)(result.fl_as_int));
}
# endif

MkFVAL2(SFloat_Sub);


/* SFloat_Neg: func[val SFloat] val SFloat */

# ifdef M68881CODE
struct obj * SFloat_Neg();
    asm(".globl _SFloat_Neg");
    asm("_SFloat_Neg:");
    asm("fnegs sp@(4),fp0");
    asm("fmoves fp0,d0");
    asm("rts");
# else
MkIP(SFloat_Neg(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg1, result;

    arg1.fl_as_int = (word)opx;
    result.fl_as_float = (- arg1.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}
# endif

MkFVAL1(SFloat_Neg);


/* SFloat_Mult: func[x,y: val SFloat] val SFloat */

# ifdef M68881CODE
struct obj * SFloat_Mult();
    asm(".globl _SFloat_Mult");
    asm("_SFloat_Mult:");
    asm("fmoves sp@(4),fp0");
    asm("fsglmuls  sp@(8),fp0");
    asm("fmoves fp0,d0");
    asm("rts");
# else
MkIP(SFloat_Mult(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2, result;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    result.fl_as_float = arg1.fl_as_float * arg2.fl_as_float;
    return((struct obj *)(result.fl_as_int));
}
# endif

MkFVAL2(SFloat_Mult);


/* SFloat_Div: func[x,y: val SFloat] val SFloat */

# ifdef M68881CODE
struct obj * SFloat_Div();
    asm(".globl _SFloat_Div");
    asm("_SFloat_Div:");
    asm("fmoves sp@(4),fp0");
    asm("fsgldivs  sp@(8),fp0");
    asm("fmoves fp0,d0");
    asm("rts");
# else
MkIP(SFloat_Div(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2, result;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    result.fl_as_float = arg1.fl_as_float / arg2.fl_as_float;
    return((struct obj *)(result.fl_as_int));
}
# endif

MkFVAL2(SFloat_Div);


/* SFloat_Lt: func[x,y: val SFloat] val Bool */

# ifdef M68881CODE
struct obj * SFloat_Lt();
    asm(".globl _SFloat_Lt");
    asm("_SFloat_Lt:");
    asm("moveq  #0,d0");
    asm("fmoves sp@(4),fp0");
    asm("fcmps  sp@(8),fp0");
    asm("fslt d0");
    asm("negb d0");
    asm("rts");
# else
MkIP(SFloat_Lt(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    return( (struct obj *)
	( arg1.fl_as_float < arg2.fl_as_float)
    );
}
# endif

MkFVAL2(SFloat_Lt);


/* SFloat_Le: func[x,y: val SFloat] val Bool */

# ifdef M68881CODE
struct obj * SFloat_Le();
    asm(".globl _SFloat_Le");
    asm("_SFloat_Le:");
    asm("moveq  #0,d0");
    asm("fmoves sp@(4),fp0");
    asm("fcmps  sp@(8),fp0");
    asm("fsle d0");
    asm("negb d0");
    asm("rts");
# else
MkIP(SFloat_Le(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    return( (struct obj *)
	( arg1.fl_as_float <= arg2.fl_as_float)
    );
}
# endif

MkFVAL2(SFloat_Le);


/* SFloat_Eq: func[x,y: val SFloat] val Bool */
/* Here an integer comparison won't quite do */
/* because of signed 0s.                     */

# ifdef M68881CODE
struct obj * SFloat_Eq();
    asm(".globl _SFloat_Eq");
    asm("_SFloat_Eq:");
    asm("moveq  #0,d0");
    asm("fmoves sp@(4),fp0");
    asm("fcmps  sp@(8),fp0");
    asm("fseq d0");
    asm("negb d0");
    asm("rts");
# else
MkIP(SFloat_Eq(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    return( (struct obj *)
	( arg1.fl_as_float == arg2.fl_as_float)
    );
}
# endif

MkFVAL2(SFloat_Eq);


/* SFloat_Ne: func[x,y: val SFloat] val Bool */

# ifdef M68881CODE
struct obj * SFloat_Ne();
    asm(".globl _SFloat_Ne");
    asm("_SFloat_Ne:");
    asm("moveq  #0,d0");
    asm("fmoves sp@(4),fp0");
    asm("fcmps  sp@(8),fp0");
    asm("fsne d0");
    asm("negb d0");
    asm("rts");
# else
MkIP(SFloat_Ne(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    return( (struct obj *)
	( arg1.fl_as_float != arg2.fl_as_float)
    );
}
# endif

MkFVAL2(SFloat_Ne);


/* SFloat_Ge: func[x,y: val SFloat] val Bool */

# ifdef M68881CODE
struct obj * SFloat_Ge();
    asm(".globl _SFloat_Ge");
    asm("_SFloat_Ge:");
    asm("moveq  #0,d0");
    asm("fmoves sp@(4),fp0");
    asm("fcmps  sp@(8),fp0");
    asm("fsge d0");
    asm("negb d0");
    asm("rts");
# else
MkIP(SFloat_Ge(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    return( (struct obj *)
	( arg1.fl_as_float >= arg2.fl_as_float)
    );
}
# endif

MkFVAL2(SFloat_Ge);


/* SFloat_Gt: func[x,y: val SFloat] val Bool */

# ifdef M68881CODE
struct obj * SFloat_Gt();
    asm(".globl _SFloat_Gt");
    asm("_SFloat_Gt:");
    asm("moveq  #0,d0");
    asm("fmoves sp@(4),fp0");
    asm("fcmps  sp@(8),fp0");
    asm("fsgt d0");
    asm("negb d0");
    asm("rts");
# else
MkIP(SFloat_Gt(opx,opy))
struct obj *opx;    /* val SFloat */
struct obj *opy;    /* val SFloat */
{
    fi arg1, arg2;

    arg1.fl_as_int = (word)opx;
    arg2.fl_as_int = (word)opy;
    return( (struct obj *)
	( arg1.fl_as_float > arg2.fl_as_float)
    );
}
# endif

MkFVAL2(SFloat_Gt);


/* SFloat_Dot: func[x,y: val Short; length_y: val Short] val SFloat */
/* Turns a whole number, a fraction part and the length of the      */
/* fraction part, and turns it into an SFloat.                      */

MkIP(SFloat_Dot(opx,opy,ly))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
struct obj *ly;     /* val Short */
{
register int i;
double scale = 0.0;  /* Initialize so g.c. doesn't get confused */
fi result;

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
    result.fl_as_float = ((double) ((long) opx))
			 + (((double) ((long) opy))*scale);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL3(SFloat_Dot);

/* SFloat_In: func[val Short] val SFloat */
/* Convert a Short to an SFloat          */

MkIP(SFloat_In(opx))
struct obj *opx;    /* val Short */
{
    fi result;

    result.fl_as_float = (long)opx;
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_In);

/* SFloat_Out: func[val SFloat] val Short */
/* Convert a SFloat to a Short            */

MkIP(SFloat_Out(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg;

    arg.fl_as_int = (word) opx;

    if (arg.fl_as_float >= 2147483648.0
	|| arg.fl_as_float <= -2147483649.0) {
	russell_error("SFloat too big for conversion to Short\n");
    }
    return((struct obj *)((long) arg.fl_as_float));
}

MkFVAL1(SFloat_Out);


/* SFloat_Put: func[ val SFloat ] val SFloat */

MkIP(SFloat_Put(n))
struct obj *n;
{
    fi arg;

    arg.fl_as_int = (word) n;
    printf("%.6g", arg.fl_as_float);
    return(n);
}

MkFVAL1(SFloat_Put);


/* SFloat_Get: func[ var Void ] val Float */

MkIP(SFloat_Get())
{
    fi result;

    scanf(" %f", result.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL0(SFloat_Get);


/* SFloat_Atan: func[val SFloat] val SFloat */

MkIP(SFloat_Atan(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg, result;

    arg.fl_as_int = (word) opx;
    result.fl_as_float  =  atan (arg.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_Atan);


/* SFloat_Cos: func[val SFloat] val SFloat */

MkIP(SFloat_Cos(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg, result;

    arg.fl_as_int = (word) opx;
    result.fl_as_float  =  cos(arg.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_Cos);


/* SFloat_Sin: func[val SFloat] val SFloat */

MkIP(SFloat_Sin(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg, result;

    arg.fl_as_int = (word) opx;
    result.fl_as_float  =  sin(arg.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_Sin);


/* SFloat_Sqrt: func[val SFloat] val SFloat */

MkIP(SFloat_Sqrt(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg, result;

    arg.fl_as_int = (word) opx;
    if (arg.fl_as_float < 0.0) {
	russell_error("Square root of negative number\n");
    }
    result.fl_as_float  =  sqrt(arg.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_Sqrt);


/* SFloat_Exp: func[val SFloat] val SFloat */

MkIP(SFloat_Exp(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg, result;

    arg.fl_as_int = (word) opx;
    result.fl_as_float  =  exp(arg.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_Exp);


/* SFloat_Ln: func[val SFloat] val SFloat */

MkIP(SFloat_Ln(opx))
struct obj *opx;    /* val SFloat */
{
    fi arg, result;

    arg.fl_as_int = (word) opx;
    if (arg.fl_as_float <= 0.0) {
	russell_error("Logarithm of nonpositive number\n");
    }
    result.fl_as_float  =  log(arg.fl_as_float);
    return((struct obj *)(result.fl_as_int));
}

MkFVAL1(SFloat_Ln);


/* SFloat_Exponent: func[val SFloat] val Short */

MkIP(SFloat_Exponent(opx))
struct obj *opx;    /* val SFloat */
{
    long arg = (long) opx;

    /* Assumes IEEE single precision */
	return((struct obj *) (((arg & 0x7f800000) >> 23) - 126));
}

MkFVAL1(SFloat_Exponent);


/* SFloat_to_Float: func[val SFloat] val Float */
/* Convert to double precision.                */
MkIP(SFloat_to_Float(opx))
struct obj * opx;
{
    fi arg1;
    register struct obj *op;    /* val Float */
#   define FLOATSZ 2

    op = ralloc(FLOATSZ);

    arg1.fl_as_int = (long)opx;
    *((double *)op) = arg1.fl_as_float;
    return(op);
}

MkFVAL1(SFloat_to_Float);


/* SFloat_Shift: func[val SFloat; val Short] val SFloat */

MkIP(SFloat_Shift(opx,opy))
struct obj *opx;    /* val Float */
struct obj *opy;    /* val Short */
{
short biased_exp;
long new_exp;
fi result;

    /* Assumes IEEE single precision floating point */
    biased_exp = (((long)(opx)) & 0x7f800000) >> 23;

    new_exp = biased_exp + ((long) opy);
    if (new_exp >= 255) {
	russell_error("Floating point overflow in shift\n");
    }
    if (new_exp <= 0) {
	/* Underflow */
	    /* Could be smarter */
	result.fl_as_float = 0.0;
    } else {
	result.fl_as_int =  (word)opx;
	/* Clear exponent field */
	    result.fl_as_int &= 0x807fffff;
	/* Put in new exponent */
	    result.fl_as_int |= (new_exp << 23);
    }
    return((struct obj *)(result.fl_as_int));
}

MkFVAL2(SFloat_Shift);

		      
/*  SFloat_Puts: func[val SFloat] val ChStr */
 
MkIP(SFloat_Puts(opx))
struct obj *opx;    /* val SFloat */
{
register struct obj *op;    /* val ChStr */
register int lw;
int lgth;
char buf[40];
fi arg;

    arg.fl_as_int = (word) opx;
    sprintf(buf, "%.6g", arg.fl_as_float);
    lgth = strlen(buf)+1;
    lw = (lgth + (sizeof (word)) - 1) / (sizeof (word));
 
    op = ralloc(lw);

    strcpy((char *)(op -> obj_component), buf);

    return(op);
}

MkFVAL1(SFloat_Puts);


/* SFloat_to_Long: func[val SFloat] val Long */
/* Hint: Read the description of the IEEE single precision */
/* floating point format                                   */
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

MkIP(SFloat_to_Long(x))
struct obj * x;
{
register int exponent = ((int) (SFloat_Exponent(x)));
long tmp_long[2];/* long version of x * 2**(23-exponent) */
			/* i.e. x with fraction interpreted as  */
			/* integer                              */
long shift_count_tmp[2];
			/* val Long representation of shift count */
register unsigned long tmp;

    /* Set up tmp_long */
	tmp_long[0] = 1; /* size */
	tmp = ((unsigned long) x);
	tmp_long[1] = (tmp & 0x7fffff) | 0x800000;
			/* msbs, exponent removed, implied bit added */

    /* Adjust for sign */
	if (tmp & 0x80000000) {
	    tmp_long[1] = -tmp_long[1];
	}

    /* Build Long representation of shift count */
	shift_count_tmp[0] = 1;
	shift_count_tmp[1] = exponent - 24;

    /* Shift to adjust for correct exponent.  This also results */
    /* in the result getting properly allocated on the heap     */
	return(Long_Shift(tmp_long, shift_count_tmp));

}

MkFVAL1(SFloat_to_Long);


/*  SFloat - the type value */

MkTVAL(SFloat) = {
    &FVAL(SFloat_Mult),
    &FVAL(SFloat_Add),
    &FVAL(SFloat_Neg),
    &FVAL(SFloat_Sub),
    &FVAL(SFloat_Dot),
    &FVAL(SFloat_Div),
    &FVAL(SFloat_Assign),
    &FVAL(SFloat_Lt),
    &FVAL(SFloat_Le),
    &FVAL(SFloat_Ne),
    &FVAL(SFloat_Eq),
    &FVAL(SFloat_Gt),
    &FVAL(SFloat_Ge),
    &FVAL(SFloat_In),
    &FVAL(SFloat_New),
    &FVAL(SFloat_init_New),
    &FVAL(SFloat_Out),
    &FVAL(SFloat_ValueOf),
    &FVAL(SFloat_Atan),
    &FVAL(SFloat_Cos),
    &FVAL(SFloat_Exp),
    &FVAL(SFloat_Exponent),
    &FVAL(SFloat_Get),
    &FVAL(SFloat_Ln),
    &FVAL(SFloat_Put),
    &FVAL(SFloat_Puts),
    &FVAL(SFloat_Shift),
    &FVAL(SFloat_Sin),
    &FVAL(SFloat_Sqrt),
    &FVAL(SFloat_to_Float),
    &FVAL(SFloat_to_Long),
};
