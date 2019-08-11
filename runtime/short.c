
/************************************************************************/
/* short (32-bit) integer data type					*/
/*									*/
/************************************************************************/


#include "types.h"

#define SHORTSZ 1

/* Short_New: func[] var Short */

MkIP(Short_New())
{
register struct obj *op;    /* var Short */
    
    op = ralloc(SHORTSZ);
    op->obj_component[0] = 0;
    return(op);
}

MkFVAL0(Short_New);

/* Short_init_New: func[val Short] var Short */

MkIP(Short_init_New(x))
struct obj * x;
{
register struct obj *op;    /* var Short */
    
    op = ralloc(SHORTSZ);
    op->obj_component[0] = (word)x;
    return(op);
}

MkFVAL1(Short_init_New);


/* Short_Assign: func[var Short; val Short] val Short */

MkIP(Short_Assign(lop,rop))
struct obj *lop;    /* var Short */
struct obj *rop;    /* val Short */
{
    lop->obj_component[0] = (word)rop;
    return(rop);
}

MkFVAL2(Short_Assign);


/* Short_AddAssign: func[var Short; val Short] val Short (+=) */

MkIP(Short_AddAssign(lop,rop))
struct obj *lop;    /* var Short */
struct obj *rop;    /* val Short */
{
    lop->obj_component[0] =
	 ((long) (lop -> obj_component[0])) + (long)rop;
    return(rop);
}

MkFVAL2(Short_AddAssign);


/* Short_SubAssign: func[var Short; val Short] val Short (-=) */

MkIP(Short_SubAssign(lop,rop))
struct obj *lop;    /* var Short */
struct obj *rop;    /* val Short */
{
    lop->obj_component[0] =
	 ((long) (lop -> obj_component[0])) - (long)rop;
    return(rop);
}

MkFVAL2(Short_SubAssign);


/* Short_ValueOf: func[var Short] val Short */

MkIP(Short_ValueOf(aop))
struct obj *aop;    /* var Short */
{
    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(Short_ValueOf);


/* Short_Add: func[x,y: val Short] val Short */

MkIP(Short_Add(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long sum;

    sum = ( ((long)opx) + ((long)opy) );
    return( (struct obj *)(sum) );
}

MkFVAL2(Short_Add);


/* Short_Sub: func[x,y: val Short] val Short */

MkIP(Short_Sub(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long diff;

    diff = ( ((long)opx) - ((long)opy) );
    return( (struct obj *)(diff) );
}

MkFVAL2(Short_Sub);


/* Short_Neg: func[val Short] val Short   (unary -) */

MkIP(Short_Neg(op))
struct obj *op;    /* val Short */
{
long result;

    result =  - ((long)op);
    return( (struct obj *)(result) );
}

MkFVAL1(Short_Neg);


/* Short_Mult: func[x,y: val Short] val Short */

MkIP(Short_Mult(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long prod;

    prod = ( ((long)opx) * ((long)opy) );
    return( (struct obj *)(prod) );
}

MkFVAL2(Short_Mult);


/* Short_Div: func[x,y: val Short] val Short */

MkIP(Short_Div(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long quot;

    quot = ( ((long)opx) / ((long)opy) );
    return( (struct obj *)(quot) );
}

MkFVAL2(Short_Div);


/* Short_Mod: func[x,y: val Short] val Short */

MkIP(Short_Mod(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long result;

    result = ( ((long)opx) % ((long)opy) );
    return( (struct obj *)(result) );
}

MkFVAL2(Short_Mod);


/* Short_Exp: func[x,y: val Short] val Short */

MkIP(Short_Exp(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long result = 1;
long exp = (long)opy;

    if (exp < 0) {
        return(0);
    } else {
        while (exp > 0) {
	    result *= (long)opx;
            exp--;
        }
    }
    return( (struct obj *)(result) );
}

MkFVAL2(Short_Exp);


/* Short_Lt: func[x,y: val Short] val Bool */

MkIP(Short_Lt(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
    return( (struct obj *)
	( ((long)opx) < ((long)opy) )
    );
}

MkFVAL2(Short_Lt);


/* Short_Le: func[x,y: val Short] val Bool */

MkIP(Short_Le(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
    return( (struct obj *)
	( ((long)opx) <= ((long)opy) )
    );
}

MkFVAL2(Short_Le);


/* Short_Eq: func[x,y: val Short] val Bool */

MkIP(Short_Eq(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
    return( (struct obj *)
	( ((long)opx) == ((long)opy) )
    );
}

MkFVAL2(Short_Eq);


/* Short_Ne: func[x,y: val Short] val Bool */

MkIP(Short_Ne(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
    return( (struct obj *)
	( ((long)opx) != ((long)opy) )
    );
}

MkFVAL2(Short_Ne);


/* Short_Ge: func[x,y: val Short] val Bool */

MkIP(Short_Ge(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
    return( (struct obj *)
	( ((long)opx) >= ((long)opy) )
    );
}

MkFVAL2(Short_Ge);


/* Short_Gt: func[x,y: val Short] val Bool */

MkIP(Short_Gt(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
    return( (struct obj *)
	( ((long)opx) > ((long)opy) )
    );
}

MkFVAL2(Short_Gt);

/*  Short_0 thru Short_9: func[]: val Short */
/*  Value is encoded in ep                  */

MkIP(Short_Const(A))
struct ar * A;
{
  /*
   * this simply moves the first word of the activation record
   * into the function result return register
   */
    return((struct obj *)(A -> ar_static_link));
}

MkEPFVAL0(Short_0,Short_Const,0);
MkEPFVAL0(Short_1,Short_Const,1);
MkEPFVAL0(Short_2,Short_Const,2);
MkEPFVAL0(Short_3,Short_Const,3);
MkEPFVAL0(Short_4,Short_Const,4);
MkEPFVAL0(Short_5,Short_Const,5);
MkEPFVAL0(Short_6,Short_Const,6);
MkEPFVAL0(Short_7,Short_Const,7);
MkEPFVAL0(Short_8,Short_Const,8);
MkEPFVAL0(Short_9,Short_Const,9);


/* Short_Concat: func[x,y: val Short] val Short */

MkIP(Short_Concat(opx,opy))
struct obj *opx;    /* val Short */
struct obj *opy;    /* val Short */
{
long sum;

    sum = ( (((long)opx)*10) + ((long)opy) );
    return( (struct obj *)(sum) );

}

MkFVAL2(Short_Concat);


/* Short_Shift: func[op,nbits: val Short] val Short */

MkIP(Short_Shift(op,nbits))
struct obj *op;    /* val Short */
struct obj *nbits;    /* val Short */
{
long result;

    if (((long)nbits) >= 0) {
	result = ((long) op) << ((long) nbits);
    } else {
	result = ((long) op) >> (-((long) nbits));
    }
    return( (struct obj *)(result) );
}

MkFVAL2(Short_Shift);


/* Short_Put: func[ val Short ] val Short */

MkIP(Short_Put(n))
struct obj *n;
{
    printf("%d",(long)n);
    return(n);
}


MkFVAL1(Short_Put);

/* Short_Puts: func[ val Short ] val ChStr */
# define SHORTPRINTSZ 4

MkIP(Short_Puts(n))
struct obj *n;
{
register struct obj *op;    /* var Short */
    
    op = ralloc(SHORTPRINTSZ);
    sprintf((char *)op,"%ld",(long)n);
    return(op);
}

MkFVAL1(Short_Puts);

/* Short_Get: func[ var Void ] val Short */

MkIP(Short_Get())
{
long n;

    if (scanf(" %d",&n) != 1) {
       ERRMSG("Short$get: Bad input character");
       print_tr_stack();
       ABORT("Short$get: Bad input character");
    };
    return( (struct obj *)(n) );
}

MkFVAL0(Short_Get);

/*  Short - the type value */

MkTVAL(Short) = {
    &FVAL(Short_0),
    &FVAL(Short_1),
    &FVAL(Short_2),
    &FVAL(Short_3),
    &FVAL(Short_4),
    &FVAL(Short_5),
    &FVAL(Short_6),
    &FVAL(Short_7),
    &FVAL(Short_8),
    &FVAL(Short_9),

    &FVAL(Short_Mod),
    &FVAL(Short_Mult),
    &FVAL(Short_Exp),
    &FVAL(Short_Add),
    &FVAL(Short_AddAssign),
    &FVAL(Short_Neg),
    &FVAL(Short_Sub),
    &FVAL(Short_SubAssign),
    &FVAL(Short_Div),
    &FVAL(Short_Assign),
    &FVAL(Short_Lt),
    &FVAL(Short_Le),
    &FVAL(Short_Ne),
    &FVAL(Short_Eq),
    &FVAL(Short_Gt),
    &FVAL(Short_Ge),
    &FVAL(Short_New),
    &FVAL(Short_init_New),
    &FVAL(Short_ValueOf),
    &FVAL(Short_Concat),
    &FVAL(Short_Get),
    &FVAL(Short_Put),
    &FVAL(Short_Puts),
    &FVAL(Short_Shift),
};
