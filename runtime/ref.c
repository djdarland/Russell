/**/
/* This implements the function
/*    Ref: func[T: type {}] type t { New; :=; V; =;
/*                                   Nil: func[] val t;
/*                                   In: func[var T] val t;
/*                                   Out: func[val t; var Void] var T }
/*  In and Out are implemented as identity functions.
/**/

#include "types.h"

#define REFSZ 1

#define REF_NIL ((struct obj *)(UNINIT))


/* Ref_New: func[] var Ref[T] */

MkIP(Ref_New())
{
register struct obj *op;    /* var  */

    op = ralloc_comp(REFSZ);
    op -> obj_component[0] = (word) REF_NIL;
    return(op);
}

MkFVAL0(Ref_New);


/* Ref_Nil : func [] val Ref[T] */
MkIP(Ref_Nil())
{
    return(REF_NIL);
}

MkFVAL0(Ref_Nil);


/* Ref_Eq: func[x,y: val Ref[T]] val Boolean */
MkIP(Ref_Eq(lop,rop))
struct obj * lop;
struct obj * rop;
{
    return ((struct obj *)(lop == rop));
}

MkFVAL2(Ref_Eq);


/* Ref_Assign: func[var Ref[T]; val Ref[T]] val Ref[T] */

MkIP(Ref_Assign(lop,rop))
struct obj *lop;    /* var Ref[T] */
struct obj *rop;    /* val Ref[T] */
{
    lop->obj_component[0] = ((word)rop);
    return(rop);
}

MkFVAL2(Ref_Assign);


/* Ref_ValueOf: func[var Ref[T]] val Ref[T] */

MkIP(Ref_ValueOf(aop))
struct obj *aop;    /* var Ref[T] */
{
    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(Ref_ValueOf);

           
/* Ref_In: func[var T] val Ref[T] */

MkIP(Ref_In(aop))
struct obj *aop;    /* var T */
{
    return(aop);
}

MkFVAL1(Ref_In);


/* Ref_Out: func[val Ref[T], var Void] var T */

MkIP(Ref_Out(aop, bop))
struct obj *aop;    /* val Ref[T] */
struct obj *bop;    /* var Void   */
{
    return(aop);
}

MkFVAL2(Ref_Out);


MkTVAL(Ref_Type) = {
    &FVAL(Ref_Assign),
    &FVAL(Ref_Eq),
    &FVAL(Ref_In),
    &FVAL(Ref_New),
    &FVAL(Ref_Nil),
    &FVAL(Ref_Out),
    &FVAL(Ref_ValueOf),
    &FVAL(Ref_Out)    /* allow both ^ and Out */
};


/* Ref: func[T:type {}] type ...  */

MkIP(Ref(aop))
struct obj *aop;    /* type {} */
{
    return((struct obj *) Ref_Type);
}

MkFVAL1(Ref);

