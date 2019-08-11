/**/
/* Boolean data type
/**/

#include "types.h"
#include <stdio.h>

#define BOOLSZ 1


/* Bool_New: func[] var Bool */

MkIP(Bool_New())
{
register struct obj *op;    /* var Int */
register struct obj **opp;

    op = ralloc(BOOLSZ);
    op -> obj_component[0] = 0;
    return(op);
}

MkFVAL0(Bool_New);

/* Bool_init_New: func[val Bool] var Bool */

MkIP(Bool_init_New(x))
struct obj * x;
{
register struct obj *op;    /* var Int */
register struct obj **opp;

    op = ralloc(BOOLSZ);
    op->obj_component[0] = (word) x;
    return(op);
}

MkFVAL1(Bool_init_New);


/* Bool_Assign: func[var Bool; val Bool] val Bool */

MkIP(Bool_Assign(lop,rop))
struct obj *lop;    /* var Bool */
struct obj *rop;    /* val Bool */
{
    lop->obj_component[0] = ((word)rop);
    return(rop);
}

MkFVAL2(Bool_Assign);


/* Bool_ValueOf: func[var Bool] val Bool */

MkIP(Bool_ValueOf(aop))
struct obj *aop;    /* var Bool */
{
    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(Bool_ValueOf);


/* Bool_And: func[x,y: val Bool] val Bool */

MkIP(Bool_And(opx,opy))
struct obj *opx;    /* val Bool */
struct obj *opy;    /* val Bool */
{

    return( (struct obj *)
	( ((word)opx) && ((word)opy) )
    );
}

MkFVAL2(Bool_And);


/* Bool_Or: func[x,y: val Bool] val Bool */

MkIP(Bool_Or(opx,opy))
struct obj *opx;    /* val Bool */
struct obj *opy;    /* val Bool */
{

    return( (struct obj *)
	( ((word)opx) || ((word)opy) )
    );
}

MkFVAL2(Bool_Or);


/* Bool_Not: func[val Bool] val Bool */

MkIP(Bool_Not(op))
struct obj *op;     /* val Bool */
{

    return( (struct obj *)
	( !((word)op) )
    );
}

MkFVAL1(Bool_Not);


/* Bool_Eq: func[x,y: val Bool] val Bool */

MkIP(Bool_Eq(opx,opy))
struct obj *opx;    /* val Bool */
struct obj *opy;    /* val Bool */
{
    return( (struct obj *)
	( opx == opy )
    );
}

MkFVAL2(Bool_Eq);


/* Bool_Ne: func[x,y: val Bool] val Bool */

MkIP(Bool_Ne(opx,opy))
struct obj *opx;    /* val Bool */
struct obj *opy;    /* val Bool */
{
    return( (struct obj *)
	( opx != opy )
    );
}

MkFVAL2(Bool_Ne);


/* Bool_Put: func[val Bool] val Bool */
MkIP(Bool_Put(x))
struct obj * x;   /* val Bool */
{
    if ((int) x) {
        fputs("True", stdout);
    } else {
        fputs("False", stdout);
    }
    return(x);
}

MkFVAL1(Bool_Put);

/* Bool_Puts: func[val Bool] val ChStr */
MkIP(Bool_Puts(x))
struct obj * x;   /* val Bool */
{
    if ((int) x) {
        return((struct obj *)"True");
    } else {
        return((struct obj *)"False");
    }
}

MkFVAL1(Bool_Puts);

/*  true,false: func[] val Bool */

MkIP(Bool_True())
{
    return( (struct obj *) 1 );
}

MkIP(Bool_False())
{
    return( (struct obj *) 0 );
}

MkFVAL0(Bool_True);
MkFVAL0(Bool_False);


MkTVAL(Bool) = {
    &FVAL(Bool_Assign),
    &FVAL(Bool_Ne),
    &FVAL(Bool_Eq),
    &FVAL(Bool_False),
    &FVAL(Bool_New),
    &FVAL(Bool_init_New),
    &FVAL(Bool_True),
    &FVAL(Bool_ValueOf),
    &FVAL(Bool_And),
    &FVAL(Bool_Not),
    &FVAL(Bool_Or),
    &FVAL(Bool_Put),
    &FVAL(Bool_Puts),
};
