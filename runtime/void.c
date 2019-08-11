/*
 * Void data type
 */

#include "types.h"


/*  Null: func[] val Void */

MkIP(Void_Null())
{
    return( (struct obj *) 0 );
}

MkFVAL0(Void_Null);


/*  Builtin Void variables (n.b. there is no New function) */

MkVAR(Void_0,1);


/*  The actual type value */

MkTVAL(Void) = {
    &FVAL(Void_Null),
};
