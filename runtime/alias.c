#include "types.h"

/**/
/* A primitive Alias function
/**/


/*  Alias: func[x,y:var T; T: type{}] val Bool */

MkIP(Alias(x,y,t))
struct obj *t, *x, *y;
{
    return( (struct obj *)(x == y) );
}

MkFVAL3(Alias);
