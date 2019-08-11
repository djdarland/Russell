# include "parm.h"

# include <stdio.h>

# include "ststructs.m"
# include "streedefs.h"
 
main()
{
    int *p,*q;
    p =lock(  mknode(NUMID,1234, NIL)  );
    q = emptylist();
    prtree(q);
    addright(q,p);
    prtree(q);
    addright(q,p);
    prtree(q);
    abort();
    vfree( unlock(p) );
    prtree(q);
    vfree(q);
    
}

/* junk to make it load */

getname() {}

yyerror() { abort(); }

int yynerrs;
int yyvline;
int yydebug = 17;
