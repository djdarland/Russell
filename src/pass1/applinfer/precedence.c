# include "parm.h"

# include "stree/ststructs.mh"

# include "precedence.h"

extern int yynerrs;
extern int pend[];

precedence(p) /* return the operator precedence associated with */
              /* denotation p.                                  */
NODE * p;
{
    register unsigned stti;  /* string table index of id */
    register int lvl;

	switch (p -> kind) {
		case BLOCKDENOTATION :
            return(p -> bld_precedence);
        case USELIST :
            return(p -> usl_precedence);
        case WORDCAND :
            return(2);
        case WORDCOR :
            return(1);
        case OPRID :
        case LETTERID :
            stti = p -> id_str_table_index;
            if (stti > pend[NLEVELS - 1])
                if((p -> kind) == OPRID)
                    return(3 /* default precedence */);
                else
                    return(INFINITE);
            for (lvl = 0; stti > pend[lvl]; lvl++);
            return(lvl);
        default :
            return(INFINITE);
    }
}
