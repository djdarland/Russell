# include "parm.h"

# include "stree/ststructs.mh"

# include "is_local.h"

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

char * getname();

/*
 *  hascomp(type_signature, string_table_index)
 * 
 *  Returns either TRUE or FALSE depending on whether the given type
 * signature has a component with name pointed to by the string
 * table index.
 */

hascomp(sig,ind)
NODE * sig;
int ind;
{
    register NODE * p;
    register boolean is_char;
                            /* ind refers to a single character quoted id */
    char * name;            /* identifier name */
    char character;         /* identifier name (non-quote char) when      */
			    /* is_char is true.                           */
    int i;

#   ifdef DEBUG
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("hascomp: bad type signature\n");
            abort();
        }
#   endif

    name = getname(ind);
    if(is_char = (name[0] == '\'' && name[2] == '\'')) character = name[1];
    maplist(p, sig -> ts_clist, {
	if (p -> kind == TSCOMPONENT) {
	    if (p -> tsc_id -> id_str_table_index == ind) {
		return(TRUE);
	    }
	}
	/* Check whether it is a character with a default signature */
	    if(p -> kind == DEFCHARSIGS && is_char) {

                unsigned word;
		int bitno;
		int wordno;
		unsigned * base = &(p -> dcs_0);
		unsigned * s;

		wordno = ((int) character) / WORDLENGTH;
		word = *(base + wordno);
		bitno = ((int) character) - wordno * WORDLENGTH;

		if ( is_char && (((int) word) << bitno) < 0) {
		    /* type component appears in this node */
		    return(TRUE);
		}
	    }
	IFDEBUG(
	    if(p -> kind != TSCOMPONENT && p -> kind != DEFCHARSIGS) {
		dbgmsg("hascomp: bad tsc\n");
	    }
	)
    });
    /* no matching component */
    return(FALSE);
}

