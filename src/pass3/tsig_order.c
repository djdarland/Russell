# define DEBUG
# include "parm.h"

# include "stree/ststructs.mh"

# include "stree/Array.h"

# include "pass4/sigs.h"

int comp_tsc();             /* comparison routine for sorting         */

void add_dcse();            /* Add to the exception list of the default */
			    /* character signature list                 */

static NODE * type_sig;     /* copy of parameter to tsig_order        */
			    /* used by comp_tsc                       */ 

char * getname();

/*
 *  tsig_order(type_signature)
 *
 *  Rearrange the type_signature in the canonical order given
 * in the WHAT file.
 */

void tsig_order(tsig)
NODE * tsig;
{
    Array * a;  /* array representation of the type signature */
    int a_len;  /* number of pointers in a */
    NODE * constants;   /* pointer to composite DEFCHARSIGS node being built */
    register int i,j;
    unsigned *p, *q;
    NODE * r;

    a = list_to_array(tsig -> ts_clist);
    a_len = a->a_size;


    constants = lock(mknode(DEFCHARSIGS,0,0,0,0));
    /* build constants node */
        for(i = 0; i < a_len; i++) {
	    if (a->a_body[i] -> kind == DEFCHARSIGS) {
		p = &(a->a_body[i] -> dcs_0);
                q = &(constants -> dcs_0);
                for (j = 0; j < NVECTORS; j++) {
                    *q++ |= *p++;
                }
                /* Cancel the node in the array */
		    unlock(a->a_body[i]);
		    a->a_body[i] = NIL;
            } else {
                register NODE * csig;    /* signature of component */

#               ifdef DEBUG
		    if(a->a_body[i] -> kind != TSCOMPONENT) {
                        dbgmsg("tsig_order: bad tsig component\n");
                    }
#               endif
		csig = a->a_body[i] -> tsc_signature;
#               ifdef DEBUG
		    if(csig -> kind != FUNCSIGNATURE &&
		       csig -> kind != TYPESIGNATURE &&
		       csig -> kind != VALSIGNATURE) {
			dbgmsg("tsig_order: bad tsc sig, kind = %s\n",
			       kindname(csig -> kind));
			abort(a->a_body[i],csig);
		    }
#               endif
		if ( is_const(csig, tsig) ) {
		    char * nm 
			 = getname(a->a_body[i] -> tsc_id -> id_str_table_index);

		    if ( nm[0] == '\'' && nm[2] == '\'' ) {
			/* This is a constant */
			unsigned *word;
			int bitno;
			char character = nm[1];
			int wordno = ((int) character) / WORDLENGTH;

			word = (&(constants -> dcs_0) + wordno);
			bitno = ((int) character) - wordno * WORDLENGTH;
			*word |= 1 << (WORDLENGTH - bitno - 1);
			/* cancel array entry */
			    unlock(a->a_body[i]);
			    a->a_body[i] = NIL;
			if (special_tp(csig -> fsig_special) != NOT_SPECIAL
			    || csig -> fsig_inline_code != NIL
			    || csig -> fsig_construction != NIL) {
			    if (constants -> dcs_exceptions == NIL) {
				initfld(&(constants -> dcs_exceptions), emptylist());
			    }
			    add_dcse(constants -> dcs_exceptions, character,
				     csig -> fsig_inline_code,
				     csig -> fsig_special,
				     csig -> fsig_construction);
			}
		    }
		}
	    }
        }

    /* Remove all NIL array entries */
        i = 0;  /* next location to be copied into */
	for(j = 0; j < a_len; j++) {
	    if(a->a_body[j] != NIL) {
		r = a->a_body[j];
		/* Clear old location so reference counts dont get */
		/* messed up.                                      */
		    a->a_body[j] = NIL;
		a->a_body[i++] = r;
	    }
        }
	a_len = i;


    /* Sort the remaining array */
	type_sig = tsig;
	qsort(&a->a_body[0], a_len, (sizeof (NODE *)), comp_tsc);


    /* Put everything back into the type signature node */
	chgfld(&(tsig -> ts_clist), mklist(constants,-1));
        for (i = 0; i < a_len; i++) {
	    addright(tsig -> ts_clist, a->a_body[i]);
	}
	free_array(a);
}


/* compare 2 TSCOMPONENT structures */
int comp_tsc(p, q)
NODE **p, **q;

{
    register int i;
    i =  strcmp(getname((*p) -> tsc_id -> id_str_table_index),
		getname((*q) -> tsc_id -> id_str_table_index));
    if (i == 0)
	return(comp_st((*p) -> tsc_signature, (*q) -> tsc_signature,
		       type_sig, type_sig));
    else
	return(i);
}

/* 
 *  is_const(sig,tsig)
 *  returns TRUE iff sig has the form
 *  func[] val local_type_id and if it is not special.
 *  (It is unsafe to lose special information, e.g. in enum constructions.
 *  Thus we check for its presence.  Special constants are never part
 *  of strings compiled with in-line code.  Thus this shouldn't matter.)
 */

boolean is_const(sig,tsig)
NODE * sig, * tsig;
{

    if(sig == NIL) return(FALSE);
    if(sig -> kind == FUNCSIGNATURE
       && is_empty(sig -> fsig_param_list)
       && sig -> fsig_result_sig != ERR_SIG
       && sig -> fsig_result_sig -> kind == VALSIGNATURE) {
	    NODE * tden = sig -> fsig_result_sig -> val_denotation;

	    if( (tden->kind == LETTERID || tden->kind == OPRID)
		 && (tden -> id_str_table_index == -1
		     || tden -> id_last_definition == tsig)) {
		return(TRUE);
	    }

    }
    if((sig -> kind == LETTERID || sig -> kind == OPRID)
       && sig -> id_last_definition != NIL
       && sig -> id_last_definition -> kind == DECLARATION
       && sig -> id_last_definition -> decl_sig_transp
       && sig -> id_last_definition -> post_num < sig -> post_num) {
	return(is_const(sig -> id_last_definition -> decl_denotation));
    }
    return(FALSE);
}

/* 
 *  Add an exception with the given character, in-line code, special, and
 * construction fields to the character constant eception list l.
 * This is a DESTRUCTIVE operation.
 */
void add_dcse(l, character, in_line, spcl, construction)
NODE *l;
int character;
char *in_line;
int spcl;
NODE *construction;
{
    NODE * new_node = mknode(DCSEXCEPTION);

#   ifdef DEBUG
	if (l -> kind != LISTHEADER
	    || (!is_empty(l) && (first(l)) -> kind != DCSEXCEPTION) ) {
	    dbgmsg("add_dcse: bad exception list\n");
	}
#   endif
    new_node -> dcse_char = character;
    new_node -> dcse_inline = in_line;
    new_node -> dcse_special = spcl;
    new_node -> dcse_construction = construction;
    mapinslist(s, l, {
	if (s == NIL || s -> dcse_char > character) {
	    INSERT(new_node);
	    break;
	} else if (s -> dcse_char == character) {
	    /* Entries are never deleted.  Thus there could still be an */
	    /* obsolete entry for the same character.                   */
	    REPLACE(new_node);
	    break;
	}
    });
}
