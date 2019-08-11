# define DEBUG
# define TRACE
# undef TRACE

# ifdef TRACE
#   define IFTRACE(x) x
# else
#   define IFTRACE(x) 
# endif
# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "pass1/stt/sttdefs.h"

# include "sigs.h"

# include "../pass3/is_local.h"

extern unsigned stplinks[];

extern FILE * unparse_file;

/*
 * fixhints(sig, sig2)
 *
 *   Returns a structure which is a copy of the first signature except
 * that code generation information is adjusted to be consistent with that
 * given by the second signature.
 *   Code generation info is not meaningful if the two signatures don't match.
 *   Inline code is preserved only if it is (pointer-) identical in the
 * two signatures.
 *   A NIL value for sig2 is treated as an unknown signature.
 */

NODE * fixhints(sig,sig2)
NODE * sig,*sig2;
{
    boolean mod_flag = FALSE; /* One of the descendants was modified */
    boolean in_line_differs, special_differs, constr_differs;
    boolean slink_known_differs;
    NODE * tmpcopy[MAXFIELDS];/* temporary version of result         */
    NODE ** s;
    NODE * p, *r;
    NODE * ncopy;             /* copy to be returned                 */
    register int i;
    register struct cn * c;
    int j;

    if (sig == ERR_SIG || sig == NIL || sig2 == ERR_SIG ) {
	return(sig);
    }

    switch ( sig -> kind ) {
	case VALSIGNATURE:
	case VARSIGNATURE:
	    return(sig);

	case TSCOMPONENT:
	    if (sig2 -> kind != TSCOMPONENT) return(sig);
	    p = fixhints(sig -> tsc_signature, sig2 -> tsc_signature);
	    if (p != sig -> tsc_signature) {
		ncopy = copynode(sig);
		chgfld(&(ncopy -> tsc_signature), p);
		return(ncopy);
	    } else {
		return(sig);
	    }

	case DEFCHARSIGS:
	    /* Delete the exception information.  */
	    /* This so unlikely to arise ...      */
	    if (sig -> dcs_exceptions != NIL) {
		p = copynode(sig);
		chgfld(&(p -> dcs_exceptions), NIL);
	    }
	    return(p);

	case TYPESIGNATURE:
#           ifdef TRACE
	      printf("fixhints: \n");
	      unparse_file = stdout;
	      unparse(sig);
	      printf("\n");
	      unparse(sig2);
	      printf("refcounts: %d, %d\n", sig -> refcount, sig2 -> refcount);
#           endif
	    if (sig2 == NIL) {
		ncopy = copynode(sig);
				    /* should be locked ... */
		ncopy -> ts_simple_type = FALSE;
		ncopy -> ts_const_code = NIL;
		ncopy -> ts_string_code = NIL;
		ncopy -> ts_element_code = NIL;
		return(ncopy);
	    }
	    if (sig2 -> kind != TYPESIGNATURE) return(sig);
	    p = sig -> ts_clist;
	    r = sig2 -> ts_clist;
            i = 0;
            j = length(p);
            /* get sufficiently large chunk of memory to temporarily copy */
	    /* the list, thus minimizing number of allocations.                 */
                if (j <= MAXFIELDS) {
		    s = tmpcopy;
                } else {
                    s = (NODE **) malloc(j * sizeof(NODE *));
                }
	    map2lists(q, p, t, r, {
		IFTRACE(
		    printf("fixhints: tsc:\n");
		    unparse_file = stdout;
		    unparse(q);
		    printf("\n");
		    unparse(t);
		    printf("refcounts: %d, %d\n", q -> refcount, t -> refcount);
		)
		s[i] = fixhints(q, t);
		IFTRACE(
		    printf("Returned refcount: %d\n", s[i] -> refcount);
		)
                        /* should be locked but ... */
		if(s[i] != q) {
		    IFTRACE(
			printf("Modified\n");
		    )
                    mod_flag = TRUE;
                }
                i++;
	    });
            if (mod_flag) {
	      /* convert the temporary copy in s to a real list structure, */
	      /* put it into a new type signature node, and return it      */
                NODE * result;
                result = emptylist();
                for (i = 0; i < j; i++) {
		    addright(result,s[i]);
#                   ifdef TRACE
			printf("Added %X(%d)\n", s[i], s[i] -> refcount);
			if (s[i] -> kind == TSCOMPONENT) {
			    unparse_file = stdout;
			    unparse(s[i] -> tsc_id);
			    printf(":");
			    unparse(s[i] -> tsc_signature);
			    printf("\n");
			} else {
			    printf("constants");
			}
#                   endif
		}
		if (j > MAXFIELDS) free(s);
		ncopy = copynode(sig);
				 /* again, should be locked ... */
		chgfld(&(ncopy -> ts_clist), result);
	    } else {
		if (j > MAXFIELDS) free(s);
		if (sig -> ts_simple_type && !(sig2 -> ts_simple_type)
		    || sig -> ts_const_code != sig2 -> ts_const_code
		    || sig -> ts_string_code != sig2 -> ts_string_code
		    || sig -> ts_element_code != sig -> ts_element_code) {
		    ncopy = copynode(sig);  /* again .. */
		} else {
		    return(sig);
		}
	    }
#           define make_consistent(field,val) \
	      if (sig -> field != sig2 -> field) {ncopy -> field = val;}
	    make_consistent(ts_simple_type, FALSE);
	    make_consistent(ts_const_code, NIL);
	    make_consistent(ts_string_code, NIL);
	    make_consistent(ts_element_code, NIL);
	    return(ncopy);

	case FUNCSIGNATURE:
	    if (sig2 == NIL) {
		ncopy = copynode(sig);  /* should be locked ... */
		ncopy -> fsig_inline_code = NIL;
		return(ncopy);
	    }
	    if (sig2 -> kind != FUNCSIGNATURE) return(sig);
	    p = fixhints(sig -> fsig_result_sig, sig2 -> fsig_result_sig);
	    in_line_differs =
		(sig -> fsig_inline_code != sig2 -> fsig_inline_code);
	    special_differs =
		(sig -> fsig_special != sig2 -> fsig_special);
	    constr_differs = 
		(sig -> fsig_construction != sig2 -> fsig_construction);
	    slink_known_differs =
		(sig -> fsig_slink_known != sig2 -> fsig_slink_known);
	    if (p != sig -> fsig_result_sig || in_line_differs
		|| special_differs || constr_differs || slink_known_differs) {
		ncopy = copynode(sig);
	    } else {
		ncopy = sig;
	    }
	    if (p != sig -> fsig_result_sig) {
		chgfld(&ncopy -> fsig_result_sig, p);
	    }
	    if (in_line_differs) {
		ncopy -> fsig_inline_code = NIL;
	    }
	    if (special_differs) {
		ncopy -> fsig_special = NOT_SPECIAL;
	    }
	    if (constr_differs) {
		ncopy -> fsig_construction = NIL;
	    }
	    if (slink_known_differs) {
		ncopy -> fsig_slink_known = FALSE;
	    }
	    return(ncopy);

#       ifdef DEBUG
	  default:
	    dbgmsg("fixhints: bad signature");
	    abort(sig,sig2);
#       endif

    }
}

/*
 * Return the integer constant represented by the expression e,
 * if it can be easily determined.  Return UNKNOWN otherwise.
 * Should be perhaps be replaced by is_int_const.
 */
# define UNKNOWN -1

long int_value(e)
NODE * e;
{
    extern NODE * id_Integer;

    switch (e -> kind) {
	case LETTERID:
	    if (e -> id_def_found && e -> sel_type == NIL &&
		e -> id_last_definition -> kind == DECLARATION &&
		e -> id_last_definition -> post_num < e -> post_num) {
		    /* Dont follow recursive declarations ... */
		return(int_value(e -> id_last_definition -> decl_denotation));
	    } else {
		return(UNKNOWN);
	    }

	case UQSTR:
	    {
		NODE * sel_tp = e -> sel_type;
	    
		if (sel_tp -> kind != LETTERID
		    || (!sel_tp -> id_def_found)
		    || (!is_declared_by(sel_tp,
					id_Integer -> id_last_definition))) {
		    return(UNKNOWN);
		} else {
		    /* Integer constant */
		    return(atoi(e -> str_string));
		}
	    }

	case APPLICATION:
	    if (e -> ap_operator -> kind != OPRID
		|| e -> ap_operator -> sel_type == NIL) {
		return(UNKNOWN);
	    } else {
		NODE * sel_tp = e -> ap_operator -> sel_type;
		extern long /* sttrelptr */ indx_plus;
		
		if (sel_tp -> kind != LETTERID
		    || (!sel_tp -> id_def_found)
		    || (!is_declared_by(sel_tp,
					id_Integer -> id_last_definition))) {
		    return(UNKNOWN);
		}
		if (e -> ap_operator -> id_str_table_index == indx_plus) {
		    long arg1 = int_value(first(e -> ap_args));
		    long arg2 = int_value(last(e -> ap_args));
		    if (arg1 != UNKNOWN && arg2 != UNKNOWN) {
			return(arg1 + arg2);
		    } else {
			return(UNKNOWN);
		    }
		} else {
		    /* Should check for other operators here */
		    return(UNKNOWN);
		}
	    }

	default:
	    return(UNKNOWN);
    }
}

/*
 * Set various special fields in the type signature sig to reflect
 * the fact that it represents a type returned by the built-in
 * Array operation.  The arguments to the Array application are
 * size and etype.
 * This assumes argument signatures are known.
 */
void fix_array_sig(sig, size, etype)
NODE * sig;
NODE * size;
NODE * etype;
{
    long size_val = int_value(size);
    NODE * e_sig = sig_structure(etype -> signature);
    NODE * e_V_sig;
    NODE * e_New_sig;
    sttrelptr comp_id;
    extern sttrelptr indx_subscr;
    extern sttrelptr indx_New;
    extern sttrelptr indx_ValueOf;
    extern sttrelptr indx_size;
    boolean std_e_V;        /* Element type has standard V function */
    boolean std_e_New;      /* Element type has standard New        */
    boolean ptr_e_New;      /* Element type has pointer New         */
    extern NODE * id_New;
    extern NODE * id_ValueOf;
    extern FILE * unparse_file;
    int tp;

#   ifdef VERBOSE
	printf("entering fix_array_sig(%X, ...)\nelement sig:\n", sig);
	unparse_file = stdout;
	unparse(e_sig);
	printf("\n");
#   endif
    if (sig == ERR_SIG || e_sig == ERR_SIG
	|| e_sig -> kind != TYPESIGNATURE) return;
    if (size_val == UNKNOWN || size_val > MAX_SP_VAL) {
	size_val = 0;
    }
    e_V_sig = getcomp(e_sig, id_ValueOf, NIL,
		      NIL, NIL, NIL, FALSE);
    if (e_V_sig == ERR_SIG) return;
    e_New_sig = getcomp(e_sig, id_New, NIL,
			NIL, NIL, NIL, FALSE);
    if (e_New_sig == ERR_SIG) return;
    if (e_V_sig == NIL || e_New_sig == NIL) {
	return;
	/* Error may not yet have been caught, but it will be */
    }
    std_e_V = (special_tp(e_V_sig -> fsig_special) == STD_VALUEOF
	       && special_val(e_V_sig -> fsig_special) == 1);
    tp = special_tp(e_New_sig -> fsig_special);
    std_e_New = (tp == STD_NEW
		 && special_val(e_New_sig -> fsig_special) == 1);
    ptr_e_New = (tp == UNION_NEW || tp == PROD_NEW ||
		 (tp == PTR_NEW
		  && special_val(e_New_sig -> fsig_special) == 1));
#   ifdef VERBOSE
	printf("V special = 0x%X, New special = 0x%X\n",
	       e_V_sig -> fsig_special, e_New_sig -> fsig_special);
	printf("std_e_V = %d, std_e_New = %d, ptr_e_New = %d\n",
		std_e_V, std_e_New, ptr_e_New);
#   endif
    maplist(s, sig -> ts_clist, {
	if (s -> kind == TSCOMPONENT) {
	    comp_id = s -> tsc_id -> id_str_table_index;
	    if (comp_id == indx_New) {
		if (std_e_New) {
		    s -> tsc_signature -> fsig_special =
			special(ARRAY_STD_NEW, size_val);
		} else if (ptr_e_New) {
		    s -> tsc_signature -> fsig_special =
			special(ARRAY_PTR_NEW, size_val);
		}
	    } else if (comp_id == indx_ValueOf) {
		if (std_e_V) {
		    s -> tsc_signature -> fsig_special =
			special(ARRAY_VALUEOF, size_val);
		}
	    } else if (comp_id == indx_size) {
		s -> tsc_signature -> fsig_special =
		    special(ARRAY_SIZE, size_val); 
	    } else if (comp_id == indx_subscr) {
		if (s -> tsc_signature -> fsig_result_sig -> kind
		    == VARSIGNATURE) {
		    s -> tsc_signature -> fsig_special =
			special(ARRAY_VAR_SUB, size_val);
		} else {
		    s -> tsc_signature -> fsig_special =
			special(ARRAY_VAL_SUB, size_val);
		}
	    }
	}
    });
}

/* Clear all slink_known fields in the function signature or */
/* recursively in the components of the type signature.      */
void clear_slink_known(sig)
NODE * sig;
{
    NODE * p;
    NODE * q;

    if (sig == ERR_SIG || sig == NIL) {
	return;
    }

    switch ( sig -> kind ) {
	case LETTERID:
	case OPRID:
	    if (p -> id_last_definition == NIL) {
		dbgmsg("clear_slink_known: unresolved id reference\n");
	    }
	    /* We claim that signatures bound to identifiers cant contain  */
	    /* any interesting optimization info.  Thus there's none to be */
	    /* cleared.                                                    */
	    break;

	case VARSIGNATURE:
	case VALSIGNATURE:
	case SIGNATURESIG:
	    break;

	case TYPESIGNATURE:
	    p = sig -> ts_clist;
	    maplist(q, p, {
		if (q -> kind == TSCOMPONENT) {
		    clear_slink_known(q -> tsc_signature);
		}
	    });
	    break;

	case FUNCSIGNATURE:
	    sig -> fsig_slink_known = FALSE;
	    break;

#       ifdef DEBUG
	  default:
	    dbgmsg("clear_slink_known: bad signature");
	    abort(sig);
#       endif

    }
}

/* Return a copy of sig with all fsig_construction fields in the  */
/* signature cleared.                                             */
NODE * clear_construction(sig)
NODE * sig;
{
    NODE * p;
    NODE * q;

    if (sig == ERR_SIG || sig == NIL) {
	return(sig);
    }

    switch ( sig -> kind ) {
	case LETTERID:
	case OPRID:
#           ifdef DEBUG
	      if (p -> id_last_definition == NIL) {
		  dbgmsg("clear_construction: unresolved id reference\n");
	      }
#           endif
	    /* We claim that signatures bound to identifiers cant contain  */
	    /* any interesting optimization info.  Thus there's none to be */
	    /* cleared.                                                    */
	    return(sig);

	case VARSIGNATURE:
	case VALSIGNATURE:
	case SIGNATURESIG:
	    return(sig);

	case TYPESIGNATURE:
	    {
	      boolean modified = FALSE;
	      NODE * new_comp_list = emptylist();
	      NODE * new_comp;

	      maplist(s, sig -> ts_clist, {
		if (s -> kind == TSCOMPONENT) {
		    p = clear_construction(s -> tsc_signature);
		    if (p != s -> tsc_signature) {
			modified = TRUE;
			new_comp = copynode(s);
			chgfld(&(new_comp -> tsc_signature), p);
			addright(new_comp_list, new_comp);
		    } else {
			addright(new_comp_list, s);
		    }
		} else if (s -> kind == DEFCHARSIGS) {
		    if (s -> dcs_exceptions != NIL) {
			/* We punt */
			modified = TRUE;
			new_comp = copynode(s);
			chgfld(&(new_comp -> dcs_exceptions), NIL);
			addright(new_comp_list, new_comp);
		    } else {
			addright(new_comp_list, s);
		    }
		} else {
		    dbgmsg("clear_construction: bad typesignature\n");
		}
	      });
	      if (modified) {
		p = copynode(sig);
		chgfld(&(p -> ts_clist), new_comp_list);
		return(p);
	      } else {
		vfree(new_comp_list);
		return(sig);
	      }
	    }

	case FUNCSIGNATURE:
	    p = clear_construction(sig -> fsig_result_sig);
	    if (p != sig -> fsig_result_sig
		|| sig -> fsig_construction != NIL) {
		q = copynode(sig);
		chgfld (&(q -> fsig_result_sig),p);
		q -> fsig_construction = NIL;
		return(q);
	    } else {
		return(sig);
	    }

#       ifdef DEBUG
	  default:
	    dbgmsg("clear_construction: bad signature");
	    abort(sig);
#       endif

    }
}

