# define TRACE_DECL
# undef TRACE_DECL

/* Routines to write out a signature and optimization information */

# ifdef TRACE_DECL
#   define IFTRACE_DECL(x) x
# else
#   define IFTRACE_DECL(x)
# endif

# define EXTERN_LIMIT 5   /* Maximum number of nested extern { ... } 's */

/* This assumes that putw can write a pointer */

# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "pass3/is_local.h"

extern boolean Gflag;

extern FILE * unparse_file;

static NODE * Outer_arg;

struct decl_entry {
    NODE * de_decl;
    int de_number;
    struct decl_entry * de_next;
} *decl_nums;       /* list of declaration nodes and corresponding numbers */
                    /* This is a silly data structure, but the list is     */
                    /* unlikely to have length > 1                         */

static int decl_num = 0;  /* last number assigned to a declaration */

/* Add a new declartion to decl_nums.  Assign it the next available number */
# define add_decl(decl) { \
    struct decl_entry * o = (struct decl_entry *) \
    				malloc(sizeof (struct decl_entry)); \
    o -> de_number = (++decl_num); \
    o -> de_decl = decl; \
    o -> de_next = decl_nums; \
    decl_nums = o; \
}

# define NONE -1

/* Get the number associated with decl.  Return NONE if there isn't any */
static get_decl_num(decl)
NODE * decl;
{
    struct decl_entry *p = decl_nums;

    while (p != NIL ) {
        if (decl -> pre_num == p -> de_decl -> pre_num) return(p -> de_number);
        p = p -> de_next;
    }
    return(NONE);
}

void sig_out1();

/* Write a 0 terminated string onto Soutfile.
 * A NIL pointer is represented as a string consisting of a single FF
 * (delete) character
 */
put_string(Soutfile, s)
FILE * Soutfile;
char * s;
{
    if (s == NIL) {
        putc(0xff, Soutfile);
    } else {
        fputs(s, Soutfile);
    }
    putc(0, Soutfile);
}

/* Identifiers are written out as
 *              kind
 *              representation kind
 *              selection expression (if any)
 *              declaration number (local) or address (global)
 *                                 (not used for selection)
 *              name (0 terminated string, empty if local type id)
 *
 * The following options exist for the representation kind field:
 */
# define LOCALREP  0
# define GLOBALREP 1
# define SELECTREP 2 

/* Write out an identifier */
put_name(Soutfile, p)
register NODE *p;
FILE *Soutfile;
{
    int string_index;
    char * name;
    int rep;
    int dn;

#   ifdef DEBUG
        if(p -> kind != LETTERID && p -> kind != OPRID) {
            dbgmsg("put_name: bad identifier\n");
        }
#   endif
    putw(p -> kind, Soutfile);
    if (p -> sel_type != NIL) {
        rep = SELECTREP;
    } else {
        if (p -> id_last_definition != NIL
            && (dn = get_decl_num(p -> id_last_definition)) != NONE) {
            rep = LOCALREP;
        } else {
            rep = GLOBALREP;
        }
    }
    putw(rep, Soutfile);
#   ifdef TRACE_DECL
	printf("put_name: identifier: %s; index: %d", 
	       (p -> id_str_table_index == -1? "(anon)"
		: getname(p -> id_str_table_index)),
	       p -> id_str_table_index);
#   endif
    switch(rep) {
        case SELECTREP:
            sig_out1(Soutfile, p -> sel_type);
#           ifdef TRACE_DECL
		printf(" (selected)\n");
#           endif
            break;
        case GLOBALREP:
            putw(p -> id_last_definition, Soutfile);
#           ifdef TRACE_DECL
		printf(" (global)\n");
#           endif
            break;
        case LOCALREP:
            putw(dn, Soutfile);
#           ifdef TRACE_DECL
		printf(" (declaration: %d; pre_num: %d)\n", dn,
		       p -> id_last_definition -> pre_num);
#           endif
            break;
    }
    string_index = p -> id_str_table_index;
#   ifdef DEBUG
        if (string_index <= -2) {
            dbgmsg("put_name: funny id name: %X\n", string_index);
        }
#   endif
    if (string_index == -1) {
        /* local type id is represented as empty string */
        putc(0, Soutfile);
    } else {
        put_string(Soutfile, getname(string_index));
    }
}

/* Write a representation of the expression tree headed by p onto Soutfile */
/* This is the same representation used by sig_in.  It is designed to be   */
/* relatively efficient.  Local identifiers are represented by the number  */
/* of their declaration.  Such numbers are assigned in preorder fashion.   */
/* Globally declared identifiers are saved as character strings.           */
void sig_out(Soutfile, p)
register NODE * p;
FILE * Soutfile;
{
    decl_nums = NIL;
    Outer_arg = p;
    sig_out1(Soutfile, p);
}

void sig_out1(Soutfile, p)
register NODE * p;
FILE * Soutfile;
{
register NODE * v;

    if (p == NIL) {
        putw(-1, Soutfile);
        return;
    }

    switch ( p -> kind ) {

        case DECLARATION:
                putw(DECLARATION, Soutfile);
                /* write representation of identifier */
		  put_name(Soutfile, p -> decl_id);
		putw(p -> decl_sig_transp, Soutfile);
                sig_out1(Soutfile, p -> decl_signature);
                sig_out1(Soutfile, p -> decl_denotation);
                break;

        case BLOCKDENOTATION:
                putw(BLOCKDENOTATION, Soutfile);
                maplist(v, p -> bld_declaration_list, {
                    add_decl(v);
		    IFTRACE_DECL(
			printf("Declaration: %s, decl: %d\n",
				getname(v -> decl_id -> id_str_table_index),
				decl_num);
		    )
                });
                putw(length(p -> bld_declaration_list), Soutfile);
                maplist(v, p -> bld_declaration_list, {
                    sig_out1(Soutfile, v);
                });
                putw(length(p -> bld_den_seq), Soutfile);
                maplist(v, p -> bld_den_seq, {
                    sig_out1(Soutfile, v);
                });
		break;

        case APPLICATION:
                putw(APPLICATION, Soutfile);
                sig_out1(Soutfile, p -> ap_operator);
                putw(length(p -> ap_args), Soutfile);
                maplist(v, p -> ap_args, {
                    sig_out1(Soutfile, v);
                });
		break;

        case LOOPDENOTATION:
        case GUARDEDLIST:
                putw(p -> kind, Soutfile);
                putw(length(p -> gl_list), Soutfile);
                maplist(v, p -> gl_list, {
                    sig_out1(Soutfile, v);
                });
		break;

        case GUARDEDELEMENT:
                putw(GUARDEDELEMENT, Soutfile);
                sig_out1(Soutfile, p->ge_guard);
                sig_out1(Soutfile, p->ge_element);
		break;

        case OPRID:
	case LETTERID:
#               ifdef DEBUG
		    if (!p -> id_def_found) {
			dbgmsg("Sig_out: unresolved identifier reference\n");
			abort(p);
		    }
#               endif
		if (p -> sel_type == NIL
		    && p -> id_last_definition != NIL
		    && p -> id_last_definition -> kind == DECLARATION
		    && p -> id_last_definition -> decl_sig_transp) {
#                       ifdef TRACE_DECL
			    printf("Writing out ");
			    unparse_file = stdout;
			    unparse(p -> id_last_definition -> decl_denotation);
			    printf("instead of identifier ");
			    unparse(p);
			    printf("\n");
#                       endif
			sig_out1(Soutfile,
				 p -> id_last_definition -> decl_denotation);
		} else {
		    put_name(Soutfile, p);
		}
                break;

        case FUNCCONSTR:
                putw(FUNCCONSTR, Soutfile);
                sig_out1(Soutfile, p -> signature);
                sig_out1(Soutfile, p -> fc_body);
                break;

        case USELIST:
                putw(USELIST, Soutfile);
                putw(length(p -> usl_type_list), Soutfile);
                maplist(q, p -> usl_type_list, {
                    sig_out1(Soutfile, q);
                });
                putw(length(p -> usl_den_seq), Soutfile);
                maplist(q, p -> usl_den_seq, {
                    sig_out1(Soutfile, q);
                });
		break;

        case MODPRIMARY:
                if (p -> mp_type_modifier == NIL) {
                    /* forgetting is unimportant here */
                        sig_out1(Soutfile, p -> mp_primary);
                } else {
		    add_decl(p);
#                   ifdef TRACE_DECL
			printf("Modified type: decl: %d\n",
				decl_num);
#                   endif
                    putw(MODPRIMARY, Soutfile);
                    sig_out1(Soutfile, p -> mp_primary);
                    sig_out1(Soutfile, p -> mp_type_modifier);
                }
                break;

        case QSTR:
        case UQSTR:
                if (p -> str_expansion == NIL) {
                    sig_out1(Soutfile, expand_str(p));
                } else {
                    sig_out1(Soutfile, p -> str_expansion);
                }
                break;

	case PRODCONSTRUCTION:
        case UNIONCONSTRUCTION:
                add_decl(p);
#               ifdef TRACE_DECL
		    printf("Type construction: decl: %d\n",
			    decl_num);
#               endif
                putw(p -> kind, Soutfile);
                sig_out1(Soutfile, p -> prod_local_type_id);
                putw(length(p -> prod_components), Soutfile);
                maplist(s, p -> prod_components, {
                    sig_out1(Soutfile, s);
                });
		break;

	case WORDELSE:
                putw(WORDELSE, Soutfile);
		break;

        case PARAMETER:
                putw(PARAMETER, Soutfile);
                sig_out1(Soutfile, p -> par_id);
                sig_out1(Soutfile, p -> par_signature);
                break;

        case FUNCSIGNATURE:
                /* Try to fill in in-line code, if not already there */
                    if (p -> fsig_inline_code == NIL
                        && p -> fsig_construction != NIL) {
                        p -> fsig_inline_code = p -> fsig_construction
                                                  -> signature
                                                  -> fsig_inline_code;
                    }
                putw(FUNCSIGNATURE, Soutfile);
		putw(p -> fsig_special, Soutfile);
		if (Gflag) {
		    put_RIC(p -> fsig_inline_code, Soutfile);
		} else {
		    put_string(Soutfile, p -> fsig_inline_code);
		}
                /* Add parameters to declaration list */
                    maplist(s, p -> fsig_param_list, {
                        add_decl(s);
			IFTRACE_DECL(
			    printf("Parameter: %s, decl: %d\n",
				   (s -> par_id == NIL? "(anon)" :
				    getname(s -> par_id -> id_str_table_index)),
				   decl_num);
			)
		    });
                putw(length(p -> fsig_param_list), Soutfile);
                maplist(s, p -> fsig_param_list, {
                    sig_out1(Soutfile, s);
		});
                sig_out1(Soutfile, p -> fsig_result_sig);
                /* Preserve info about function construction if available */
#                   define CONSTR_UNKNOWN 0
#                   define CONSTR_AVAIL 1
#                   define SLINK_AVAIL 2
                    if (p -> fsig_construction == NIL) {
                        putw(CONSTR_UNKNOWN, Soutfile);
                    } else {
                      NODE * constr = p -> fsig_construction;

                      if (p -> fsig_slink_known) {
                        putw(SLINK_AVAIL, Soutfile);
                      } else {
                        putw(CONSTR_AVAIL, Soutfile);
		      }
		      putw(constr -> fc_complexity, Soutfile);
#                     ifdef VERBOSE
			unparse_file = stdout;
			printf("Signature: ");
			unparse(p);
			printf(" bound to construction %s\n",
			       constr -> fc_code_label);
#                     endif
                      put_string(Soutfile, constr -> fc_code_label);
		      putw(constr -> ar_static_level, Soutfile);
		      putw(constr -> ar_size, Soutfile);
		    }
                break;

        case VALSIGNATURE:
                putw(VALSIGNATURE, Soutfile);
                sig_out1(Soutfile, p -> val_denotation);
                break;

        case VARSIGNATURE:
                putw(VARSIGNATURE, Soutfile);
                sig_out1(Soutfile, p -> var_denotation);
		break;

	case SIGNATURESIG:
		putw(SIGNATURESIG, Soutfile);
		break;

        case TYPESIGNATURE:
                add_decl(p);
#               ifdef TRACE_DECL
		    printf("Type signature: decl: %d\n",
			   decl_num);
		    unparse_file = stdout;
		    unparse(p);
		    printf("\n");
#               endif
                putw(TYPESIGNATURE, Soutfile);
                sig_out1(Soutfile, p -> ts_local_type_id);
                putw(length(p -> ts_clist), Soutfile);
                maplist(s, p -> ts_clist, {
                    sig_out1(Soutfile, s);
                });
                /* preserve optimization information: */
                    put_string(Soutfile, p -> ts_const_code);
                    put_string(Soutfile, p -> ts_string_code);
		    put_string(Soutfile, p -> ts_element_code);
		    putw(p -> ts_string_max, Soutfile);
		    putw(p -> ts_simple_type, Soutfile);
#               ifdef TRACE_DECL
		    printf("Finished type signature\n");
#                   endif
                break;

        case TSCOMPONENT:
                putw(TSCOMPONENT, Soutfile);
                sig_out1(Soutfile, p -> tsc_id);
                sig_out1(Soutfile, p -> tsc_signature);
                break;

        case DEFCHARSIGS:
                {
                    int i;
                    unsigned * base = &(p -> dcs_0);

                    putw(DEFCHARSIGS, Soutfile);
                    for(i = 0; i < NVECTORS; i++) {
                        putw(base[i], Soutfile);
                    }
                }
		break;

	case REXTERNDEF:
		putw(REXTERNDEF, Soutfile);
		put_string(Soutfile, p -> r_ext_name);
		break;

        case RECORDCONSTRUCTION:
        case EXTENSION:
        case ENUMERATION:
        case RECORDELEMENT:
        case WITHLIST:
        case EXPORTLIST:
        case HIDELIST:
        case EXPORTELEMENT:
		dbgmsg("Signature output can't handle %s yet\n",
                       kindname(p -> kind));
                break;

        case LISTHEADER:
        case FREEVARNODE:
        case WORDCAND:
        case WORDCOR:
        case EXTERNDEF:
	default:
                dbgmsg("sig_out: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}
