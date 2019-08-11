# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "pass4/sigs.h"

FILE * unparse_file;

/*
 * Write out a brief human readable version of the syntax tree headed
 * by p onto unparse_file.  Abbreviations are used wherever it is likely
 * to be appropriate.  The aim is for the output to be no longer
 * than a few hundred characters, and to be much shorter for a typical
 * signature.
 * An uninitialized unparse_file is interpreted as stderr.
 */

unparse(p)
register NODE * p;
{
register NODE * v;

    if (unparse_file == (FILE *)0) {
        unparse_file = stderr;
    }
    if (p == NIL) return;
    if (p == ERR_SIG) {
        fprintf(unparse_file, "(erroneous)");
        return;
    }

    switch ( p -> kind ) {

        case BLOCKDENOTATION:
                if (p -> bld_declaration_list == NIL
		    || is_empty(p -> bld_declaration_list)) {
		    if (p -> bld_den_seq == NIL) {
			fprintf(unparse_file, "<place holder>");
		    } else {
		      if (length(p -> bld_den_seq) == 1) {
                        unparse(first(p -> bld_den_seq));
		      } else {
                        fprintf(unparse_file, "(");
                        unparse(first(p -> bld_den_seq));
                        fprintf(unparse_file, "; ... )");
		      }
		    }
                } else {
                    fprintf(unparse_file, "let ");
                    unparse(first(p -> bld_declaration_list) -> decl_id);
                    fprintf(unparse_file, " == ... in ... ni");
                }
		break;

        case APPLICATION:
                unparse(p -> ap_operator);
                fprintf(unparse_file, "[");
                maplist(v, p->ap_args, {
                    unparse(v);
                    if(v != last(p -> ap_args)) {
                        fprintf(unparse_file, ",");
                    }
                });
                fprintf(unparse_file, "]");
		break;

        case LOOPDENOTATION:
                fprintf(unparse_file, "do ");
                if (!is_empty(p -> gl_list)) {
                    unparse(first(p -> gl_list) -> ge_guard);
                    fprintf(unparse_file, " ==> ... ");
                }
                fprintf(unparse_file, "od");
                break;

        case GUARDEDLIST:
                fprintf(unparse_file, "if ");
                if (!is_empty(p -> gl_list)) {
                    unparse(first(p -> gl_list) -> ge_guard);
                    fprintf(unparse_file, " ==> ... ");
                }
                fprintf(unparse_file, "fi");
                break;

        case OPRID:
        case LETTERID:
                if (p -> id_str_table_index == -1) {
                    fprintf(unparse_file, "(local id)");
                } else if (((int) p -> id_str_table_index) <= 0) {
                    fprintf(unparse_file, "(anonymous)");
                } else {
                    fprintf(unparse_file, "%s", getname(p -> id_str_table_index));
                }
                break;

        case FUNCCONSTR:
                fprintf(unparse_file, "func[");
                if (!is_empty(p -> signature -> fsig_param_list)) {
                    fprintf(unparse_file, "... ");
                }
                fprintf(unparse_file, "] {");
                unparse(p -> fc_body);
                fprintf(unparse_file, "}");
                break;

        case USELIST:
                fprintf(unparse_file, "use ... in ");
                unparse(first(p -> usl_den_seq));
                if (length(p -> usl_den_seq) != 1) {
                    fprintf(unparse_file, "; ...");
                }
                fprintf(unparse_file, " ni");
                break;

	case MODPRIMARY:
                unparse(p -> mp_primary);
                if (p -> mp_type_modifier != NIL) {
                    switch( p -> mp_type_modifier -> kind ) {
                        case EXPORTLIST:
                            fprintf(unparse_file, " export ...");
                            break;
                        case HIDELIST:
                            fprintf(unparse_file, " hide ...");
                            break;
                        case WITHLIST:
                            fprintf(unparse_file, " with ...");
                            break;
                    }
                }
                break;

        case QSTR:
                fprintf(unparse_file, "\"");
                fprintf(unparse_file, p -> str_string);
                fprintf(unparse_file, "\"");
                break;

        case UQSTR:
                fprintf(unparse_file, p -> str_string);
                break;

        case ENUMERATION:
                fprintf(unparse_file, "enum{ ... }");
                break;

	case PRODCONSTRUCTION:
                fprintf(unparse_file, "prod{ ... }");
                break;

	case UNIONCONSTRUCTION:
                fprintf(unparse_file, "union{ ... }");
                break;

        case RECORDCONSTRUCTION:
                fprintf(unparse_file, "record{ ... }");
                break;

        case WORDELSE:
                fprintf(unparse_file, "else");
                break;

	case EXTERNDEF:
                fprintf(unparse_file, "extern \"");
                fprintf(unparse_file, p -> ext_name);
                fprintf(unparse_file, "\"");
                break;

        case REXTERNDEF:
                fprintf(unparse_file, "extern {\"");
                fprintf(unparse_file, p -> r_ext_name);
                fprintf(unparse_file, "\"}");
                break;

        case PARAMETER:
                unparse(p -> par_id);
                fprintf(unparse_file, ":");
                unparse(p -> par_signature);
                break;

        case TSCOMPONENT:
                unparse(p -> tsc_id);
                break;
    
        case DEFCHARSIGS:
                {
                    unsigned *q = &(p -> dcs_0);
                    boolean has_consts = FALSE;

                    while (q < &(p -> dcs_0) + NVECTORS) {
                        if (*q != 0) has_consts = TRUE;
                        q++;
                    }
                    if (has_consts) {
                        fprintf(unparse_file, "(constants)");
                    }
                    break;
                }

        case EXTENSION:
                fprintf(unparse_file, "extend{");
                unparse(p -> ext_denotation);
                fprintf(unparse_file, "}");
                break;

        case FUNCSIGNATURE:
                fprintf(unparse_file, "func[");
                maplist(s, p -> fsig_param_list, {
                    unparse(s);
                    if (s != last(p -> fsig_param_list)) {
                        fprintf(unparse_file, ";");
                    }
                });
                fprintf(unparse_file, "]");
                unparse(p -> fsig_result_sig);
                break;

        case VARSIGNATURE:
                fprintf(unparse_file, "var ");
                unparse(p -> var_denotation);
                break;

        case VALSIGNATURE:
                fprintf(unparse_file, "val ");
                unparse(p -> val_denotation);
		break;

	case SIGNATURESIG:
		fprintf(unparse_file, "signature");
		break;

        case TYPESIGNATURE:
                fprintf(unparse_file, "type ");
                unparse(p -> ts_local_type_id);
                fprintf(unparse_file, "{");
                maplist(s, p -> ts_clist, {
                    unparse(s);
                    if (s != last(p -> ts_clist)) {
                        fprintf(unparse_file, ";");
                    }
                });
                fprintf(unparse_file, "}");
                break;

        default:
                dbgmsg("unparse: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}
