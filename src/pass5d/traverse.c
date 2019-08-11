#define VERBOSE
#undef VERBOSE
/*
 *  Sets the decl_needed, mp_needed, and fc_body_needed fields 
 * in subtrees of p which might be executed if control reaches p.
 */

# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"

extern LIST declsort();

# ifdef VERBOSE
#   define IFVERBOSE(x) x
# else
#   define IFVERBOSE(x)
# endif

extern FILE * Goutfile;

extern FILE * unparse_file;

extern boolean Tflag; /* generate trace code */

extern boolean Vflag; /* generate optimization messages */

extern unsigned indx_put; /* should be sttrelptr ... */

extern int avail_loc;

extern void Gexpression();
extern void Gfc_add();
extern int Glevel;  /* Current nesting level */

/*
 *  Traverse the subtree headed by p and generate code for subexpressions
 * which might need to be evaluated.
 */
Gtraverse(p)
register NODE * p;
{
register NODE * v;

    if (p == NIL) return;

    switch ( p -> kind ) {

        case BLOCKDENOTATION :
		{
#                   ifdef VERBOSE
			unparse_file = stdout;
			printf("Traversing BLOCKDENOTATION:\n");
			unparse(p);
			printf("\n");
#                   endif
		    if ( p -> bld_flags & REQUIRES_AR ) {
			Glevel++;
		    }
		    maplist (v, (LIST)decl_sort(p->bld_declaration_list), {
			ASSERT (v->kind == DECLARATION,
				"Gtraverse: decl expected");
			if (v -> decl_needed) {
			    int tmp_loc = avail_loc++;

			    IFVERBOSE(
				printf("Compiling rhs: ");
				unparse(v -> decl_id);
				printf("\n");
			    )
			    /* Evaluate rhs. Store into a.r. */
			      gen2(DCL, tmp_loc, DCL_INT);
			      Gexpression (v-> decl_denotation, tmp_loc, FALSE);
			      gen3(STI, AR, v -> displacement, tmp_loc);
			      gen1(UDC, tmp_loc);
			} else {
			  /* Generate code for nested function       */
			  /* constructions or modified primary nodes */
			  /* that may be evaluated.		     */
			    IFVERBOSE(
				printf("Traversing rhs: ");
				unparse(v -> decl_id);
				printf("\n");
			    )
			    Gtraverse (v -> decl_denotation);
			}
		    }
		    );
		    maplist (v,p->bld_den_seq, {
			IFVERBOSE(
			    printf("Traversing body expression:\n");
			    unparse(v);
			    printf("\n");
			)
			Gtraverse(v);
		    });
		    if ( p -> bld_flags & REQUIRES_AR ) {
			Glevel--;
		    }
		    break;
		}
		
        case APPLICATION:
		Gtraverse(p -> ap_operator);
		maplist(v,p->ap_args,Gtraverse(v));
		break;

        case LOOPDENOTATION:
        case GUARDEDLIST:
		maplist(v,p->gl_list,Gtraverse(v));
		break;

        case GUARDEDELEMENT:
		Gtraverse(p->ge_guard);
		Gtraverse(p->ge_element);
		break;

        case OPRID:
        case LETTERID:
		if (p -> sel_type != NIL) {
		    Gtraverse(p->sel_type);
		}
		break;

        case FUNCCONSTR:
		if (p -> fc_body_needed) {
		  if (p -> fc_complexity & NO_SL) {
		    Gfc_add(p, Glevel+1, TRUE /* only fast version */);
		  } else {
		    Gfc_add(p, Glevel+1, FALSE);
		  }
		} else {
		  if (Vflag) {
		    printf("Suppressing code generation for %s\n",
			   p -> fc_code_label);
		  }
		  Glevel++;  /* presumably not necessary, but ... */
		  Gtraverse(p -> fc_body);
		  Glevel--;
                }
		break;

	case USELIST:
		maplist(q, p -> usl_den_seq, Gtraverse(q));
		break;

	case MODPRIMARY:
#               ifdef VERBOSE
		    unparse_file = stdout;
		    printf("Traversing MODPRIMARY:\n");
		    unparse(p);
		    printf("\n");
#               endif
		if (p -> mp_needed) {
#                   ifdef VERBOSE
			printf("Compiling primary\n");
#                   endif
		    Gexpression(p, SK, FALSE);
		} else {
#                   ifdef VERBOSE
			printf("Traversing primary\n");
#                   endif
		    Gtraverse(p -> mp_primary);
		    if (p -> mp_type_modifier != NIL
		        && p -> mp_type_modifier -> kind == WITHLIST) {
		      maplist (q, p -> mp_type_modifier -> wl_component_list, {
			IFVERBOSE(
			    printf("Traversing with list component:\n");
			    unparse(q -> decl_id);
			    printf("\n");
			)
			Gtraverse(q -> decl_denotation);
		      });
		    }
		}
		break;

	case ENUMERATION:
	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
		/* Subexpressions are not evaluated */
		break;

	case QSTR:
	case UQSTR:
		Gtraverse(p -> sel_type);
		/* There can't be anything of interest in the expansion */
		break;

	case WORDELSE:
        case EXTERNDEF:
        case REXTERNDEF:
		break;

        case RECORDCONSTRUCTION:
                maplist(s, p -> rec_component_list, {
                  Gtraverse(s -> re_denotation);
                });
                break;

        case EXTENSION:
                Gtraverse(p -> ext_denotation);
		break;

	case VALSIGNATURE:
	case VARSIGNATURE:
	case FUNCSIGNATURE:
	case TYPESIGNATURE:
	case SIGNATURESIG:
		break;

        case RECORDELEMENT:
	case DECLARATION:
	case PARAMETER:
	case LISTHEADER: /* should never get here */
	case TSCOMPONENT:
        case DEFCHARSIGS:
	case WITHLIST:
        case EXPORTLIST:
        case EXPORTELEMENT:
        case ALLCONSTANTS:
	case HIDELIST:
	case WORDCAND:
	case WORDCOR:
	default:
		dbgmsg("Gtraverse: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}
