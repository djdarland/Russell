#define DEBUG

#define VERBOSE
#undef VERBOSE

/*
 *  Sets the decl_needed, mp_needed, and fc_body_needed fields 
 * in subtrees of p which might be executed if control reaches p.
 * Adds object files to link list if nested entry points are
 * directly referenced.
 *  Set the ID_IMPORTED and VAR_NONTR_REF of decl_special fields.
 *  Set id_forward_ref and decl_can_be_refd fields.
 *  Requires that level fields for DECLARATION and MODPRIMARY nodes,
 * as well as ar_static_level fields for function constructions be set
 * correctly.
 */

# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codeutil.h"
# include "../runtime/runtime.h"
# include "pass3/is_local.h"
# include "pass4/sigs.h"

# ifdef VERBOSE
#   define IFVERBOSE(x) x
# else
#   define IFVERBOSE(x) 
# endif

extern FILE * Voutfile;

extern FILE * unparse_file;

extern NODE * insrtptr;     /* Pointer to user program */

extern boolean has_externs;  /* Program contains REXTERN nodes */

extern boolean Tflag; /* generate trace code */

extern boolean Gflag; /* generate intermediate code */

extern boolean Vflag; /* verbose */

extern boolean Oflag; /* optimize */

extern unsigned indx_put; /* should be sttrelptr ... */

static int Clevel = -1;      /* Current nesting level */

extern void add_objfile();

extern boolean is_int_const();

extern char * rindex();

/* Determine whether the rhs of a declaration needs to be evaluated */
/* due to potential side effects, or due to possible reference      */
/* by a separately compiled program.                                */
boolean eval_decl(p)
NODE *p;
{
    int decl_den_kind, decl_sig_kind;
    NODE * decl_sig;
    extern NODE * declerr;
    extern NODE * declsig();

    if (has_externs && !is_descendant(p, insrtptr)) {
	/* Outside user program.  May be referenced by -c compilations. */
	return(TRUE);
    }
    decl_sig = p -> decl_signature;
    if (decl_sig == NIL) {
	decl_sig = declsig(p);
	if (declerr != SUCCESS) return(TRUE);
    } else if (decl_sig == ERR_SIG) {
	return(TRUE);
    }
    decl_sig_kind = decl_sig -> kind;
    decl_den_kind = p -> decl_denotation -> kind;
    if (decl_sig_kind == VARSIGNATURE
	|| decl_sig_kind == VALSIGNATURE
	|| decl_sig_kind == FUNCSIGNATURE
	   && decl_den_kind != FUNCCONSTR
	   && (impure(p -> decl_signature)
	       || calls_put(p -> decl_denotation))
	|| decl_sig_kind == TYPESIGNATURE
	   && calls_put(p -> decl_denotation)) {
	    return(TRUE);
    } else {
	    return(FALSE);
    }
}

/* Add the object file containing the named entry point */
void add_extracted_objfile(entry_name)
char * entry_name;
{
    char * filename = (char *) malloc(strlen(entry_name));
    char * p, *q;

    /* Copy starting after first underscore char */
	for(p = entry_name; *p != '_'; p++);
	p++;
	q = filename;
	while(*q++ = *p++);
    /* Replace tail with ".o" */
	p = rindex(filename, '.');
	/* Replace embedded periods by slashes */
	    for (q = filename; q < p; q++) {
		if (*q == '.') {
		    *q = '/';
		}
	    }
	*(p+1) = 'o';
	*(p+2) = '\0';

    if (filename[0] != '\0') {
	add_objfile(filename);
    }
    free(filename);
}

/* Mark the body of the function construction p as evaluable at run-time */
body_accessible(p)
register NODE * p;
{
    int Olevel = Clevel;

    if (p -> fc_body_needed) return;
    Clevel = p -> ar_static_level;
    p -> fc_body_needed = TRUE;
    accessible(p -> fc_body);
    Clevel = Olevel;
}

/* Mark an identifier and its declaration as accessible.  Set ID_IMPORTED */
/* if appropriate.  VAR_NONTR_REF is not affected.                        */
id_accessible(p)
NODE *p;
{
    if (!is_int_const(p)) {
	if (p -> sel_type != NIL) {
	    accessible(p->sel_type);
	} else {
#           ifdef DEBUG
		int Olevel = Clevel;
#           endif
	    NODE * def = p -> id_last_definition;

	    /* Clevel will be reset from level fields in DECLARATION */
	    /* and MODPRIMARY.                                       */
	    accessible(def);
#           ifdef DEBUG
		if (Clevel != Olevel) {
		    dbgmsg("id_accessible: Clevel clobbered\n");
		    abort(Clevel,Olevel);
		}
#           endif
	    if (def -> kind == DECLARATION && def -> level != Clevel) {
#               ifdef VERBOSE
		    unparse_file = stdout;
		    printf("Non-local reference from level %d to ", Clevel);
		    unparse(p);
		    printf("\n");
#               endif
		def -> decl_special |= ID_IMPORTED;
	    }
	}
    }
}

/* Mark the expression p as needed at run-time */
accessible(p)
register NODE * p;
{
register NODE * v;

    if (p == NIL) return;
    if (p -> kind == PARAMETER) return;
    if (p -> kind != DECLARATION
	&& p -> signature -> kind == SIGNATURESIG) {
	/* Evaluation of signatures is trivial.  */
	return;
    }

    switch ( p -> kind ) {

	case DECLARATION:
		if (p -> decl_needed) break;
		{
		    int Olevel = Clevel;
		    p -> decl_needed = TRUE;
		    Clevel = p -> level;
		    accessible(p -> decl_denotation);
		    Clevel = Olevel;
		}
		break;

	case BLOCKDENOTATION:
		/* mark forward declarations etc. */
		    label_decls(p -> bld_declaration_list);
		maplist(v,p->bld_den_seq,accessible(v));
		/* declarations are likely to not be accessible due */
		/* to inline expansion.                             */
		maplist (v, p -> bld_declaration_list, {
		    NODE * sig;

		    if (v -> decl_needed) {
			continue; /* already taken care of */
		    }
		    sig = v -> decl_denotation -> signature;
		    if ((eval_decl(v) && !is_int_const(v -> decl_denotation))
			|| Tflag
			   && sig -> kind == TYPESIGNATURE
			   && hascomp(sig, indx_put)) {
			   /* Either side effects are possible, or */
			   /* we need put function for tracing.    */
			   accessible(v);
		    }
		});
		break;

	case APPLICATION:
		v = p -> ap_operator -> signature;
		/* Try to fill in in-line code if not already there */
		    if (v -> fsig_inline_code == NIL
			&& v -> fsig_construction != NIL) {
			v -> fsig_inline_code = v -> fsig_construction
						  -> signature
						  -> fsig_inline_code;
		    }
		if (impure(v)
		    && !is_id(p -> ap_operator)
		    || calls_put(p -> ap_operator)) {
#                   ifdef VERBOSE
			unparse_file = stdout;
			printf("Accessible operator (impure or put): ");
			unparse(p -> ap_operator);
			printf("\n");
#                   endif
                    accessible(p -> ap_operator);
		} else if (v -> fsig_inline_code == NIL) {
		    if (Oflag && v -> fsig_construction != NIL) {
			map2lists(a, p -> ap_args,
				  f, v -> fsig_construction -> signature -> fsig_param_list, {
			    if (f -> par_only_def == NIL
				|| is_real_def(f -> par_only_def)
				   && comp_st(a, f -> par_only_def,
					      NIL, NIL) == 0) {
				f -> par_only_def = a;
			    } else {
				f -> par_only_def = MULTIPLE_DEFS;
				/* Could possibly be MULTIPLE_TP_DEFS */
			    }
			});
		    }
		    if (v -> fsig_construction != NIL &&
			(v -> fsig_slink_known ||
			 (v -> fsig_construction -> fc_complexity & NO_SL))) {
			if (v -> fsig_construction -> fc_body -> kind
			    == EXTERNDEF
			    && v -> fsig_construction -> fc_body -> ext_name
			       == NIL) {
			    /* Dummy for separately compiled function */
			    add_extracted_objfile(v -> fsig_construction
						    -> fc_code_label);
			} else {
#                           ifdef VERBOSE
				printf("Accessible construction: %s,",
				       v -> fsig_construction
					 -> fc_code_label);
				if (v -> fsig_slink_known) {
				    printf("Static link known\n");
				} else {
				    printf("Simple construction\n");
				}
#                           endif
			    /* Construction is in this compilation unit */
			    v -> fsig_construction -> fc_complexity
						   |= DIR_CALL;
				/* Don't perform "optimizations" that   */
				/* would make the operator accessible.  */
			    body_accessible(v -> fsig_construction);
			}
		    } else {
#                       ifdef VERBOSE
			    unparse_file = stdout;
			    if (v -> fsig_construction == NIL) {
			      printf("Accessible operator (unknown constr): ");
			    } else {
			      printf("Accessible operator (unknown sl): ");
			    }
			    unparse(p -> ap_operator);
			    printf("\n");
#                       endif
			if (v -> fsig_construction != NIL
			    && v -> fsig_construction -> fc_body -> kind
			       == EXTERNDEF
			    && v -> fsig_construction -> fc_body -> ext_name
			       == NIL) {
			    /* Dummy for separately compiled function     */
			    /* Make sure it gets linked, even if we later */
			    /* decide not to compile operator.            */
			    add_extracted_objfile(v -> fsig_construction
						    -> fc_code_label);
			}
			accessible(p -> ap_operator);
		    }
		}
		/* Arguments are accessible, but V and := and similar */
		/* operations have to be treated specially.           */
		{
		    int s = special_tp(v -> fsig_special);

		    switch(s) {
			case RECORD_VALUEOF:
			case PROD_VALUEOF:
			case ENUM_VALUEOF:
			    s = STD_VALUEOF;
			    break;
			case RECORD_ASSIGN:
			case PROD_ASSIGN:
			case ENUM_ASSIGN:
			case STD_PASSIGN:
			case STD_MASSIGN:
			case STD_TASSIGN:
			    s = STD_ASSIGN;
			    break;
		    }
		    if (s == STD_VALUEOF || s == STD_ASSIGN) {
			/* Deal with first argument specially */
			NODE * arg1 = first(p -> ap_args);

			if (arg1 -> kind == LETTERID
			    || arg1 -> kind == OPRID) {
			    id_accessible(arg1);
			} else {
			    accessible(arg1);
			}
			if (s == STD_ASSIGN) {
			    accessible(second(p -> ap_args));
			}
		    } else if (Oflag && Gflag
			       && v -> fsig_inline_code != NIL) {
			/* Use in-line code to determine whether we */
			/* need addresses correponding to variable  */
			/* arguments.                               */
			int argcount = 1;

			maplist(s, p -> ap_args, {
			    if ((s -> kind == LETTERID
				|| s -> kind == OPRID)
				&& s -> sel_type == NIL
				&& s -> id_last_definition -> kind
				   == DECLARATION) {
				NODE * sig = sig_structure(s -> signature);

			      if (sig -> kind == VARSIGNATURE
				  && !(s -> id_last_definition -> decl_special
				       & VAR_NONTR_REF)) {
				if (only_indirect_ref(v -> fsig_inline_code,
						      argcount)) {
				    id_accessible(s);
				} else {
				    accessible(s);
				}
			      } else {
				accessible(s);
			      }
			    } else {
				accessible(s);
			    } 
			    argcount++;
			});
		    } else {
			maplist(s,p->ap_args,accessible(s));
		    }
		}
		break;

        case LOOPDENOTATION:
        case GUARDEDLIST:
		maplist(v,p->gl_list,accessible(v));
		break;

        case GUARDEDELEMENT:
		accessible(p->ge_guard);
		accessible(p->ge_element);
		break;

        case OPRID:
	case LETTERID:
		if (p -> signature -> kind == VARSIGNATURE
		    /* Can't be selection */
		    && p -> id_last_definition -> kind == DECLARATION) {
		    /* Got here without going through V or := */
		    if (Vflag) {
			printf("Direct reference to variable cell ");
			unparse_file = stdout;
			unparse(p);
			findvl(p -> vlineno);
			printf(" from line %d\n", getrl());
		    }
		    p -> id_last_definition -> decl_special |= VAR_NONTR_REF;
		}
		id_accessible(p);
		break;

	case FUNCCONSTR:
		(p -> fc_complexity) |= NEED_CL;
		/* Need function value.  Thus we don't know the */
		/* identity of parameters.                      */
		  maplist(q, p -> signature -> fsig_param_list, {
		    q -> par_only_def = MULTIPLE_DEFS;
		  });
		if (p -> fc_body_needed) break;
		p -> fc_body_needed = TRUE;
		Clevel++;
		accessible(p -> fc_body);
		Clevel--;
		break;

	case USELIST:
		maplist(q, p -> usl_den_seq, accessible(q));
		break;

	case MODPRIMARY:
		{
		  int Olevel = Clevel;
#                 ifdef VERBOSE
		    printf("Accessible type modification:\n");
		    unparse_file = stdout;
		    unparse(p);
		    printf("\n");
#                 endif
		  if (p -> mp_needed) break;
		  p -> mp_needed = TRUE;
		  Clevel = p -> level;
		  accessible(p -> mp_primary);
		  if (p -> mp_type_modifier != NIL
		    && p -> mp_type_modifier -> kind == WITHLIST) {
		    /* Mark forward references */
			(void) label_wl(p);
		    maplist (q, p -> mp_type_modifier -> wl_component_list, {
			IFVERBOSE(
			  printf("Accessible wlc:\n");
			  unparse_file = stdout;
			  unparse(q -> decl_id);
			  printf("\n");
			)
			accessible(q -> decl_denotation);
		    });
		  }
		  Clevel = Olevel;
		}
		break;

        case QSTR:
	case UQSTR:
		{
		    NODE * sig = p -> sel_type -> signature;
		    int maxlen;

		    ASSERT(sig -> kind == TYPESIGNATURE,
			   "accessible: bad string type\n");
		    if (sig -> ts_string_max == -1) {
			maxlen = MAXSTRLEN;
		    } else {
			maxlen = sig -> ts_string_max;
		    }
		    if (sig -> ts_string_code != NIL 
			&& sig -> ts_element_code != NIL
			&& strlen(p -> str_string) <= maxlen
			&& ! calls_put(p -> sel_type)) {
			/* Nothing else needed */
		    } else {
			accessible(p -> str_expansion);
		    }
		}
		break;

	case ENUMERATION:
	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
		/* Subexpressions are not evaluated */
		break;

	case WORDELSE:
        case EXTERNDEF:
        case REXTERNDEF:
		break;

	case RECORDCONSTRUCTION:
                maplist(s, p -> rec_component_list, {
                  accessible(s -> re_denotation);
                });
                break;

        case EXTENSION:
                accessible(p -> ext_denotation);
                break;

	case PARAMETER:
	case RECORDELEMENT:
        case FUNCSIGNATURE:
	case LISTHEADER: /* should never get here */
	case VARSIGNATURE:
	case VALSIGNATURE:
	case TYPESIGNATURE:
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
		dbgmsg("accessible: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}

extern void Vexpression();
extern void fc_add();
extern int Vlevel;  /* Current nesting level */

/*
 *  Traverse the subtree headed by p and generate code for subexpressions
 * which might need to be evaluated.
 */
Vtraverse(p)
register NODE * p;
{
register NODE * v;

    if (p == NIL) return;

    switch ( p -> kind ) {

        case BLOCKDENOTATION :
		{
		    if ( p -> bld_flags & REQUIRES_AR ) {
			Vlevel++;
		    }
		    maplist (v, (LIST)decl_sort(p->bld_declaration_list), {
			ASSERT (v->kind == DECLARATION,
				"Vtraverse: decl expected");
			if (v -> decl_needed) {
			    Vexpression (v-> decl_denotation);
			    POP_DISP ("ap",v->displacement,
			              "# store declared value");
			} else {
			  /* Generate code for nested function       */
			  /* constructions or modified primary nodes */
			  /* that may be evaluated.		     */
			    Vtraverse (v -> decl_denotation);
			}
		    }
		    );
		    maplist (v,p->bld_den_seq, {
			Vtraverse(v);
		    });
		    if ( p -> bld_flags & REQUIRES_AR ) {
			Vlevel--;
		    }
		    break;
		}
		
        case APPLICATION:
		Vtraverse(p -> ap_operator);
		maplist(v,p->ap_args,Vtraverse(v));
		break;

        case LOOPDENOTATION:
        case GUARDEDLIST:
		maplist(v,p->gl_list,Vtraverse(v));
		break;

        case GUARDEDELEMENT:
		Vtraverse(p->ge_guard);
		Vtraverse(p->ge_element);
		break;

        case OPRID:
        case LETTERID:
		if (p -> sel_type != NIL) {
		    Vtraverse(p->sel_type);
		}
		break;

        case FUNCCONSTR:
		if (p -> fc_body_needed) {
		  if (p -> fc_complexity & NO_SL) {
		    fc_add(p, Vlevel+1, TRUE /* only fast version */);
		  } else {
		    fc_add(p, Vlevel+1, FALSE);
		  }
		} else {
		  Vlevel++;  /* presumably not necessary, but ... */
		  Vtraverse(p -> fc_body);
		  Vlevel--;
                }
		break;

	case USELIST:
		maplist(q, p -> usl_den_seq, Vtraverse(q));
		break;

	case MODPRIMARY:
		if (p -> mp_needed) {
		    Vexpression(p);
		    POP("r0", "# type modification value not used");
		} else {
		    Vtraverse(p -> mp_primary);
		    if (p -> mp_type_modifier != NIL
		        && p -> mp_type_modifier -> kind == WITHLIST) {
		      maplist (q, p -> mp_type_modifier -> wl_component_list, {
			Vtraverse(q -> decl_denotation);
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
		Vtraverse(p -> sel_type);
		/* There can't be anything of interest in the expansion */
		break;

	case WORDELSE:
        case EXTERNDEF:
        case REXTERNDEF:
		break;

        case RECORDCONSTRUCTION:
                maplist(s, p -> rec_component_list, {
                  Vtraverse(s -> re_denotation);
                });
                break;

        case EXTENSION:
                Vtraverse(p -> ext_denotation);
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
		dbgmsg("Vtraverse: bad kind, kind = %d\n", p -> kind);
		abort();

    };
    return;
}
