# include <stdio.h>
# include "parm.h"
# include "stree/ststructs.mh"
# include "datatypes/stack.h"
# include "stree/is_ptr.h"


extern add_id(), restore();

/*   Stack of enclosing type signature nodes.
 * Used in determining the defining nodes
 * for identifiers with a -1 string table index.
 * (See streedefs.h.) 
 * NOT NEEDED - see process_id.c
   STACK lti_stack = emptystack();
 */

/*  Pointer to next surrounding use list.  Used  */
/* to set fields in identifier, string and       */
/* use list nodes.                               */
NODE * usl_ptr = NIL;


/*
 *  Insert the links comprising the symbol table into the syntax tree.
 */

build_symtab(x)
NODE * x;
{

    if (x == NIL)
        /* Ignore missing clauses. */
            return;

    switch ( x -> kind ) {

        case LISTHEADER:
            /* Process every element on the list. */
                maplist(v, x, build_symtab(v));
            break;


        case DECLARATION: /* Only for program body declaration. */
            /* Add id to symtab. */
                add_id(x, x);

            /* Process program body. Leave decl_block pointing to NIL. */
                build_symtab(x->decl_denotation);
            break;


        case PARAMETER:
            /* Should never get here. */
                bs_error(x);


        case RECORDELEMENT:
            /* Should never get here. */
                bs_error(x);


        case VARSIGNATURE:
            /* Process denotation. */
                build_symtab(x->var_denotation);
            break;


        case VALSIGNATURE:
            /* Process denotation. */
                build_symtab(x->val_denotation);
            break;


        case FUNCSIGNATURE:
            /* Add the parameter identifiers to symtab. */
                maplist(v, x->fsig_param_list, add_id(v, x));

            /* Process the parameter signatures and the function */
            /* result signature.                                 */
                maplist(v, x->fsig_param_list,
                        build_symtab(v->par_signature));
                build_symtab(x->fsig_result_sig);

            /* Restore surrounding scope. */
                maplist(v, x->fsig_param_list, restore(v));
            break;
             

        case TYPESIGNATURE:
            /* Add local id to symtab. */
                add_id(x, x);
                /* push(x,lti_stack); */
            /* Process each component signature. */
                maplist(v, x->ts_clist,
                            switch (v->kind) {
				case TSCOMPONENT:
                                    /* v -> tsc_id -> id_last_definition = NIL; */
                                    build_symtab(v->tsc_signature);
                                    break;
                                case DEFCHARSIGS:
                                    /* Ignore. */
                                    break;
                                default:
                                    bs_error(v);
                            } );

            /* Restore surrounding scope. */
                restore(x);
                /* pop(lti_stack); */
            break;


        case TSCOMPONENT:
            /* Should never get here. */
                bs_error(x);


        case BLOCKDENOTATION:
            /* Add declaration identifiers to symtab. */
                maplist(v, x->bld_declaration_list, add_id(v, x));

            /* Process declaration signatures, declaration denotations, */
            /* and the block denotation.                                */
                maplist(v, x->bld_declaration_list,
                        { build_symtab(v->decl_signature);
                          build_symtab(v->decl_denotation); });
                build_symtab(x->bld_den_seq);

            /* Restore surrounding scope. */
                maplist(v, x->bld_declaration_list, restore(v));
            break;


        case USELIST:
            x->usl_previous_list = usl_ptr;
            /* Process list of types with old use list. */
                build_symtab(x->usl_type_list);
            /* Process body with new use list */
                usl_ptr = x;
                build_symtab(x->usl_den_seq);
            /* restore old usl_ptr value */
                usl_ptr = x->usl_previous_list;
            break;


        case APPLICATION:
            /* Process operator and args. */
                build_symtab(x->ap_operator);
                build_symtab(x->ap_args);
            break;


        case ENUMERATION:
	    maplist(v, x -> enum_id_list, {
		x -> id_last_definition = x;
	    });
            break;


        case EXTENSION:
            /* Process denotation. */
                build_symtab(x->ext_denotation);
            break;


        case UNIONCONSTRUCTION:
	case PRODCONSTRUCTION:
            /* Add local type name to symtab. */
		add_id(x, x);
	    if (x -> kind == PRODCONSTRUCTION) {
	      /* Add field names to symtab. */
		maplist(v, x -> prod_components,
			add_id(v,x));
	    }

            /* Process the component signatures */
                maplist(v, x->prod_components,
                        build_symtab(v->par_signature));

            /* Restore surrounding scope. */
                restore(x);
		if (x -> kind == PRODCONSTRUCTION) {
		  maplist(v, x -> prod_components,
			  restore(v));
		}

            break;
             

        case RECORDCONSTRUCTION:
            /* Process record component denotations. */
                maplist(v, x->rec_component_list, {
                    v -> re_id -> id_last_definition = v -> re_id;
		    build_symtab(v->re_denotation);
		});
            break;


        case WITHLIST:
            /* Process declaration denotations. */
                maplist(v, x->wl_component_list,
			{ 
                          v -> decl_id -> id_last_definition = NIL;
                          build_symtab(v->decl_signature);
                          build_symtab(v->decl_denotation);
                        } );

            break;


        case MODPRIMARY:
            /* Process primary and modifier. */
                build_symtab(x->mp_primary);

                /* add local id to symtab */
                    add_id(x,x);

                build_symtab(x->mp_type_modifier);

                /* restore surrounding scope */
                    restore(x);
            break;


        case EXPORTLIST:
        case HIDELIST:
            /* Process the export element list. */
                build_symtab(x->el_export_element_list);
            break;

        case EXPORTELEMENT:
	    x -> ee_id -> id_last_definition = x -> ee_id;
	    build_symtab(x -> ee_signature);
	    build_symtab(x -> ee_export_list);
            break;

        case ALLCONSTANTS:
            /* Nothing to do. */
            break;


        case WORDELSE:
            /* Nothing to do. */
            break;


        case WORDCAND:
            /* Nothing to do. */
            break;


        case WORDCOR:
            /* Nothing to do. */
            break;


        case GUARDEDLIST:
            /* Process guarded list. */
                build_symtab(x->gl_list);
            break;


        case LOOPDENOTATION:
            /* Process guarded list. */
                build_symtab(x->gl_list);
            break;


        case GUARDEDELEMENT:
            /* Process guard and element. */
                build_symtab(x->ge_guard);
                build_symtab(x->ge_element);
            break;


	case OPRID: /* Just a use, not a definition. */
	case LETTERID:
            /* Process selection type and signature */
                build_symtab(x->sel_type);
                build_symtab(x->signature);
            /* Fill in last definition field. */
                process_id(x);
            break;


	case QSTR:
	case UQSTR:
	    /* process selection type */
		build_symtab(x -> sel_type);

            /* fill in use list field */
                x->str_use_list = usl_ptr;
            break;


        case FUNCCONSTR:
	    /* Add the parameter identifiers to symtab. */
#               ifdef DEBUG
		  if(!is_ptr(x -> signature) || x -> signature == NIL) {
		    dbgmsg("build_symtab: bad fn signature\n");
		    abort();
		  }
#               endif
		maplist(v, x->signature->fsig_param_list, add_id(v, x));

            /* Process the parameter signatures, the function result */
	    /* signature, and the function body.                     */
		maplist(v, x->signature->fsig_param_list,
			build_symtab(v->par_signature));
		build_symtab(x->signature->fsig_result_sig);
		build_symtab(x->fc_body);

            /* Restore surrounding scope. */
                maplist(v, x->signature->fsig_param_list, restore(v));
	    break;

        case REXTERNDEF:
	case EXTERNDEF:
	    break;

    }
}


/*
 *  bs_error(x)
 *
 *  Print an error message and abort.
 */
bs_error(x)
NODE * x;
{
    dbgmsg("build_symtab: bad node: node addr = %o, kind = %s",
            x, kindname(x->kind));
    abort();
}


