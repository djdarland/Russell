#
/*
 *  add_id(defnode, scopenode);
 *  NODE * defnode, scopenode;
 *
 *  defnode   - Definition node.
 *  scopenode - The root of the subtree for which this definition is valid.
 *
 *  Add the new definiton of the id to the symbol table and adjust Idtable.
 *  The basic operations are:
 *      idnode->last_definition = defnode (local type ids only)
 *      idnode->use_list = current use list
 *      defnode->previous_definition = previous definition (from Idtable)
 *      Update Idtable
 *      if declaration or parameter, defnode->scope = scopenode
 *
 *  Nothing is done if idnode is NIL, e.g. in the case of anonymous 
 *  parameters.
 */

# include "parm.h"
# include "pass1/stt/sttdefs.h"
# include "stree/ststructs.mh"
# include "Idtable.h"

extern NODE * usl_ptr;

static NODE * idnode;
static NODE ** prevdef_field;

Identry * retrieve();

add_id(defnode, scopenode)
NODE * defnode, * scopenode;
{   Identry * ip;

    findfields(defnode);
    /* update scope fields and set last_definition for local type ids */
        switch(defnode->kind) {
            case DECLARATION:
                defnode->decl_scope = scopenode;
                break;
            case PARAMETER:
		defnode->par_scope = scopenode;
                break;
            default:
                if (idnode != NIL) idnode -> id_last_definition = defnode;
                break;
        }

    if (idnode == NIL) return;
    idnode->id_use_list = usl_ptr;
    ip = retrieve(idnode->id_str_table_index);
    *prevdef_field = ip->i_value;
    ip->i_value = defnode;
}

/*
 *  Given a definition node for an identifier,
 *
 *  set 
 *     idnode to point to the identifier node for the declareed identifier
 *     prevdef_field to the address of the field pointing to an enclosing
 *          declaration.
 */

static findfields(defnode)
NODE * defnode;
{
    switch(defnode->kind) {
            case DECLARATION:
                idnode = defnode->decl_id;
                prevdef_field = &(defnode->decl_previous_definition);
                break;
            case PARAMETER:
                idnode = defnode->par_id;
                prevdef_field = &(defnode->par_previous_definition);
                break;
            case TYPESIGNATURE:
                idnode = defnode->ts_local_type_id;
                prevdef_field = &(defnode->ts_previous_definition);
                break;
            case UNIONCONSTRUCTION:
            case PRODCONSTRUCTION:
                idnode = defnode->prod_local_type_id;
                prevdef_field = &(defnode->prod_previous_definition);
                break;
            case MODPRIMARY:
                {
                    NODE * modifier = defnode -> mp_type_modifier;

                    switch(modifier -> kind){
                        case WITHLIST:
                            idnode = modifier->wl_local_type_id;
                            prevdef_field = &(modifier->wl_previous_definition);
                            break;
                        case EXPORTLIST:
                        case HIDELIST:
                            idnode = modifier->el_local_type_id;
                            prevdef_field = &(modifier->el_previous_definition);
                            break;
                    }
                }
                break;
#         ifdef DEBUG
            default:
                    dbgmsg("add_id: bad defnode: kind=%s\n",
                            kindname(defnode->kind));
#         endif
    }
}
