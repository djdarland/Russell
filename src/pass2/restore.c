#
/*
 *  restore(defnode)
 *  NODE * defnode;
 *
 *  defnode - A definition node, giving a definition to an id.
 *
 *  Remove the knowledge of this definition of the identifier from
 *  the Idtable.
 *  Ignore the request if the definition node in fact does not define
 *  an identifier, e.g. if it is an ononymous parameter.
 *
 */

# include "parm.h"
# include "pass1/stt/sttdefs.h"
# include "stree/ststructs.mh"
# include "Idtable.h"
# define DEBUG yes

restore(defnode)
NODE * defnode;
{   Identry * ip;
    NODE * idnode;        /* The node of the defined id. */
    NODE **prevdef_field; /* The previous definition field in defnode. */

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
            default:
#               ifdef DEBUG
                    dbgmsg("restore: bad defnode: kind=%s\n",
                            kindname(defnode->kind));
                    abort();
#               endif
                break;
    }

    if (idnode == NIL) return;
    ip = (Identry *)retrieve(idnode->id_str_table_index);
    ip->i_value = *prevdef_field;
}

