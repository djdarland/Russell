/*
 *  process_id(idnode)
 *
 *  If the last_definition field of the id node is not NIL,
 *  point it at the last definition of that id (as given in Idtable).
 */
# include "parm.h"
# include "pass1/stt/sttdefs.h"
# include "stree/ststructs.mh"
# include "Idtable.h"
# include "datatypes/stack.h"

Identry * retrieve();

/* extern STACK lti_stack;  /* stack of enclosing type signature nodes */
extern NODE * usl_ptr;   /* pointer to node for next enclosing use list */

process_id(idnode)
NODE * idnode;
{   register Identry * ip;

    if (idnode->id_use_list == NIL)
        idnode->id_use_list = usl_ptr;
    /* Not necessary. Subsequent passes treat this specially */
	if (idnode->id_str_table_index == -1) {
	    /* innermost local type id *
		idnode->id_last_definition = top(lti_stack);
	    */
    } else if (idnode->id_last_definition == NIL) {
	ip = retrieve(idnode->id_str_table_index);
	idnode->id_last_definition = ip->i_value;
    }
}      
