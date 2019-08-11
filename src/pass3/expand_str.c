# include "parm.h"

# include "stree/ststructs.mh"

extern NODE * sig_const;
extern int indx_empty;
extern int indx_sconc;
extern int indx_pconc;

/*
 * expand_str(pointer_to_string_node)
 *
 *  Returns a tree containing the expanded version of the string.
 * It is assumed that the selection type is explicitly present in
 * the argument node.  It is also assumed that the selection type
 * has the necessary components, and that the characters in the string
 * have constant signature.
 */

NODE *
expand_str(string)
NODE * string;
{
    char * next_char;     /* next character to be added to expansion    */
    NODE * expansion;     /* current partial expansion                  */
    NODE * id_node;       /* identifier node currently being generated  */
    NODE * conc_op;       /* concatenation operator node                */
    char * const_template = "'X'";

#   ifdef DEBUG
        if (string -> sel_type == NIL) {
            dbgmsg("expand_str: missing selection type\n");
            abort();
        }
#   endif
    next_char = string -> str_string;
    /* Build first node of expansion */
        switch (string -> kind) {
            case QSTR:
                id_node = mknode(LETTERID, indx_empty);
		initfld(&(id_node -> sel_type), string -> sel_type);
		id_node -> id_def_found = TRUE;
                break;
            case UQSTR:
#               ifdef DEBUG
		    if (*next_char == '\0') {
                        dbgmsg("expand_str: empty unquoted string\n");
                    }
#               endif
                const_template[1] = * next_char++;
		id_node = mknode(LETTERID, stt_enter(const_template,4));
		initfld(&(id_node -> sel_type), string -> sel_type);
		id_node -> id_def_found = TRUE;
                break;
#           ifdef DEBUG
                default:
                    dbgmsg("expand_str: bad string node\n");
#           endif
        }
    expansion = mknode(APPLICATION, id_node, emptylist());

    if (*next_char) {
        /* build conc_op */
            switch(string -> kind) {
                case QSTR:
		    conc_op = mknode(LETTERID, indx_sconc);
                    break;
                case UQSTR:
		    conc_op = mknode(LETTERID, indx_pconc);
                    break;
            }
	    initfld(&(conc_op -> sel_type), string -> sel_type);
	    conc_op -> id_def_found = TRUE;
    }

    /* add remaining characters */
        while (*next_char) {
            const_template[1] = * next_char++;
	    id_node = mknode(LETTERID, stt_enter(const_template,4));
	    initfld(&(id_node -> sel_type), string -> sel_type);
	    id_node -> id_def_found = TRUE;
            expansion = mknode( APPLICATION,
                                conc_op,
                                mklist( expansion,
                                        mknode( APPLICATION,
                                                id_node,
						emptylist()
                                        ),
                                        -1
                                )
			);
        }
    return(expansion);
}
