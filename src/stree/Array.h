/*
 * Template for an array of pointers to Nodes.
 */


typedef struct {
    int a_size;             /* # of pointers in the array */
    NODE * a_body[1];
    			/* The .c files assume a 0 sized array, which	*/
    			/* isn't legal anymore.  This is safe.		*/
    } Array;

#define a_start a_body[0]

Array * list_to_array();
