# include "parm.h"

# include "stree/ststructs.mh"

# include "precedence.h"

/*
 * outermost_op(h,t)
 *
 * input: A denotation in the form of an array of pointers to primaries.
 *        h points to the start of the array, t to the end. The array
 *        should not be empty.
 *
 * output: A pointer to the outermost operator in the array, as determined
 *         by the heuristic.
 *
 * heuristic: 1) If the denotation contains primaries of less than
 *               "infinite" precedence, then the outermost operator
 *               is the leftmost primary in the rightmost sequence of
 *               of primaries of lowest precedence.
 *               In the case where the lowest precedence is that of the
 *               exponentiation operator, the outermost operator is just
 *               the leftmost operator of lowest precedence.
 *            2) If the denotation does not contain any primaries of less
 *               than infinite precedence, then the outermost operator
 *               is the only primary which is not bracketed (i.e. is not
 *               a list of primaries), and it may have no more than one 
 *               right arg and one left arg.
 *            3) If there is no such primary, then this routine fails.
 *               Mkappl2 will usually use the first primary in this case.
 *
 * 	WARNING: This is a HORRIBLE algorithm.  But we don't have the
 *		time to fix it.
 */

NODE ** outermost_op(h, t)
NODE **h, **t;
{
    NODE ** op;
    int  lowest_prec;   /* the lowest precedence of a primary 
                           in the denotation                  */
         

    /* Determine the lowest precedence in the denotation */
        {   int i;
            NODE ** p;

            lowest_prec = INFINITE;
            for (p = h; p != t+1; p++) {
                i = precedence(*p);
                if ( i < lowest_prec )
                    lowest_prec = i;
            }
        }


    switch ( lowest_prec ) {
        case INFINITE:
            /* Find unbracketed primary and check if ok */
                {   NODE ** p;
                    boolean has_left_arg,   /* op has a bracketed left arg */
                            has_right_arg;
                    int len;                /* # of primaries in array */

                    op = NIL;
                    for (p = h; p != t+1; p++) {
                        if ( !is_list(*p) )
                            op = p;
                    }
                    if ( op == NIL ) {
                        /* there's no reasonable operator, return the NIL */
			    break;
                    }

                    len = t - h + 1;
                    has_left_arg =  ( op == h ? FALSE : is_list(*h) );
                    has_right_arg = ( op == t ? FALSE : is_list(*t) );
                    if ( !(    has_left_arg &&  has_right_arg && len == 3
                           || !has_left_arg &&  has_right_arg && len == 2
                           ||  has_left_arg && !has_right_arg && len == 2
                          ) ) {
			/* Cant uniquely determine operator */
			    op = NIL;
			    break;
                    }
                }
            break;

        case EXPLEVEL:
	case ASGNLEVEL:
	case DEREFLEVEL:
            /* Find leftmost primary */
                {   NODE ** p;

                    for (p = h;;p++)
                        if ( precedence(*p) == lowest_prec ) {
                            op = p;
                            break;
                        }
                }
            break;

        default:
            /* Find leftmost primary in rightmost sequence */
                {   NODE ** p;
                    boolean in_seq;

                    in_seq = FALSE;
                    for (p = h; p != t+1; p++) {
                        if ( precedence(*p) == lowest_prec ) {
                            if ( !in_seq )
                                op = p;
                        } else
                            in_seq = FALSE;
                    }
                }
    }

    return ( op );

}
