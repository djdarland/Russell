#include "types.h"

#define NEW_OFFSET 0 /* Offset of New function in argument type */
#define V_OFFSET 1   /* Offset of ValueOf function in argument type */

/* This implements Array: func[size: Short; component_type: type{New,V}]
 *                            type {. (*var subscr*); . (*val subscr*); New; V}
 *
 *   Array values and variables are represented as vectors of length size+1.
 * The first entry is the size of the array.  (This enables simple in-line
 * subscription code.)  The rest are values or variables of the component
 * type.
 */

/* Array_New: func[] var Array[size,type] */

MkIP(Array_New(A))
struct ar * A;  /* Passed environment.  Static link points to object */
		/* containing size and element type.                 */
{
    struct obj * envptr;
    register int i;
    struct obj *op;    /* var array[...] */
    int size;                   /* size of array object */
    struct obj *type;           /* component type */

    /* get size, type */
	envptr = (struct obj *)(A -> ar_static_link);
	size = envptr -> obj_component[0];
	type = (struct obj *) (envptr -> obj_component[1]);
    /* Allocate the array object */
	op = ralloc_comp(size + 1);
    /* initialize entries to size (1st) and type$New[] */
	op -> obj_component[0] = (word) size;
	for (i = 1; i <= size; i++) {
	  op -> obj_component[i] = 
		(word) call_russell0(type -> obj_component[NEW_OFFSET]);
	}
    return(op);
}


/* Array_ValueOf: func[var Array[size,type]] val Array[size,type] */

MkIP(Array_ValueOf(A))
struct ar * A;
{
    struct obj * array_var;
    struct obj *envptr;
    register int i;
    struct obj *op;    /* val array[...] */
    int size;                   /* size of array object */
    struct obj *type;           /* component type */

    array_var = A -> ar_arg1;
    /* get size, type */
	envptr = (struct obj *)(A -> ar_static_link);
	size = envptr -> obj_component[0];
	type = (struct obj *) (envptr -> obj_component[1]);
    /* allocate copy */
	op = ralloc_comp(size+1);
    /* Fill in copy with size and type$ValueOf[array_var.[i-1]] */
	op -> obj_component[0] = size;
	for (i = 1; i <= size; i++) {
	  op -> obj_component[i] =
	    (word) call_russell1(type -> obj_component[V_OFFSET],
				 array_var -> obj_component[i]);
	}
    return(op);
}


/* Array_ValueOf: func[var Array[size,type]] val Array[size,type] */
/* Fast version.  Works only in typical case                      */

MkIP(fast_Array_ValueOf(array_var))
struct obj * array_var;
{
register int i;
long array_size = array_var -> obj_component[0];
struct obj *op;    /* var array[...] */
register long size = array_size + 1;   /* size of array object */

    /* allocate copy */
      op = ralloc_comp(size);
    /* Fill in copy with size and type$ValueOf[array_var.[i-1]] */
      op -> obj_component[0] = array_size;
      for (i = 1; i < size; i++) {
	op -> obj_component[i] =
	  * ((word *) (array_var -> obj_component[i]));
      }
    return(op);
}

/* Array_New: Fast version for atomic data     */
/* Works only for typical element type         */
MkIP(fast_Array_New(array_size))
register long array_size;
{
register int i;
struct obj *op;    /* var array[...] */
register struct obj * el_op;     /* pointer to current element variable */

    /* allocate new array */
	op = ralloc_comp(array_size+1);
    /* Fill in size */
	op -> obj_component[0] = array_size;
    /* Fill in array with pointers to size 1 cells */
	for (i = 1; i <= array_size; i++) {
	  el_op = ralloc(1);
	  /* initialize to 0 */
	    el_op->obj_component[0] = 0;
	  op -> obj_component[i] = (word) el_op;
	}
    return(op);
}

/* Array_New: Fast version for composite data     */
/* Works only for typical element type            */
MkIP(fast_pArray_New(array_size))
register long array_size;
{
register int i;
struct obj *op;    /* var array[...] */
register struct obj * el_op;     /* pointer to current element variable */

    /* allocate new array */
	op = ralloc_comp(array_size + 1);
    /* Fill in size */
	op -> obj_component[0] = array_size;
    /* Fill in array with pointers to size 1 cells */
	for (i = 1; i <= array_size; i++) {
	  el_op = ralloc_comp(1);
	  /* initialize to invalid pointer */
	    el_op->obj_component[0] = UNINIT;
	  op -> obj_component[i] = (word) el_op;
	}
    return(op);
}

/* Array_New: Fast version for atomic data     */
/* and contiguous allocation.                  */
/* Works only for typical element type.        */
/* Assumes that components can live only as    */
/* long as the whole array.                    */
MkIP(contig_Array_New(array_size))
register long array_size;
{
register int i;
struct obj *op;    /* var array[...] */
register struct obj * el_op;     /* pointer to current element variable */

    /* allocate new array & component variables in one chunk */
	op = ralloc((array_size << 1) + 1);
    /* Fill in size */
	op -> obj_component[0] = array_size;
    /* Fill in array itself with pointers to "components" */
	for (i = 1; i <= array_size; i++) {
	  op -> obj_component[i]
		= (word)(&(op -> obj_component[i+array_size]));
	}
    return(op);
}

/* Array_New: Fast version for composite data     */
/* and contiguous allocation.                     */
/* Works only for typical element type.           */
/* Assumes that components can live only as       */
/* long as the whole array.                       */
MkIP(contig_pArray_New(array_size))
register long array_size;
{
register int i;
struct obj *op;    /* var array[...] */
register struct obj * el_op;     /* pointer to current element variable */
register long size = (array_size << 1) +1;

    /* allocate new array & component variables in one chunk */
	op = ralloc_comp(size);
    /* Fill in size */
	op -> obj_component[0] = array_size;
    /* Fill in array itself with pointers to "components" */
	for (i = 1; i <= array_size; i++) {
	  op -> obj_component[i]
		= (word)(&(op -> obj_component[i+array_size]));
	}
    /* Fill in components */
#       if UNINIT
	  for (i = array_size+1; i <= size; i++) {
	    op -> obj_component[i] = (word)(UNINIT);
	  }
#       endif
    /* 0 is a bad pointer, so components don't need to be filled in */
    return(op);
}


MkIP(Array_Subscript(A))
struct ar * A;
{
    struct obj *array_var, *index;

    array_var = A -> ar_arg1;
    index = A -> ar_arg2;
    if( (short)index < 0 
	|| (short)index >= array_var -> obj_component[0] ) {
	Array_error((short)index);
    }
    return((struct obj *) (array_var -> obj_component[((short)index) + 1]));
}

Array_error(index)
short index;
{
    fprintf(stderr, "Subscript %d out of range\n", index);
    print_tr_stack();
    ABORT("Subscript out of range");
}

MkIP(Array_size(A))
struct ar * A;
{
    return((struct obj *)(A -> ar_static_link));
}

# define NFUNCS 5   /* Number of functions in the array type */
# define ENV_SIZE 2 /* Size of pseudo-environment            */

MkIP(Array(size,type))
struct obj *size, *type;
{
    register int i;
    struct obj *op; /* type {.,.,New,V} */ /* needs to be somewhere where the */
					   /* garbage collector can find it   */
    register struct obj *p;
    struct obj *envp = (struct obj *) 0; 
			/* pointer to pseudo-environment containing size */
			/* and type.                                     */

    /* allocate type object */
        op = ralloc_comp(NFUNCS);

    /* Check size */
	if (((long) size) < 0) {
	    russell_error("Array can't have negative size\n");
	}

    /* allocate and initialize pseudo-environment object */
	envp = ralloc_comp(ENV_SIZE);
	envp->obj_component[0] = (word) size;
	envp->obj_component[1] = (word) type;

    /* allocate function objects and put them into array type */
	for (i = 0; i < NFUNCS; i++) {
	    p = ralloc_comp(sizeof(struct funcobj)/sizeof(word));
	    switch (i) {
		case 0:
		case 1: /* subscription */
		  ((struct funcobj *) p) -> fo_arlgth = 3;
		  ((struct funcobj *) p) -> fo_ep = envp;
		  ((struct funcobj *) p) -> fo_ip = Array_Subscript;
		  break;
		case 2: /* New */
		  ((struct funcobj *) p) -> fo_arlgth = 1;
		  ((struct funcobj *) p) -> fo_ep = envp;
		  ((struct funcobj *) p) -> fo_ip = Array_New;
		  break;
		case 3: /* ValueOf */  
		  ((struct funcobj *) p) -> fo_arlgth = 2;
		  ((struct funcobj *) p) -> fo_ep = envp;
		  ((struct funcobj *) p) -> fo_ip = Array_ValueOf;
		  break;
		case 4: /* size */
		  ((struct funcobj *) p) -> fo_arlgth = 1;
		  ((struct funcobj *) p) -> fo_ep = size;
		  ((struct funcobj *) p) -> fo_ip = Array_size;
		break;
	    }
	    op -> obj_component[i] = (word)p;
	}
    return(op);
}

MkFVAL2(Array);
