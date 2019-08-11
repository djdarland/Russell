/* Function definitions used by compiler in building type constructions */

/* Product values for products with n components are represented by blocks */
/* of length n.  They are never changed.  Product variables are blocks of  */
/* size 1 which point to a product value.  Product variables are           */
/* initialized to a guaranteed bad pointer, so that selection form such an */
/* object will produce an error.                                           */
/* (Note that the garbage collector must not follow such bad pointers.)    */

/* Union values have size 2.  The first entry is the component, and the    */
/* second is a tag.  Union variables are represented exactly like product  */
/* variables.                                                              */

/* Enumeration values are represented as 16 bit unsigned integers.         */
/* Enumeration variables are pointers to locations holding such values.    */

#include "types.h"

#define UNION_VAL 0
#define UNION_TAG 1

/* Get the ith component of a structured object. The index i is stored as ep. */
/* Used for products and records.                                             */
struct obj *
P_R_ith(A)
struct ar * A;
{
    return((struct obj *)
	   (A -> ar_arg1 -> obj_component[(long)(A -> ar_static_link)]));
}

/* Allocate an object of size i.  The size is represented as the ep. */
/* The initial values are given by arguments to the function.        */
/* (The number of arguments must equal i.)                           */
struct obj *
P_R_Make(A)
struct ar * A;
{
register int size;
struct obj *op;
register struct obj **arg;
int j;

    /* get size, arguments */
	size = (long)(A -> ar_static_link);
	arg = &(A -> ar_arg1);
    op = ralloc_comp(size);
    /* initialize fields to arguments */
	for (j = 0; j < size; j++) {
	  op -> obj_component[j] = (word) (arg[j]);
	}
    return(op);
}

/* Allocate an object that can hold one pointer */
struct obj *
P_U_New()
{
register struct obj *op;    /* var prod { ... } */

    op = ralloc_comp(1);
    op->obj_component[0] = UNINIT;
    return(op);
}

/* Allocate an object that can hold one enumeration value */
#define ENUM_SIZE 1
struct obj *
E_New()
{
register struct obj *op;    /* var enum { ... } */

    op = ralloc(ENUM_SIZE);
    op->obj_component[0] = (word) 0;
    return(op);
}

/* Pointer or enumeration value assignment operation */
struct obj *
PUE_Assign(lop,rop)
struct obj *lop;    /* var Prod { ... } */
struct obj *rop;    /* val Prod { ... } */
{
    lop->obj_component[0] = (word)rop;
    return(rop);
}

MkCF2(PUE_Assign)

/* Get the value stored in product, enumeration, or union variable */
struct obj *
PUE_ValueOf(op)
struct obj *op;    /* var Prod { ... } */
{
    return((struct obj *)(op->obj_component[0]));
}

MkCF1(PUE_ValueOf)

void union_err();

/* Check that the second component of arg is i, the value stored as ep.   */
/* If so, return the first.                                               */
struct obj *
Union_Proj(A)
struct ar * A;
{
    if (((word)(A -> ar_static_link))
	!= A -> ar_arg1 -> obj_component[UNION_TAG]) {
	union_err();
    }
    return ((struct obj *) (A -> ar_arg1 -> obj_component[UNION_VAL]));
}

void union_err()
{
    russell_error("Illegal projection from union type\n");
}

/* Make the argument into a union value.  The tag field is set to the ep */
/* value.                                                                */
# define UNION_SIZE 2
struct obj *
Union_Inj(A)
struct ar * A;
{
    register struct obj *op;    /* val union { ... } */

    /* allocate new union value */
        op = ralloc_comp(UNION_SIZE);

    /* set up value and tag */
	op -> obj_component[UNION_VAL] = (word) (A -> ar_arg1);
	op -> obj_component[UNION_TAG] = (word) (A -> ar_static_link);
    
    return (op);
}

/* Return true if the second component of arg is i, the value stored as ep. */
/* Otherwise return false.                                                  */
struct obj *
Union_Inq(A)
struct ar * A;
{
    return ((struct obj *) (((word)(A -> ar_static_link))
			    == A -> ar_arg1 -> obj_component[UNION_TAG]));
}

/* Return the enumeration constant represented by the value stored as ep */
struct obj *
Enum_Element(A)
struct ar * A;
{
    /* get component number from pseudo environment */
	return ( (struct obj *)(A -> ar_static_link) );
}

/* Enumeration equality and inequality tests */
struct obj *
Enum_eq(lop,rop)
struct obj *lop, *rop;
{
    return ((struct obj *)((unsigned)lop == (unsigned)rop));
}

MkCF2(Enum_eq)

struct obj *
Enum_ne(lop,rop)
struct obj *lop, *rop;
{
    return ((struct obj *)((unsigned)lop != (unsigned)rop));
}

MkCF2(Enum_ne)

/* Enumeration cardinality function.  Cardinality is stored as ep */
struct obj *
Enum_Card(A)
struct ar * A;
{
    return ((struct obj *) (A -> ar_static_link));
}

/* Enumeration successor and predecessor functions */
void
pred_error()
{
    russell_error("Can't take the predecessor of first element\n");
}

void
succ_error()
{
    russell_error("Can't take the successor of last element\n");
}

struct obj *
Enum_Pred(op)
struct obj *op;
{
    register int n = (unsigned long)op;
    if (n <= 0) pred_error();
    return ((struct obj *)(n - 1));
}

MkCF1(Enum_Pred)

struct obj *
Enum_Succ(A)
struct ar * A;
{
    register word n = (word)(A -> ar_arg1);

    if (n >= (int)(A -> ar_static_link) - 1) succ_error();
    return ((struct obj *)(((unsigned long)(A -> ar_arg1)) + 1));
}

/* Identity function - used for Ord and OrdInv of enumeration */
struct obj *
Identity(op)
struct obj *op;
{
    return(op);
}

MkCF1(Identity)

# define N_RECORD_FUNCTIONS 3   /* Number of component functions in */
                                /* pseudo-environment.              */
# define ASSIGN_OFFSET 0
# define NEW_OFFSET 1
# define VALUEOF_OFFSET 2

typedef struct obj * (* fn)();

/* New function for records. Calls the corresponding functions */
/* of the component types to allocate component variables.     */
/* The pseudo-environment pointer is assumed to point to a     */
/* a block of 3n longwords.  Each block of 3 lwords contains   */
/* pointers to the assignment, New and ValueOf operations      */
/* of a component type.                                        */
struct obj *
Record_New(A)
struct ar * A;
{
register struct obj *envptr; /* Pointer to vector of component   */
			     /* functions.                       */
register int i;
struct obj *op;    /* var record{...} */
int size;                   /* number of fields in record */

    /* get function vector */
	envptr = (struct obj *)(A -> ar_static_link);
	/* Compute size of the record from size of pseudo-environment */
	    size = GC_size(envptr)/(3 * BYTES_PER_WORD);
    /* allocate record object */
        op = ralloc_comp(size);
    /* initialize entries to component$New[] */
        for (i = 0; i < size; i++) {
          fn f = (fn)(envptr -> obj_component[NEW_OFFSET + 3*i]);
          if (f == 0) {
              /* Ran off the end of pseudo-environment */
              break;
          }
          op -> obj_component[i] = (word) call_russell0((struct obj *)f);                                          
        }
    return(op);
}


/* Record_ValueOf: func[var record { ... }] val record { ... } */

struct obj *
Record_ValueOf(A)
struct ar * A;
{
register struct obj *envptr; /* Pointer to vector of component       */
			     /* functions.                           */
struct obj * rec_var;
register int i;
struct obj *op;    /* var record{...} */
int size;                   /* number of fields in record */

    /* get argument */
	rec_var = A -> ar_arg1;
    /* get function vector */
	envptr = (struct obj *)(A -> ar_static_link);
        /* Compute size of the record form size of pseudo-environment */
	    size = GC_size(envptr)/(3 * BYTES_PER_WORD);
    /* allocate record object */
	op = ralloc_comp(size);
    /* initialize entries to component$ValueOf[component] */
        for (i = 0; i < size; i++) {
          fn f = (fn)(envptr -> obj_component[VALUEOF_OFFSET + 3*i]);
          if (f == 0) break;
          op -> obj_component[i] = 
             (word) call_russell1((struct obj *)f,
             		          rec_var -> obj_component[i]);
        }
    return(op);
}

/* Record_assign: func[var record { ... }; val record { ... }] */
/*                                          val record { ... } */

struct obj *
Record_Assign(A)
struct ar * A;
{
register struct obj *envptr; /* Pointer to vector of component */
			     /* functions.                     */
struct obj * rec_var;
struct obj * rec_val;
register int i;
struct obj *op;    /* var record{...} */
register struct obj **opp;
int size;                    /* number of fields in record */


    /* get arguments */
	rec_var = A -> ar_arg1;
	rec_val = A -> ar_arg2;
    /* get function vector */
	envptr = (struct obj *)(A -> ar_static_link);
        /* Compute size of the record form size of pseudo-environment */
	    size = GC_size(envptr)/(3 * BYTES_PER_WORD);
    /* apply component assignment operations to each component */
        for (i = 0; i < size; i++) {
          fn f = (fn)(envptr -> obj_component[ASSIGN_OFFSET + 3*i]);
          if (f == 0) break;
          (void) call_russell2((struct obj *)f,
                               rec_var -> obj_component[i],
                               rec_val -> obj_component[i]);
        }
    return(rec_val);
}

