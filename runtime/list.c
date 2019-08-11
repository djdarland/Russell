/**/
/* List data type.  This implements a function:
/*
/*          List: func[t:type{}] type l {
/*                      New; :=; V;
/*                      '' : func[] val l (*empty list*);
/*                      is_nil : func[val l] val Boolean (* is empty *);
/*                      ^* : func[val l; val t] val l;
/*                                  (* cons, with args reversed *);
/*                      cons: func[val t; val l] val l;
/*                                  (* add at left end *);
/*                      head: func[val t] val l (* first element *);
/*                      tail: func[val l] val l (* all but first *);
/*                 }
/**/

#include "types.h"

/*
 * A list variable is a pointer to a 1-word cell that contains a list value.
 * A list value is represented as in LISP.  The 0th component is the tail,
 * The 1st component is the head.
 */

#define LISTVARSZ 1
#define LISTVALSZ 2

# ifdef DEBUG
    extern struct obj * global_ar;   /* Debugging only */
    extern char end;
# endif

/* List_New: func[] var List */

MkIP(List_New())
{
register struct obj *op;    /* var List */

    op = ralloc_comp(LISTVARSZ);
    op->obj_component[0] = (word) UNINIT;
    return(op);
}

MkFVAL0(List_New);


/* List_Assign: func[var List; val List] val List */

MkIP(List_Assign(lop,rop))
struct obj *lop;    /* var List */
struct obj *rop;    /* val List */
{
    lop->obj_component[0] = (word)rop;
    return(rop);
}

MkFVAL2(List_Assign);


/* List_ValueOf: func[var List] val List */

MkIP(List_ValueOf(aop))
struct obj *aop;    /* var List */
{
register unsigned gcmask;   /* MUST BE r11 */

    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(List_ValueOf);


/*  List_Cons: func[x: val Element; y: val List] val List */

MkIP(List_Cons(opx,opy))
struct obj *opx;    /* val Element */
struct obj *opy;    /* val List */
{
register struct obj *op;         /* var List */

    op = ralloc_comp(LISTVALSZ);
    op->obj_component[0] = (word)opy;
    op->obj_component[1] = (word)opx;
    return(op);
}

MkFVAL2(List_Cons);


/*  List_RevCons: func[x: val list; y: val Element] val List */

MkIP(List_RevCons(opx,opy))
struct obj *opx;    /* val List */
struct obj *opy;    /* val Element */
{
register struct obj *op;         /* var List */

    op = ralloc_comp(LISTVALSZ);
    op->obj_component[0] = (word)opx;
    op->obj_component[1] = (word)opy;
    return(op);
}

MkFVAL2(List_RevCons);


list_error()
{
    ERRMSG("Attempt to take head or tail of empty list");
    print_tr_stack();
    ABORT("Attempt to take head or tail of empty list");
}

/*  List_Head: func[Val List] val List */

MkIP(List_Head(op))
register struct obj *op;     /* val List */
{
    if (op == (struct obj *)UNINIT) {
        list_error();
    }
    return( (struct obj *)(op->obj_component[1]));
}

MkFVAL1(List_Head);


/*  List_Tail: func[Val List] val List */

MkIP(List_Tail(op))
register struct obj *op;     /* val List */
{
    if (op == (struct obj *)UNINIT) {
        list_error();
    }
    return((struct obj *)(op->obj_component[0]));
}

MkFVAL1(List_Tail);


/* List_Is_nil: func[val List] val Bool */

MkIP(List_Is_nil(opx))
struct obj *opx;    /* val List */
{
    return( (struct obj *)
	( opx == (struct obj *)UNINIT )
    );
}

MkFVAL1(List_Is_nil);


/* List_Nil: func[] val List */

MkIP(List_Nil())
{
    return( (struct obj *)UNINIT );
}

MkFVAL0(List_Nil);


/*  ListT - the type value */

MkTVAL(ListT) = {
    &FVAL(List_Nil),
    &FVAL(List_Assign),
    &FVAL(List_New),
    &FVAL(List_ValueOf),
    &FVAL(List_RevCons),
    &FVAL(List_Cons),
    &FVAL(List_Head),
    &FVAL(List_Is_nil),
    &FVAL(List_Tail),
};


/* List: func [T: type{}] type ... */

MkIP(List(T))
struct obj * T; /* type {} */
{
    return((struct obj *) ListT);
}

MkFVAL1(List);
