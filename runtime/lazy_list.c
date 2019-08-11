
/**/
/* Lazy list or "stream" data type.  This implements a function:
/*
/*          List: func[t:type{}] type l {
/*                      New; :=; V;
/*                      '' : func[] val l (*empty list*);
/*                      is_nil : func[val l] val Boolean (* is empty *);
/*                      cons: func[val t; val l] val l;
/*                                  (* add at left end *);
/*                      cons: func[val t; func[] val l] val l;
/*                      head: func[val t] val l (* first element *);
/*                      tail: func[val l] val l (* all but first *);
/*                 }
/* The second version of cons is lazy in its second argument, i.e.
/* it builds a list which conceptually ends in the list computed
/* by the second argument.  The function is only evaluated when that
/* portion of the list is accessed.
/**/

#include "types.h"

extern struct obj * global_ar;

/*
 * A list variable is a pointer to a 1-word cell that contains a list value.
 * A list value is represented as in the List type, except that a third field
 * is added to indicate whether the tail still needs to be evaluated.
 * Many operations from the simple List type are reused here.
 */

struct llnode {
    struct llnode * ll_tail;
    struct obj * ll_head;
    int ll_tag;
#       define EVALD -1
#       define UN_EVALD 0  /* positive if single ref to tail and size known. */
};

#define LLISTVARSZ 1
#define LLISTVALSZ (sizeof (struct llnode) / sizeof (word))

#define MAX_STACK_AR 16


/*  LList_Cons: func[x: val Element; y: val LList] val LList */

MkIP(LList_Cons(opx,opy))
struct obj *opx;    /* val Element */
struct obj *opy;    /* val LList */
{
struct obj *op;         /* var List */

    op = ralloc_comp(LLISTVALSZ);
    ((struct llnode *)op) -> ll_head = (struct obj *)opx;
    ((struct llnode *)op) -> ll_tail = (struct llnode *)opy;
    ((struct llnode *)op) -> ll_tag = (word)EVALD;
    return(op);
}

MkFVAL2(LList_Cons);


/*  LList_LCons: func[x: val Element; y: func [] val LList] val LList */

MkIP(LList_LCons(opx,opy))
struct obj *opx;    /* val Element */
struct obj *opy;    /* val LList */
{
register struct obj *op;         /* var List */
    
    op = ralloc_comp(LLISTVALSZ);
    ((struct llnode *)op) -> ll_head = (struct obj *)opx;
    ((struct llnode *)op) -> ll_tail = (struct llnode *)opy;
    ((struct llnode *)op) -> ll_tag = (word)UN_EVALD;
    return(op);
}

MkFVAL2(LList_LCons);


extern void list_error();

/*  List_Tail: func[Val LList] val LList */
/*  First a version that assumes an unevaluated, non-nil operand and */
/* evaluates it, returning nothing.                                  */

void LList_Tail1(op)
struct obj *op;     /* val LList */
{
    register struct llnode * Rop = (struct llnode *)op;
    register struct funcobj *f;
    register struct ar * ar_ptr;
    register int len;
    register int tag = Rop -> ll_tag;
    word ar_block[MAX_STACK_AR];

    f = (struct funcobj *)(Rop -> ll_tail);
    if (((unsigned)(len = f -> fo_arlgth)) > MAX_STACK_AR) {
	/* either negative, i.e. requires heap, or too big */
	    if ( len < 0 ) { len = -len; }
	    ar_ptr = (struct ar *)ralloc_comp(len);
    } else {
	ar_ptr = (struct ar *)ar_block;
    }
    ar_ptr -> ar_static_link = (struct ar *)(f -> fo_ep);
    Rop -> ll_tail = (struct llnode *)((*(f -> fo_ip))(ar_ptr));
    if (tag > 0 /* Single reference, size known */) {
      /* Deallocate the closure */
      rfree(f);
    }
    Rop -> ll_tag = EVALD;
}

/* And now the general version: */

MkIP(LList_Tail(op))
struct obj *op;     /* val LList */
{
    register struct llnode * Rop = (struct llnode *)op;

    if (Rop == (struct llnode *)UNINIT) {
        list_error();
    }
    if (Rop -> ll_tag >= 0 /* UNEVALD */) {
	LList_Tail1((struct obj *)Rop);
    }
    return((struct obj *)(Rop -> ll_tail));
}

MkFVAL1(LList_Tail);


/*  LListT - the type value */

extern struct funcobj FVAL(List_Nil);
extern struct funcobj FVAL(List_Assign);
extern struct funcobj FVAL(List_New);
extern struct funcobj FVAL(List_ValueOf);
extern struct funcobj FVAL(List_Head);
extern struct funcobj FVAL(List_Is_nil);

MkTVAL(LListT) = {
    &FVAL(List_Nil),
    &FVAL(List_Assign),
    &FVAL(List_New),
    &FVAL(List_ValueOf),
    &FVAL(LList_Cons),
    &FVAL(LList_LCons),
    &FVAL(List_Head),
    &FVAL(List_Is_nil),
    &FVAL(LList_Tail),
};


/* LList: func [T: type{}] type ... */

MkIP(LList(T))
struct obj * T; /* type {} */
{
    return((struct obj *) LListT);
}

MkFVAL1(LList);
