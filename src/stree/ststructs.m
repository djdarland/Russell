/*
 *  Structure declaration macros for use with streedefs.h
 *  and basic structure manipulation primitives.
 */

@ include "datatypes/consnodes.h"

@ define MAXFIELDS 20 /* maximum number of fields in a node */
 
# define TOP \
#   define COUNT 0 \
typedef union Node {

# define START(knm,snm) \
  struct { \
@   define knm COUNT \
#   incr COUNT \
#   define STRUCTURENM snm

# define FIELD(type, nm) \
   type STRUCTURENM^_^nm; \
@ ifndef nm \
@   define nm STRUCTURENM . STRUCTURENM^_^nm \
@ endif @@

/* A leading H indicates the field is hidden, and not a parameter to mknode */
# define INT(fld) FIELD(int,fld)
# define UNSIGNED(fld) FIELD(unsigned,fld)
# define REFCNT(fld) FIELD(unsigned,fld)
# define NODEKIND(fld) FIELD(int,fld)
# define NODESTAR(fld) FIELD(union Node *,fld)
# define HNODESTAR(fld) FIELD(union Node *,fld)
# define SIG(fld) FIELD(union Node *,fld)
# define HSIG(fld) FIELD(union Node *,fld)
# define VLINENO(fld) UNSIGNED(fld)
# define STTINDX(fld) UNSIGNED(fld)
# define LISTPTR(fld) NODESTAR(fld)
# define HLISTPTR(fld) NODESTAR(fld)
# define LBACKREF(fld) NODESTAR(fld)
# define BACKREF(fld) NODESTAR(fld)
# define CNSTAR(fld) FIELD(ConsNode *,fld)
# define STRPTR(fld) FIELD(char *,fld)
# define HSTRPTR(fld) FIELD(char *,fld)
# define BITVECTOR(fld) UNSIGNED(fld)
# define NBPTR(fld) NODESTAR(fld)

# define FINISH } STRUCTURENM;

# define BOTTOM \
# decr COUNT \
    char Node_dummy[10000000];	/* Make anything that relies on  */ \
				/* sizeof(NODE) break.		*/ \
} NODE; \
@ define LASTKINDVALUE COUNT \


@define NIL 0
@define LIST NODE *

# include "streedefs.h"

/* definitions for sig_done field (used by pass 4) */
@   define SIG_UNKNOWN 0
@   define SIG_IN_PROGRESS 1
@   define SIG_DONE 2

/* definitions for fsig_special field (used for code generation) */
@   define NOT_SPECIAL 0
@   define PROD_NEW 1
@   define PROD_ASSIGN 2
@   define PROD_VALUEOF 3
@   define PROD_MK 4
@   define PROD_PROJ 5
@   define UNION_NEW 6
@   define UNION_ASSIGN 7
@   define UNION_VALUEOF 8
@   define UNION_INJ 9
@   define UNION_PROJ 10
@   define UNION_INQ 11
@   define RECORD_NEW 12
@   define RECORD_ASSIGN 13
@   define RECORD_VALUEOF 14
@   define RECORD_MK 15
@   define RECORD_VAL_FIELD 16
@   define RECORD_VAR_FIELD 17
@   define ENUM_NEW 18
@   define ENUM_ASSIGN 19
@   define ENUM_VALUEOF 20
@   define ENUM_EQ 21
@   define ENUM_NE 22
@   define ENUM_ELEMENT 23   /* Also used for First, Last */
@   define ENUM_CARD 24
@   define ENUM_PRED 25
@   define ENUM_SUCC 26
@   define IDENTITY 27        /* Used for extension, Ord and OrdInv */
@   define STD_ASSIGN 28    /* Value is # of longwords to copy */
@   define STD_NEW 29       /* Value is # of longwords to allocate */
@   define STD_VALUEOF 30   /* Value is # of longwords to allocate */
@   define STD_PUT 31   /* Not impure, put writes */
@   define STD_CALLCC 32  /* Callcc, all bets are off */
@   define STD_ARRAY 33   /* Behaves like builtin array function */
@   define ARRAY_STD_NEW 34  /* Array New function, initialized to 0's   */
			     /* Value is size of Array, 0 if unknown     */
@   define ARRAY_PTR_NEW 35  /* Array New function, initialized to undef */
@   define ARRAY_VALUEOF 36  /* Array Valueof, components var's are ptrs */
			     /* to size 1 objects.                       */
@   define ARRAY_SIZE 37
@   define ARRAY_VAL_SUB 38
@   define ARRAY_VAR_SUB 39
@   define PTR_NEW 40        /* Works like PROD_NEW, etc */
@   define INIT_NEW 41       /* New with an argument for initialization */
@   define OTHER_BUILTIN 42  /* Known to be builtin, not PUT ot CALLCC */
@   define STD_PASSIGN 43    /* Behaves like Short$+= */
@   define STD_MASSIGN 44    /* Behaves like Short$-= */
@   define STD_TASSIGN 45    /* Behaves like Short$*= */
@   define UNDEF_CONST 46    /* Returns the value UNINIT */
@   define UNION_INJ0  47    /* Union inject without argument */

@   define special_tp(x)  (((unsigned long)(x)) >> 26)
@   define special_val(x)  ((x) & 0x3ffffff)

@   define MAX_SP_VAL 0x3ffffff

@ ifdef DEBUG
@   define special(tp,val)  ((val) > 0x3ffffff? \
			     printf("Compiler error - prod too big\n") : \
			     ((tp) << 26) | (val) )
@ else
@   define special(tp,val)  (((tp) << 26) | (val))
@ endif

@define is_good(l) ((l) == NIL || \
     ((unsigned) ((l)->kind)) <= LASTKINDVALUE) /* is l possibly meaningful ? */
@define is_list(l) ((l) != NIL && \
                    (l)->kind == LISTHEADER)    /* is l a list? */
@define is_empty(l) ((l)->lh_first == NIL)      /* is l an empty list? */
@define is_singleref(v) ((v) -> refcount == 1)  /* is v only referenced once? */
@define is_refd(v) ((v) -> refcount)            /* is v referenced at all? */
@define first(l) ((NODE *)cn_head((l)->lh_first))       /* return first element of l */
@define second(l) ((NODE *)cn_head(cn_tail((l)->lh_first)))
                                                /* return second element of l */
@define last(l) ((NODE *)cn_head((l)->lh_last))         /* return last element of l */
@define emptylist() mknode(LISTHEADER,NIL,NIL)  /* make an empty list */
@define chgfld(pp, v) (*(pp) = (v))
@define initfld(pp, v) (*(pp) = (v))

void maprlist();                                /* defined in mknode.c */
NODE * mknode();							  /* defined in mknode.c */
NODE * copynode();
LIST mklist();
LIST copylist();
LIST addright();
LIST addleft();
LIST split();
LIST conc();

/* general list traversal */
/* macros - use only when */
/* nothing else will work */
/* Assumes the list l is  */
/* nonempty.              */
@define DECLARE_ITER ConsNode * mL__0O
@define INIT_ITER(v,l) mL__0O = (l) -> lh_first; v = (NODE *)cn_head(mL__0O)
@define NEXT_ITER(v)   \
    mL__0O = cn_tail(mL__0O); \
    v = (mL__0O == NIL? NIL: (NODE *) cn_head(mL__0O))

/* Run v through l executing */
/* stmt for every value of l */
@define maplist(v,l,stmt) \
{   register NODE * v; \
    register ConsNode * mL__0O;  /* an unreproducible identifier */ \
    for(mL__0O = ((l) -> lh_first); mL__0O != NIL; mL__0O = cn_tail(mL__0O) ) {\
        v = (NODE *) cn_head(mL__0O); \
        stmt; \
    } \
}

/* identical to maplist, except that v is a pointer to the pointer */
/* to the current node.  Allows easy replacement of list elements. */
@define maplistp(v,l,stmt) \
{   register NODE ** v; \
    register ConsNode * mL__0O;  /* an unreproducible identifier */ \
    for(mL__0O = ((l) -> lh_first); mL__0O != NIL; mL__0O = cn_tail(mL__0O) ) {\
	v = (NODE **)(& cn_head(mL__0O)); \
        stmt; \
    } \
}

/* Can be used inside the preceding macro to determine whether this is */
/* the last iteration                                                  */
@define LAST_ITER (cn_tail(mL__0O) == NIL)

						/* A two part version. Needed */
						/* since cpp chokes on long   */
						/* arguments.                 */
@define begin_maplist(v,l) \
{   register NODE * v; \
    register ConsNode * mL__0O;  /* an unreproducible identifier */ \
    for(mL__0O = ((l) -> lh_first); mL__0O != NIL; mL__0O = cn_tail(mL__0O) ) {\
	v = (NODE *) cn_head(mL__0O);

@define end_maplist \
    } \
}



						/* insert a new element into */
						/* the list being traversed  */
						/* by maplist.               */
						/* The element is inserted   */
						/* before the current element*/
						/* must be used with         */
						/* mapinslist                */
@define INSERT(v) \
lock(v); \
if (mL__0OP == NIL) { \
    mL__0OL -> lh_first = mL__0OP = cn_cons(v, mL__0OL -> lh_first); \
} else { \
    cn_settail(mL__0OP, cn_cons(v, mL__0O)); \
    mL__0OP = cn_tail(mL__0OP); \
} \
if (mL__0O == NIL) mL__0OL -> lh_last = mL__0OP;


						/* Replace the current list  */
						/* element by v              */
@define REPLACE(v) \
lock(v);  \
vfree(unlock((NODE *) cn_head(mL__0O))); \
cn_sethead(mL__0O, v);

						/* delete current element    */
						/* use with mapinslist       */
						/* No attempt is made to free*/
						/* cons nodes                */
@ifdef DEBUG
@ define DELETE \
    if (mL__0O == NIL) { \
	dbgmsg("Attempt to delete NIL\n"); \
	abort(); \
    } \
    vfree(unlock((NODE *) cn_head(mL__0O))); \
    if (mL__0OP == NIL) { \
	mL__0OL -> lh_first = cn_tail(mL__0O); \
    } else { \
	cn_settail(mL__0OP, cn_tail(mL__0O)); \
    } \
    if (mL__0OL -> lh_last == mL__0O) { \
	mL__0OL -> lh_last = mL__0OP; \
    } \
    mL__0Odeld = TRUE
@else
@  define DELETE \
    vfree(unlock((NODE *) cn_head(mL__0O))); \
    if (mL__0OP == NIL) { \
	mL__0OL -> lh_first = cn_tail(mL__0O); \
    } else { \
	cn_settail(mL__0OP, cn_tail(mL__0O)); \
    } \
    if (mL__0OL -> lh_last == mL__0O) { \
	mL__0OL -> lh_last = mL__0OP; \
    } \
    mL__0Odeld = TRUE
@endif

						/* Note that the following   */
						/* loop construct is intended*/
						/* only for insertions and   */
						/* deletions. It             */
						/* includes a final iteration*/
						/* with a NIL loop variable  */
						/* to allow insertions at the*/
						/* end.                      */
@define mapinslist(v,l,stmt) \
{   register NODE * v; \
    register ConsNode * mL__0O;  /* an unreproducible identifier */ \
    NODE * mL__0OL = (l); \
    ConsNode * mL__0OP = NIL; \
    boolean mL__0Odeld = FALSE; \
    mL__0O = ((l) -> lh_first); \
    do { \
	if (mL__0O != NIL) v = (NODE *) cn_head(mL__0O); else v = NIL; \
        stmt; \
	if(!mL__0Odeld) {mL__0OP = mL__0O;} else {mL__0Odeld = FALSE;} \
	if (mL__0O != NIL) mL__0O = cn_tail(mL__0O); \
    } while (mL__0OP != NIL); \
}

@define map2lists(v1,l1,v2,l2,stmt) \
{   register NODE * v1, * v2; \
	register ConsNode * mL__1O, * mL__2O;  /* unreproducible identifiers */ \
	for(mL__1O = ((l1) -> lh_first), mL__2O = ((l2) -> lh_first);\
		mL__1O != NIL && mL__2O != NIL;\
		mL__1O = cn_tail(mL__1O), mL__2O = cn_tail(mL__2O) ) {\
		v1 = (NODE *) cn_head(mL__1O); v2 = (NODE *) cn_head(mL__2O); \
        stmt; \
    } \
}

@define begin_map2lists(v1,l1,v2,l2) \
{   register NODE * v1, * v2; \
	register ConsNode * mL__1O, * mL__2O;  /* unreproducible identifiers */ \
	for(mL__1O = ((l1) -> lh_first), mL__2O = ((l2) -> lh_first);\
		mL__1O != NIL && mL__2O != NIL;\
		mL__1O = cn_tail(mL__1O), mL__2O = cn_tail(mL__2O) ) {\
		v1 = (NODE *) cn_head(mL__1O); v2 = (NODE *) cn_head(mL__2O);

@define end_map2lists \
    } \
}

/* check whether an identifier is declared by a given declaration node */
@define is_declared_by(id_node, def_node)   \
    ((id_node) -> id_last_definition -> pre_num == (def_node) -> pre_num)

