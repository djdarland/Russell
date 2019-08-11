
/*  Structure definition and macros for manipulation of LISP style lists.
 */


# define ANYTHING  char
# define NIL 0

/*
 *  The consnode structure and its basic operations.
 */
typedef struct cn {
    struct cn * cn_tl_field;
    ANYTHING * cn_hd_field; } ConsNode;

# define cn_tail(l) ( (struct cn *) ((l) -> cn_tl_field) )

# define cn_head(l) ( (l) -> cn_hd_field )

# define is_null_cn(l) ( (l) == NIL )

/* The following macros should be used for assignment to the head and
 * tail fields of a consnode.
 */

# define cn_sethead(nodeptr,fvalue)  \
        ( (nodeptr) -> cn_hd_field = (ANYTHING *)(fvalue) )

# define cn_settail(nodeptr,list)  \
        ( (nodeptr) -> cn_tl_field = (list)  )

ConsNode * cn_cons();

ConsNode * cn_del_hd();
