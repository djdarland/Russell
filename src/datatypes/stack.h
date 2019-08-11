/* These are structure declarations and macro definitions for a traversable */
/* stack with marks */

/* These use the routines in consnodes.c and the macros in consnodes.h */

typedef ConsNode * STACK;
						 
typedef ConsNode * MARK;

# define push(x,S) (S) = cn_cons((x),(S))

# define pop(S) (S) = cn_del_hd((S))

# define top(S) cn_head((S))

# define is_stempty(S) cn_null((S))

# define emptystack() NIL

# define mark(S) (S)

# define is_top_marked(S,mark) ((S) == (mark))

/* The following defines a loop construct to traverse a stack, starting */
/* with the top */

# define st_foreach(var,stack)  \
    {   STACK st__0O_tmp; /* how's that for an obscure variable name ? */\
        for ( st__0O_tmp = (stack);  \
              ((var) = top(st__0O_tmp), !is_stempty(st__0O_tmp)); \
              st__0O_tmp = cn_tail(st__0O_tmp) ) {

# define end_foreach  \
        }             \
    }



