/*
 * size table definition macros for use with streedefs.h
 */

# define TOP int stsize[] = {

# define START(x,y) \
#   define SIZE 0

# define INT(fld) \
#   incr SIZE
# define UNSIGNED(fld) \
#   incr SIZE
# define REFCNT(fld) \
#   incr SIZE
# define NODEKIND(fld) \
#   incr SIZE
# define NODESTAR(fld) \
#	incr SIZE
# define HNODESTAR(fld) \
#       incr SIZE
# define SIG(fld) \
#       incr SIZE
# define HSIG(fld) \
#       incr SIZE
# define CNSTAR \
#   incr SIZE
# define VLINENO(fld) \
    UNSIGNED(fld)
# define STTINDX(fld) \
#   incr SIZE
# define LISTPTR(fld) \
#   incr SIZE
# define HLISTPTR(fld) \
#   incr SIZE
# define LBACKREF(fld) \
#   incr SIZE
# define BACKREF(fld) \
#	incr SIZE
# define STRPTR(fld) \
#	incr SIZE
# define HSTRPTR(fld) \
#	incr SIZE
# define BITVECTOR(fld) \
#	incr SIZE
# define NBPTR(fld) \
	NODESTAR(fld)

# define FINISH SIZE,

# define BOTTOM };

# include "streedefs.h"
