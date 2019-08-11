# define NOTYPEDEFS

# include "../parm.h"

/*
 * stptrs declaration macros for use with streedefs.h
 *  declares an array of bit vectors identifying those fields
 *  which are considered to be pointers for reference count
 *  purposes.
 */

# define TOP unsigned stptrs[] = {

# define START(x,y) 0 \
#	set SHIFT WORDLENGTH \
#	decr SHIFT

# define DECSHIFT \
#   decr SHIFT

# define INT(fld) DECSHIFT
# define UNSIGNED(fld) DECSHIFT
# define NODESTAR(fld) +(1<<SHIFT) DECSHIFT
# define HNODESTAR(fld) NODESTAR(fld)
# define SIG(fld) NODESTAR(fld)
# define HSIG(fld) NODESTAR(fld)
# define CNSTAR(fld) DECSHIFT
# define REFCNT(fld) DECSHIFT
# define NODEKIND(fld) DECSHIFT
# define VLINENO(fld) DECSHIFT
# define STTINDX(fld) DECSHIFT
# define LISTPTR(fld) NODESTAR(fld)
# define HLISTPTR(fld) DECSHIFT
# define LBACKREF(fld) DECSHIFT
# define BACKREF(fld) DECSHIFT
# define BITVECTOR(fld) DECSHIFT
# define STRPTR(fld) DECSHIFT
# define HSTRPTR(fld) DECSHIFT
# define NBPTR(fld) NODESTAR(fld)

# define FINISH ,

# define BOTTOM };

# include "streedefs.h"
