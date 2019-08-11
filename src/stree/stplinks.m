# define NOTYPEDEFS

# include "../parm.h"

/*
 * stplinks declaration macros for use with streedefs.h.
 *  Stplinks is an array of bit vectors defining the
 *  'primary links' in the syntax tree.  These are
 *  the links that need to tbe followed to traverse
 *  each part of the tree exactly once.
 */

# define TOP unsigned stplinks[] = {

# define START(x,y) 0 \
#	set SHIFT WORDLENGTH \
#	decr SHIFT

# define DECSHIFT \
#   decr SHIFT

# define INT(fld) DECSHIFT
# define UNSIGNED(fld) DECSHIFT
# define NODESTAR(fld) +(1<<SHIFT) DECSHIFT
# define SIG(fld) NODESTAR(fld)
# define HNODESTAR(fld) NODESTAR(fld)
# define HSIG(fld) HNODESTAR(fld)
# define CNSTAR(fld) DECSHIFT 
# define REFCNT(fld) DECSHIFT
# define NODEKIND(fld) DECSHIFT
# define VLINENO(fld) DECSHIFT
# define STTINDX(fld) DECSHIFT
# define LISTPTR(fld) NODESTAR(fld)
# define HLISTPTR(fld) NODESTAR(fld)
# define LBACKREF(fld) DECSHIFT
# define BACKREF(fld) DECSHIFT
# define BITVECTOR(fld) DECSHIFT
# define STRPTR(fld) DECSHIFT
# define HSTRPTR(fld) DECSHIFT
# define NBPTR(fld) DECSHIFT

# define FINISH ,

# define BOTTOM };

# include "streedefs.h"
