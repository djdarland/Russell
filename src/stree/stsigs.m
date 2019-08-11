# define NOTYPEDEFS

# include "../parm.h"

/*
 *  stsigs declaration macros for use with streedefs.h.
 *  Stsigs is an array of bit vectors defining the
 *  signature pointers in the syntax tree.  Note that only pointers
 *  from denotation nodes to signatures filled in by
 *  the deduction pass are represented.
 */

# define TOP unsigned stsigs[] = {

# define START(x,y) 0 \
#	set SHIFT WORDLENGTH \
#	decr SHIFT

# define DECSHIFT \
#   decr SHIFT

# define INT(fld) DECSHIFT
# define UNSIGNED(fld) DECSHIFT
# define SIG(fld) +(1<<SHIFT) DECSHIFT
# define HSIG(fld) SIG(fld)
# define NODESTAR(fld) DECSHIFT
# define HNODESTAR(fld) NODESTAR(fld)
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
