# define NOTYPEDEFS

# include "../parm.h"

/*
 * stmkfields declaration macros for use with streedefs.h
 */

# define TOP unsigned stmkfields[] = {

# define START(x,y) 0 \
#	set SHIFT WORDLENGTH \
#	decr SHIFT

# define DECSHIFT \
#	decr SHIFT

# define SETBIT  +(1<<SHIFT) DECSHIFT

# define INT(fld) DECSHIFT
# define UNSIGNED(fld) SETBIT
# define NODESTAR(fld) SETBIT
# define SIG(fld) SETBIT
# define HNODESTAR(fld) DECSHIFT
# define HSIG(fld) DECSHIFT
# define CNSTAR(fld) SETBIT
# define REFCNT(fld) DECSHIFT
# define NODEKIND(fld) DECSHIFT
# define VLINENO(fld) DECSHIFT
# define STTINDX(fld) SETBIT
# define LISTPTR(fld) SETBIT
# define HLISTPTR(fld) DECSHIFT
# define LBACKREF(fld) DECSHIFT
# define BACKREF(fld) DECSHIFT
# define BITVECTOR(fld) SETBIT
# define STRPTR(fld) SETBIT
# define HSTRPTR(fld) DECSHIFT
# define NBPTR(fld) SETBIT

# define FINISH ,

# define BOTTOM };

# include "streedefs.h"
