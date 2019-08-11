/*
 * Print format table generation macros
 *
 * The codes are:
 *	   code   print as
 *		a		address
 *		c		character string
 *		i		integer
 *		l		virtual line number
 *		p		pointer (follow it down)
 *		s		string table offset
 *		u		unsigned integer
 *		v		bit vector
 *		x		don't print
 */

# define SIGDEBUG SIGDEBUG
# undef SIGDEBUG

# define TOP \
    char *typedescr[] = {

# define START(x,name) \
    "name","

# define INT(x) i
# define UNSIGNED(x) u
# define NODESTAR(x) p
# ifdef SIGDEBUG
#         define HNODESTAR(x) p
#         define SIG(x) p
#         define HSIG(x) p
# else
#         define HNODESTAR(a) x
#         define SIG(a) x
#         define HSIG(a) x
# endif
# define REFCNT(a) x
# define NODEKIND(a) x
# define VLINENO(x) l
# define CNSTAR(a) x
# define STTINDX(x) s
# define LISTPTR(a) p
# define HLISTPTR(a) p
# define LBACKREF(a) x
# define BACKREF(x) a
# define STRPTR(a) c
# define HSTRPTR(a) c
# define BITVECTOR(a) v
# define NBPTR(n) a

# define FINISH ",

# define BOTTOM };
