/*
 *  Russell Compiler parameter file
 */

#ifndef PARMH

# define PARMH
/* # include <whoami.h> */
# ifdef vax
#   define VAX
# endif
# ifdef pyr
#   define PYRAMID
# endif
# ifdef sun
#   define SUN
#   ifdef mc68000
#     define MACH_TYPE M_68020
#   else
#     define MACH_TYPE M_SPARC
#   endif
# endif
    /* This refers to the machine used for compilation, not necessarily */
    /* the target machine.                                              */

# define GEN_C   /* Use the portable C-based code generator */

# define EXTENDED_RANGE   /* VAX movo instruction available */
# undef EXTENDED_RANGE

#   define WORDLENGTH 32
#   define LOGWL 5
    /* This is assumed in several places, e.g. streedefs.h */
    /* and other files which mention DEFCHARSIGS nodes     */

/* The following functions were useful when we reference counted the */
/* syntax tree.  Now we let the garbage collector worry about it.    */
#   define salloc GC_malloc
#   define alloc GC_malloc
#   define malloc GC_malloc
#   define free GC_free

/* Translate the gc1.X functions to the 2.X ones: */
#   define gc_malloc GC_malloc
#   define gc_malloc_atomic GC_malloc_atomic

#   define vfree(x) (x)
#   define unlock(x) (x)
#   define lock(x) (x)

# define TABWIDTH 8
# define NINCR 7 /* maximum growth of in-line code as a result */
		 /* of substitution of constant name.          */
# define MAXSTRCODELEN 2000  /* maximum length of in-line assembly code */
			     /* for a string                            */
# define MAXSTRLEN  150 /* maximum length of string for inline expansion */

# define MAXINTLEN 4  /* length of longest Short which is guaranteed to */
		      /* be representable.                              */
# define GMAXINTLEN 9 /* Same, for intermediate code generator */

# define MAXNOBJFILES  100 /* Maximum number of object files which can be */
			   /* loaded at once.                             */

# define MAXNLIBRARIES 10 /* Maximum number of library arguments to ld */

/*
 *	Various global type definitions
 */
# ifndef NOTYPEDEFS
    typedef char boolean;
#   define TRUE    1
#   define FALSE   0
	/* We assume that booleans are initialized to FALSE */

    typedef char byte;
# endif

/*
 *	Tuneable constants
 */
# define STTNBLKS 64		/* Max number of blocks in string table. */

# define RCNAME (char *) strcat(RROOT,"/src/")
# define PPNAME "/lib/cpp"
# define OPTNAME (char *) strcat(RROOT,"/src/opt.awk")
# define RICFILTERNAME (char *) strcat(RROOT,"/src/RICfilter/filter")
# define RICOPTNAME (char *) strcat(RROOT,"/src/RICopt")
# ifdef GEN_C
#   define CGNAME (char *) strcat(RROOT, "/src/RIC_to_C/to_C")
# else
#   define CGNAME (char *) strcat(RROOT, "/src/cg")
# endif

# define OBJFILELIST ".objfilelist"



/* debugging error messages are output using dbgmsg */
# define dbgmsg   printf("\n***Compiler Error - ");printf

/* diagnostic debugging messages are output using diagmsg */
/* SHOULD DISAPPEAR */
# define diagmsg  printf

/* specialized error output routines */
/* parameters can be augmented by err_node and err_msg */
# ifdef ERR_NODE_DEFINED
#   define errmsg0(p,fmt) \
    { \
      if (err_node != NIL) { \
	findvl(err_node -> vlineno); \
	fprintf(stderr, err_msg); \
	fprintf(stderr, " (file: %s, line: %d)\n", getname(getfn()), getrl()); \
	fprintf(stderr, " - Detailed error:\n"); \
      } \
      findvl(p -> vlineno); \
      fprintf(stderr, fmt); \
      fprintf(stderr, " (file: %s, line: %d)\n", getname(getfn()), getrl()); \
      yynerrs++; \
    }
# else
#   define errmsg0(p,fmt) \
    { \
      findvl(p -> vlineno); \
      fprintf(stderr, fmt); \
      fprintf(stderr, " (file: %s, line: %d)\n", getname(getfn()), getrl()); \
      yynerrs++; \
    }
# endif

# ifdef ERR_NODE_DEFINED
#   define errmsg1(p,fmt,arg) \
    {  \
      if (err_node != NIL) { \
	findvl(err_node -> vlineno); \
	fprintf(stderr, err_msg); \
	fprintf(stderr, " (file: %s, line: %d)\n", getname(getfn()), getrl()); \
	fprintf(stderr, " - Detailed error:\n"); \
      } \
      findvl(p -> vlineno); \
      fprintf(stderr, fmt, arg); \
      fprintf(stderr, " (file: %s, line: %d)\n", getname(getfn()), getrl()); \
      yynerrs++; \
    }
# else
#   define errmsg1(p,fmt,arg) \
    {  \
      findvl(p -> vlineno); \
      fprintf(stderr, fmt, arg); \
      fprintf(stderr, " (file: %s, line: %d)\n", getname(getfn()), getrl()); \
      yynerrs++; \
    }
# endif

/* turn off miscellaneous debugging      */

/* to turn on allocation debugging define BAD here */

#endif PARMH
