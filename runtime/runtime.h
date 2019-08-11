/*********************************/
/*                               */
/*  Russell Runtime Environment  */
/*                               */
/*********************************/

#define _RUNTIME_

#include "../src/gc/gc.h"

#ifdef M68K_SUN
#   define M68K
	     /* Doesn't run on an HP machine yet. */
#endif

/* bad address, known to code generator.  Should be cheap to load */
/* into a register.                                               */

#define UNINIT (0x0)

/* allocation routines */

# define BYTES_PER_WORD (sizeof(word))
# define BYTES_TO_WORDS(n) ((n)/BYTES_PER_WORD)
# define WORDSZ 32
# define LOGWL 5

/* Object type:  This no longer makes sense.  History ... */
struct obj {
    word obj_component[1];  /* treats obj as list of words */
};

# define ralloc(n) (struct obj *)GC_malloc_atomic(BYTES_PER_WORD*(n))
# define ralloc_comp(n) (struct obj *)GC_malloc(BYTES_PER_WORD*(n))
# define rfree(p) GC_free(p)

/* error handling */
# ifdef PCR
#   define ERRMSG(s) XR_ConsoleMsg(s); XR_ConsoleMsg("\n")
#   define MSG(s) XR_ConsoleMsg(s); XR_ConsoleMsg("\n")
#   define FLUSH_MSG
#   define ABORT(s) XR_CallDebugger(s)
# else
#   include <stdio.h>
#   define ERRMSG(s) fputs(stderr,s); fputs(stderr,"\n")
#   define MSG(s) puts(s)
#   define FLUSH_MSG fflush(stdout)
#   define ABORT(s) abort(s)
# endif

/*  function-valued objects */

struct funcobj {
    int fo_arlgth;      /* Act. record length.  Negative if heap a.r. may be */
			/* required.                                         */
    struct obj *fo_ep;
    struct obj *((*fo_ip)());
};

char * startup_sfp; /* Frame pointer for Russell startup routine */

extern int trace_flag;

