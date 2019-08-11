#include "runtime.h"
#include "types.h"
#include <signal.h>
#include <stdio.h>

/**/
/* Russell startup routine
/**/

/* tables defining initial environment */

#  include "initenv.cpp"

char **g_argv;

int trace_flag;

struct obj * global_ar = 0;   /* Pointer to global activation record */

/* Temporary replacement */
void print_tr_stack() {}

/* Dummy procedure called to convince the compiler that certain variables */
/* are live.                                                              */
void no_op(x)
{
}

/* Initialize memory allocation if necessary, allocate an initial */
/* activation record of size sz, and return a pointer to it.      */

char **xargv;  /* Global names for argc and argv used by f77 */
long xargc;

struct obj * russell_set_up(argc,argv,sz)
int argc;
char **argv;
long sz;
{
register struct obj * p;

  /* If xargc is nonzero, we assume we are in Fortran, and use */
  /* xargc and xargv instead of the arguments.                 */
      if (xargc != 0) {
	argc = xargc;
	argv = xargv;
      }

  /* save argv, so that Russell program can get at it later */
      g_argv = argv;

  /* Build activation record for Russell main pgm   */
      {
	register int i;

	p = ralloc_comp(sz);
	/* first parameter to Russell main program is argc */
	    initenv_ar[0] = argc;
	for( i = 0; i < ((sizeof initenv_ar)/sizeof(long)); i++ ) {
	    p->obj_component[i+1] = initenv_ar[i];
	}
      }
  return(p);
}

