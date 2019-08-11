#include "runtime.h"
#include "types.h"
#include <signal.h>
#include <stdio.h>

char * startup_sfp = 0;

long entry_ar_sz;

extern struct obj * global_ar;

struct obj * russell_set_up();

/* int holdsigs(); */

void segv_handler();

void intr_handler();

/* Russell main program */

void main(argc,argv)
int argc;
char **argv;
{
    word dummy;

  /* save current fp register for collect routine */
  /* stack marking terminating condition          */
  /* Nonzero startup_sfp also signals that main   */
  /* routine is a Russell program.                */
  /* Since we are trying to do this in a portable */
  /* way, we content ourselves with an            */
  /* approximation.  This turns out to be         */
  /* perfectly sufficient.                        */
      startup_sfp = (char *) (&dummy);

  /* Build global activation record, etc. */
      global_ar = russell_set_up(argc, argv, entry_ar_sz);

  /* set up signals */
      /* (void) signal(SIGSEGV, segv_handler); */
      /* (void) signal(SIGINT, intr_handler); */

  /* Call Russell main pgm with global activation record.  */
      /* We hope that stack is properly aligned here.  */
      /* Should really be explicitly guaranteed, but   */
      /* that's hard to do in a portable way.          */
	  russell_top_level(global_ar);
  exit(0);
}
