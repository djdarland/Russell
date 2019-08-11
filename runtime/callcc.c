# define DEBUG
# include <stdio.h>
# include <signal.h>
# include "runtime.h"
# include "types.h"
# include "callcc.h"

/*
 * This is an implementation of Callcc, a Russell translation of the
 * Scheme call/cc construct.  Continuations are represented as a
 * function object with code given below as _continuation, and
 * an environment as defined in callcc.h
 * The Russell Signal facility is also implemented here.
 *
 * The user's program actually calls the functions Callcc and Signal
 * (see the file callcc_save_regs.s); the functions in this file are
 * then called by those.
 */

/* extern */ struct obj * trace_stack;

void sig_handler();

/*
 * restart from environment saved in cont_env structure pointed to by ap
 * res_val as the returned result.  Note that this gets hairy because
 * activation record of _continuation is likely to get clobbered half-way
 * through its execution.  Registers or the bss segment MUST be used for
 * local variables.
 */

/* Semi-portable implementation.  General idea due to A. Demers.      */
/* Assumes downward growing stack.  No doubt contains other machine   */
/* dependencies.  Amazingly, there are no asm's though.               */
/* At first glance, it's a bit slower than the older non-portable     */
/* versions.  But the fact that we don't need to deal with signals    */
/* probably more than makes up for the apparent added work.           */
void _continuation1();

void _continuation(A)
struct ar * A;
{
    register struct cont_env * ce;    /* environment for continuation */
    register word * new_top;          /* new top of stack             */
    register int stack_len;           /* length of saved stack (in words) */
    char d[2048];                     /* enlarge activation record.  */

    /* get saved continuation environment */
	ce = (struct cont_env *)(A -> ar_static_link);

    /* Compute starting address for stack restore */
	new_top =  ((word *) startup_sfp) - (ce -> ce_stack_length);

    if (d > (char *)new_top) {
	/* Copying the stack back would overwrite current a.r.   */
	/* Longjmp would no doubt get confused.  Recurse a while */
	/* to get our stack pointer below the one for the        */
	/* continuation.                                         */
	  _continuation(A);
	/* Try to make the preceding look like it's not a tail recursive */
	/* call.                                                         */
	  abort("cant get here");
    } else {
	_continuation1(A);
	abort("cant get here");
    }
}

static struct obj * ret_val;

void _continuation1(A)
struct ar * A;
{
    register struct cont_env * ce;    /* environment for continuation */
    register word * new_top;          /* new top of stack             */
    register int stack_len;           /* length of saved stack (in words) */
    register word * nstack_base;      /* start of saved stack  */

    /* get saved continuation environment */
	ce = (struct cont_env *)(A -> ar_static_link);

    /* Save return value in case A gets clobbered. */
	ret_val = A -> ar_arg1;

    /* Compute starting address and length for stack restore */
	new_top =	((word *) startup_sfp) - (ce -> ce_stack_length);
	nstack_base =	&(ce -> ce_saved_stack[0]);

    /* restore stack to original configuration */
	bcopy(nstack_base, new_top, (ce -> ce_stack_length) * sizeof(word));

    /* restore trace stack */
        trace_stack = ce -> ce_trace_stack;
        if (trace_flag) {
            printf("Switched to previous continuation: now executing:\n");
            print_tr_stack();
            printf("Continuing:\n");
	}

    longjmp(ce -> ce_jb, 1);
}

int dummy() { }

/* Return a pointer to somewhere in get_sp's act. record */
word * get_sp()
{
    word a;
    return(&a);
}

# ifdef SPARC
    extern void save_regs_in_stack();
# endif

/* Callcc : func [body: func[cc: func[ val T ] val Void; var Void] val T;
 *                T: type {};
 *                var Void] val T
 */

/* Semi-portable implementation. */

MkIP(Callcc(body))
struct obj * body;
{
    word * csp;       /* A conservative approximation to the current */
		      /* stack pointer.                              */

    register word * i, *j;
    register int save_length;  /* in words */

    struct obj ** opp;
    struct obj * continuation;
    struct cont_env * ce;
    word * save_addr;

    csp = get_sp();
#   ifdef DEBUG
	if (((long)csp) & 3) {
	    fprintf(stderr, "Unaligned stack pointer\n");
	    abort("unaligned stack pointer");
	}
#   endif

    /* allocate continuation environment object */
	save_length = ((word *)startup_sfp) - csp;
	ce = (struct cont_env *)
		    ralloc_comp(save_length
				+ BYTES_TO_WORDS(sizeof (struct cont_env)));

    /* fill in ce header fields */
	ce -> ce_trace_stack = trace_stack;
	ce -> ce_stack_length = save_length;
	/* Set up jmp_buf */
#         ifdef SPARC
	    save_regs_in_stack();
#         endif
	  if (setjmp(ce -> ce_jb)) {
	    /* If we're here, then we just invoked the continuation we */
	    /* built below. Return to caller with value passed to      */
	    /* continuation.                                           */
	    return(ret_val);
	  }

    /* copy stack */
	save_addr = &(ce -> ce_saved_stack[0]);
	for (i = save_addr, j = csp; j < (word *)startup_sfp; ) {
	    *i++ = *j++;
	}

    /* allocate continuation object */
        continuation = ralloc(sizeof(struct funcobj)/sizeof(word));

    /* fill in continuation object */
	((struct funcobj *) continuation) -> fo_arlgth = 2;
	((struct funcobj *) continuation) -> fo_ep = (struct obj *)ce;
	((struct funcobj *) continuation) -> fo_ip = (struct obj *((*)())) _continuation;

    return((struct obj *)call_russell1(body, continuation));
}

MkFVAL1(Callcc); 


/*
 * Map of signal numbers to continuations
 */

struct cont_env * sig_cont[NSIG];


/* Signal: func [sig_num: val Short; var Void] val Boolean;
 */

MkIP(Signal(sig_num))
int sig_num;
{
    register word * csp; /* Approximation to current stack pointer*/
    register word *i, *j;
    register int save_length;  /*  in words */
    struct obj ** opp;
    struct obj * continuation; /* must be accessible to collector */
    struct cont_env * ce;      /* ditto; continuation environment */
    word * save_addr;


    csp = get_sp();
#   ifdef DEBUG
	if (((long)csp) & 3) {
	    fprintf(stderr, "Unaligned stack pointer\n");
	    abort("unaligned stack pointer");
	}
#   endif

    /* Check for validity of sig_num */
        sig_num &= 0x7fff;
        if (sig_num >= NSIG) {
            fprintf(stderr, "Signal: bad signal number: %d\n", sig_num);
            print_tr_stack();
            abort();
        }

    /* allocate continuation environment object */
	save_length = ((word *)startup_sfp) - csp;
	ce = (struct cont_env *)
		    ralloc_comp(save_length
				+ BYTES_TO_WORDS(sizeof (struct cont_env)));

    /* fill in ce header fields */
	ce -> ce_trace_stack = trace_stack;
	ce -> ce_stack_length = save_length;
	/* Set up jmp_buf */
#         ifdef SPARC
	    save_regs_in_stack();
#         endif
	  if (setjmp(ce -> ce_jb)) {
	    /* If we're here, then we just invoked the continuation we */
	    /* built below. Return to caller with value passed to      */
	    /* continuation.                                           */
	    return(ret_val);
	  }
 
    /* copy stack */
	save_addr = &(ce -> ce_saved_stack[0]);
	for (i = save_addr, j = csp; j < (word *)startup_sfp; ) {
	    *i++ = *j++;
	}
 
    /* Add continuation to sig_cont list */
        sig_cont[sig_num] = ce;

    /* Set up signal */
        signal(sig_num, sig_handler);

    return(0);
}

MkFVAL1(Signal); 


/* UnSignal: func[val Short; var Void] val Void  */
/* Undoes the effect of a Signal call.           */
MkIP(UnSignal(sig_num))
int sig_num;
{
    void segv_handler();
    void intr_handler();

    sig_cont[sig_num] = NULL_CE;
    if (sig_num == SIGINT) {
	signal(SIGINT, intr_handler);
    } else if (sig_num == SIGSEGV) {
	/* signal(SIGSEGV, segv_handler); */
    } else {
	signal(sig_num, SIG_DFL);
    }
}

MkFVAL1(UnSignal);


/* Start up continuation associated with sig_num */
void sig_handler(sig_num)
int sig_num;
{
    unsigned act_record[2]; /* ar that we're going to pass to _continuation */

    register struct cont_env * ce;

    ce = sig_cont[sig_num];
    
    /* Set up the handler again, in case this signal implementation */
    /* dropped it.  Recent SunOS's don't, but AIX still does.	    */
        signal(sig_num, sig_handler);

    if (ce == NULL_CE) {
        switch(sig_num) {
            case SIGSEGV:
                segv_handler();
                return;
            case SIGINT:
                intr_handler();
                return;
            default:
                fprintf(stderr, "Internal error: bad signal\n");
                abort();
        }
    } else {
        /* We don't want this signal to be held anymore */
            sigsetmask(0);
	/* Call _continuation with argument of true and ep = ce */
	    act_record[0] = (unsigned) ce;	/* ar ptr is ce		*/
	    act_record[1] = 1;			/* 1st arg is True	*/
	    _continuation((word *) act_record);
    }
}
