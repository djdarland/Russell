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

extern int trace_flag;

void sig_handler();

int holdsigs();

/*
 * restart from environment saved in cont_env structure pointed to by ap
 * res_val as the returned result.  Note that this gets hairy because
 * activation record of _continuation is likely to get clobbered half-way
 * through its execution.  Registers or the bss segment MUST be used for
 * local variables.
 */
MkIP(_continuation(A))
struct ar * A;
{
				      /* M68K VAX  RT			    */
    register struct cont_env * ce;    /* a5   r11 r11 saved environment	    */
    register word * new_top;	      /* a4   r10 r10 new top of stack	    */
    register int stack_len;	      /* d7   r9  r9 length of saved stack */
				      /*		(in words)	    */
    register word * nstack_base;      /* a3   r8  r8  start of saved stack  */
    register word Rres_val;	      /* d6   r7  r7  return value	    */
    static int Omask;                 /* signal mask before holdsigs        */

    /* Make sure we don't get interrupted in the middle of this */
        Omask = holdsigs();
    /* save result where it won't get clobbered */
	Rres_val = ((word)(A -> ar_arg1));

    /* get saved continuation environment */
	ce = (struct cont_env *)(A -> ar_static_link);

    /* Compute starting address and length for stack restore */
	new_top =	((word *) startup_sfp) - (ce -> ce_stack_length);
	nstack_base =	&(ce -> ce_saved_stack[0]);

    /* restore stack to original configuration */
	stack_len = (ce -> ce_stack_length << 2);/* r9 := saved len */
	asm("movl r10,sp");                  /* new_top */
	asm("movl r10,fp");
	asm("movc3 r9,(r8),(r10)"); /* stack_len, nstack_base */

    /* restore trace stack */
        trace_stack = ce -> ce_trace_stack;
        if (trace_flag) {
            printf("Switched to previous continuation: now executing:\n");
            print_tr_stack();
            printf("Continuing:\n");
	}
    /* reenable interrupts */
	sigsetmask(Omask);
    /* We assume that all registers except d0 or r0 were saved with	*/
    /* the environment, and will be restored on return.			*/
	return((struct obj *)Rres_val);
}

int dummy() { }

/*
 * Both __Signal and __Callcc must be called indirectly through a
 * routine that saves and restores all registers except r0 or d0.
 *.

/* Callcc : func [body: func[cc: func[ val T ] val Void; var Void] val T;
 *                T: type {};
 *                var Void] val T
 */

MkIP(_Callcc(body,caller_fp))
struct obj * body;
word * caller_fp; 	/* bottom of caller's stack frame; used by RT only */
{
  /*
   * copy of current frame pointer - 
   * must be a5 for M68K, r11 for VAX
   */
    register word * cfp;

    register word * i, *j;
    register int save_length;  /* in words */

    struct obj ** opp;
    struct obj * continuation;
    struct cont_env * ce;
    word * save_addr;

    /* cfp := FP_REG */
	asm("movl fp,r11");

    /* allocate continuation environment object */
	save_length = ((word *)startup_sfp) - cfp;
	ce = (struct cont_env *)
		    ralloc_comp(save_length
				+ BYTES_TO_WORDS(sizeof (struct cont_env)));

    /* fill in ce header fields */
	ce -> ce_trace_stack = trace_stack;
	ce -> ce_stack_length = save_length;

    /* copy stack */
	save_addr = &(ce -> ce_saved_stack[0]);
	for (i = save_addr, j = cfp; j < (word *)startup_sfp; ) {
	    *i++ = *j++;
	}

    /* allocate continuation object */
        opp = &(objfreelist[sizeof(struct funcobj)/sizeof(word)]);
        if( (continuation = *opp) == ((struct obj *)0) ) {
            continuation = allocobj(sizeof(struct funcobj)/sizeof(word));
        }
        *opp = continuation->obj_link;

    /* fill in continuation object */
        ((struct funcobj *) continuation) -> fo_arlgth = 2;
        ((struct funcobj *) continuation) -> fo_ep = (struct obj *)ce;
        ((struct funcobj *) continuation) -> fo_ip = _continuation;

    return((struct obj *)call_russell1(body, continuation));
}

struct obj * Callcc();

asm(".globl _Callcc");
asm("_Callcc:");
asm(".word 0xffe");
asm("pushl 4(ap)");
asm("calls $1,__Callcc");
asm("ret");

MkFVAL1(Callcc); 


/*
 * Map of signal numbers to continuations
 */

struct cont_env * sig_cont[NSIG];


/* Signal: func [sig_num: val Short; var Void] val Boolean;
 */

MkIP(_Signal(sig_num))
int sig_num;
{
    register word * cfp; /* must be a5 or r11; copy of current frame pointer*/
    register word *i, *j;
    register int save_length;  /*  in words */
    struct obj ** opp;
    struct obj * continuation; /* must be accessible to collector */
    struct cont_env * ce;      /* ditto; continuation environment */
    word * save_addr;

    /* cfp := FP_REG */
	asm("movl fp,r11");

    /* Check for validity of sig_num */
        sig_num &= 0x7fff;
        if (sig_num >= NSIG) {
            fprintf(stderr, "Signal: bad signal number: %d\n", sig_num);
            print_tr_stack();
            abort();
        }

    /* allocate continuation environment object */
	save_length = ((word *)startup_sfp) - cfp;
	ce = (struct cont_env *)
		    ralloc_comp(save_length
				+ BYTES_TO_WORDS(sizeof (struct cont_env)));

    /* fill in ce header fields */
	ce -> ce_trace_stack = trace_stack;
	ce -> ce_stack_length = save_length;
 
    /* copy stack */
	save_addr = &(ce -> ce_saved_stack[0]);
	for (i = save_addr, j = cfp; j < (word *)startup_sfp; ) {
	    *i++ = *j++;
	}
 
    /* Add continuation to sig_cont list */
        sig_cont[sig_num] = ce;

    /* Set up signal */
        signal(sig_num, sig_handler);

    return(0);
}

struct obj * Signal();


asm(".globl _Signal");
asm("_Signal:");
asm(".word 0xffe");
asm("pushl 4(ap)");
asm("calls $1,__Signal");
asm("ret");

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
