#include <stdio.h>
#include <signal.h>
#include "runtime.h"
#include "types.h"
#include "callcc.h"

extern char ** g_argv;

extern int trace_flag;

/* argv: func[i: val Short] val ChStr (returns ith element of argv) */

MkIP(Argv(i))
struct obj *i;    /* val Short */
{
    return((struct obj *)(g_argv[(int) i]));
}

MkFVAL1(Argv);

/* Handle a runtime error - the simple case: */
russell_error(s)
{
    ERRMSG(s);
    print_tr_stack();
    ABORT(s);
}

/* r_abort: func[] val Void */

MkIP(r_abort())
{
    russell_error("User requested abort\n");
}

MkFVAL0(r_abort);


/* put_any:  Print any object */
MkIP(put_any(x))
struct obj * x;
{
    printf("0x%X", x);
}

MkFVAL1(put_any);


/* Eof: func[var Void] val Boolean  */
MkIP(Eof(v))
struct obj *v;   /* var Void */
{
    return((struct obj *)(feof(stdin) != 0));
}

MkFVAL1(Eof);

MkIP(expand_hp(n))
int n;
{
    GC_expand_hp(n);
}

MkFVAL1(expand_hp);


cond_error()
{
    fprintf(stderr, "No applicable guard\n");
    print_tr_stack();
    abort();
}

forward_error()
{
    fprintf(stderr, "Illegal forward reference\n");
    print_tr_stack();
    abort();
}

/* Signal handlers */

void segv_handler()
{
    fprintf(stderr, "Segmentation violation (uninitialized variable?)\n");
    print_tr_stack();
    abort();
}

void intr_handler()
{
    char c;

    fflush(stdout);

    for (;;) {
        fprintf(stderr,
                "\ns for stack trace, c to continue, t to toggle tracing,\n");
        fprintf(stderr, "anything else to quit -");
        do {
            c = getchar();
        } while (c == '\n' || c =='\r');
        switch (c) {
            case 's':
                printf("\n");
                print_tr_stack();
                break;

            case 'c':
                return;

            case 't':
                if (trace_flag) {
                    trace_flag = 0;
                } else {
                    trace_flag = 1;
                }
                break;

            case '\r':
            case '\n':
                break;
        
            default:
                exit(1);
        }
    }
}

/*
 * Map of signal numbers to continuations
 */

struct cont_env * sig_cont[NSIG];


/*
 * Some old C auxiliary code assumes this:
 */

# undef ralloc

struct obj * ralloc(n)
int n;
{
    return((struct obj *)GC_malloc(BYTES_PER_WORD*n));
}
