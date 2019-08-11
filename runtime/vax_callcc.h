# define NULL_CE ((struct cont_env *)0)

struct cont_env {
    struct obj * ce_trace_stack;
    int ce_stack_length;    /* in words */
    word ce_saved_stack[1]; /* saved from fp at callcc entry to startup_sfp */
                            /* all registers are saved on entry to callcc   */
};

