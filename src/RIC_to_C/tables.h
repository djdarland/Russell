/* Externally visible declarations for loc_tab, tmp_tab and args */

char * flush_all_exprs();   /* Store all temporary values */
char * flush_all_except();  /* Same, but exclude a v. register */
char * flush_all_non_const();  /* exclude constant expressions */
char * flush_all_non_const_except();
			    /* exclude constants and the specified v. reg. */
char * flush_vr();          /* Store contents of a give v. register */
char * get_name();          /* Get temporary corr. to v. register   */
char * get_expr();          /* Get expression evaluating to v.r. contents */
void add_vr();              /* Add an entry to v.r. table       */
void rem_vr();              /* Remove an entry from the virtual reg. table */
void add_vr_def();          /* Add an expression for the value of a v.r. */
void rem_vr_def();          /* Remove expression and temporary for a v.r. */
# define add_undef_vr(vr_no) add_vr(vr_no, CS_NIL, 0);
void init_tmps();	    /* Initialize descriptors for predefined locs */
void reset_tmps();          /* Make sure that no temporaries are in use. */
char * par_name();          /* The name of the ith parameter */
char * tmp_name();          /* The name of the i'th temporary */
char * par_names();         /* Return a comma separated list of i par. names */
char * tmp_decls();         /* Declarations for n temporaries */
char * itos();              /* String rep of an integer */
char * rmcntrl();           /* Replace control characters by C escapes */
char * arg_list();          /* Generate list of n argument values */
void dead_tmp();            /* Add to list of dead temporaries */
void rem_tmps();            /* Free dead temporaries.          */

extern int max_tmp;         /* Highest numbered temporary used */
# define ARG_FLAG 0x20000000
# define ARGLOC(i) ((i) | ARG_FLAG)

# define is_param(c) (c == 'a')  /* c is the first character of a parameter */
				 /* name.  We assume is_arg is false of all */
				 /* other names.                            */
