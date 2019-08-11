/* A table containing information associated with virtual registers.    */
/* Currently this simply maps locations to temporary names and possibly */
/* expressions representing the current register value.                 */
/* We use a chained hash table representation.                          */

# define VERBOSE /* Print messages about unusual, but not illegal, situations */
# undef VERBOSE

# include <stdio.h>
# include "../parm.h"
# include "strings.h"
# include "tables.h"
# include "op_codes.h"

extern char * tmp_name();
extern int get_next_free();
extern void free_tmp();

# define HT_SIZE 16        /* Size of location hash table.                  */
			   /* Should be small, since traversals are common. */
# define MOD_HT_SIZE(n) ((n) & (HT_SIZE - 1))
# define RE_NIL ((struct reg_entry *)0)

struct reg_entry {
    long vr;            /* Virtual register number */
    char * tmp_name;    /* Name of temporary implementing this virtual */
			/* register.                                   */
			/* Filled in only when requested.              */
    long tmp_num;       /* The number that is part of the preceding    */
			/* temporary name.                             */
			/* Valid only if tmp_name is not CS_NIL.       */
#   define PREDEF_TMP -1
    char * vr_exp;        /* A C expression that evaluates to the contents */
			  /* of this temporary.                            */
			  /* Tmp_name is valid for reading only if this is */
			  /* CS_NIL.                                       */
    struct reg_entry * next; /* Next entry corr. to this hash bucket.  */
};

struct reg_entry * reg_tab[HT_SIZE];

char * tl_expr;     /* Current expression to compute the value of TL */
		    /* This may contain other variables, and is thus */
		    /* handled seperately.                           */

/* Apply f to each entry in reg_tab.  Return the concatenation of the */
/* results.  Returns a string of the strings.[ch] variety, not a C    */
/* string.                                                            */
char * for_each_vr(f)
char *((*f)());
{
    register int i;
    register struct reg_entry *p;
    char * result = "";

    for (i = 0; i < HT_SIZE; i++) {
	for (p = reg_tab[i]; p != RE_NIL; p = p -> next) {
	    result = concat(result, f(p));
	}
    }
    return(result);
}

/* Return code to insure that the contents of virtual register p are   */
/* reflected in its associated temporary (or a new one if it had none) */
char * flush_expr(p)
struct reg_entry *p;
{
    char * result;

    char * assign;
    if (p -> vr_exp != CS_NIL) {
	if (p -> tmp_name == CS_NIL) {
	    int tmp = get_next_free();

	    p -> tmp_num = tmp;
	    p -> tmp_name = tmp_name(tmp);
	}
	if (p -> vr == AR || p -> vr == GF) {
	    assign = " = (word *)";
	} else {
	    assign = " = (word)";
	}
	result = concat(concat(p -> tmp_name, assign),
			concat(p -> vr_exp, ";\n"));
	p -> vr_exp = CS_NIL;
    } else {
	result = "";
    }
    return(result);
}

static long special_reg;

/* The same, but ignore special_reg */
char * flush_expr_except_special_reg(p)
struct reg_entry *p;
{
    if (p -> vr != special_reg) {
	return(flush_expr(p));
    } else {
	return("");
    }
}

/* Flush, unless the expression is a constant.            */
/* We assume that compound expressions are parenthesized. */
char * flush_expr_except_const(p)
struct reg_entry * p;
{
    char * exp = p -> vr_exp;
    register char first;
    
    if (exp != CS_NIL) {
	GET_FIRST(exp, first);
	if (first != '-' && first != '"' && (first > '9' || first < '0')) {
	    return(flush_expr(p));
	} else {
	    return("");
	}
    } else {
	return("");
    }
}

/* The same, but ignore special_reg */
char * flush_non_const_except_special_reg(p)
struct reg_entry *p;
{
    if (p -> vr != special_reg) {
	return(flush_expr_except_const(p));
    } else {
	return("");
    }
}


/* Return code to store all virtual register values in appropriate */
/* temporaries.                                                    */
char * flush_all_exprs()
{
    return(for_each_vr(flush_expr));
}

/* The same, but exclude vr */
char * flush_all_except(vr_no)
long vr_no;
{
    special_reg = vr_no;
    return(for_each_vr(flush_expr_except_special_reg));
}

/* Flush all nonconstant expressions */
char * flush_all_non_const()
{
    return(for_each_vr(flush_expr_except_const));
}

/* The same, but exclude vr */
char * flush_all_non_const_except(vr_no)
long vr_no;
{
    special_reg = vr_no;
    return(for_each_vr(flush_non_const_except_special_reg));
}

/* Return code to store the given virtual register in its temporary */
char * flush_vr(vr_no)
long vr_no;
{
    int indx = MOD_HT_SIZE(vr_no);
    register struct reg_entry * p = reg_tab[indx];

    while(p != RE_NIL && p -> vr != vr_no) p = p -> next;
    if (p == RE_NIL) {
	/* Doesn't exist.  We assume it's dead. */
	if (!(vr_no & ARG_FLAG)) {
	    fprintf(stderr, "Removing nonexistent virtual register %d\n",
		    vr_no);
	}
	return("");
    } else {
	return(flush_expr(p));
    }
}

/* Return the entry corresponding to a given register number, or RE_NIL */
/* if there is none.                                                    */
struct reg_entry * get_entry(vr_no)
long vr_no;
{
    int indx = MOD_HT_SIZE(vr_no);
    register struct reg_entry * p = reg_tab[indx];

    while(p != RE_NIL && p -> vr != vr_no) p = p -> next;
    return(p);
}

/* Return the temporary name associated with a virtual register. */
char * get_name(vr_no)
long vr_no;
{
    struct reg_entry * p = get_entry(vr_no);

    if (p == RE_NIL) {
	fprintf(stderr, "No temporary for register %d\n", vr_no);
	return("tmp???");
    } else {
	if (p -> tmp_name == CS_NIL) {
	    int tmp = get_next_free();

	    p -> tmp_num = tmp;
	    p -> tmp_name = tmp_name(tmp);
	}
	return(p -> tmp_name);
    }
}

/* Return the temporary name or expression associated with a */
/* virtual register.                                         */
char * get_expr(vr_no)
long vr_no;
{
    struct reg_entry * p = get_entry(vr_no);

    if (p == RE_NIL) {
	fprintf(stderr, "No temporary for register %d\n", vr_no);
	return("tmp???");
    } else {
	if (p -> vr_exp != CS_NIL) {
	    return(p -> vr_exp);
	}
	if (p -> tmp_name == CS_NIL) {
	    int tmp = get_next_free();

	    fprintf(stderr, "Referencing uninitialized temporary %d\n", vr_no);
	    p -> tmp_num = tmp;
	    p -> tmp_name = tmp_name(tmp);
	}
	return(p -> tmp_name);
    }
}

/* Add a mapping of a virtual register to the given temporary name */
/* and number.                                                     */
void add_vr(vr_no, name, num)
long vr_no;
char * name;
long num;
{
    int indx = MOD_HT_SIZE(vr_no);
    register struct reg_entry * p = reg_tab[indx];
    register struct reg_entry * q = p;

    while(q != RE_NIL && q -> vr != vr_no) q = q -> next;
    if (q != RE_NIL) {
	fprintf(stderr, "Redefinition of register %d\n", vr_no);
	p -> tmp_name = name;
	p -> tmp_num = num;
    } else {
	q = (struct reg_entry *) GC_malloc( sizeof (struct reg_entry) );
	q -> vr = vr_no;
	q -> tmp_name = name;
	q -> tmp_num = num;
	q -> next = p;
	reg_tab[indx] = q;
    }
}

/* Add an expression for the value of a virtual register that was previously */
/* entered into the table.                                                   */
void add_vr_def(vr_no, expr)
long vr_no;
char * expr;
{
    int indx = MOD_HT_SIZE(vr_no);
    register struct reg_entry * p = reg_tab[indx];

    while(p != RE_NIL && p -> vr != vr_no) p = p -> next;
    if (p == RE_NIL) {
	fprintf(stderr, "Adding definition of undeclared v. register %d\n",
		vr_no);
	add_undef_vr(vr_no);
	add_vr_def(vr_no, expr);
	return;
    }
    p -> vr_exp = expr;
}

/* Remove the expression associated with a virtual register. */
void rem_vr_def(vr_no)
long vr_no;
{
    int indx = MOD_HT_SIZE(vr_no);
    register struct reg_entry * p = reg_tab[indx];

    while(p != RE_NIL && p -> vr != vr_no) p = p -> next;
    if (p == RE_NIL) {
#       ifdef VERBOSE
	  fprintf(stderr, "Removing definition of undeclared v. register %d\n",
		  vr_no);
#       endif
	return;
    }
    p -> vr_exp = CS_NIL;
}


/* Remove the mapping for the given virtual register.  Mark the */
/* corresponding temporary as free.                             */
/* If the register is predefined, just invalidate the           */
/* expression for its contents.                                 */
void rem_vr(vr_no)
long vr_no;
{
    int indx = MOD_HT_SIZE(vr_no);
    register struct reg_entry **current_p = reg_tab + indx;
    register struct reg_entry * current = *current_p;

    while (current != RE_NIL && current -> vr != vr_no) {
	current_p = &(current -> next);
	current = *current_p;
    }
    if (current == RE_NIL) {
#       ifdef VERBOSE
	  fprintf(stderr,
		  "Deleted nonexistent virtual register mapping for %d\n",
		  vr_no);
#       endif
    } else {
	if (current -> tmp_name != CS_NIL) {
	    if (current -> tmp_num  ==  PREDEF_TMP) {
		current -> vr_exp = CS_NIL;
	    } else {
		dead_tmp(current -> tmp_num);
		*current_p = current -> next;
	    }
	} else {
	    *current_p = current -> next;
	}
    }
}

/* Add a definition of a predefined virtual register */
# define add_predef(vr_no, name) add_vr(vr_no, name, PREDEF_TMP, CS_NIL);

void init_tmps()
{
    add_predef(AR, "AR");
    add_predef(GF, "GF");
    add_predef(UN, "0x0");
    add_predef(SP, "sp??");  /* Should not be used */
    add_predef(RL, "RL");
    add_predef(TL, "TL");  /* Rarely used, we hope */
    add_predef(C0, "0L");
    add_predef(C1, "1L");
    add_predef(C2, "2L");
    add_predef(C3, "3L");
    add_predef(C4, "4L");
}
