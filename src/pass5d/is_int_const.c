/* Determine whether an expressions is obviously a mchine integer */
/* constant.  Used to implement a very simple version of constant */
/* propagation.                                                   */
# define DEBUG DEBUG
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

extern int yydebug;
extern int yynerrs;

extern FILE * unparse_file;

extern char str_code_buf[];

long int_const_val;

extern NODE * id_Integer;   /* The built-in integer type */

extern NODE * id_plus;

extern NODE * id_times;

char * Int_string_code;
char * Int_add_code;
char * Int_mul_code;

static int is_initialized = 0;

boolean is_int_const1();

/* Initialize the above */
void is_const_init()
{
    NODE * ld = id_Integer -> id_last_definition;
    NODE * tsig;
    NODE * plus_sig;
    NODE * times_sig;

    is_initialized = 1;
    if (ld -> kind == DECLARATION) {
	tsig = ld -> decl_signature;
    } else if (ld -> kind == PARAMETER) {
	tsig = ld -> par_signature;
    } else {
	dbgmsg("is_const_init: bad built-in integer type\n");
    }
    Int_string_code = tsig -> ts_string_code;
    if (Int_string_code == NIL) {
	fprintf(stderr, "Warning: no integer string code\n");
	Int_string_code = "X";  /* pointer distinguishable from */
				/* everything else.             */
    }
    plus_sig = getcomp(tsig, id_plus, NIL, NIL, NIL, NIL, FALSE);
    times_sig = getcomp(tsig, id_times, NIL, NIL, NIL, NIL, FALSE);
    Int_add_code = plus_sig -> fsig_inline_code;
    Int_mul_code = times_sig -> fsig_inline_code;
    if (Int_add_code == NIL) {
	fprintf(stderr, "Warning: no integer addition code\n");
	Int_add_code = "X";  /* pointer distinguishable from */
			     /* everything else.             */
    }
    if (Int_mul_code == NIL) {
	fprintf(stderr, "Warning: no integer multiplication code\n");
	Int_mul_code = "X";  /* pointer distinguishable from */
			     /* everything else.             */
    }
}

/* Determine whether the expression p is known to be an integer constant */
/* If so, set int_const_val to the value.                                */
/* Returns FALSE if p can involve a side effect.                         */
/* Unlike int_value, we make no assumptions about expression signatures  */
boolean is_int_const(p)
NODE *p;
{
    if (!is_initialized) {
	is_const_init();
    }
    if (!has_sig(p)) { return(FALSE); }
    if (p -> signature -> kind == VALSIGNATURE) {
	return(is_int_const1(p));
    } else {
	return(FALSE);
    }
}

boolean is_int_const1(p)
NODE *p;
{
    NODE * decl;
    boolean result;

    switch(p -> kind) {
	case LETTERID:
	case OPRID:
	    decl = p -> id_last_definition;
	    
	    if (decl == NIL /* selection */
		|| decl -> kind != DECLARATION
		|| (decl -> decl_special & NOT_DECL_CONST)) {
		return(FALSE);
	    }
	    if (decl -> decl_special & DECL_CONST) {
		int_const_val = decl -> decl_const_val;
		return(TRUE);
	    }
	    if (decl -> post_num >= p -> post_num) {
		/* Dont follow recursive declarations. */
		return(FALSE);
	    }
	    result = is_int_const1(p -> id_last_definition -> decl_denotation);
	    /* Remember for next time */
		if (result) {
		    decl -> decl_special |= DECL_CONST;
		    decl -> decl_const_val = int_const_val;
		} else {
		    decl -> decl_special |= NOT_DECL_CONST;
		}
	    return(result);
	case UQSTR:
	    if (p -> sel_type -> signature -> ts_string_code
		== Int_string_code) {
		int_const_val = atoi(p -> str_string);
		return(TRUE);
	    } else {
		return(FALSE);
	    }
	case APPLICATION:
	    {
		NODE * op_sig = p -> ap_operator -> signature;
		NODE * args = p -> ap_args;
		long l_arg, r_arg;

		switch(special_tp(op_sig -> fsig_special)) {
		    case IDENTITY:
			return(is_int_const1(first(args)));
		    case ENUM_ELEMENT:
		    case ENUM_CARD:
			int_const_val = special_val(op_sig -> fsig_special);
			return(TRUE);
		}
		if (op_sig -> fsig_inline_code == Int_add_code) {
		    if (!is_int_const1(first(args))) { return(FALSE); }
		    l_arg = int_const_val;
		    if (!is_int_const1(second(args))) { return(FALSE); }
		    r_arg = int_const_val;
		    int_const_val = l_arg + r_arg;
		    return(TRUE);
		} else if (op_sig -> fsig_inline_code == Int_mul_code) {
		    if (!is_int_const1(first(args))) { return(FALSE); }
		    l_arg = int_const_val;
		    if (!is_int_const1(second(args))) { return(FALSE); }
		    r_arg = int_const_val;
		    int_const_val = l_arg * r_arg;
		    return(TRUE);
		} else {
		    return(FALSE);
		}
	    }
	default:
	    return(FALSE);
    }
}
