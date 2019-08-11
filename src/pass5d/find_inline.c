# include "parm.h"
# include <stdio.h>

# include "stree/ststructs.mh"

# include "pass5d/op_codes.h"

# include "pass5d/codegen.h"

extern char str_code_buf[];

extern boolean Tflag;  /* Generate trace code */

extern boolean Oflag;  /* Produce fast code, sacrificing compiler speed */
		       /* and space as necessary.                       */

extern boolean Vflag;  /* Verbose: report successful optimizations */

extern FILE * unparse_file;

extern long int_const_val;  /* Set by is_int_const */

char * construct_inline();  /* Really returns sequence of RIC instructions */

char * Ginline_cnvt();	/* Not really a character pointer, but ... */

# define MAXINLINELEN 20    /* longer sequences will not be incorporated into */
			    /* other sequences.                               */

/* CURRENTLY RATHER PRIMITIVE */

/* Fill in the fsig_inline_code field in the function construction fc */
/* if possible.	 Assumes that signatures inside body are known.       */

void Gfind_inline(fc)
NODE * fc;
{
    NODE * fsig = fc -> signature;
    NODE * body = fc -> fc_body;
    int code_len = 0;
    char * code;
    int n_percents = 0;   /* number of percent signs in inline code */
    NODE * p;
    int i;

#   ifdef DEBUG
	if (fc -> kind != FUNCCONSTR) {
	    dbgmsg("find_inline: bad function construction\n");
	}
#   endif
    /* printf("find_inline: %s\n", fc -> fc_code_label); */
    if (fsig -> fsig_inline_code != NIL) return;
    if (body -> kind == EXTERNDEF) {
	int nargs = length(fsig -> fsig_param_list)
		    - n_vacuous_params(fsig -> fsig_param_list);
	char buf[MAXSTRCODELEN];
	extern boolean vacuous_arg();

	if (10 * nargs + 2*strlen(body -> ext_name) + 50 > MAXSTRCODELEN) {
	    /* Might overflow buffer */
	    return;
	}
	str_code_buf[0] = '\0';
	for (i = nargs; i >= 1; i--) {
	    sprintf(buf, "ARG %d,$%d;", i, i);
	    strcat(str_code_buf, buf);
	};
	sprintf(buf, "EXT \"%s\"; LBA \"%s\"; CLC %d;",
		body -> ext_name, body -> ext_name, nargs);
	strcat(str_code_buf, buf);
	strcat(str_code_buf, "MOV RL,RS");
	fsig -> fsig_inline_code = Ginline_cnvt(str_code_buf);
	return;
    }
    if (Tflag) {
        /* In-line expansion would hinder tracing */
        return;
    }
    /* Construct in-line code for the body, if possible. */
	if (Oflag) {
	    fsig -> fsig_inline_code = construct_inline(body, RS, fc);
	    if (Vflag && fsig -> fsig_inline_code != NIL) {
		printf("Constructed in-line code sequence for: ");
		if (fc -> fc_code_label == NIL) {
		    unparse_file = stdout;
		    unparse(fc);
		    printf("\n");
		} else {
		    printf("%s\n", fc -> fc_code_label);
		}
	    }
	}
    return;
}

/* Construct in-line code for the expression expr occuring in the body */
/* of the function fc.  The in-line code leaves the resulting value in */
/* rloc.  Returns NIL if things get too messy.                         */
char * construct_inline(expr,rloc,fc)
NODE * expr;
long rloc;
NODE * fc;
{
    extern long avail_loc;
    int param_num;
    NODE * params;
    extern boolean vacuous_arg();
    static char buf[50];

    if (is_int_const(expr)) {
	sprintf(buf, "LDN %d,%d", int_const_val, rloc);
	return(Ginline_cnvt(buf));
    }
    switch(expr -> kind) {
	case OPRID:
	case LETTERID:
	    if (expr -> sel_type != NIL
		|| expr -> id_last_definition -> kind != PARAMETER
		|| expr -> id_last_definition -> par_scope != fc) {
		return(NIL);
	    }
	    if (vacuous_arg(expr -> signature)) {
		sprintf(buf, "HINT OPT,1; MOV UN,%d", rloc);
		return(Ginline_cnvt(buf));
	    }
	    /* Find the position of the parameter in the parameter list */
		params = fc -> signature -> fsig_param_list;
		param_num = 1;
		maplist(s, params, {
		    if (s -> pre_num ==
			expr -> id_last_definition -> pre_num) {
			break;
		    }
		    /* Must be explicitly passed, since the one we're */
		    /* looking for is not vacuous, and it follows.    */
		    param_num++;
		});
		sprintf(buf, "MOV $%d,%d", param_num, rloc);
		return(Ginline_cnvt(buf));
	    break;

	case BLOCKDENOTATION:
	    if (length(expr -> bld_den_seq) != 1) {
		return(NIL);
	    }
	    maplist (s, expr -> bld_declaration_list, {
		if (eval_decl(s) && !is_int_const(s -> decl_denotation)) {
		    /* We'd have to include code for r.h.s. */
		    return(NIL);
		    /* Other declarations are still likely to cause this */
		    /* to fail, since we can't compile references to the */
		    /* identifiers.  But simple function or type decls   */
		    /* and integer constants are OK.                     */
		}
	    });
	    return(construct_inline(first(expr -> bld_den_seq), rloc, fc));

	case APPLICATION:
	    {
		NODE * op_sig = expr -> ap_operator -> signature;
		struct RIC_instr * inline_code = NIL;
		struct RIC_instr * operator_code;
		struct RIC_instr * udc_code;
		NODE * args = expr -> ap_args;
		int nargs;
		int c_arg;  /* position of current arg, 0 = leftmost */
		extern long avail_loc;
		long first_loc = avail_loc; /* loc for first argument */
		int i;

		if (op_sig -> fsig_inline_code == NIL) {
		    return(NIL);
		}
		if (RIC_len(op_sig -> fsig_inline_code) > MAXINLINELEN) {
		    return(NIL);
		}
		nargs = 0;
		i = 0;
		maplist(s, args, {
		    i++;
		    if (!vacuous_arg(s -> signature)) {
			nargs = i;
		    }
		});
		avail_loc += nargs;
		c_arg = 0;
		/* Build in-line code for arguments */
		  maplist(s, args, {
		    if (c_arg < nargs) {
			char * arg_code;
			arg_code = construct_inline(s, first_loc + c_arg, fc);
			if (arg_code == NIL) {
			    free_RIC(inline_code);
			    return(NIL);
			}
			inline_code = cat_RIC(arg_code, inline_code);
		    }
		    c_arg++;
		  });
		/* Add declarations for argument locations */
		  for (i = 0; i < nargs; i++) {
		    sprintf(buf, "DCL %d,INT", first_loc + i);
		    inline_code = cat_RIC(Ginline_cnvt(buf), inline_code);
		  }
		/* Build operator code */
		  operator_code = copy_RIC(op_sig -> fsig_inline_code);
		  operator_code = RIC_inst_args(operator_code, first_loc);
		  operator_code = RIC_inst_rs(operator_code, rloc);
		/* build undeclare instructions */
		  udc_code = NIL;
		  for (i = 0; i < nargs; i++) {
		    sprintf(buf, "UDC %d", first_loc + i);
		    udc_code = cat_RIC(Ginline_cnvt(buf), udc_code);
		  }
		operator_code = cat_RIC(operator_code, udc_code);
		inline_code = cat_RIC(inline_code, operator_code);
		return((char *)inline_code);
	    }
	    break;
	default:
	    return(NIL);
    }
}


/* Convert the special function descriptor from function signature to */
/* inline code.							      */
/* Clobbers str_code_buf					      */
char * Gspcl_to_inline(spcl)
unsigned spcl;
{
#   define MAX_PROD_EXP_LEN 10
    int tp = special_tp(spcl);
    int val = special_val(spcl);
    int i;
    char * result;

    str_code_buf[0] = '\0';
    switch(tp) {
	case PROD_NEW:
	case UNION_NEW:
	case ENUM_NEW:
	    sprintf(str_code_buf, "ALH C1,RS; STI RS,0,C0");
	    break;
	case PROD_ASSIGN:
	case UNION_ASSIGN:
	case ENUM_ASSIGN:
	    strcpy(str_code_buf, "STI $1,0,$2; MOV $2,RS");
	    break;
	case PROD_VALUEOF:
	case UNION_VALUEOF:
	case ENUM_VALUEOF:
	    strcpy(str_code_buf, "LDI $1,0,RS");
	    break;
	case PROD_MK:
	    if (val > MAX_PROD_EXP_LEN) return(NIL);
	    switch(val) {
	      case 1:
		strcpy(str_code_buf, "ALH C1,T1;");
		break;
	      case 2:
		strcpy(str_code_buf, "ALH C2,T1;");
		break;
	      default:
		sprintf(str_code_buf,
			"LDN %d,T1; ALH T1,T1;",
			val);
		break;
	    }
	    for(i = 0; i < val; i++) {
		char buf[50];
		sprintf(buf, "STI T1,%d,$%d;", i, i+1);
		strcat(str_code_buf, buf);
	    }
	    strcat(str_code_buf, "MOV T1,RS");
	    break;
        case PROD_PROJ:
        case RECORD_VAL_FIELD:
	case RECORD_VAR_FIELD:
	    sprintf(str_code_buf, "LDI $1,%d,RS", val);
	    break;
	case UNION_INJ:
	    switch(val) {
	      case 0:
		sprintf(str_code_buf,
			"ALH C2,T1; STI T1,0,$1; STI T1,1,C0; MOV T1,RS");
		break;
	      case 1:
		sprintf(str_code_buf,
			"ALH C2,T1; STI T1,0,$1; STI T1,1,C1; MOV T1,RS");
		break;
	      case 2:
		sprintf(str_code_buf,
			"ALH C2,T1; STI T1,0,$1; STI T1,1,C2; MOV T1,RS");
		break;
	      case 3:
		sprintf(str_code_buf,
			"ALH C2,T1; STI T1,0,$1; STI T1,1,C3; MOV T1,RS");
		break;
	      default:
		sprintf(str_code_buf,
			"ALH C2,T1; STI T1,0,$1; DCL T2,INT; LDN %d,T2; STI T1,1,T2; UDC T2; MOV T1,RS",
			val);
		break;
	    }
	    break;
	case UNION_INJ0:
	    /* Same thing, but with a vacuous argument */
	    sprintf(str_code_buf,
		    "ALH C2,T1; DCL T2,INT; LDN %d,T2; STI T1,1,T2; UDC T2; MOV T1,RS",
		    val);
	    break;
	case UNION_INQ:
	    sprintf(str_code_buf, "DCL T2,INT; LDI $1,1,T2; LDN %d,T1; EQI T1,T2,RS; UDC T2", val);
	    break;
	case UNION_PROJ:
	    /* Should be improved ... */
	    if (val <= 4) {
		sprintf(str_code_buf, "HINT OPT,6; LDI $1,1,T1; EQI C%d,T1,TL; BRT \"1f\"; EXT \"_union_err\"; ERR \"_union_err\"; LBL \"1\"; LDI $1,0,RS", val);
	    } else {
		sprintf(str_code_buf, "DCL T2,INT; HINT OPT,7; LDN %d,T2; LDI $1,1,T1; EQI T2,T1,TL; BRT \"1f\"; EXT \"_union_err\"; ERR \"_union_err\"; LBL \"1\"; UDC T2; LDI $1,0,RS", val);
	    }
	    break;
	case ENUM_EQ:
	    strcpy(str_code_buf, "EQI $1,$2,RS");
	    break;
	case ENUM_NE:
	    strcpy(str_code_buf, "NEI $1,$2,RS");
	    break;
	case ENUM_CARD:
	case ENUM_ELEMENT:
	    sprintf(str_code_buf, "LDN %d RS", val);
	    break;
	case IDENTITY:
	    strcpy(str_code_buf, "MOV $1,RS");
	    break;
	case ENUM_PRED:
	    sprintf(str_code_buf, "HINT OPT,5; GTI $1,C0,TL; BRT \"1f\"; EXT \"_pred_error\"; ERR \"_pred_error\"; LBL \"1\"; SBI $1,C1,RS");
	    break;
	case ENUM_SUCC:
	    /* Should be improved ... */
	    sprintf(str_code_buf, "HINT OPT,6; LDN %d,T1; LTI $1,T1,TL; BRT \"1f\"; EXT \"_succ_error\"; ERR \"_succ_error\"; LBL \"1\"; ADI $1,C1,RS", val-1);
	    break;
	case UNDEF_CONST:
	    sprintf(str_code_buf, "MOV UN,RS");
	    break;
	default:
	    return(NIL);
    }
    /* symbolic representation of code is in str_code_buf */
    return(Ginline_cnvt(str_code_buf));
}
