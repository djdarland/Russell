/* 
 *  This is the zeroth approximation to the Russell code generator.
 *  I have started with printastx.c from pass5a and have begun to hack.
 *      Jim Hook  27 March 1982 
 *  Trying to revive this code.  Switched from display to static chain.
 *  Adding stack based calls, in-line calls, and type modification.
 *      Hans Boehm 7 Feb 1984
 *  Adding type constructions, etc.
 *      Hans Boehm 15 June 1984
 *  Added jsb calls, etc.
 *      Hans Boehm 6 Oct 1984
 *  Adding stack based variable allocation.
 *      Hans Boehm 4 June 1986
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codeutil.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"
# include "../runtime/runtime.h"

# define UNDEF (HEAPLIM+1)  /* runtime rep of undefined value */

extern int yydebug;
extern int yynerrs;

extern boolean Pflag;  /* Generate profiling code */
extern boolean Tflag;  /* Generate trace code */
extern boolean Vflag;  /* Print optimization information */
extern boolean hflag;  /* Put activation reords on heap */

extern char * entry_name; /* "" for main compilation */

boolean mentions_r11();

void type_expr();  /* generate code for type in record element */

char str_code_buf[MAXSTRCODELEN]; /* used for strings, "special" fns */
                  /* and by find_inline              */
		  /* and for object file names       */

extern int Ventry_mask,Vgc_mask;
extern int Vlevel;   /* current static nesting level */
 
extern NODE * Vcurrent;    /* Function currently being compiled */
extern FILE * Voutfile;

void Vexpression();

/* Generate code for the application p */
Vappl(p)
NODE *p;
{
    register NODE *v;
    register int argcount;
    char * in_line;     /* in-line code, NIL if not known */
    boolean stack_call; /* use stack for activation rec.  */
    char * size_reg;    /* Name of register for a.r. size */
			/* used only for stack based call */
    char * ar_reg;      /* Register with pointer to a.r.  */
			/* used only in non-stack call    */
    boolean in_memory;  /* size_reg or ar_reg is not a reg*/
    NODE * construction  =  p -> ap_operator -> signature
			      -> fsig_construction;
    boolean slink_known; /* used only if construction is  */
			 /* known.  Indicates that static */
			 /* can be gotten by indirecting  */
			 /* through current static link.  */
    NODE * result_sig = p -> signature;
    NODE * op_sig = p -> ap_operator -> signature;
    int op_kind = p -> ap_operator -> kind;
    long op_special = op_sig -> fsig_special;
    extern boolean is_id(); /* defined in pass5d */
    boolean op_is_id = is_id(p -> ap_operator);
    boolean appl_impure = impure(op_sig);
    boolean op_impure = (appl_impure && !op_is_id)
			|| calls_put(p -> ap_operator);
    int i;


    /* Handle some special operations */
      switch (special_tp(op_special)) {
	case STD_VALUEOF:
	  {
	    NODE * arg = first(p -> ap_args);

	    if (arg -> kind == LETTERID 
		&& arg -> id_last_definition -> kind == DECLARATION
		&& (arg -> id_last_definition -> decl_special & VAR_ON_STACK)) {
		register NODE * v;
		char * r10 = "r10";
		char * display_reg; /* name of reg used for a.r. pointer */

		v = arg -> id_last_definition;
		putcomment1("\t\t\t# Value of Identifier %s",
			    getname(arg -> id_str_table_index));
		DISPLAY ( display_reg, v -> level, r10,
			  "# display entry in place");
		if (display_reg == r10) Ventry_mask |= R10;
		PUSH_DISP (display_reg, v->displacement,
					"# referenced object");
		return;
	    } else {
		break;
	    }
	  }
	case ARRAY_VALUEOF:
	  if (Vflag) {
	      printf("Using fast array ValueOf inside %s\n",
		     Vcurrent -> fc_code_label);
	  }
	  Vexpression(first(p -> ap_args));
	  SET_GC_INFO;
	  CODE("\t.globl\t_fast_Array_ValueOf");
	  CODE("\tcalls\t$1,_fast_Array_ValueOf");
	  CODE("\tpushl\tr0\n");
	  return;
	case ARRAY_STD_NEW:
	case ARRAY_PTR_NEW:
	  {
	    long size_val = special_val(op_special);
	    NODE * sel_type_sig;
	    NODE * size_sig;
	    NODE * size_appl;
	    NODE * size_id;
	    extern NODE * id_size;
	    NODE * op = p -> ap_operator;

	    if (size_val == 0) {
		/* Try to put size of array rep on top of the stack */
		/* using size function of the array type            */
		  if (op_kind != LETTERID || op -> sel_type == NIL) {
		      break;
		  }
		  sel_type_sig = op -> sel_type -> signature;
		  size_sig = getcomp(sel_type_sig, id_size, NIL, NIL,
				     NIL, NIL, NIL, FALSE);
		  if (size_sig == NIL || special_tp(size_sig -> fsig_special)
					 != ARRAY_SIZE) {
		      /* No appropriate size operation */
		      break;
		  }
		  /* Construct size application and generate code for it */
		    size_id = copynode(id_size);
		    initfld(&(size_id -> sel_type), op -> sel_type);
		    size_id -> id_def_found = TRUE;
		    size_appl = mknode(APPLICATION, size_id, emptylist());
		    tl_findsig(size_appl, FALSE);
		    Vappl(size_appl);
	    } else {
	      /* Push constant onto the stack */
		fprintf(Voutfile, "\tpushl\t$%d\n", size_val);
	    }
	    SET_GC_INFO;
	    switch(special_tp(op_special)) {
	      case ARRAY_STD_NEW:
		if (Vflag) {
		    printf("Using fast standard array allocation inside %s\n",
			   Vcurrent -> fc_code_label);
		}
		CODE("\t.globl\t_fast_Array_New");
		CODE("\tcalls\t$1,_fast_Array_New");
		break;
	      case ARRAY_PTR_NEW:
		if (Vflag) {
		    printf("Using fast pointer array allocation inside %s\n",
			   Vcurrent -> fc_code_label);
		}
		CODE("\t.globl\t_fast_ptr_Array_New");
		CODE("\tcalls\t$1,_fast_ptr_Array_New");
		break;
	    }
	    CODE("\tpushl\tr0");
	    return;
	  }
      }
    /* determine type of calling sequence */
      in_line = op_sig -> fsig_inline_code;
      /* if it's impure and not id, ignore in-line code */
	if (op_impure) {
#         ifdef VERBOSE
	    printf("Clearing inline code\n");
#         endif
          in_line = NIL;
        }
      if (in_line == NIL) {
	switch(result_sig -> kind) {
	  case TYPESIGNATURE:
	    stack_call = FALSE;
	    break;
	  case FUNCSIGNATURE:
	    stack_call = FALSE;
	    break;
          case VALSIGNATURE:
#                           ifdef DEBUG
            if (!has_sig(result_sig -> val_denotation)) {
              dbgmsg("codegen: Missing res. type signature\n");
              prtree(p);
              abort();
            }
#                           endif
	    stack_call = result_sig -> val_denotation
			 -> signature -> ts_simple_type;
	    break;
	  case VARSIGNATURE:
#                           ifdef DEBUG
             if (!has_sig(result_sig -> val_denotation)) {
              dbgmsg("codegen: Missing res. type signature\n");
              prtree(p);
              abort();
             }
#                           endif
	    stack_call = result_sig -> var_denotation
			 -> signature -> ts_simple_type;
	    break;
        }
        /* Check for an impure function */
	    if (appl_impure) {
                stack_call = FALSE;
            }
        /* Check for bad VAR parameters */
	    maplist(q, p -> ap_args, {
		NODE * sig = q -> signature;
		if (sig -> kind == VARSIGNATURE) {
		    ASSERT (has_sig(sig -> var_denotation),
		      "Missing argument type signature");
		    if (!sig -> var_denotation -> signature
			     -> ts_simple_type) {
			stack_call = FALSE;
		    }
		}
	    });
      }
      if (construction != NIL) {
	stack_call = stack_call
		     || (construction -> fc_complexity & NO_SL)
		     || (construction -> fc_complexity & NO_CONSTR);
      }
      if (hflag) {
	stack_call = FALSE;
      }
    if (in_line != NIL) {
      maprlist(p -> ap_args, Vexpression);
      /* add r11 to Ventry_mask if it is mentioned */
	if(mentions_r11(in_line)) {
	    Ventry_mask |= R11;
	}
      fprintf(Voutfile,
	      in_line,
	      Vgc_mask << 16, Vgc_mask << 16);
      fprintf(Voutfile, "\n");
    } else /* not in-line */ {
      if (stack_call & Vflag) {
	printf("Function %s calls ", Vcurrent -> fc_code_label);
	if (construction == NIL) {
	    extern FILE * unparse_file;

	    unparse_file = stdout;
	    unparse(p -> ap_operator);
	} else {
	    printf("%s", construction -> fc_code_label);
	}
	printf(" with stack a.r.\n");
      }
      if (construction != NIL) {
	  slink_known = p -> ap_operator -> signature
			  -> fsig_slink_known;
      }
      if (construction == NIL
	  || (!slink_known && !(construction -> fc_complexity & NO_SL))) {
	  Vexpression (p -> ap_operator);
      } else if (op_impure) {
          /* evaluate operator for side effects */
            Vexpression (p -> ap_operator);
            POP ("r0", "# clobber unneeded function value");
      } else {
	  /* May contain needed function construction */
	    Vtraverse (p -> ap_operator);
      }
      argcount = length(p -> ap_args);
      if (!stack_call) {
	/* allocate activation record */
	  if (construction != NIL && slink_known) {
            FXD_NEWOBJ(construction -> ar_size);
	  } else {
	    fprintf(Voutfile,"\tmovl\t*(sp),r1");
            putcomment ("# Get Size of Activation Record");
            NEWOBJ;
	  }
	  ar_reg = Vnewreg();
	  Vgc_mask |= Vreg_bit;
	  in_memory = (Vreg_bit == 0);
          if (construction == NIL || !slink_known) {
            fprintf(Voutfile, "\tmovl\t(sp),r1");
            putcomment("# Function Value");
            fprintf (Voutfile,"\tmovl\t%d(r1),(r0)",
                     FO_EP);
          } else {
            char * display_reg;
                  /* name of reg used for a.r. pointer */
            char * r10 = "r10";
    
            DISPLAY ( display_reg, 
                      ((construction -> ar_static_level) - 1),
                      r10, "# display entry in place" );
            if (display_reg == r10) Ventry_mask |= R10;
            fprintf(Voutfile, "\tmovl\t%s,(r0)",
                    display_reg);
          }
          putcomment ("# initialize static link to ep");
	  fprintf(Voutfile,"\tmovl\tr0,%s",ar_reg);
	  putcomment("# Save Activation Record object");
	  if (in_memory) {
	    PUSH("r0", "# make sure it's accessible");
	  }
      } else {
	/* clear space for local variables */
	  if (construction == NIL) {
	    size_reg = Vnewreg();
	    in_memory = (Vreg_bit == 0);
	    fprintf(Voutfile,"\tmovl\t*(sp),%s\n",size_reg);
	    if (in_memory) {
	      fprintf(Voutfile, "\tmovl\t%s,r10\n", size_reg);
	      Ventry_mask |= R10;
	    }
	    Ventry_mask |= movc_regs;
	    fprintf(Voutfile,"\tmoval\t%d[%s],r2",
			     -4*(argcount+1),
			     in_memory? "r10" : size_reg);
	    putcomment("# size of local variable area");
	    fprintf(Voutfile,"\tsubl2\tr2,sp");
	    putcomment("# reserve space for local variables");
	    fprintf(Voutfile,"\tmovc5\t$0,(sp),$0,r2,(sp)\n");
	  } else { /* construction known */
	    ASSERT(construction -> kind == FUNCCONSTR,
		   "codegen.c: fn construction expected");
	    switch(i = (construction -> ar_size
			- argcount - 1)) {
	      case 0:
		break;
	      case 1:
		fprintf(Voutfile,"\tclrl\t-(sp)");
		putcomment("# 1 local variable");
		break;
	      case 2:
		fprintf(Voutfile,"\tclrq\t-(sp)");
		putcomment("# 2 local variables");
		break;
	      case 3:
                fprintf(Voutfile,"\tclrq\t-(sp)");
		putcomment("# 3 local variables");
                fprintf(Voutfile,"\tclrl\t-(sp)\n");
		break;
              case 4:
#                               ifdef EXTENDED_RANGE
                  fprintf(Voutfile,"\tclro\t-(sp)");
#                               else
                  fprintf(Voutfile,"\tclrq\t-(sp)\n");
                  fprintf(Voutfile,"\tclrq\t-(sp)");
#                               endif
		putcomment("# 4 local variables");
		break;
	      default: fprintf(Voutfile,
			   "\tsubl2\t$%d,sp", 4*i);
		       putcomment(
			   "# reserve space for locals");
		       fprintf(Voutfile,
			   "\tmovc5\t$0,(sp),$0,$%d,(sp)\n",
			   4*i);
		       Ventry_mask |= movc_regs;
	    }
	  }
      }
      maprlist (p-> ap_args, Vexpression);
      if (!stack_call) {
	char * treg = in_memory? "r10" : ar_reg;

	if (in_memory) {
	  Vgc_mask |= R10;
	  fprintf(Voutfile,"\tmovl\t%s,r10\n", ar_reg);
	}
	/* copy arguments to activation record */
	  switch(argcount) {
	    case 0:
	      break;
	    case 1:
	      POP_DISP(treg, 1, "# copy argument");
	      break;
	    case 2:
	      fprintf(Voutfile,"\tmovq\t(sp)+,4(%s)\n", treg);
	      putcomment("# copy 2 arguments");
	      break;
	    case 3:
	      POP_DISP(treg, 1, "# copy 3 args");
	      fprintf(Voutfile,"\tmovq\t(sp)+,8(%s)\n", treg);
	      break;
#                         ifdef EXTENDED_RANGE
	    case 4:
	      fprintf(Voutfile,"\tmovo\t(sp)+,4(%s)\n", treg);
	      putcomment("# copy 4 arguments");
	      break;
#                         endif
	    default:
	      Ventry_mask |= movc_regs;
	      fprintf(Voutfile,"\tmovc3\t$%d,(sp),4(%s)\n",
		      4*argcount, treg);
	      CODE("\tmovl\tr1,sp");
	  }
	if (in_memory) {
	  POP ("r0", "# clobber a.r. pointer");
	}
	if (construction == NIL || !slink_known) {
	  POP ("r0","# Function Value");
        }
        SET_GC_INFO;
	if (construction != NIL) {
	  fprintf(Voutfile,"\tcallg\t(%s),%s\n",
		  treg, construction -> fc_code_label);
	} else {
	  fprintf(Voutfile,"\tcallg\t(%s),*%d(r0)\n",
		  treg, FO_IP);
	}
	Vretreg(ar_reg);
      } else /* stack call */ if (construction == NIL) {
	/* arguments are in place */
	if (in_memory) {
	  Vgc_mask |= R10; /* entry_mask already set */
	  fprintf(Voutfile, "\tmovl\t%s,r10\n", size_reg);
	}
	fprintf(Voutfile,"\tmovl\t-4(sp)[%s],r0",
		in_memory? "r10" : size_reg);
	putcomment("# function value");
	PUSH_DISP("r0",(FO_EP/ObjSize),"# static link");
	SET_GC_INFO;
	fprintf(Voutfile,"\tcallg\t(sp),*%d(r0)\n",
		FO_IP);
	fprintf(Voutfile,"\tmoval\t4(sp)[%s],sp",
		in_memory? "r10" : size_reg);
	putcomment("# pop a.r. and function value");
	Vretreg(size_reg);
      } else /* stack call, construction known */ {
	if (construction -> fc_complexity & NO_SL) {
	  fprintf(Voutfile,"\tjsb\tF%s\n",
			    construction -> fc_code_label);
	  if(argcount > 0) {
	    /* pop arguments */
	      fprintf(Voutfile,"\taddl2\t$%d,sp\n",4*argcount);
	  }
	} else /* need static link */ {
	  if (slink_known) {
	    char * display_reg; /* name of reg used for a.r. pointer */
	    char * r10 = "r10";
    
	    DISPLAY ( display_reg,
		      ((construction -> ar_static_level) - 1),
		      r10, "# display entry in place" );
	    if (display_reg == r10) Ventry_mask |= R10;
	    PUSH(display_reg, "# static link");
	  } else {
	    fprintf(Voutfile, "\tmovl\t%d(sp),r0\n",
		    4 * ((construction -> ar_size) - 1));
	    PUSH_DISP("r0",(FO_EP/ObjSize),"# static link");
	  }
	  SET_GC_INFO;
	  fprintf(Voutfile,"\tcallg\t(sp),%s\n",
		  construction -> fc_code_label);
	  if (!slink_known) {
	    fprintf(Voutfile,"\taddl2\t$%d,sp",
		    4 * ((construction -> ar_size) + 1));
	    putcomment("# pop a.r. and function value");
	  } else {
	    fprintf(Voutfile,"\taddl2\t$%d,sp",
		    4 * (construction -> ar_size));
	    putcomment("# pop a.r.");
	  }
	}
      }
      PUSH ("r0","# push value returned by function");
    }  /* end not in-line */
}
