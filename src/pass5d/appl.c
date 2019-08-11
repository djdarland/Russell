# define OBSOLETE
# undef OBSOLETE
# define DEBUG

# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

extern int yydebug;
extern int yynerrs;

extern FILE * unparse_file;

extern char str_code_buf[];

extern int avail_loc;

extern int Glevel;

extern NODE * Gcurrent;  /* Current function construction */

extern boolean Vflag;

extern boolean Nflag;

extern boolean Oflag;

extern boolean fflag;   /* No frame pointer, can't update SP */

extern boolean Fflag;   /* Same, but HINT ONS is not assumed to be */
			/* understood.   (Fflag ==> fflag)         */
			/* In its absence, HINT ONS followed by    */
			/* HINT DEA is assumed to compile to a     */
			/* noop.                                   */

extern boolean hflag;   /* All activation records on heap. */

extern FILE * Goutfile;

extern boolean sl_available;    /* Static link available for current */
				/* function construction.            */

boolean Gpush_size();

boolean is_id();

extern boolean is_int_const();

extern NODE * equiv_expr();

static int arg_loc;

/* Check whether a signature describes an object containing no    */
/* runtime information.  Works with either argument or parameter  */
/* signature.                                                     */
boolean vacuous_arg(p)
NODE *p;
{
    extern NODE * var_Void;

    switch (p -> kind) {
	case VALSIGNATURE:
	    return(FALSE);
	case VARSIGNATURE:
	    return(comp_st(p, var_Void, NIL, NIL) == 0);
	case FUNCSIGNATURE:
	    return(FALSE);
	case TYPESIGNATURE:
	    return(tsig_length(p) == 0);
	case SIGNATURESIG:
	    return(TRUE);
	case LETTERID:
	case OPRID:
	    if (p -> id_last_definition -> kind == DECLARATION
		&& p -> id_last_definition -> decl_sig_transp) {
		return(vacuous_arg(p -> id_last_definition -> decl_denotation));
		/* We checked in pass3 that this isn't circular */
	    } else {
		ASSERT(p -> id_last_definition -> kind == PARAMETER,
		       "vacuous_arg: strange signature identifier\n");
		return(FALSE);
	    }
	default:
	    dbgmsg("vacuous_arg: bad argument kind\n");
	    abort(p);
    }
}

/* A version of maprlist (see mknode.c) that applies fn          */
/* to subexpressions in the list up to (and including) the last  */
/* nonvacuous one with a trailing TRUE argument.  fn is invoked  */
/* on the remaining list arguments with a trailing FALSE         */
/* argument.                                                     */
static boolean maprl1_non_vacuous(l,fn)
ConsNode * l;
void (*fn)();
{
    register boolean tail_non_vacuous = FALSE;

    if (l != NIL) {
      tail_non_vacuous = maprl1_non_vacuous(cn_tail(l),fn)
			 || !vacuous_arg(((NODE *)cn_head(l)) -> signature);
      (*fn) (cn_head(l), tail_non_vacuous);
    }
    return(tail_non_vacuous);
}

void maprlist_non_vacuous(l, fn)
LIST l;
void (*fn)();
{
    (void) maprl1_non_vacuous(l -> lh_first, fn);
}

boolean reg_ok = FALSE;
                 /* A variable allocated to a register is OK for */
		 /* this call to Garg_expression.  If encountered*/
		 /* dont generate any code.                      */
/* Evaluate the argument expression p into arg_loc after         */
/* decrementing it.  This is useful only because                 */
/* maprlist_non_vacuous does not call functions with a location  */
/* argument.  If needed is FALSE, then p does not have run-time  */
/* significance, and should not be evaluated into arg_loc. It    */
/* may still need to be evaluated for side effects.              */
Garg_expression(p, needed)
NODE *p;
boolean needed;
{
    int SAVarg_loc = arg_loc - 1;
    int SAVreg_ok = reg_ok;

    if (needed) {
	if (reg_ok && (p -> kind == LETTERID || p -> kind == OPRID)
	    && p -> sel_type == NIL
	    && p -> id_last_definition -> kind == DECLARATION
	    && (p -> id_last_definition -> decl_special & VAR_IN_REG)) {
	    /* Do nothing.  This is already handled by substitution into */
	    /* the in-line code.                                         */
	} else {
	    reg_ok = FALSE;
	    Gexpression(p, SAVarg_loc, FALSE);  /* May clobber arg_loc */
	       /* Nested functions would be good for something after all */
	}
    } else {
	SAVarg_loc ++;
        if (!is_id(p) &&
            (p -> signature -> kind != TYPESIGNATURE
	     || calls_put(p))) {
	    if (Vflag) {
	        printf("Evaluating vacuous argument ");
	        unparse_file = stdout;
	        unparse(p);
	        printf(" for effect\n");
	    }
	}
	reg_ok = FALSE;
	Gexpression(p, SK, FALSE);
    }
    arg_loc = SAVarg_loc;
    reg_ok = SAVreg_ok;
}

# ifdef UNDEFINED
int arg_no;

/* Call Gexpression with temporary. Then issue an ARG instruction */
/* with that temporary, and with argument number given by arg_no. */
/* The decrement arg_no.                                          */
/* This is useful only because maprlist can only call functions   */
/* with one argument.                                             */
Gdir_arg_expression(p, needed)
NODE *p;
boolean needed;
{
    if (needed) {
	int SAVarg_no = arg_no;
	int tmp_loc = avail_loc++;
	
	gen2(DCL, tmp_loc, DCL_INT);
	Gexpression(p, tmp_loc, FALSE);  /* May clobber arg_no */
	gen2(ARG, SAVarg_no, tmp_loc);
	gen1(UDC, tmp_loc);
	arg_no = SAVarg_no - 1;
    } else if (!is_id(p) &&
	       (p -> signature -> kind != TYPESIGNATURE
		|| calls_put(p))) {
	if (Vflag) {
	    printf("Evaluating vacuous argument ");
	    unparse_file = stdout;
	    unparse(p);
	    printf(" for effect\n");
	}
	Gexpression(p, SK, FALSE);
    }
}

#endif

/* Evaluate p into the current location of a memory block pointed to   */
/* by heap_loc. Decrement the                                          */
/* current location counter (heap_offset).  The value of heap_loc      */
/* is a pointer to the heap block.                                     */
/* Needed has the same meaning as for Garg_expression.                 */
static int heap_loc;
static int heap_offset;

Gheap_expression(p, needed)
NODE *p;
boolean needed;
{
    if (needed) {
	int tmp_loc = avail_loc++;
	int SAVheap_loc = heap_loc;
	int SAVheap_offset = heap_offset;

	gen2(DCL, tmp_loc, DCL_INT);
	Gexpression(p, tmp_loc, FALSE);
	    /* May have clobbered heap_loc, heap_offset */
	gen3(STI, SAVheap_loc, SAVheap_offset, tmp_loc);
	gen1(UDC, tmp_loc);
	heap_offset = SAVheap_offset - 1;
	heap_loc = SAVheap_loc;
    } else if (!is_id(p) &&
	       (p -> signature -> kind != TYPESIGNATURE
		|| calls_put(p))) {
	if (Vflag) {
	    printf("Evaluating vacuous argument ");
	    unparse_file = stdout;
	    unparse(p);
	    printf(" for effect\n");
	}
	Gexpression(p, SK, FALSE);
    }
}

/* Genarate code for loc := abs(loc) */
gen_abs(loc)
int loc;
{
#   ifdef OBSOLETE
      gen3(GEI, loc, C0, TL);
      genl(BRT, "1f");
      gen2(NGI, loc, loc);
      genl(LBL, "1");
#   else
      gen2(ABI, loc, loc);
#   endif
}

/* Determine whether an expression is an identifier */
boolean is_id(p)
NODE *p;
{
    if (p -> kind != LETTERID && p -> kind != OPRID) {
	return(FALSE);
    } else {
	return(TRUE);
    }
}


/* Generate RIC code for the function application p */
/* Code leaves the result rloc.                     */
/* last_expr is TRUE only if this is the last call  */
/* in the function body.                            */
Gappl(p, rloc, last_expr)
NODE *p;
int rloc;
boolean last_expr;
{
    register NODE *v;
    register int argcount;
    struct RIC_instr * in_line;     /* in-line code, NIL if not known */
    boolean stack_call; /* use stack for activation rec.  */
    boolean test_closure = TRUE;
			/* If stack_call is false, generate */
			/* code to check for possible stack */
			/* activation record at run time    */
    int size_loc;       /* location for a.r. size         */
                        /* used only for stack based call */
    int ar_loc;         /* pointer to a.r.                */
    int fn_loc;         /* pointer to function object     */
    int old_sp_loc;     /* stack pointer before call        */
    int first_arg_loc;
    NODE * op = (Oflag ? equiv_expr(p -> ap_operator) : p -> ap_operator);
    NODE * construction  =  op -> signature
			       -> fsig_construction;
    boolean slink_known; /* used only if construction is  */
                         /* known.  Indicates that e.p.   */
                         /* can be gotten by indirecting  */
                         /* through current static link.  */
    NODE * result_sig = p -> signature;
    NODE * op_sig = p -> ap_operator -> signature;
    long op_special = op -> signature -> fsig_special;
    boolean op_is_id = is_id(p -> ap_operator);
    boolean appl_impure = impure(op_sig);
    boolean op_impure = (appl_impure && !op_is_id)
			|| calls_put(p -> ap_operator);
    boolean reuse_ar;   /* This is a tail recursive call that allows */
			/* reuse of callers activation record.       */
    boolean slink_needed;  /* Callee requires full a.r.  as parameter */
    boolean bogus_slink_ok; /* Callee will not look at its global env. */
			    /* pointer.                                */
    boolean direct_rec_call;  /* A direct tail recursive call to the same */
			      /* function construction                    */
    int Rlevel = Glevel; /* Real static nesting level.  We may need to fix */
			 /* up AR during tail calls, so we temporarily     */
			 /* change Glevel.                                 */
    int i;


    ASSERT( !last_expr || rloc == RL, "Gappl: bad last_expr value\n");

    if (is_int_const(p)) {
	extern long int_const_val;

	gen2(LDN, int_const_val, rloc);
	return;
    }
    /* Calculate number of arguments */
      argcount = 0;
      i = 0;
      maplist(s, p -> ap_args, {
	i++;
	if (!vacuous_arg(s -> signature)) {
	    argcount = i;
	}
      });
    /* Handle some special operations */
      switch (special_tp(op_special)) {
	case ARRAY_VALUEOF:
	  if (Vflag) {
	      printf("Using fast array ValueOf inside %s\n",
		     Gcurrent -> fc_code_label);
	  }
	  /* Pass argument */
	    {
		int i = avail_loc++;

		gen2(DCL, i, DCL_INT);
		Gexpression(first(p -> ap_args), i, FALSE);
		gen2(ARG, 1, i);
		gen1(UDC, i);
	    }
	  genl(EXT, "_fast_Array_ValueOf");
	  genl(LBA, "_fast_Array_ValueOf");
	  gen1(CLC, 1);
	  gen2(MOV, RL, rloc);
	  return;
	case ARRAY_STD_NEW:
	case ARRAY_PTR_NEW:
	  {
	    long size_val = special_val(op_special);

	    if (!Gpush_size(size_val, p -> ap_operator)) {
	   	    /* Note that passing op in the above is wrong, since */
	   	    /* it can lead to recomputing id bindings, and to id */
	   	    /* references that are not in my closure.		 */
		/* Can't find size */
		break;
	    }
	    switch(special_tp(op_special)) {
	      case ARRAY_STD_NEW:
		if (Vflag) {
		    printf("Using fast standard array allocation inside %s\n",
			   Gcurrent -> fc_code_label);
		}
		genl(EXT, "_fast_Array_New");
		gen3(HINT, AL, 0, 0);
		genl(LBA, "_fast_Array_New");
		gen1(CLC, 1);
		break;
	      case ARRAY_PTR_NEW:
		if (Vflag) {
		    printf("Using fast pointer array allocation inside %s\n",
			   Gcurrent -> fc_code_label);
		}
		genl(EXT, "_fast_pArray_New");
		gen3(HINT, AL, 0, 0);
		genl(LBA, "_fast_pArray_New");
		gen1(CLC, 1);
		break;
	    }
	    gen2(MOV, RL, rloc);
	    return;
	  }
	case RECORD_VALUEOF:
	case PROD_VALUEOF:
	case ENUM_VALUEOF:
	case STD_VALUEOF:
	  {
	    NODE * arg1 = first(p -> ap_args);
	    NODE * def;

	    if (arg1 -> kind != LETTERID && arg1 -> kind != OPRID
		|| arg1 -> sel_type != NIL) {
		break;
	    }
	    def = arg1 -> id_last_definition;
	    if (def -> kind != DECLARATION) {
		/* Compile it normally */
		break;
	    }
	    if (def -> decl_special & VAR_IN_REG) {
	      /* Just move it to the desired place */
		gen2(MOV, def -> displacement /* var. location */, rloc);
	    } else if (def -> decl_special & VAR_ON_STACK) {
	      /* Handle common case directly to save compilation time */
		if (def -> level == 0) {
		    gen3(LDI, GF, def -> displacement, rloc);
		} else if (def -> level == Glevel) {
		    gen3(LDI, AR, def -> displacement, rloc);
		} else {
		    break;
		}
	    } else {
		break;
	    }
	    return;
	  }
	case ARRAY_VAR_SUB:
	  {
	    int size = special_val(op_special);
	    int i, j, k, m;
	    NODE * arg1 = first(p -> ap_args);
	    NODE * def;

	    if (size == 0) {
		/* Not statically known */
		break;
	    }
	    if (arg1 -> kind != LETTERID && arg1 -> kind != OPRID
		|| arg1 -> sel_type != NIL) {
		break;
	    }
	    def = arg1 -> id_last_definition;
	    if (def -> kind != DECLARATION) {
		/* Compile it normally */
		break;
	    }
	    if (!(def -> decl_special & ARRAY_CONTIG)) {
		break;
	    }

	    /* Save an indirection by indexing directly to the particular */
	    /* array element, rather than going through the header.       */
		i = avail_loc++;
		j = avail_loc++;
		k = avail_loc++;
		m = avail_loc++;
		gen2(DCL, i, DCL_ADDR);
		gen2(DCL, j, DCL_INT);  /* subscript */
		gen2(DCL, k, DCL_INT);  /* size (+1) */
		Gexpression(second(p -> ap_args), j, FALSE);
		/* Subscript check */
		  gen2(HINT, OPT, 12);
		  gen3(GEI, j, C0, TL);
		  genl(BRF, "1f");
		  gen2(DCL, m, DCL_INT);
		  gen2(LDN, size, m);
		  gen3(LTI, j, m, TL);
		  gen1(UDC, m);
		  genl(BRT, "2f");
		  genl(LBL, "1");
		  gen2(ARG, 1, j);
		  genl(EXT, "_Array_error");
		  genl(ERR, "_Array_error");
		  genl(LBL, "2");
		gen2(LDN, size + 1, k);
		Gident(first(p -> ap_args), i);
		gen3(ADP, i, k, i);
		gen3(ADP, i, j, rloc);
		gen1(UDC, i);
		gen1(UDC, j);
		gen1(UDC, k);
	    return;
	  }

	case RECORD_ASSIGN:
	case PROD_ASSIGN:
	case ENUM_ASSIGN:
	case STD_ASSIGN:
	  {
	    NODE * arg1 = first(p -> ap_args);
	    NODE * def;

	    if (arg1 -> kind != LETTERID && arg1 -> kind != OPRID
		|| arg1 -> sel_type != NIL) {
		break;
	    }
	    def = arg1 -> id_last_definition;
	    if (def -> kind != DECLARATION
		|| !(def -> decl_special & VAR_IN_REG)) {
		/* Compile it normally */
		break;
	    }
	    /* specify l.h.s as destination location */
		Gexpression(second(p -> ap_args),
			    def -> displacement, FALSE);
			/* Note: OK to compile as tail-recursive call  */
			/* if otherwise appropriate,                   */
			/* since lhs of assignment is guaranteed dead. */
			/* But this violates some consistency checks.  */
		if (rloc != SK) {
		    gen2(MOV, def -> displacement, rloc);
		}
	    return;
	  }
	case STD_PASSIGN:
	case STD_MASSIGN:
	case STD_TASSIGN:
	  {
	    NODE * arg1 = first(p -> ap_args);
	    NODE * def;

	    if (arg1 -> kind != LETTERID && arg1 -> kind != OPRID
		|| arg1 -> sel_type != NIL) {
		break;
	    }
	    def = arg1 -> id_last_definition;
	    if (def -> kind != DECLARATION
		|| !(def -> decl_special & VAR_IN_REG)) {
		/* Compile it normally */
		break;
	    }
	    {
		int i = avail_loc++;

		gen2(DCL, i, DCL_INT);
		Gexpression(second(p -> ap_args), i, FALSE);
		switch (special_tp(op_special)) {
		    case STD_PASSIGN:
			gen3(ADI, def -> displacement, i, def -> displacement);
			break;
		    case STD_MASSIGN:
			gen3(SBI, def -> displacement, i, def -> displacement);
			break;
		    case STD_TASSIGN:
			gen3(MLI, def -> displacement, i, def -> displacement);
			break;
		}
		if (rloc != SK) {
		    gen2(MOV, def -> displacement, rloc);
		}
		gen1(UDC, i);
	    }
	    return;
	  }
      }

    /* determine type of calling sequence */
      in_line = (struct RIC_instr *)(op -> signature -> fsig_inline_code);
      if (in_line == NIL) {
        switch(result_sig -> kind) {
          case TYPESIGNATURE:
	    stack_call = FALSE;
	    break;
          case FUNCSIGNATURE:
	    stack_call = FALSE;
	    break;
          case VALSIGNATURE:
#           ifdef DEBUG
              if (!has_sig(result_sig -> val_denotation)) {
                dbgmsg("codegen: Missing res. type signature\n");
                prtree(p);
                abort();
              }
#           endif
            stack_call = result_sig -> val_denotation
                         -> signature -> ts_simple_type;
            break;
          case VARSIGNATURE:
#           ifdef DEBUG
             if (!has_sig(result_sig -> val_denotation)) {
              dbgmsg("codegen: Missing res. type signature\n");
              prtree(p);
              abort();
             }
#           endif
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
		     || (construction -> fc_complexity & NO_AR_REFS);
	test_closure = FALSE;
      }
    if (hflag) {
	stack_call = FALSE;
    }
    if (fflag && construction == NIL
	|| Fflag && !(construction -> fc_complexity & NO_CALLCC)) {
	stack_call = FALSE;
	/* Otherwise we might deallocate the activation record */
	/* more than once.                                     */
    }
    if (in_line != NIL) {
      /* if it is impure and not id, evaluate operator for effect */
	  if (op_impure) {
	      if (Vflag) {
		printf("Evaluating impure operator for effect: ");
		unparse_file = stdout;
		unparse(p -> ap_operator);
		printf("\n\t(using in-line code for call)\n");
	      }
	    Gexpression (p -> ap_operator, SK, FALSE);
	  }
      first_arg_loc = avail_loc;
      arg_loc = avail_loc = avail_loc + argcount;
      for (i = first_arg_loc; i < arg_loc; i++) {
	gen2(DCL, i, DCL_INT);
      }
      reg_ok = TRUE;
      maprlist_non_vacuous(p -> ap_args, Garg_expression);
      reg_ok = FALSE;
      /* write in-line expansion, after adjusting for VAR_IN_REG arguments */
      {
	struct RIC_instr * revised_in_line = in_line;
	struct RIC_instr * RIC_tmp;
	int argcnt = 1;
	NODE * argsig;

	if (Oflag) {
	  maplist(s, p -> ap_args, {
	    if ((s -> kind == LETTERID
		 || s -> kind == OPRID)
		&& s -> sel_type == NIL
		&& s -> id_last_definition -> kind == DECLARATION
		&& (s -> id_last_definition -> decl_special & VAR_IN_REG)) {
		RIC_tmp = revised_in_line;
		revised_in_line = unindirect(revised_in_line, argcnt,
					     s -> id_last_definition
					       -> displacement);
		if (RIC_tmp != in_line) {
		    free_RIC(RIC_tmp);
		}
	    }
	    argcnt ++;
	  });
	}
	write_RIC_seq(Goutfile, revised_in_line, first_arg_loc, rloc);
	if (revised_in_line != in_line) {
	  free_RIC(revised_in_line);
	}
      }
      for (i = first_arg_loc; i < first_arg_loc + argcount; i++) {
	gen1(UDC, i);
      }
    } else /* not in-line */ {
      boolean no_ar_ref_passed =
			 (Gcurrent -> fc_complexity & NO_AR_REFS)
			 || argcount == 0;
			 /* No reference to the current activation      */
			 /* record can conceivably be passed to callee. */
			 /* We should probably examine the arguments    */
			 /* to get more precise information.            */
      slink_needed = ((construction == NIL)
		      || ((construction -> fc_complexity & NO_SL) == 0));
      bogus_slink_ok = (construction != NIL &&
			!(construction -> fc_complexity & SL_ACC));
		    /* Expecting full activation record.  Need to pass */
		    /* AR.  However global env. ptr is not used.       */
      direct_rec_call = (last_expr && (construction != NIL)
			&& (construction -> pre_num == Gcurrent -> pre_num)
			&& stack_call && no_ar_ref_passed
			&& (construction -> pre_num != 0)
			&& (sl_available == slink_needed));
      ASSERT(!direct_rec_call || (Gcurrent -> fc_complexity & DIR_REC),
	     "Gappl: unexpected tail recursion\n");
      reuse_ar = direct_rec_call ||
		 last_expr && stack_call && no_ar_ref_passed
			   && (Gcurrent -> fc_complexity & NO_AR_REFS)
			   && construction != NIL
			   && (construction -> ar_size <= Gcurrent -> ar_size)
			   && sl_available && slink_needed
			   && (construction -> ar_static_level <=
			       Gcurrent     -> ar_static_level
			       || bogus_slink_ok);
	/* This is safe only if no refs to current ar can be preserved. */
	/* That means current a.r. is on stack, and can't be reused if  */
	/* heap a.r. is required.                                       */
	/* The last condition insures that the callee cannot reference  */
	/* the caller through the callers static link.                  */
      if (Vflag) {
	printf("Function %s calls ", Gcurrent -> fc_code_label);
	if (construction == NIL) {
	    unparse_file = stdout;
	    unparse(p -> ap_operator);
	} else {
	    printf("%s", construction -> fc_code_label);
	}
	if (direct_rec_call) {
	  printf(" tail recursively\n");
	} else if (reuse_ar) {
	  printf(" with recycled stack a.r.\n");
	} else if (stack_call) {
	  if (slink_needed) {
	    printf(" with stack a.r.\n");
	  } else {
	    printf(" with partial a.r.\n");
	  }
	} else if (test_closure) {
	  printf(" with stack or heap a.r.\n");
	} else {
	  printf(" with heap a.r.\n");
	}
      }
      if (slink_needed && !reuse_ar) {
	  size_loc = avail_loc++;
	  gen2(DCL, size_loc, DCL_INT);
      }
      if (construction != NIL) {
          slink_known = p -> ap_operator -> signature
			  -> fsig_slink_known;
	  if ((construction -> fc_complexity & CP_GLOBALS)
	      /* Non-locals are copied, need real environment */
	      /* Note: CP_GLOBALS ==> closure will be built   */
	      || (Gcurrent -> fc_complexity & CP_GLOBALS)
		 && construction -> ar_static_level > 1) {
	      /* Need static link in current closure */
		  ASSERT((construction -> fc_complexity & NEED_CL)
			 || (!slink_needed) || bogus_slink_ok,
			 "Gappl: reference to nonexistent closure\n");
		  slink_known = FALSE;
	  }
      }
      /* Evaluate operator if necessary */
	if (construction == NIL
	   || (!slink_known && slink_needed && !bogus_slink_ok)) {
            fn_loc = avail_loc++;
	    gen2(DCL, fn_loc, DCL_ADDR);
	    Gexpression (p -> ap_operator, fn_loc, FALSE);
	} else if (op_impure) {
          /* evaluate operator for side effects */
	    if (Vflag) {
		printf("\t- Operator evaluated for effect\n");
	    }
	    Gexpression (p -> ap_operator, SK, FALSE);
	} else {
	  /* May contain needed function construction */
	    Gtraverse (p -> ap_operator);
	}
      /* Compute size of a.r. */
	if (!reuse_ar) {
	  if (construction != NIL) {
	    if (slink_needed) {
		ASSERT2(construction -> ar_size > 0,
			"function %X has bad ar size field\n",
			construction);
		gen2(LDN, construction -> ar_size, size_loc);
	    } /* else we dont explicitly allocate */
	  } else {
	    gen3(LDI, fn_loc, FO_SIZE, size_loc);
	  }
	}
      /* allocate activation record */
	if (reuse_ar) {
	  if (sl_available) {
	      int i = Glevel - Gcurrent -> ar_static_level;

	      if (i > 0) {
		/* Set ar_loc to the act. record for the entire function. */
		ar_loc = avail_loc++;
		gen2(DCL, ar_loc, DCL_ADDR);
		gen3(LDI, AR, 0, ar_loc);
		while (--i > 0) {
		    gen3(LDI, ar_loc, 0, ar_loc);
		}
	      } else {
		ar_loc = AR;
	      }
	  } else {
	      ar_loc = -1; /* garbage, shouldnt be used */
	  }
	} else if (slink_needed) {
	  ar_loc = avail_loc++;
	  gen2(DCL, ar_loc, DCL_ADDR);
	  if (stack_call) {
	    if (construction == NIL) {
		gen_abs(size_loc);
	    }
	    if (fflag) {
		gen1(HINT, ONS);
		gen2(ALH, size_loc, ar_loc);
	    } else {
		gen1(ALS, size_loc);
		gen2(MOV, SP, ar_loc);
	    }
	  } else if (test_closure && !fflag) {
	    old_sp_loc = avail_loc++;
	    gen2(DCL, old_sp_loc, DCL_INT);
	    gen2(MOV, SP, old_sp_loc);
	    gen3(GEI, size_loc, C0, TL);
	    genl(BRT, "1f");
	    gen2(NGI, size_loc, size_loc);
	    gen2(ALH, size_loc, ar_loc);
	    genl(BR, "2f");
	    genl(LBL, "1");
	    gen1(ALS, size_loc);
	    gen2(MOV, SP, ar_loc);
	    genl(LBL, "2");
	  } else {
	    if (construction == NIL) {
		gen_abs(size_loc);
	    }
	    gen2(ALH, size_loc, ar_loc);
	  }
	}
      /* evaluate arguments and put them in activation record  or */
      /* pass them using ARG instructions  (in reverse order).    */
	if (! slink_needed || reuse_ar) {
	  /* Evaluate all arguments before we clobber old a.r. */
	  /* If we are not building an activation record, we   */
	  /* need to do all evaluations first, so that ARG     */
	  /* instructions appear before the correct call.      */
	    int next_loc;
	    int ar_offset = 1;
	    int cur_arg_no;

	    /* Grab locations for all args and declare them */
	      first_arg_loc = avail_loc;
	      next_loc = arg_loc = avail_loc = avail_loc + argcount;
	      for (i = first_arg_loc; i < next_loc; i++) {
		gen2(DCL, i, DCL_INT);
	      }
	    maprlist_non_vacuous(p -> ap_args, Garg_expression);
	    /* Now stuff them into a.r. */
	      if (slink_needed) {
		for (i = first_arg_loc; i < next_loc; i++) {
		  gen3(STI, ar_loc, ar_offset, i);
		  ar_offset++;
		}
	      } else if (!reuse_ar) {
		/* Pass them with ARG instructions */
		  cur_arg_no = argcount;
		  for (i = next_loc - 1; i >= first_arg_loc; i--) {
		    gen2(ARG, cur_arg_no--, i);
		  }
		  ASSERT(cur_arg_no == 0, "Appl: incorrect arg count");
	      } else {
		extern int first_param_loc;

		/* This must be a call to the current function */
		ASSERT(first_param_loc != 0,
		       "Appl: bad tail recursion\n");
		for (i = 0; i < argcount; i++) {
		  gen2(MOV, first_arg_loc + i, first_param_loc + i);
		}
		ASSERT(argcount == Gcurrent -> ar_size - 1,
		       "Appl: bad arg count for tail recursion\n");
	      }
	    /* undeclare argument locations */
	      for (i = first_arg_loc; i < next_loc; i++) {
		gen1(UDC, i);
	      }
	} else {
	  heap_loc = ar_loc;
	  heap_offset = argcount;
	  maprlist_non_vacuous (p-> ap_args, Gheap_expression);
	}
      /* store static link */
	if (slink_needed) {
	  if (bogus_slink_ok) {
	      if (!stack_call) {
		/* Avoid bogus references to heap objects */
		gen3(STI, ar_loc, 0, UN);
	      } else {
#               ifdef DEBUG
		  gen3(STI, ar_loc, 0, UN);
#               endif
	      }
	  } else if (construction == NIL || !slink_known) {
	      i = avail_loc++;
	      gen2(DCL, i, DCL_INT);
	      gen3(LDI, fn_loc, FO_EP, i);
	      gen3(STI, ar_loc, 0, i);
	      gen1(UDC, i);
	  } else {
	    if (direct_rec_call) {
	      /* It's already there */
	    } else {
	      int ep_loc;
                  /* name of loc used for a.r. pointer */

#             ifdef DEBUG
		if (Glevel < ((construction -> ar_static_level) - 1)) {
		  dbgmsg ("Negative level difference for function call\n");
		  fprintf (stderr, "Current: %d; Construction: %d; Application:\n",
			   Glevel, (construction -> ar_static_level));
		  unparse_file = stderr;
		  unparse(p);
		  fprintf(stderr, "\n");
		  abort(p);
		}
#             endif
	      DISPLAY ( ep_loc, ((construction -> ar_static_level) - 1));
	      gen3(STI, ar_loc, 0, ep_loc);
	      UNDISPLAY(ep_loc);
	    }
	  }
	}
      /* Pass AR as the only argument.  Old value is implicitly saved */
      /* at the time of the call.                                     */
	  if (slink_needed && !direct_rec_call) {
	    gen2(ARG, 1, ar_loc);
	  }
      /* Make the call */
	  if (direct_rec_call) {
	    /* Set up correct AR */
	      if (Glevel != Gcurrent -> ar_static_level) {
		gen2(MOV, ar_loc, AR);
	      }
	    if (slink_needed) {
		strcpy(str_code_buf, "R");
	    } else {
		strcpy(str_code_buf, "RF");
	    }
	    strcat(str_code_buf, construction -> fc_code_label);
	    genl(BR, str_code_buf);
	  } else {
	    if (construction != NIL) {
	      if (!slink_needed) {
		strcpy(str_code_buf, "F");
		strcat(str_code_buf, construction -> fc_code_label);
		genl(EXT, str_code_buf);
#               ifdef UNDEFINED
		/* We assume that CLC can't result in CALLCC call */
		  if (Nflag || construction -> fc_complexity & NO_CALLCC) {
		    gen1(HINT, NSC);
		  }
#               endif
		genl(LBA, str_code_buf);
		gen1(CLC, argcount);
	      } else {
		genl(EXT, construction -> fc_code_label);
		if (Nflag || construction -> fc_complexity & NO_CALLCC) {
		    gen1(HINT, NSC);
		}
		genl(CLL, construction -> fc_code_label);
	      }
	    } else /* NIL construction */ {
	      if (Nflag) {
		gen1(HINT, NSC);
	      }
	      gen2(CLI, fn_loc, FO_IP);
	    }
	  }
      /* remove activation record */
	  if (reuse_ar) {
	    if (sl_available && ar_loc != AR) {
	      gen1(UDC, ar_loc);
	    }
	  } else {
	    if (stack_call) {
	      if (slink_needed) /* otherwise CLC takes care of it */ {
		if (fflag) {
		  /* AR is on heap, but we can explicitly deallocate */
		    gen1(HINT, ONS);
		    if (construction != NIL) {
		      int sz = construction -> ar_size;

		      if (sz < 0) { sz = -sz; }
		      gen3(HINT, DEA, ar_loc, construction -> ar_size);
		    } else {
		      gen3(HINT, DEA, ar_loc, 0);
		    }
		} else {
		    gen3(ADP, SP, size_loc, SP);
		}
	      }
	    } else if (test_closure && !fflag) {
	      gen2(MOV, old_sp_loc, SP);
	      gen1(UDC, old_sp_loc);
	    }
	    /* In the heap case there's nothing to do */
	    if (slink_needed) {
	        gen1(UDC, ar_loc);
	    }
	  }
      /* undeclare temporaries */
	if (slink_needed && ! reuse_ar) {
	    gen1(UDC, size_loc);
	}
	if (construction == NIL
	    || (slink_needed && !slink_known && !bogus_slink_ok)) {
          gen1(UDC, fn_loc);
        }
      /* Put result in its place */
	if (rloc != RL) {
	    gen2(MOV, RL, rloc);
	}
    }  /* end not in-line */
}

/*
 *   Generate an ARG 1 instr with the size of the array corresponding to the
 * type from which op is selected.  The size may either be given explicitly,
 * or an attempt will be made to infer it from the operator.
 *   Returns TRUE if it succeeded, FALSE otherwise.
 */
boolean Gpush_size(size, op)
int size;
NODE * op;
{
    NODE * sel_type_sig;
    NODE * size_sig;
    NODE * size_appl;
    NODE * size_id;
    extern NODE * id_size;

    if (size == 0) {
	/* Try to put size of array rep on top of the stack */
	/* using size function of the array type            */
	  if (op -> kind != LETTERID || op -> sel_type == NIL) {
	      return(FALSE);
	  }
	  sel_type_sig = op -> sel_type -> signature;
	  size_sig = getcomp(sel_type_sig, id_size, NIL, NIL,
			     NIL, NIL, NIL, FALSE);
	  if (size_sig == NIL || special_tp(size_sig -> fsig_special)
				 != ARRAY_SIZE) {
	      /* No appropriate size operation */
	      return(FALSE);
	  }
	  /* Construct size application and generate code for it */
	    size_id = copynode(id_size);
	    initfld(&(size_id -> sel_type), op -> sel_type);
	    size_id -> id_def_found = TRUE;
	    size_appl = mknode(APPLICATION, size_id, emptylist());
	    checksigs(size_appl, FALSE);
	    {
		int i = avail_loc++;

		gen2(DCL, i, DCL_INT);
		Gappl(size_appl, i, FALSE);
		gen2(ARG, 1, i);
		gen1(UDC, i);
	    }
    } else {
      /* Pass a constant */
	int i = avail_loc++;

	gen2(DCL, i, DCL_INT);
	gen2(LDN, size, i);
	gen2(ARG, 1, i);
	gen1(UDC, i);
    }
    return(TRUE);
}
