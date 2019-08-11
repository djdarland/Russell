# define DEBUG

# define TRACE
# undef TRACE
# include <stdio.h>
# include "../pass5d/op_codes.h"
# include "../parm.h"
# include "RIC.h"

# define HASHT_SIZE 32

# define mod_sz(n)  ((n) & 0x1f)

typedef struct RIC_instr * RICp;

# define NO_REG -2

char label_buf[MAXLABELSZ+1];

struct RIC_instr * cbb = RIC_nil; /* A List of instructions representing the */
				  /* current basic block.                    */
struct RIC_instr * cbb_last = RIC_nil;
				  /* Pointer to the end of the above list */

/* Add the instruction p to cbb, following q, or at the */
/* beginning if q = RIC_nil                             */
void add_after(p, q)
RICp p, q;
{
    if (q == RIC_nil) {
	p -> next_instr = cbb;
	/* p -> prev_instr = RIC_nil; */
	cbb = p;
    } else {
	p -> next_instr = q -> next_instr;
	p -> prev_instr = q;
	q -> next_instr = p;
    }
    if (p -> next_instr == RIC_nil) {
	cbb_last = p;
    } else {
	p -> next_instr -> prev_instr = p;
    }
}

/* Delete the instruction p from cbb.  Also delete preceding HINT */
/* LBR and LBA instructions that refer to p.                      */
void delete(p)
register RICp p;
{
    register RICp prev = p -> prev_instr;
    long hint_arg;

    /* Delete instructions modifying p */
      while (prev != RIC_nil 
	   && (prev -> op_code == LBR
	       || prev -> op_code == LBA
	       || prev -> op_code == HINT
		  && ((hint_arg = prev -> arg[0]) == NP
		      || hint_arg == AL
		      || hint_arg == NSC
		      || hint_arg == STSZ
		      || hint_arg == PT))) {
	delete(prev);
	prev = p -> prev_instr;
      }
    if (prev == RIC_nil) {
	cbb = p -> next_instr;
    } else {
	p -> prev_instr -> next_instr = p -> next_instr;
    }
    if (p -> next_instr == RIC_nil) {
	cbb_last = p -> prev_instr;
    } else {
	p -> next_instr -> prev_instr = p -> prev_instr;
    }
    /* We don't free the node, since that would complicate traversals */
}

/* Deallocate and clear the current basic block */
void clear_block()
{
    register RICp last, next;

    last = cbb;
    while (last != RIC_nil) {
	next = last -> next_instr;
	GC_free(last);
	last = next;
    }
    cbb = RIC_nil;
    cbb_last = RIC_nil;
}

/* Write out the current basic block onto stdout */
void write_block()
{
    register RICp p;
    register int opc;
    register char * s;

    p = cbb;
    while (p != RIC_nil) {
	opc = p -> op_code;
	putw(opc, stdout);
	if (p -> label_arg) {
	    for (s = p -> label; *s != '\0'; s++) {
		putchar(*s);
	    }
	    putchar('\0');
	} else {
	    putw(p -> arg[0], stdout);
	    putw(p -> arg[1], stdout);
	    putw(p -> arg[2], stdout);
	}
	p = p -> next_instr;
    }
}

/* Build a three address instruction */
RICp make_instr(opc, arg1, arg2, arg3)
int opc, arg1, arg2, arg3;
{
    RICp result = (RICp) GC_malloc(sizeof (struct RIC_instr));

    result -> op_code = opc;
    result -> label_arg = FALSE;
    result -> arg[0] = arg1;
    result -> arg[1] = arg2;
    result -> arg[2] = arg3;
    /* result -> next_instr = result -> prev_instr = RIC_nil; */
    /* result -> second_decl = FALSE; */
    return(result);
}

/* Build an instruction containing a label */
RICp make_lbl_instr(opc, lbl)
int opc;
char * lbl;
{
    int len = strlen(lbl);
    RICp result = (RICp) GC_malloc(sizeof (struct RIC_instr) + len);

    result -> op_code = opc;
    result -> label_arg = TRUE;
    strcpy(result -> label, lbl);
    /* result -> next_instr = result -> prev_instr = RIC_nil; */
    return(result);
}

/* Read an instruction from stdin.  Return a pointer to */
/* the instruction.                                     */
RICp read_instr()
{
    int opc;
    int i;
    int arg;

    opc = getw(stdin);
    if (feof(stdin)) {
	return(RIC_nil);
    }
    if (opc >= N_OP_CODES) {
	fprintf(stderr, "RICfilter: bad op code\n");
	exit(1);
    }
    if (opc <= MAX_LABEL_OP) {
	char *p = label_buf;
	int c;
	int i = 0;
	    
	while ((c = getchar()) != '\0' && c != EOF) {
	    *p++ = c;
	    if (++i >= MAXLABELSZ) {
		fprintf(stderr, "Label too long\n");
		p = label_buf;
	    }
	}
	*p = '\0';
	return(make_lbl_instr(opc, label_buf));
    } else {
	int arg1, arg2, arg3;

	arg1 = getw(stdin);
	arg2 = getw(stdin);
	arg3 = getw(stdin);
	if (opc == HINT && arg1 == OPT) {
	    int cnt = arg2;

	    /* Discard cnt instructions */
		for (; cnt > 0; cnt --) {
		    GC_free(read_instr());
		}
	    return(read_instr());
	} else {
	    return(make_instr(opc, arg1, arg2, arg3));
	}
    }
}


# define is_real_reg(r) ((r) >= FIRST_AVAIL_LOC || (r) == TL \
			 || (r) == RL || (r) == T2)

/* Set the result_reg, arg_reg, and side_effect fields   */
/* of instruction p.  Among special registers, only RL,  */
/* TL, and T2 are considered to be "real registers".     */
void set_regs(p)
RICp p;
{
    register int result = NO_REG;
    register int op1 = NONE;
    register int op2 = NONE;
    int op1r, op2r;
    register boolean has_se = FALSE;    /* Instruction has a side effect */

    switch(p -> op_code) {
	case BR:
	case BRT:
	case BRF:
	case LBL:
	case EXT:
	case LBA:
	case BFN:
	case TFB:
	case TFE:
	case PRO:
	case ADT:
	case ERR:
	case BSF:
	case DCL:
	case UDC:
	case RTN:
	case IDT:
	case DDT:
	case FDT:
	case LBR:
	    has_se = TRUE;
	    break;
	case STI:
	    op1 = 0;
	    op2 = 2;
	    has_se = TRUE;
	    break;
	case TAR: 
	    op1 = 0;
	    op2 = 1;
	    has_se = TRUE;
	    break;
	case HINT:
	    switch(p -> arg[0]) {
		case DEA:
		case STSZ:
		case DEAD:
		case LIVE:
		    op1 = 1;
		    break;
	    }
	    has_se = TRUE;
	    break;
	case ARG:
	    op1 = 1;
	    has_se = TRUE;
	    break;
	case ALS:
	case PSH:
	    op1 = 0;
	    has_se = TRUE;
	    break;
	case CLL:
	case CLC:
	    has_se = TRUE;
	    result = RL;
	    break;
	case CLI:
	    has_se = TRUE;
	    op1 = 0;
	    result = RL;
	    break;
	case LDL:
	case TRU:
	case FLS:
	case LDS:
	    result = p -> arg[0];
	    break;
	case ALH:
	case ALA:
	    has_se = TRUE;
	    op1 = 0;
	case GAR:
	case LDN:
	    result = p -> arg[1];
	    break;
	case MOV:
	case NGI:
	case ABI:
	case NOT:
	case NGF:
	case EXF:
	    op1 = 0;
	    result = p -> arg[1];
	    break;
	case LDI:
	case LDC:
	    op1 = 0;
	    result = p -> arg[2];  
	    break;   
	case ADI:
	case SBI:
	case MLI:
	case DVI:
	case EQI:
	case LTI:
	case GTI:
	case NEI:
	case LEI:
	case GEI:
	case SHI:
	case AND:
	case OR:
	case ADP:
	case ADF:
	case SBF:
	case MLF:
	case DVF:
	case EQF:
	case LTF:
	case GTF:
	case NEF:
	case LEF:
	case GEF:
	case SHF:
	    op1 = 0;
	    op2 = 1;
	    result = p -> arg[2];  
	    break;   
	default:
	    fprintf(stderr, "RICfilter: strange opcode: %d\n", p -> op_code);
	    abort();
	    break;
    }
#   ifdef DEBUG
	if (result == NO_REG && !has_se) {
	    fprintf(stderr, "RICfilter: vacuous instruction: %d\n",
		    p -> op_code);
	    abort();
	}
#   endif
    if (result == SK) {
	result = NO_REG;
    } else if (result < FIRST_AVAIL_LOC && result != RL &&
	       result != TL && result != T2) {
	result = NO_REG;
	has_se = TRUE;
    }
    op1r = p -> arg[op1];
    if (!is_real_reg(op1r)) {
	op1 = NONE;
    }
    op2r = p -> arg[op2];
    if (!is_real_reg(op2r)) {
	op2 = NONE;
    }
    p -> op1_reg = op1;
    p -> op2_reg = op2;
    p -> result_reg = result;
    p -> side_effect = has_se;
}

/* The hash table structure used to keep track of info associated with regs */
/* as we are making a pass over the code.                                   */
struct reg_descr {
    struct reg_descr * next_reg;
    struct reg_descr * next_mapped_reg;  /* Link for mapped regs list */
    int this_reg;
    int status;
#       define S_LIVE 0
#       define S_DEAD 1 /* known to be dead at this point */
#       define S_UDC  2 /* Only subsequent use us in UDC  */
    int equiv_reg;  /* Register to be used instead of this one.    */
		    /* Guaranteed to have the same contents as     */
		    /* this one.                                   */
		    /* Note that equiv_reg will never itself be    */
		    /* forwarded to another one.                   */
    struct reg_descr * mapped_regs;
		    /* Descriptors of registers mapped to this one via */
		    /* equiv_regs field.                               */
    long const_val;  /* Constant value contained in this register */
#       define NOT_CONSTANT 0x40000000
};

typedef struct reg_descr * regp;

# define reg_nil ((regp) 0)

regp hasht[HASHT_SIZE];

regp lookup();

/* Invalidate register equivalences and constant information involving r */
void clear_equiv(r)
regp r;
{
    regp p, q, s;
    regp * t_ptr;
    int equiv;
#   ifdef DEBUG
	boolean foundit = FALSE;
#   endif

#   ifdef TRACE
	fprintf(stderr, "Clear_equiv: %d\n", r -> this_reg);
#   endif
    /* Clear mappings of other registers to this one. */
      for (p = r -> mapped_regs; p != reg_nil; p = q) {
#   ifdef TRACE
	fprintf(stderr, "\tClearing mapping from %d\n", p -> this_reg);
#   endif
	q = p -> next_mapped_reg;
#       ifdef DEBUG
	    if (p -> equiv_reg != r -> this_reg) {
		fprintf(stderr,
			"RICfilter: Register equivalence table messed up\n");
		abort(r,p);
	    }
#       endif
	p -> equiv_reg = NO_REG;
	p -> next_mapped_reg = reg_nil;
      }
    /* Clear mapping info for r */
      if ((equiv = r -> equiv_reg) != NO_REG && is_real_reg(equiv)) {
	s = lookup(equiv);
	/* remove r from s's list of mapped registers */
	  for (t_ptr = &(s -> mapped_regs); *t_ptr != reg_nil;
	       t_ptr = &((*t_ptr) -> next_mapped_reg)) {
#           ifdef DEBUG
	      if ((*t_ptr) -> equiv_reg != r -> equiv_reg) {
		fprintf(stderr, "RICfilter: bad reg on list\n");
		abort(*t_ptr, r);
	      }
#           endif
	    /* *t_ptr is the list entry currently being examined */
	    if (*t_ptr == r) {
	      *t_ptr = r -> next_mapped_reg;
	      r -> next_mapped_reg = reg_nil;
#             ifdef DEBUG
		foundit = TRUE;
#             endif
	      break;
	    }
	  }
#       ifdef DEBUG
	    if (!foundit) {
	      fprintf(stderr,
		      "RICfilter: mapped register not on list\n");
	      abort(r,s);
	    }
#       endif
      }
      r -> equiv_reg = NO_REG;
      r -> mapped_regs = reg_nil;
      r -> const_val = NOT_CONSTANT;
}

/* Return the hash table entry corresponding to reg_no */
/* allocate an entry if there previously was none.     */
regp lookup(reg_no)
int reg_no;
{
    regp * he = &(hasht[mod_sz(reg_no)]);
    regp p;

    for (p = *he; p != reg_nil; p = p -> next_reg) {
	if (p -> this_reg == reg_no) {
	    return(p);
	}
    }
    p = (regp) GC_malloc(sizeof(struct reg_descr));
    p -> this_reg = reg_no;
    p -> next_reg = *he;
    /* p -> status = S_LIVE; */
    /* p -> mapped_regs = reg_nil; */
    /* p -> next_mapped_reg = reg_nil; */
    p -> equiv_reg = NO_REG;
    p -> const_val = NOT_CONSTANT;
    *he = p;
    return(p);
}

/* Clear the hash table of register descriptions */
/* Flush any UDCs that are still pending         */
void clear_hasht()
{
    int i;
    regp p, q;

    for (i = 0; i < HASHT_SIZE; i++) {
	for (p = hasht[i]; p != reg_nil;) {
	    q = p -> next_reg;
	    if (p -> status == S_UDC) {
#               ifdef TRACE
		    fprintf(stderr, "Flushing UDC for register %d\n", p -> this_reg);
#               endif
		add_after(make_instr(UDC, p -> this_reg, SK, SK), RIC_nil);
	    }
	    GC_free(p);
	    p = q;
	}
	hasht[i] = reg_nil;
    }
}

long const_arg_table[3];
		    /* Constant values of the first 3 arguments to */
		    /* the current function call.  NOT_CONSTANT    */
		    /* for a non-constant parameter.               */
RICp arg_instrs[3];
		    /* The ARG instructions for the first 3 args */
		    /* for the current function call.            */

/* Perform local copy propogation on the current basic block */
/* Set status to S_UDC for those regs that are undeclared    */
/* before the end of the block.  (This is done here, so that */
/* we can continue to use registers past the original UDC.)  */
/* The status of RL and TL is set to S_UDC if the locations  */
/* where undeclared after the last assignment to them.       */
/* We also maintain const_arg_table and replace calls to the */
/* floating point dot operations by constants when possible. */
void propagate()
{
    register RICp p;
    register int reg;
    register int reg2;
    regp q, r;
    int i;

    for (p = cbb; p != RIC_nil; p = p -> next_instr) {
	switch(p -> op_code) {
	  case MOV:
	    reg = p -> arg[1];
	    if (is_real_reg(reg)) {
		reg2 = p -> arg[0];
		/* Is it already a copy? */
		if (is_real_reg(reg2)) {
		    q = lookup(reg2);
		    if (q -> equiv_reg != NO_REG) {
			reg2 = q -> equiv_reg;
		    }
		}
		q = lookup(reg);
		if (reg == TL || reg == RL) {
		    q -> status = S_LIVE;   /* Preceding UDC not valid at */
					    /* end of basic block         */
		}
		if (reg == reg2) {
#                   ifdef TRACE
			fprintf(stderr, "Deleting redundant MOV\n");
#                   endif
		    delete(p);
		} else {
		    if (q -> equiv_reg != NO_REG
			&& is_real_reg(q -> equiv_reg)
			|| q -> mapped_regs != reg_nil) {
			clear_equiv(q);
		    }
		    if (reg2 != SP /* may change unexpectedly */) {
			q -> equiv_reg = reg2;
		    }
		    if (is_real_reg(reg2)) {
			r = lookup(reg2);
			q -> next_mapped_reg = r -> mapped_regs;
			r -> mapped_regs = q;
			q -> const_val = r -> const_val;
		    } else {
			/* Set const_val */
			  switch(reg2) {
			    case C0:
				q -> const_val = 0;
				break;
			    case C1:
				q -> const_val = 1;
				break;
			    case C2:
				q -> const_val = 2;
				break;
			    case C3:
				q -> const_val = 3;
				break;
			    case C4:
				q -> const_val = 4;
				break;
			    default:
				q -> const_val = NOT_CONSTANT;
				break;
			  }
		    }
#                   ifdef TRACE
			fprintf(stderr, "%d is a copy of %d\n", reg, reg2);
#                   endif
		}
	    }
	    break;

	  case ARG:
	    q = reg_nil;
	    reg = p -> arg[1];
	    if (is_real_reg(reg)) {
		q = lookup(reg);
		if (q -> equiv_reg != NO_REG) {
		    /* Replace by an equivalent register */
#                   ifdef TRACE
			fprintf(stderr,
				"Replacing reference to %d in ARG by one to %d\n",
				reg, q -> equiv_reg);
#                   endif
		    p -> arg[1] = q -> equiv_reg;
		}
	    }
	    i = p -> arg[0] - 1;  /* 0-based argument number */
	    if (i < 3) {
	      /* Update const_arg_table and arg_instrs */
		arg_instrs[i] = p;
		if (q != reg_nil /* real register */) {
		    const_arg_table[i] = q -> const_val;
		} else {
		    switch(reg) {
			case C0:
			    const_arg_table[i] = 0;
			    break;
			case C1:
			    const_arg_table[i] = 1;
			    break;
			case C2:
			    const_arg_table[i] = 2;
			    break;
			case C3:
			    const_arg_table[i] = 3;
			    break;
			case C4:
			    const_arg_table[i] = 4;
			    break;
			default:
			    const_arg_table[i] = NOT_CONSTANT;
		    }
		}
	    }
	    break;

	  case CLC:
	    {
#               define DP_DOT "_Float_Dot"
#               define SP_DOT "_SFloat_Dot"
#               define SBFSZ 120
		RICp clc_instr = p;
		RICp lba_instr = p -> prev_instr;
		RICp next_i = p -> next_instr; /* Cant be NIL */
		RICp prev_i;
		RICp const_i;
		RICp q;
		char * routine_name = lba_instr -> label;
		boolean args_constant = TRUE;
		boolean is_dp_dot;
		boolean is_sp_dot;
		char buf[SBFSZ];

		if (clc_instr -> arg[0] == 3) {
		    for (i = 0; i < 3; i++) {
			args_constant &= (const_arg_table[i] != NOT_CONSTANT);
		    }
		    if (const_arg_table[2] > SBFSZ - 30) {
			/* Possibly absurd floating point constant.  */
			/* Dont bother with it.                      */
			args_constant = FALSE;
		    }
		    if (args_constant) {
		      is_dp_dot = (strcmp(routine_name, DP_DOT) == 0);
		      is_sp_dot = (strcmp(routine_name, SP_DOT) == 0);
		    }
		    if (args_constant &&
			(is_dp_dot || is_sp_dot)) {
			/* Delete argument instructions */
			  for (i = 0; i < 3; i++) {
			    delete(arg_instrs[i]);
			  }
			/* Delete the call and LBA */
			  delete(clc_instr);
			/* Reconstruct the floating point constant */
			  sprintf(buf, "%d.%0*d",
				  const_arg_table[0],
				  const_arg_table[2],
				  const_arg_table[1]);
			/* Insert the floating point constant into the code */
			  prev_i = next_i -> prev_instr;
			  if (is_dp_dot) {
			    const_i = make_lbl_instr(DDT, buf);
			  } else {
			    const_i = make_lbl_instr(FDT, buf);
			  }
			  set_regs(const_i);
			  add_after(const_i, prev_i);
			  q = make_lbl_instr(LBA, "1");
			  set_regs(q);
			  add_after(q, prev_i);
			/* Load it into RL */
			  if (is_sp_dot) {
			    q = make_instr(LDI, RL, 0, RL);
			    set_regs(q);
			    add_after(q, const_i);
			  }
			  q = make_instr(LDL, RL, SK, SK);
			  set_regs(q);
			  add_after(q, const_i);
			  q = make_lbl_instr(LBA, "1b");
			  set_regs(q);
			  add_after(q, const_i);
		    }
		}
		/* Update register table to reflect the fact that */
		/* RL was clobbered.                              */
		  r = lookup(RL);
		  clear_equiv(r);
		  r -> status = S_LIVE;
	    }
	    break;

	  case UDC:
	    if (is_real_reg(p -> arg[0])) {
	      q = lookup(p -> arg[0]);
	      q -> status = S_UDC;
	    }
	    break;

	  case DCL:
	    q = lookup(p -> arg[0]);
	    if (q -> status == S_UDC) {
		/* previously declared and undeclared */
		p -> second_decl = TRUE;
	    }
	    q -> status = S_LIVE;
	    break;

	  case LDN:
	    reg = p -> arg[1];
	    if (reg == SK) {
		delete(p);
		break;
	    }
#           ifdef DEBUG
		if (!is_real_reg(reg)) {
		    fprintf(stderr, "RICfilter: bad LDN arg\n");
		    abort();
		}
#           endif
	    q = lookup(reg);
	    if (reg == TL || reg == RL) {
		q -> status = S_LIVE;   /* Preceding UDC not valid at */
					/* end of basic block         */
	    }
	    if (q -> mapped_regs != reg_nil) {
		clear_equiv(q);
	    }
	    q -> const_val = p -> arg[0];
	    switch(p -> arg[0]) {
		case 0:
		    q -> equiv_reg = C0;
		    break;
		case 1:
		    q -> equiv_reg = C1;
		    break;
		case 2:
		    q -> equiv_reg = C2;
		    break;
		case 3:
		    q -> equiv_reg = C3;
		    break;
		case 4:
		    q -> equiv_reg = C4;
		    break;
		default:
		    q -> equiv_reg = NO_REG;
		    break;
	    }
	    break;

	  case ALA:
	  case ALH:
	    /* These may clobber RL */
	    q = lookup(RL);
	    clear_equiv(q);
	    /* and continue: */

	  default:
	    reg = p -> result_reg;
	    if (reg != NO_REG) {
		q = lookup(reg);
		if (q -> equiv_reg != NO_REG || q -> mapped_regs != reg_nil) {
		    clear_equiv(q);
		}
		if (reg == TL || reg == RL) {
		    q -> status = S_LIVE;   /* Preceding UDC not valid at */
					    /* end of basic block         */
		}
	    }
	    if (p -> op1_reg != NONE) {
		reg = p -> arg[p -> op1_reg];
		q = lookup(reg);
		if (q -> equiv_reg != NO_REG) {
		    /* Replace by an equivalent register */
#                   ifdef TRACE
			fprintf(stderr,
				"Replacing reference to %d by one to %d\n",
				p -> arg[p -> op1_reg], q -> equiv_reg);
#                   endif
		    p -> arg[p -> op1_reg] = q -> equiv_reg;
		}
	    }
	    if (p -> op2_reg != NONE) {
		reg = p -> arg[p -> op2_reg];
		q = lookup(reg);
		if (q -> equiv_reg != NO_REG) {
		    /* Replace by an equivalent register */
#                   ifdef TRACE
			fprintf(stderr,
				"Replacing reference to %d by one to %d\n",
				p -> arg[p -> op2_reg], q -> equiv_reg);
#                   endif
		    p -> arg[p -> op2_reg] = q -> equiv_reg;
		}
	    }
	}
    }
}

/* Perform dead code elimination and insert hints about dead registers into */
/* the current basic block.                                                 */
void eliminate()
{
    RICp p;
    regp q;
    boolean deleted;

    for (p = cbb_last; p != RIC_nil; p = p -> prev_instr) {
#       ifdef DEBUG
	    if (p -> result_reg == 0) {
		fprintf(stderr, "RICfilter: Missing result info\n");
	    }
#       endif
	switch (p -> op_code) {
	    case UDC:
		/* Was already entered as undeclared in register table */
		/* Will be regenerated in a more appropriate place.    */
		delete(p);
	    break;
	    case DCL:
		q = lookup(p -> arg[0]);
		if (q -> status == S_UDC) {
		    /* Location not used */
#                   ifdef TRACE
		      fprintf(stderr, "Deleting DCL %d\n", p -> arg[0]);
#                   endif
		    delete(p);
		}
		if (p -> second_decl) {
		    q -> status = S_UDC;
		    /* Need to generate another UDC */
		} else {
		    q -> status = S_DEAD;  /* Don't need to generate UDC */
		}
		break;
	    default:
		deleted = FALSE;
		if (!(p -> side_effect)) {
		    if (p -> result_reg == NO_REG) {
#                       ifdef TRACE
			  fprintf(stderr,
				  "deleting result-less instruction, opcode = %d\n",
				  p -> op_code);
#                       endif
			delete(p);
			deleted = TRUE;
		    } else {
			q = lookup(p -> result_reg);
			if (q -> status == S_UDC
			    || q -> status == S_DEAD) {
#                           ifdef TRACE
			      fprintf(stderr,
				      "deleting dead instruction, opcode = %d, result = %d\n",
				      p -> op_code, p -> result_reg);
#                           endif
			    delete(p);
			    deleted = TRUE;
			}
		    }
		}
		if (!deleted) {
		    int opr1, opr2;

		    /* update liveness info for result register, */
		    /* and reinsert UDC for result, if necessary */
			if (p -> result_reg != NO_REG) {
			    q = lookup(p -> result_reg);
			    if (q -> status == S_UDC) /* unlikely, but ...*/ {
				add_after(make_instr(UDC, p -> result_reg,
						     SK, SK), p);
			    } else if (q -> status == S_DEAD) {
				add_after(make_instr(HINT, DEAD,
                                          	     p -> result_reg,
                                                     SK), p);
			    }
			    q -> status = S_DEAD;
			}
		    /* update liveness info for arguments, and insert UDCs */
			if (p -> op1_reg != NONE) {
			    opr1 = p -> arg[p -> op1_reg];
			    q = lookup(opr1);
			    if (q -> status == S_UDC) {
				add_after(make_instr(UDC, opr1, SK, SK), p);
			    } else if (q -> status == S_DEAD
                                       && opr1 != p -> result_reg) {
				add_after(make_instr(HINT, DEAD, opr1, SK), p);
			    }
#                           ifdef TRACE
			      fprintf(stderr,
				      "Changing status of %d to S_LIVE\n",
				      opr1);
#                           endif
			    q -> status = S_LIVE;
			}
			if (p -> op2_reg != NONE) {
			    opr2 = p -> arg[p -> op2_reg];
			    q = lookup(opr2);
			    if (q -> status == S_UDC) {
				add_after(make_instr(UDC, opr2, SK, SK), p);
			    } else if (q -> status == S_DEAD
                                       && opr2 != p -> result_reg) {
				add_after(make_instr(HINT, DEAD, opr2, SK), p);
			    }
#                           ifdef TRACE
			      fprintf(stderr,
				      "Changing status of %d to S_LIVE\n",
				      opr2);
#                           endif
			    q -> status = S_LIVE;
			}
		}
	}
    }
}

/* Perform optimizations on current basic block */
void optimize_block()
{
    /* Propagate copies */
      propagate();
    /* eliminate dead code, propagate back UDCs, and insert HINT DEADs */
      eliminate();
    /* Deallocate register hash table and flush pending UDCs */
      clear_hasht();
}

main(argc, argv)
int argc;
char ** argv;
{
    RICp ci;        /* current instruction */
    RICp pi;        /* previous instruction */
    int opc;

    GC_init();
    if (argc != 1) {
	fprintf(stderr, "%s takes no arguments\n", argv[0]);
	exit(1);
    }
    pi = RIC_nil;
    while ((ci = read_instr()) != RIC_nil) {
	set_regs(ci);
	add_after(ci,pi);
	pi = ci;
	opc = ci -> op_code;
	if (opc == BSF || opc == BFN || opc == BRT || opc == BRF
	    || opc == BR || opc == LBL) {
	    /* Process one basic block, and set up for the next one */
	      optimize_block();
	      write_block();
	      clear_block();
	      pi = RIC_nil;
	}
    }
    optimize_block();
    write_block();
    clear_block();
    exit(0);
}
