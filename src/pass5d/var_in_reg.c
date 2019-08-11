# include <stdio.h>
# include "../parm.h"
# include "op_codes.h"
# include "codegen.h"
# include "datatypes/consnodes.h"

/* Routines to deal with adjustment of in-line code for the case in     */
/* which the contents of a variable argument are in a virtual register, */
/* and we thus need to uniformly delete one level of indirection in     */
/* accessing that argument.                                             */

static ConsNode * equiv_regs;
			/* List of registers holding the value of the */
			/* argument we are currently dealing with.    */

/* Check whether a virtual register is on the above list */
static boolean is_this_arg(reg)
unsigned reg;
{
    register ConsNode * p;
    
    {
	for (p = equiv_regs; p != NIL; p = cn_tail(p)) {
	    if ((unsigned)(cn_head(p)) == reg) {
		return(TRUE);
	    }
	}
	return(FALSE);
    }
}

void clear_equiv_regs()
{
    register ConsNode *p;

    p = equiv_regs;
    while (p != NIL) {
	p = cn_del_hd(p);
    }
    equiv_regs = NIL;
}

/* Do some flow independent analysis to compute equiv_regs, the set of  */
/* v. registers that can possibly contain copies of the nth argument.   */
/* Code is a complete in-line expansion of a function.                  */
/* returns FALSE iff the argument can be moved to an externally visible */
/* register, e.g. RS                                                    */

boolean find_equiv_regs(code, n)
struct RIC_instr * code;
int n;
{
    register struct RIC_instr *p;
    register int i;
    unsigned this_arg = n | ARG_BIT;
    boolean equiv_regs_changed;

    equiv_regs = cn_cons(this_arg, NIL);
    do {
	equiv_regs_changed = FALSE;
	for (p = code; p != RIC_nil; p = p -> next_instr) {
	    if (p -> op_code == MOV) {
		if (is_this_arg(p -> arg[0]) && p -> arg[1] != SK) {
#                   ifdef DEBUG
			if ((p -> arg[1]) & ARG_BIT) {
			    dbgmsg("only_indirect_ref: clobbered argument\n");
			}
#                   endif
		    if (p -> arg[1] == RS
			|| p -> arg[1] < FIRST_AVAIL_LOC
			   && p -> arg[1] != T1
			   && p -> arg[1] != T2) {
			/* arg was moved to externally visible location */
			return(FALSE);
		    }
		    if (!is_this_arg(p -> arg[1])) {
			equiv_regs_changed = TRUE;
			equiv_regs = cn_cons(p -> arg[1], equiv_regs);
		    }
		}
	    }
	}
    } while (equiv_regs_changed);
    return(TRUE);
}


/* Determine whether only the contents of the nth argument, rather than */
/* the nth argument itself, is accessed by code.                        */

boolean only_indirect_ref(code, n)
struct RIC_instr * code;
int n;
{
    register struct RIC_instr *p;

    if (!find_equiv_regs(code, n)) {
	return(FALSE);
    }
    for (p = code; p != RIC_nil; p = p -> next_instr) {
	if (!p -> label_arg) {
	    switch(p -> op_code) {
		case LDI:
		case STI:
		    if (is_this_arg(p -> arg[2])
			|| is_this_arg(p -> arg[0]) && p -> arg[1] != 0) {
			return(FALSE);
		    }
		    break;
		case MOV:
		    if (!is_this_arg(p -> arg[0])
			&& is_this_arg(p -> arg[1])) {
			/* Don't know for sure what's in the register */
			return(FALSE);
		    }
		    break;
		case LDN:
		    if (is_this_arg(p -> arg[1])) {
			return(FALSE);
		    }
		case DCL:
		case UDC:
		    break;
		default:
		    if (is_this_arg(p -> arg[0])
			|| is_this_arg(p -> arg[1])
			|| is_this_arg(p -> arg[2])) {
			return(FALSE);
		    }
	    }
	}
    }
    clear_equiv_regs();
    return(TRUE);
}


/* Return a copy of code, modified to adjust for the fact the nth argument */
/* is a variable, whose contents are in register m.                        */
/* All LDI and STI instructions referencing the argument are replaced by   */
/* MOVs.  MOVs of the argument are deleted.  The resulting code should     */
/* contain no remaining references to the nth argument, except             */
/* possibly in a UDC instruction.                                          */
/* This assumes that only_indirect_refs(code,n) is known to TRUE.          */

struct RIC_instr * unindirect(code, n, m)
struct RIC_instr * code;
int n;
long m;
{
    register struct RIC_instr *p;
    register struct RIC_instr **ptr_p;  /* Location of pointer to node p */
    struct RIC_instr *result;  /* Location of pointer to node p */
    unsigned this_arg = n | ARG_BIT;

    if (!find_equiv_regs(code, n)) {
	dbgmsg("Unindirect: code argument inappropriate\n");
    }
    result = copy_RIC(code);
    for (p = result, ptr_p = &result; p != RIC_nil;
	 ptr_p = &(p -> next_instr), p = p -> next_instr) {
	if (!p -> label_arg) {
#           ifdef TRACE
	       extern char * op_code_table[];
	       printf("%s %d %d %d\n", op_code_table[p -> op_code],
		       p -> arg[0], p -> arg[1], p -> arg[2]);
#           endif
	    switch(p -> op_code) {
		case LDI:
		    if (is_this_arg(p -> arg[0])) {
		      /* Second argument is known to be 0. */
		      /* Replace with MOV.                 */
			p -> op_code = MOV;
			p -> arg[0] = m;
			p -> arg[1] = p -> arg[2];
			p -> arg[2] = SK;
			p -> n_args = 2;
		    }
		    break;
		case STI:
		    if (is_this_arg(p -> arg[0])) {
		      /* Second argument is known to be 0. */
		      /* Replace with MOV.                 */
			p -> op_code = MOV;
			p -> arg[0] = p -> arg[2];
			p -> arg[1] = m;
			p -> arg[2] = SK;
			p -> n_args = 2;
		    }
		    break;
		case MOV:
		    if (is_this_arg(p -> arg[0])) {
			/* Delete this instruction.  This is safe, since */
			/* we effectively perform copy propagation.      */
			*ptr_p = p -> next_instr;
			/* Should deallocate p here, but we still need */
			/* next_instr pointer ...		       */
		    }
		    break;
		/* argument can't appear in any other instructions */
	    }
	}
    }
    clear_equiv_regs();
    return(result);
}


