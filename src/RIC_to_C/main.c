/* A simple Russell intermediate code (RIC) to C translator     */
/* Stdin is assumed to be an RIC file.  We write to stdout.     */
/*   We translate a function at a time.  We build up the code   */
/* for the current function in the variable "code".  Note that  */
/* we use a string representation allowing O(1) concatenation.  */
/* Thus this is reasonably efficient.                           */
/*   Also note that we read from in_file using stdio, but we    */
/* write using plain UNIX system calls.                         */
/*   General Russell functions expect a single argument namely  */
/* a pointer to their activation record.  We expect a pointer   */
/* to the global activation record to be in a local variable    */
/* called GF.  Every function that needs to initializes this    */
/* to the value of the global variable passed on the command    */
/* line.           					        */
/*   We store expressions evaluating to virtual register        */
/* contents in the virtual register table only if the virtual   */
/* register is one of the two last                              */
/* modified virtual registers, or if the the only registers     */
/* mentioned in the expression are AR and GF.  (Arguments may   */
/* also be mentioned, since they are never changed.)            */
/* Expressions stored for the two last modified virtual regs    */
/* are not guaranteed to be valid unless they are eventually    */
/* generated in the correct order.                              */
/*   All expressions stored in the virtual register table are   */
/* fully parenthesized and evaluate to an integer or pointer    */
/* value.  The same is true of most expressions manipulated     */
/* elsewhere.                                                   */

# include <fcntl.h>
# include <stdio.h>
# include "strings.h"
# include "op_codes.h"
# include "../parm.h"
# include "../pass5d/codegen.h"
# include "tables.h"
# define GCC
# undef GCC


# define STDOUT_FD 1
# define DELAB strcat(RROOT, "/src/delab/delab")
# ifdef GCC
#   define CC "/usr/local/gcc"
# else
#   define CC "/bin/cc"
# endif

char * C_name();

FILE * in_file;

# define STANDARD_PREFIX \
concat("typedef long word;\n",  \
concat("typedef word (*word_func_ptr)();\n",    \
concat("extern word * GC_malloc();\n",   \
concat("extern void GC_free();\n",   \
concat("extern word * GC_malloc_atomic();\n",   \
concat("#define alloc(sz,result) *(word **)(&result) = GC_malloc((sz)<<2)\n", \
concat("#define alloc_nc(sz,result) result = GC_malloc((sz)<<2)\n", \
concat("#define alloc_cw(sz,result) result = (word)GC_malloc((sz)<<2)\n", \
concat("#define alloc_a(sz,result) *(word **)(&result) = GC_malloc_atomic((sz)<<2)\n", \
concat("#define alloc_a_nc(sz,result) result = GC_malloc_atomic((sz)<<2)\n", \
concat("#define alloc_a_cw(sz,result) result = (word)GC_malloc_atomic((sz)<<2)\n", \
concat("#define abs(x) ((x) > 0? (x) : -(x))\n",    \
"#define shift(x, n) ((n) < 0? (x) >> (-(n)) : (x) << (n))\n"    \
))))))))))))


boolean profile = FALSE;    /* Profiling was requested.  This is ascertained */
			    /* from the intermediate code.                   */


# define SAFE 0x40000000 /* This bit in lm or slm values indicates that it */
			 /* is safe to delay the update until we encounter */
			 /* a store to memory, an assignment to AR or GF,  */
			 /* or the end of a basic block.                   */
# define NONE ((-1) | SAFE)

long lm = NONE;     /* Last modified virtual register           */
char * lm_expr;     /* The expression for the val. of virtual   */
		    /* reg. lm.  A "strings.h" string.          */
		    /* The expression is not entered in the v.  */
		    /* register table, since it may contain a   */
		    /* placeholder.                             */
char * slm_ph;      /* A placeholder for the expression for slm */
		    /* in lm_expr.  CS_NIL if slm is not        */
		    /* referenced.  This will be replaced by    */
		    /* either a temporary name for, or an       */
		    /* expression for, slm, depending on        */
		    /* whether the assignment to slm is         */
		    /* explicitly generated.                    */
		    /* Slm_ph is always CS_NIL if slm is either */
		    /* NONE or MEMORY.                          */
		    /* Both lm_expr and slm_ph are invalid if   */
		    /* lm is NONE.				*/

long slm = NONE;    /* Second to last modified virtual register */
		    /* An expression for its value is entered   */
		    /* in the virtual register table.  It is    */
		    /* not yet appended to code, since slm is   */
		    /* likely to be declared dead before the    */
		    /* next real instruction.                   */

# define MEMORY ((-2) & ~SAFE)
		      /* Either of the above, and only one may be MEMORY, */
		      /* indicating that a store to memory has been       */
		      /* suppressed.  Such a store will always be         */
		      /* generated, but other code can possibly precede   */
		      /* it.  In particular, it is still advantagous to   */
		      /* delay generation of previous register stores     */
		      /* until we know whether the register will be live. */

char * store_code;  /* Code for a pending store. If lm is MEMORY, then */
		    /* slm_ph may be used inside store_code.           */

char * code = "";   /* Code generated so far for the body of the current function */

char * extern_decls = ""; /* Collected external declarations for the current */
			  /* function.                                       */

char * fn_name;     /* Name of current function. */

int n_params = 0;   /* number of parameter refs to current fn seen so far */

int n_args = 0;     /* Number of arguments to current call seen so far */

boolean is_simple;  /* Current function started with a BSF */

boolean gf_refd = FALSE;    /* GF is referenced in current function */

boolean ar_refd = FALSE;    /* AR could conceivably have been used.  */
			    /* of interest only for BSF functions.   */

boolean saw_ons = FALSE;    /* A HINT ONS is still pending. */

int ons_depth;          /* Current number of nested active ONS allocations */

char * global_ar_name;  /* Name of global activation record pointer */

# define ONS_MAX 50

boolean ons_stack[ONS_MAX];  /* ons_stack[i] holds if the i'th nested */
			     /* ONS marked allocation was implemented */
			     /* as a stack allocation.                */

boolean suppress = FALSE;
		    /* Suppress code currently being scanned.  We have */
		    /* seen an unconditional branch and no subseqent   */
		    /* label.                                          */

int extern_type = NONE;  /* Type of next EXT as supplied by HINT ET */

boolean idt_in_progress;
		    /* We are translating a sequence of IDT instructions */
		    /* These must be translated to a single array, since */
		    /* the compiler assumes that adjacent data is        */
		    /* generated, and C compilers are known to shuffle   */
		    /* global declarations.                              */

void finish_idt()
{
    if (idt_in_progress) {
	extern_decls = concat(extern_decls,"};\n");
	idt_in_progress = FALSE;
    }
}

void add_idt(lbl, item)
long item;
char * lbl;
{
    if (lbl != CS_NIL) {
	finish_idt();
    }
    if (!idt_in_progress) {
	if (lbl == CS_NIL) {
	    fprintf(stderr, "Unreachable integer data: %d\n", item);
	    lbl = "???";
	}
	idt_in_progress = TRUE;
	if (lbl[0] == 'L') {
	    /* Local label that may conflict with labels in other files */
	    extern_decls = concat(extern_decls, "static ");
	}
	extern_decls = concat(concat(extern_decls, "word "),
			      concat(C_name(lbl), "[] = {"));
    }
    extern_decls = concat(extern_decls, concat(itos(item), ","));
}

/* Convert the assembly language label s to its C form.  Currently we    */
/* interpret this to mean deleting its leading underscore, if there is   */
/* one, replacing embedded periods by underscores, and doubling embedded */
/* underscores after the first nondeleted sequence of underscores.       */
/* The last underscore is not doubled.                                   */
char * C_name(s)
char *s;
{
    char * result;
    register char *p, *q;
    boolean saw_us;  /* Saw an underscore */
    int n_us;        /* total number of underscores in string */
    int n_us_so_far; /* number seen so far. */
    boolean saw_non_us; /* Saw a nonunderscore character after the first u.s. */

    n_us = 0;
    for (p = s; *p != '\0'; p++) {
	if (*p == '_') n_us++;
    }

    result = (char *) GC_malloc_atomic(strlen(s) + n_us + 1);

    if (*s == '_') {
	p = s+1;
	n_us_so_far = 1;
    } else {
	p = s;
	n_us_so_far = 0;
    }
    saw_us = FALSE;
    saw_non_us = FALSE;
    q = result;
    while (*p != '\0') {
	switch(*p) {
	    case '.':
		*q++ = '_';
		break;
	    case '_':
		*q++ = '_';
		n_us_so_far++;
		saw_us = TRUE;
		if (saw_non_us && n_us_so_far != n_us) {
		  *q++ = '_';
		}
		break;
	    default:
		*q++ = *p;
		if (saw_us) {
		  saw_non_us = TRUE;
		}
	}
	p++;
    }
    *q = '\0';
    return(result);
}

char * lbr_label = CS_NIL;   /* Label of preceding LBR instruction  */
char * lba_label = CS_NIL;   /* Label of preceding LBA instruction  */

char label_buf[MAXLABELSZ+1];

/* Different kinds of type coercions on C expressions: */
# define WORD_PTR 1
# define WORD     2
# define CHAR_PTR 3
# define FLOAT 4
/* or NONE */

/* Return expr coerced to the type idicated by coercion.  Assumes expr */
/* is an identifier or enclosed in parentheses.                        */
/* Result is again enclosed in parentheses.                            */
/* The coercion involving float expect an identifier, and works on     */
/* either lhs or rhs of an assignment.                                 */
char * coerce(expr, coercion)
char * expr;
int coercion;
{
    char * result;

    switch(coercion) {
	case NONE:
	    return(expr);
	case WORD_PTR:
	    return(concat(concat("((word *)", expr), ")"));
	case WORD:
	    return(concat(concat("((word)", expr), ")"));
	case CHAR_PTR:
	    return(concat(concat("((char *)", expr), ")"));
	case FLOAT:
	    return(concat(concat("(*(float *)&", expr), ")"));
    }
}

/* Generate code to store the value of slm in its assigned temporary. */
/* Do the same for lm, if it refers to a store to memory.             */
/* The resulting code is appended to "code".                          */
/* Promote the old information related to lm to slm.                  */
/* Two calls will generate all pending code for slm and lm.           */
void flush_slm()
{
    if (slm != NONE) {
	if (slm == MEMORY) {
	    /* Store may invalidate stored expressions; flush */
	    if (lm != NONE) {
		code = concat(code, flush_all_non_const_except(lm & ~SAFE));
	    } else {
		code = concat(code, flush_all_non_const());
	    }
	    code = concat(code, store_code);
	} else {
	    if (!(slm & SAFE)) {
		code = concat(code, flush_vr(slm));
	    }
	    if (slm_ph != CS_NIL) {
		set_ph(slm_ph, get_expr(slm & ~SAFE));
		slm_ph = CS_NIL;
	    }
	}
    }
    if (lm == MEMORY) {
	code = concat(code, concat(flush_all_non_const(), store_code));
	slm = NONE;
	/* Recycle dead temporaries while we know that the can't be */
	/* referenced.                                              */
	    rem_tmps();
    } else if (lm == NONE) {
	slm = NONE;
	rem_tmps();
    } else {
	add_vr_def(lm & ~SAFE, lm_expr);
	slm = lm;
    }
    lm = NONE;
}


/* Add a virtual register as the last modified one          */
/* Assumes that lm is NONE, e.g. flush_slm has been called. */
/* Slm_ph is expected to be set by the invoker.             */
# define add_modified_vr(result, expr, safe) \
	if ((slm & ~SAFE) != ((result) & ~SAFE)) {     \
	  rem_vr_def(lm & ~SAFE);  \
	}  \
	lm = (result);  lm_expr = (expr); \
	if (safe) { \
	  lm |= SAFE;   \
	}

/* Add code for the binary operation with C prefix operator symbol "prefix"*/
/* and the indicated infix and suffic operator strings.                    */
/* The result is reflected in code.                                        */
/* Each operand may optionally be coerced to the indicated type.  Is_float */
/* indicates whether the resulting expression has type "float".            */
/* Safe indicates that the constructed expression may be moved to anywhere */
/* within the basic block, so long as there are no intervening updates to  */
/* AR or GF.                                                               */
void binary_op(result, is_float, op1, coercion1, op2, coercion2,
	       prefix, infix, suffix, safe)
long result;
boolean is_float;
long op1;
int coercion1;
long op2;
int coercion2;
char * prefix;
char * infix;
char * suffix;
boolean safe;
{
    register char * expr;
    char * op1_expr;
    char * op2_expr;
    char * ph = CS_NIL;
    boolean is_ar_ptr = (result == AR || result == GF);

    if (result == SK) return;
    /* Operands to stored expressions may get modified.   */
    /* Flush if  moves are still pending.                 */
	flush_slm();
    /* We claim that it is now safe to do as many flushes as we like, in */
    /* any order.                                                        */
    if (is_ar_ptr) {
	code = concat(code, flush_all_non_const());
	/* This also gets the old lm, if it was not a constant */
    }
    if (coercion1 == FLOAT) {
	/* Flush, so that coercion will work. */
	code = concat(code, flush_vr(op1));
	op1_expr = get_name(op1);
    } else if (op1 == (slm & ~SAFE)) {
	ph = placeholder();
	op1_expr = ph;
    } else {
	op1_expr = get_expr(op1);
    }
    if (coercion2 == FLOAT) {
	code = concat(code, flush_vr(op2));
	op2_expr = get_name(op2);
    } else if (op2 == (slm & ~SAFE)) {
	if (ph == CS_NIL) {
	    ph = placeholder();
	} 
	op2_expr = ph;
    } else {
	op2_expr = get_expr(op2);
    }
    expr = concat(concat(concat("(", prefix),
				coerce(op1_expr, coercion1)),
		  concat(concat(infix, coerce(op2_expr, coercion2)),
			 concat(suffix, ")")));
    if (is_float) {
	flush_slm();        /* Flushes what was lm at entry */
	code = concat(code, coerce(get_name(result), FLOAT));
	code = concat(code, " = ");
	code = concat(code, expr);
	code = concat(code, ";\n");
    } else {
	add_modified_vr(result, expr, safe && !is_ar_ptr);
	slm_ph = ph;
    }
    if (is_ar_ptr) {
	flush_slm();
	code = concat(code, flush_vr(result));
    }
}

/* Add code for the unary operation with C prefix operator symbol prefix   */
/* and suffix operator symbol suffix.                                      */
/* The result is reflected in code.                                        */
/* The operand may optionally be coerced to the indicated type.  Is_float  */
/* indicates whether the resulting expression has type "float".            */
void unary_op(result, is_float, op, coercion, prefix, suffix, safe)
long result;
boolean is_float;
long op;
int coercion;
char * prefix;
char * suffix;
boolean safe;
{
    register char * expr;
    char * op_expr;
    char * ph = CS_NIL;
    boolean is_ar_ptr = (result == AR || result == GF);

    if (result == SK) return;
    /* Operands to stored expressions may get modified.   */
    /* Flush if  moves are still pending.                 */
	flush_slm();
    /* We claim that it is now safe to do as many flushes as we like, in */
    /* any order.                                                        */
    if (is_ar_ptr) {
      code = concat(code, flush_all_non_const());
    }
    if (coercion == FLOAT) {
	/* Flush, so that coercion will work. */
	code = concat(code, flush_vr(op));
	op_expr = get_name(op);
    } else if (op == (slm & ~SAFE)) {
	ph = placeholder();
	op_expr = ph;
    } else {
	op_expr = get_expr(op);
    }
    expr = concat(concat(prefix, coerce(op_expr, coercion)), suffix);
    if (!is_empty(prefix) || ! is_empty(suffix)) {
	expr = concat("(", concat(expr, ")"));
    }
    if (is_float) {
	flush_slm();
	code = concat(code, coerce(get_name(result), FLOAT));
	code = concat(code, " = ");
	code = concat(code, expr);
	code = concat(code, ";\n");
    } else {
	add_modified_vr(result, expr, safe && !is_ar_ptr);
	slm_ph = ph;
    }
    if (is_ar_ptr) {
	flush_slm();
	code = concat(code, flush_vr(result));
    }
}

/* Add code for the nilary operation rhs.             */
/* We assume that rhs is parenthesized or a constant. */
void nilary_op(result, is_float, rhs, safe)
long result;
boolean is_float;
char * rhs;
boolean safe;
{
    boolean is_ar_ptr = (result == AR || result == GF);

    if (result == SK) return;
    flush_slm();
    if (is_ar_ptr) {
      code = concat(code, flush_all_non_const());
    }
    if (is_float) {
	flush_slm();
	code = concat(code, coerce(get_name(result), FLOAT));
	code = concat(code, " = ");
	code = concat(code, rhs);
	code = concat(code, ";\n");
    } else {
	add_modified_vr(result, rhs, safe && !is_ar_ptr);
	slm_ph = CS_NIL;
    }
    if (is_ar_ptr) {
	flush_slm();
	code = concat(code, flush_vr(result));
    }
}

/* Write out a string s to stdout using direct write system calls. */
/* The argument string is represented as in strings.[hc]           */
void emit(s)
char *s;  /* Not really a C string */
{
    char * flattened;
    long len;

    flattened = flatten(s, &len);
    if (write(STDOUT_FD, flattened, len) < len) {
	fprintf(stderr, "Write to stdout failed\n");
	exit(1);
    }
    GC_free(flattened);
}

main(argc, argv)
int argc;
char **argv;
{
    int opc = RTN;
    long arg1, arg2, arg3;
    char * label;   /* Label in either this instruction or preceding LBA */
    char * cmd;
    boolean has_flags;
    boolean bad_flag = FALSE;
    boolean xflag = FALSE;
    boolean Oflag = FALSE;
    char *Cfile;
    char *Gfile;
    char *sfile;
    int len;

    GC_init();
    /* Set up input and out files */
	has_flags = (argc == 5);
	if (has_flags) {
	    char *s = &(argv[1][1]);
	    
	    while (*s) {
	        switch(*s) {
	            case 'O':
	                Oflag = TRUE;
	                break;
	            case 'x':
	                xflag = TRUE;
	                break;
	            default:
	                bad_flag = TRUE;
	        }
	        s++;
	    }
	}
	if (argc < 4 || argc > 5 || bad_flag) {
	  fprintf(stderr,
	          "Usage: %s [-[O][x]] file.G file.s global_ar_name\n",
	          argv[0]);
	  exit(1);
	}
	if (has_flags) {
	  Gfile = argv[2];
	  sfile = argv[3];
	  global_ar_name = C_name(argv[4]);
	} else {
	  Gfile = argv[1];
	  sfile = argv[2];
	  global_ar_name = C_name(argv[3]);
	}
	len = strlen(sfile);
	if (len < 3 || sfile[len-2] != '.' || sfile[len-1] != 's') {
	  fprintf(stderr, "Output file %s name should end in .s\n", sfile);
	  exit(1);
	}
	Cfile = (char *)GC_malloc_atomic(len+1);
	strcpy(Cfile,sfile);
	Cfile[len-1] = 'c';
	cmd = concat(concat(DELAB, " "), Gfile);
	cmd = flatten(cmd, 0);
	in_file = popen(cmd, "r");
	/* Change stdout to be Cfile */
	  (void) close(STDOUT_FD);
	  if (open(Cfile, O_WRONLY | O_CREAT | O_TRUNC, 0644) != STDOUT_FD) {
	    fprintf(stderr, "Can't open %s\n", Cfile);
	    exit(1);
	  }

    init_tmps();
    /* Emit standard prolog */
        emit(STANDARD_PREFIX);
    /* Emit declaration for global a.r. ptr.  Declare it as word array, */
    /* so it doesn't conflict with possible IDT generated decl.         */
      emit (concat("extern word ", concat(global_ar_name, "[];\n")));

    while (!feof(in_file)) {
	if (opc != LBA) {
	    label = CS_NIL;
	}
	opc = getw(in_file);
	if (feof(in_file)) {
	    /* compensate for strange feof semantics */
	    break;
	}
	if (opc < 0 && opc > N_OP_CODES) {
	    fprintf(stderr, "Bad op code\n");
	    exit(1);
	}
	if (opc <= MAX_LABEL_OP) {
	    char *p = label_buf;
	    int c;
	    int i = 0;
	    
	    while ((c = getc(in_file)) != '\0' && c != EOF) {
		*p++ = c;
		if (++i >= MAXLABELSZ) {
		    fprintf(stderr, "Label too long\n");
		    p = label_buf;
		}
	    }
	    *p = '\0';
	    label = (char *) GC_malloc_atomic(i+1);
	    strcpy(label, label_buf);
	} else {
	    arg1 = getw(in_file);
	    arg2 = getw(in_file);
	    arg3 = getw(in_file);
	    if (!gf_refd) {
	        if (arg1 == GF && opc != LDN && opc != UDC
	            && opc != CLC && opc != HINT && opc != ARG
	            && opc != GAR) {
	            gf_refd = TRUE;
	        }
	        if (arg2 == GF && opc != LDI && opc != STI
	            && opc != LDC && opc != HINT && opc != CLI
	            && opc != DCL) {
	            gf_refd = TRUE;
	        }
		if (arg3 == GF && opc != HINT) {
		    gf_refd = TRUE;
		}
	    }
	}

	if (suppress) {
	    if (opc != LBL && opc != DCL && opc != UDC) {
		/* Don't generate dead code. */
		continue;
	    }
	}

	switch(opc) {
	    case BR:
		flush_slm();
		code = concat(code, flush_all_exprs());
		rem_tmps();
		code = concat(concat(code, "goto "), concat(C_name(label), ";\n"));
		suppress = TRUE;
		break;

	    case BRT:
		flush_slm();
		code = concat(code, flush_all_except(TL));
		code = concat(concat(concat(code, "if ("),
				     concat(get_expr(TL),") goto ")),
			      concat(C_name(label), ";\n"));
		/* TL is now known to be dead */
		rem_vr_def(TL);
		rem_tmps();
		break;

	    case BRF:
		flush_slm();
		code = concat(code, flush_all_except(TL));
		code = concat(concat(concat(code, "if (!"),
				     concat(get_expr(TL),") goto ")),
			      concat(C_name(label), ";\n"));
		/* TL is now known to be dead */
		rem_vr_def(TL);
		rem_tmps();
		break;

	    case CLL:
	    case ERR:
	    case CLC:
		{
		    char * expr;
		    char * args;
		    boolean lm_is_arg = ((lm & ARG_FLAG) &&
					 lm != MEMORY && lm != NONE);
		    boolean slm_is_arg = ((slm & ARG_FLAG) &&
					 slm != MEMORY && slm != NONE);
		
		    /* Make sure that only "safe" expressions are in v.r. */
		    /* table.  Anything else will interact badly with     */
		    /* Call/cc.  Avoid flushing arguments, by delaying    */
		    /* such flushes until after the argument list is      */
		    /* built, and the death of the argument v. regs has   */
		    /* been recorded.                                     */
		      if (!lm_is_arg) {
			flush_slm();
			flush_slm();
			args = arg_list(n_args);
		      } else if (!slm_is_arg) {
			flush_slm();
			args = arg_list(n_args);
			flush_slm();
		      } else {
			if (slm_ph != CS_NIL) {
			    fprintf(stderr, "Bad slm_ph value\n");
			    abort();
			}
			/* Add last arg def to table, so arg_list can */
			/* find it.                                   */
			/* This is safe, since the order of the two   */
			/* argument loads is irrelevant.              */
			/* The definition will be removed by arg_list */
			    add_vr_def(lm & ~SAFE, lm_expr);
			    lm = NONE;
			args = arg_list(n_args);
			flush_slm();
		      }
		    if (opc == CLL && n_args != 1) {
		      fprintf(stderr, "Wrong number of args (%d) to CLL\n",
			      n_args);
		    }
		    if (opc == CLC) {
		      if (n_args != arg1) {
			fprintf(stderr,
				"Wrong number of args (%d vs %d) to CLC\n",
				n_args, arg1);
		      }
		      label = lba_label;
		    }
		    /* label already has function type in the C program */
		    expr = concat(C_name(label), "(");
		    /* Add arguments to call */
		      expr = concat(expr, args);
		    expr = concat(expr, ")");
		    rem_vr_def(RL);  /* Remove old definition */
		    code = concat(concat(code, "RL = (word)"),
				  concat(expr, ";\n"));
		    n_args = 0;
		}
		break;

	    case LBL:
		suppress = FALSE;
		flush_slm();
		code = concat(code, flush_all_exprs());
		code = concat(concat(code, C_name(label)), ":\n");
		break;

	    case EXT:
		finish_idt();
		switch(extern_type) {
		  case DCL_INT:
		    extern_decls = concat(concat(extern_decls, "extern word "),
					  concat(C_name(label), "[];\n"));
		    break;
		  case DCL_ADDR:
		    extern_decls = concat(concat(extern_decls, "extern word *"),
					  concat(C_name(label), "[];\n"));
		    break;
		  case DCL_FLOAT:
		    extern_decls = concat(concat(extern_decls, "extern float "),
					  concat(C_name(label), "[];\n"));
		  case DCL_DBL_FLOAT:
		    extern_decls = concat(concat(extern_decls, "extern double "),
					  concat(C_name(label), "[];\n"));
		    break;
		  case NONE:
		    /* It is a function */
		    extern_decls = concat(concat(extern_decls, "extern word "),
					  concat(C_name(label), "();\n"));
		    break;
		  default:
		    fprintf(stderr, "Bad type for EXT\n");
		    break;
		}
		extern_type = NONE;
		break;

	    case LBA:
		/* Simply remember label for next instruction */
		  lba_label = label;
		break;

	    case BFN:
		/* Defer generating the header until we have seen */
		/* all of the function.                           */
		fn_name = C_name(label);
		n_params = 0;
		is_simple = FALSE;
		gf_refd = FALSE;
		ar_refd = FALSE;
		break;

	    case TFB:
	    case TFE:
		fprintf(stderr, "Tracing not yet implemented\n");
		break;

	    case PRO:
		/* Profiling is handled by the C compiler */
		profile = TRUE;
		break;

	    case ADT:
		fprintf(stderr, "Obsolete ADT instruction encountered\n");
		exit(1);

	    case BSF:
		finish_idt();
		fn_name = C_name(label);
		n_params = 0;
		is_simple = TRUE;
		gf_refd = FALSE;
		ar_refd = FALSE;
		break;

	    case LBR:
		lbr_label = label;
		break;

	    case DDT:
		finish_idt();
		if (lba_label == CS_NIL) {
		  fprintf(stderr, "Unlabelled double data\n");
		} else {
		  if (lba_label[0] == 'L') {
		     extern_decls = concat(extern_decls, "static ");
		  }
		  extern_decls = concat(concat(extern_decls, "double "),
					concat(concat(C_name(lba_label), "[] = {"),
					       concat(label, "};\n")));
		}
		break;

	    case FDT:
		finish_idt();
		if (lba_label == CS_NIL) {
		  fprintf(stderr, "Unlabelled float data\n");
		} else {
		  if (lba_label[0] == 'L') {
		     extern_decls = concat(extern_decls, "static ");
		  }
		  extern_decls = concat(concat(extern_decls, "float "),
					concat(concat(C_name(lba_label), "[] = {"),
					       concat(label, "};\n")));
		}
		break;

	    /* Instructions with numeric or v. register args: */

	    case DCL:
		add_undef_vr(arg1);
		if (lbr_label != CS_NIL) {
		    code = concat(concat("/* ", lbr_label),
				  concat(concat(" ~ ", get_name(arg1)),
					 concat(" */\n", code)));
		    lbr_label = CS_NIL;
		}
		break;

	    case UDC:
		if (arg1 == (lm & ~SAFE)) {
		    lm = NONE;
		}
		if (arg1 == (slm & ~SAFE)) {
		    if (lm != NONE && slm_ph != CS_NIL) {
			/* Still need the expression. Substitute now, */
			/* since we know slm is otherwise dead.       */
			    set_ph(slm_ph, get_expr(arg1));
			    slm_ph = CS_NIL;
		    }
		    slm = NONE;
		}
		rem_vr(arg1);
		break;

	    case ALH:
		if (arg2 == SK) break;
		flush_slm();
		if (arg1 != arg2) rem_vr_def(arg2);
		flush_slm();
		{
		  char * sz = get_expr(arg1);
		  char first_c;
		  boolean is_number;

		  GET_FIRST(sz, first_c);
		  is_number = (first_c >= '0' && first_c <= '9');
		  if (!is_number) {
		    /* Make sure that sz doesn't involve arg2's location */
		    /* unless arg1 = arg2                                */
		      code = concat(code, flush_vr(arg1));
		      sz = get_expr(arg1);
		  }
		  if ( saw_ons && is_number ) {
		    ons_stack[ons_depth++] = TRUE;
		    code = concat(concat(code, "{ word NS["),
				  concat(concat(sz,"];\n"),
					 concat(get_name(arg2), " = (word) NS;\n")));
		  } else {
		    if (saw_ons) {
			ons_stack[ons_depth++] = FALSE;
		    }
		    {
		      char * ac;
		      char * sz_expr;

		      if (!is_number && arg1 == arg2) {
			sz_expr = "sz";
		      } else {
			sz_expr = sz;
		      }
		      if (arg2 == RL) {
			/* Not all C compilers allow l-value casts. */
			/* Generate allocation code that knows that */
			/* RL has type word.                        */
			ac = "alloc_cw(";
		      } else if (arg2 == AR || arg2 == GF) {
			ac = "alloc_nc(";
		      } else {
			ac = "alloc(";
		      }
		      ac = concat(ac,
				  concat(concat(sz_expr, ","),
					 concat(get_name(arg2), ");")));
		      if (!is_number && arg1 == arg2) {
			ac = concat(concat("{ word sz = (word)", sz),
				    concat(";", concat(ac, "}")));
		      }
		      code = concat(code, concat(ac, "\n"));
		    }
		  }
		  if (ons_depth >= ONS_MAX - 2) {
		      fprintf(stderr, "Too many nested allocations\n");
		      exit(1);
		  }
		}
		if (arg1 == arg2) rem_vr_def(arg2);
		saw_ons = FALSE;
		break;

	    case GAR:
		if (arg2 != SK) {
		  nilary_op(arg2, FALSE, par_name(arg1), TRUE);
		}
		if (arg1 > n_params) n_params = arg1;
		break;

	    case ALS:
		fprintf(stderr, "Encountered ALS instruction\n");
		exit(1);

	    case LDI:
		unary_op(arg3, FALSE, arg1, WORD_PTR,
			 "", concat("[", concat(itos(arg2), "]")),
			 (arg1 == AR || arg1 == GF));
		break;

	    case STI:
		if (arg1 != SK) {
		    char * arg1_expr;
		    char * arg3_expr;
		    char * ph = CS_NIL;

		    flush_slm();
		    if (arg1 == (slm & ~SAFE)) {
			ph = placeholder();
			arg1_expr = ph;
		    } else {
			arg1_expr = get_expr(arg1);
		    }
		    if (arg3 == (slm & ~SAFE)) {
			if (ph == CS_NIL) {
			    ph = placeholder();
			}
			arg3_expr = ph;
		    } else {
			arg3_expr = get_expr(arg3);
		    }
		    lm = MEMORY;
		    slm_ph = ph;
		    store_code = concat(concat("((word *)", arg1_expr),
					concat(")[(word)",
					       concat(itos(arg2), "] = (word)")));
		    store_code = concat(store_code,
					concat(arg3_expr, ";\n"));
		}
		break;

	    case CLI:
		{
		    char * expr;
		    char * fn;
		    char * args;
		    boolean lm_is_arg = ((lm & ARG_FLAG) &&
					 lm != MEMORY && lm != NONE);
		
		    /* Make sure that only "safe" expressions are in v.r. */
		    /* table.  Anything else will interact badly with     */
		    /* Call/cc.  Avoid flushing arguments, by delaying    */
		    /* such flushes until after the argument list is      */
		    /* built, and the death of the argument v. regs has   */
		    /* been recorded.                                     */
		      flush_slm();
		      if (!lm_is_arg) {
			flush_slm();
			args = arg_list(n_args);
		      } else {
			args = arg_list(n_args);
			flush_slm();
		      }
		    if (n_args != 1) {
		      fprintf(stderr, "Wrong number of args (%d) to CLI\n",
			      n_args);
		    }
		    fn = concat(concat("((word_func_ptr *)", get_expr(arg1)),
				concat(")[(word)",
				       concat(itos(arg2), "]")));
		    expr = concat(fn, "(");
		    /* Add arguments to call */
		      expr = concat(expr, args);
		    expr = concat(expr, ")");
		    rem_vr_def(RL);  /* Remove old definition */
		    code = concat(concat(code, "RL = (word)"),
				  concat(expr, ";\n"));
		    n_args = 0;
		}
		break;

	    case LDN:
		nilary_op(arg2, FALSE, itos(arg1), TRUE);
		break;

	    case RTN:
		{
		    char * header;
		    char * par_list;

		    if (fn_name == CS_NIL) {
			fprintf(stderr, "Anonymous function\n");
			fn_name = "?anonymous?";
		    }
		    if (ons_depth != 0) {
			fprintf(stderr,
				"Nonmatching stack (de)allocations %d\n",
				ons_depth);
			exit(1);
		    }
		    /* Make sure that there are no pending stores, */
		    /* UDCs, or incomplete declarations.           */
		      flush_slm();
		      rem_tmps();
		      finish_idt();
		    /* Complete and then emit code for the function */
		      code = concat(concat(code, "return((word)"),
				    concat(get_expr(RL), ");\n}\n\n"));
		      code = concat(tmp_decls(max_tmp), code);
		      lm = NONE;
		      if (gf_refd) {
			code = concat(concat(
			      		"register word * GF = ((word *)(",
			                global_ar_name),
			              concat("[0]));\n", code));
		      }
		      if (ar_refd && is_simple) {
		        code = concat("register word * AR;\n", code);
		      }
		      code = concat("word TL;\n", code);
		      code = concat("register word RL;\n", code);
		      header = concat ("\nword ", concat(fn_name, "("));
		      if (is_simple) {
			header = concat(header,
					(par_list = par_names(n_params)));
		      } else {
			header = concat(header, "AR");
		      }
		      header = concat(header, ")\n");
		      if (is_simple) {
			if (n_params > 0) {
			  header = concat(concat(header, "word "),
					  concat(par_list, ";\n{\n"));
			} else {
			  header = concat(header, "{\n");
			}
		      } else {
			header = concat(header, "register word * AR;\n{\n");
		      }
		      code = concat(extern_decls, concat(header, code));
		      emit(code);

		    /* Get things set up for the next function. */
		      rem_vr_def(RL);
		      rem_vr_def(TL);
		      reset_tmps();
		      fn_name = CS_NIL;
		      n_params = 0;
		      gf_refd = FALSE;
		      code = "";
		      extern_decls = "";
		}
		break;

	    case LDL:
		finish_idt();
		/* Labels are attached to either functions or arrays */
		/* Thus C semantics imply that we implicitly get an  */
		/* address.                                          */
		if (lba_label == CS_NIL) {
		    fprintf(stderr, "No LBA preceding LDL\n");
		    exit(1);
		}
		nilary_op(arg1, FALSE, coerce(C_name(lba_label), WORD_PTR), TRUE);
		break;

	    case MOV:
		unary_op(arg2, FALSE, arg1, NONE, "", "",
			 (arg1 == AR || arg1 == GF));
		break;

	    case TAR:
		fprintf(stderr, "Tracing not yet implemented\n");
		break;

	    case PSH:
		fprintf(stderr, "Encountered PSH instruction\n");
		exit(1);

	    case ADP:
		{ 
		  char * displ;
		  boolean is_number;
		  char first_c;
		  boolean is_ar_ptr = (arg1 == AR || arg1 == GF);

		  if (is_ar_ptr) {
		    displ = get_expr(arg2);
		    /* Determine whether displ is a numeric constant.  */
		    /* Note that a more complex expression starting    */
		    /* with a number would be enclosed in parentheses. */
		      GET_FIRST(displ, first_c);
		      is_number = (first_c == '-'
				   || first_c >= '0' && first_c <= '9');
		  }
		  binary_op(arg3, FALSE, arg1, WORD_PTR, arg2, WORD,
			   "", "+", "",
			   is_ar_ptr && is_number);
		}
		break;

	    /* case CLC:  handled above */

	    case ALA:
		if (arg2 == SK) break;
		flush_slm();
		if (arg1 != arg2) rem_vr_def(arg2);
		flush_slm();
		{
		  char * sz = get_expr(arg1);
		  char first_c;
		  boolean is_number;

		  GET_FIRST(sz, first_c);
		  is_number = (first_c >= '0' && first_c <= '9');
		  if (!is_number) {
		    /* Make sure that sz doesn't involve arg2's location */
		    /* unless arg1 = arg2                                */
		      code = concat(code, flush_vr(arg1));
		      sz = get_expr(arg1);
		  }
		  if ( saw_ons && is_number ) {
		    ons_stack[ons_depth++] = TRUE;
		    code = concat(concat(code, "{ word NS["),
				  concat(concat(sz,"];\n"),
					 concat(get_name(arg2), " = (word) NS;\n")));
		  } else {
		    if (saw_ons) {
			ons_stack[ons_depth++] = FALSE;
		    }
		    {
		      char * ac;
		      char * sz_expr;

		      if (!is_number && arg1 == arg2) {
			sz_expr = "sz";
		      } else {
			sz_expr = sz;
		      }
		      if (arg2 == RL) {
			/* Not all C compilers allow l-value casts. */
			/* Generate allocation code that knows that */
			/* RL has type word.                        */
			ac = "alloc_a_cw(";
		      } else if (arg2 == AR || arg2 == GF) {
			ac = "alloc_a_nc(";
		      } else {
			ac = "alloc_a(";
		      }
		      ac = concat(ac,
				  concat(concat(sz_expr, ","),
					 concat(get_name(arg2), ");")));
		      if (!is_number && arg1 == arg2) {
			ac = concat(concat("{ word sz = (word)", sz),
				    concat(";", concat(ac, "}")));
		      }
		      code = concat(code, concat(ac, "\n"));
		    }
		  }
		  if (ons_depth >= ONS_MAX - 2) {
		      fprintf(stderr, "Too many nested allocations\n");
		      exit(1);
		  }
		}
		if (arg1 == arg2) rem_vr_def(arg2);
		saw_ons = FALSE;
		break;

	    case HINT:
		switch(arg1) {
		    case OPT:
			/* We assume that this was taken care of in an */
			/* earlier pass, namely RICfilter.             */
			break;
		    case NP:
		    case AL:
			/* Not relevant */
			break;
		    case DEA:
			/* Make sure no stores or references through arg2 */
			/* are pending                                    */
			  flush_slm();
			  if (arg2 == AR || arg2 == GF) {
			      code = concat(code, flush_all_non_const());
			  } else {
			      flush_slm();
			  }
			if (saw_ons) {
			  saw_ons = FALSE;
			  if (ons_stack[--ons_depth]) {
			    code = concat(code, "}\n");
			    break;
			  }
			}
			code = concat(concat(code, "GC_free("),
			              concat(get_expr(arg2), ");\n"));
			break;
		    case NSC:
		    case STSZ:
		    case PT:
			/* Not relevant */
			break;
		    case DEAD:
			if (arg2 == (lm & ~SAFE)) {
			    lm = NONE;
			}
			if (arg2 == (slm & ~SAFE)) {
			    if (lm != NONE && slm_ph != CS_NIL) {
			      /* Still need the expression. Substitute now, */
			      /* since we know slm is otherwise dead.       */
				set_ph(slm_ph, get_expr(arg2));
				slm_ph = CS_NIL;
			    }
			    slm = NONE;
			}
			rem_vr_def(arg2);
			break;
		    case GFU:
			if (is_simple) {
			  /* Make sure AR gets declared.       */
			  /* We figure the rest out ourselves. */
			  ar_refd = TRUE;
			}
			break;
		    case LIVE:
			/* We assert that there are no suppressed      */
			/* procedure calls or allocations.  Thus it is */
			/* safe to move up the effect of the hint to   */
			/* the end of the code generated so far.       */
#                       ifdef GCC
			  /* generate a bogus machine instruction requiring  */
			  /* the value of interest. This forces the compiler */
			  /* to see it as live, we hope.                     */
			  code = concat(code, "# ifdef __GNUC__\n");
			  code = concat(code, "asm volatile(\" \": : \"g\" (");
			  code = concat(code, get_expr(arg2));
			  code = concat(code, "));\n");
			  code = concat(code, "# endif\n");
#                       else
			  /* Should really be passed to the C compiler, */
			  /* but there is no good way to do that, in    */
			  /* general.                                   */
#                       endif
			break;
		    case ET:
			extern_type = arg2;
			break;
		    case ONS:
			saw_ons = TRUE;
			break;
		}
		break;

	    case ARG:
		{
		  char * rhs;
		  boolean is_const;
		  char first_c;

		  if (arg2 == (lm & ~ SAFE)) {
		      rhs = lm_expr;
		  } else {
		      rhs = get_expr(arg2);
		  }
		  GET_FIRST(rhs, first_c);
		  is_const = (first_c == '-' || first_c == '"'
			       || first_c >= '0' && first_c <= '9');
		  add_undef_vr(ARGLOC(arg1));
		  unary_op(ARGLOC(arg1), FALSE, arg2, WORD, "", "",
			    (is_const || arg2 == AR || arg2 == GF
			     || is_param(first_c)));
		  if (n_args < arg1) n_args = arg1;
		}
		break;

	    case ADI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "+", "", FALSE);
		break;

	    case SBI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "-", "", FALSE);
		break;

	    case MLI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "*", "", FALSE);
		break;

	    case DVI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "/", "", FALSE);
		break;

	    case NGI:
		unary_op(arg2, FALSE, arg1, WORD, "-", "", FALSE);
		break;

	    case IDT:
		add_idt(lba_label, arg1);
		break;

	    case EQI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "==", "", FALSE);
		break;

	    case LTI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "<", "", FALSE);
		break;

	    case GTI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", ">", "", FALSE);
		break;

	    case NEI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "!=", "", FALSE);
		break;

	    case LEI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "<=", "", FALSE);
		break;

	    case GEI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", ">=", "", FALSE);
		break;

	    case SHI:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "shift(", ",", ")", FALSE);
		break;

	    case ABI:
		unary_op(arg2, FALSE, arg1, WORD, "abs(", ")", FALSE);
		break;

	    case TRU:
		nilary_op(arg1, FALSE, "1", TRUE);
		break;

	    case FLS:
		nilary_op(arg1, FALSE, "0", TRUE);
		break;

	    case LDS:
		{
		    char * string;
		    if (lba_label == CS_NIL) {
			fprintf(stderr, "LDS: missing label\n");
			lba_label = "???";
		    }
		    string = concat("\"", concat(rmcntrl(lba_label), "\""));
		    nilary_op(arg1, FALSE, string, TRUE);
		}
		break;

	    case LDC:
		binary_op(arg3, FALSE, arg1, CHAR_PTR, arg2, WORD,
			  "", "[", "]", FALSE);
		break;

	    case AND:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "&", "", FALSE);
		break;

	    case OR:
		binary_op(arg3, FALSE, arg1, WORD, arg2, WORD, "", "|", "", FALSE);
		break;

	    case NOT:
		unary_op(arg2, FALSE, arg1, WORD, "!", "", FALSE);
		break;

	    case ADF:

	    case SBF:

	    case MLF:

	    case DVF:

	    case NGF:

	    case EXF:

	    default:
		fprintf(stderr, "Unrecognized op code: %d\n", opc);
		exit(1);
	}
	if (opc != LBA) {
	    lba_label = CS_NIL;
	}
    }
    /* Emit any declarations that are still pending */
      finish_idt();
      if (!is_empty(extern_decls)) {
	emit(extern_decls);
      }
    /* Compile the result */
    {
	(void) close(STDOUT_FD);
	if (open("/dev/tty", O_WRONLY | O_CREAT | O_TRUNC, 0644) != STDOUT_FD) {
	    fprintf(stderr, "/dev/tty reopen failed - continuing\n");
	}
	pclose(in_file);
	/* Replace ourselves with a cc process.  It is important that */
	/* all subprocesses have been waited for at this point, since */
	/* cc neglects to pay attention to values returned by the     */
	/* wait system call.                                          */
	    if (profile) {
		execl(CC, "cc", "-S", Oflag? "-O" : "-g" , "-p", "-o", sfile, Cfile, 0);
	    } else {
		execl(CC, "cc", "-S", Oflag? "-O" : "-g" , "-o", sfile, Cfile, 0);
	    }
	    fprintf(stderr, "Couldn't exec %s\n", CC);
	    exit(1);
    }
}
