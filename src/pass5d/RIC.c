# define DEBUG DEBUG
#include <stdio.h>
#include <ctype.h>
#include "../parm.h"
#include "op_codes.h"
#include "codegen.h"

#define OCLENGTH 9  /* maximum length of mnemonics */

#define MAXLOC 100000  /* Largest plausible location number */

/* Miscellaneous routines for conversion between representations */
/* of Russell Intermediate Code                                  */

/* hash table structure for op codes and special locations */
typedef struct hrecord {
    char hr_name[OCLENGTH+1];
    int hr_val;
    struct hrecord * hr_next;
} hrecord;

# define NIL 0

# define TABSIZE 50
# define hash(s) ((((s)[0]<<2) + (((s)[1]) == '\0'? 0 : (s)[1] + (s)[2]))%TABSIZE)

static hrecord * htable[TABSIZE];

/* Add entry with name nm and value vl to the hash table.  No check is made */
/* to see whether it is already there.                                      */
void add_RIC_table(nm, val)
char * nm;
int val;
{
    register int index = hash(nm);
    register hrecord *  nhr = (hrecord *)gc_malloc(sizeof (hrecord));

    nhr -> hr_next = htable[index];
    strcpy(nhr -> hr_name,nm);
    nhr -> hr_val = val;
    htable[index] = nhr;
}

extern char * op_code_table[];
#define NONE ((char *)0)

/* Initialize htable to contain opcodes and predefined location names */
void init_RIC_table()
{
    int i;
    /* Op codes */
      for ( i = 0; i < N_OP_CODES; i++ ) {
          if (op_code_table[i] != NONE) {
              add_RIC_table(op_code_table[i], i);
          }
      }
    /* Special locations */
      add_RIC_table("RS", RS);
      add_RIC_table("AR", AR);    /* activation record pointer */
      add_RIC_table("SP", SP);    /* stack pointer             */
      add_RIC_table("GF", GF);    /* global frame pointer      */
      add_RIC_table("UN", UN);    /* undefined value constant  */
      add_RIC_table("SK", SK);    /* value sink                */
      add_RIC_table("RL", RL);    /* function result location  */
      add_RIC_table("TL", TL);    /* test result location      */
      add_RIC_table("C0", C0);    /* constant 0                */
      add_RIC_table("C1", C1);    /* constant 1                */
      add_RIC_table("C2", C2);    /* constant 2                */
      add_RIC_table("C3", C3);    /* constant 3                */
      add_RIC_table("C4", C4);    /* constant 4                */
      add_RIC_table("T1", T1);    /* temporary 1               */
      add_RIC_table("T2", T2);    /* temporary 2               */
    /* Types for DCL instruction */
      add_RIC_table("ADDR", DCL_ADDR);
      add_RIC_table("INT", DCL_INT);
      add_RIC_table("FLOAT", DCL_FLOAT);
      add_RIC_table("DBL_FLOAT", DCL_DBL_FLOAT);
    /* Kinds for HINT instruction */
      add_RIC_table("OPT", OPT);
      add_RIC_table("NP", NP);
      add_RIC_table("PT", PT);
      add_RIC_table("AL", AL);
      add_RIC_table("DEA", DEA);
      add_RIC_table("NSC", NSC);
      add_RIC_table("STSZ", STSZ);
      add_RIC_table("DEAD", DEAD);
      add_RIC_table("GFU",GFU);
      add_RIC_table("LIVE",LIVE);
      add_RIC_table("ET", ET);
      add_RIC_table("ONS", ONS);
}

/* Look up the indicated name in the hash table. Return the associated */
/* value.   Returns MISSING if entry is not found                      */
int find_RIC_table(name)
char * name;
{
    register hrecord * ce = htable[hash(name)];
    
    while (ce != NIL) {
        if (strcmp(ce -> hr_name, name) == 0) {
            return (ce -> hr_val);
        }
        ce = ce -> hr_next;
    }

    return(MISSING);
}

/* Copy the string pointed to by p to field_buf.  Stop after OCLENGTH */
/* characters, or after a \0, \b, \t, \n.                             */
/* Return a pointer to the field separator.                           */
static char field_buf[OCLENGTH+1];

static char * extract_field(p)
char * p;
{
    register int i;
    register char c;
    
    for (i = 0; i < OCLENGTH; i ++) {
        c = *p++;
        switch(c) {
            case '\0':
            case ' ':
            case '\t':
	    case '\n':
	    case ',':
	    case ';':
                field_buf[i] = '\0';
                return(p-1);
            default:
                field_buf[i] = c;
        }
    }
    field_buf[OCLENGTH] = '\0';
    return(p);
}

/* Free a sequence of RIC instructions */
void free_RIC(p)
struct RIC_instr * p;
{
    struct RIC_instr *current, *next;

    current = p;
    while (current != (struct RIC_instr *) 0) {
	next = current -> next_instr;
	free(current);
	current = next;
    }
}

/* Write out a sequence of RIC instructions onto stream f */
put_RIC(seq, f)
struct RIC_instr * seq;
FILE * f;
{
    struct RIC_instr *current;

    for (current = seq; current != (struct RIC_instr *) 0;
	 current = current -> next_instr) {
	putc(current -> op_code, f); /* Must be non-zero */
	putc(current -> label_arg, f); putc(current -> n_args, f);
	if (current -> label_arg) {
	    char * s = current -> label;
	    for (; *s != '\0'; s++) {
		putc(*s, f);
	    }
	    putc(0, f);
	} else {
	    putw(current -> arg[0], f);
	    putw(current -> arg[1], f);
	    putw(current -> arg[2], f);
	}
    }
    /* Write a terminating null op code */
    putc(0,f);
}

extern long avail_loc;

/* Read a sequence of RIC instructions written by put_RIC       */
/* update avail_loc to be larger than any location encountered. */
struct RIC_instr * get_RIC(f)
FILE * f;
{
    struct RIC_instr *current;
    struct RIC_instr *last;
    struct RIC_instr *result = (struct RIC_instr *)0;
    int opcode;
    int label_arg;
    int n_args;
    long arg0;
    char label_buf[MAXLABELSZ+1];
	       
    while ((opcode = getc(f)) != 0) {
	label_arg = getc(f);
	n_args = getc(f);
	if (feof(f)) {
	    dbgmsg("Unexpected end of file\n");
	    abort();
	}
	if (label_arg) {
	    char * s = label_buf;
	    char * t;

	    while((*s++ = getc(f)) != '\0');
	    current = (struct RIC_instr *) gc_malloc(sizeof(struct RIC_instr)
						     + s - label_buf);
	    /* Fill in the label field */
	      s = current -> label;
	      t = label_buf;
	      while (*s++ = *t++);
	} else {
	    current = (struct RIC_instr *) gc_malloc(sizeof(struct RIC_instr));
	    current -> arg[0] = arg0 = getw(f);
	    current -> arg[1] = getw(f);
	    current -> arg[2] = getw(f);
	    current -> label[0] = '\0';
	}
	/* update avail_loc */
	  if (opcode == DCL && arg0 >= avail_loc) {
	    if ((arg0 & ARG_BIT) || arg0 == RS) {
		dbgmsg("get_RIC: Attempt to declare argument or result\n");
		abort(arg0);
	    }
#           ifdef DEBUG
	      if (((unsigned)arg0) > 10000000) {
		dbgmsg("get_RIC: Bad declaration\n");
		abort(arg0);
	      }
#           endif
	    avail_loc = arg0+1;
	  }
	current -> op_code = opcode; current -> n_args = n_args;
	current -> label_arg = label_arg;
	current -> next_instr = (struct RIC_instr *)0;
	/* Add current to the end of result sequence */
	  if (result == (struct RIC_instr *)0) {
	    result = last = current;
	  } else {
	    last -> next_instr = current;
	    last = current;
	  }
    }
    return(result);
}


/* Parse the text line *s, which is assumed to contain a mnemonic        */
/* description of an RIC instruction.  Return a pointer to a newly       */
/* allocated RIC_instr node.   A pointer to the ';', \n, or \0 character */
/* trailing the line is returned in *s.                                  */
struct RIC_instr * parse_RIC(s)
char **s;
{
#   define skip_blanks while (*p == ' ' || *p == '\t' || *p == ',') { p++; }
#   define AT_END (*p == '\0' || *p == '\n' || *p == ';')
    char *p = *s;
    int op_code;
    int i;
    struct RIC_instr * result;

    skip_blanks;
#   ifdef DEBUG
	if (AT_END) {
            dbgmsg ("Empty RIC line\n");
            abort();
        }
#   endif
    p = extract_field(p);
    op_code = find_RIC_table(field_buf);
#   ifdef DEBUG
	if (op_code == MISSING) {
            dbgmsg ("Bad RIC opcode: %s\n", field_buf);
            abort();
        }
#   endif
    skip_blanks;
    if (*p == '\"') {
	char * q;
        int i = 0;

        /* label */
#       ifdef DEBUG
            if (i > MAX_LABEL_OP) {
                dbgmsg("Label with big op code, oc = %d\n", i);
            }
#       endif
        p++;
	result = (struct RIC_instr *) gc_malloc(sizeof(struct RIC_instr)
						+ ((char *)(index(p,'"'))) - p);
	result -> op_code = op_code;
	result -> n_args = 1;
	result -> next_instr = (struct RIC_instr *)0;
	q = result -> label;
	while ((*q = *p++) != '\"') {
	    if (*q == '\\') {
		/* Next 3 characters are octal escape */
		int j,k;

		k = 0;
		for (j = 0; j < 3; j++) {
		    k = 8*k + (*p++) - '0';
		}
		*q = k;
	    }
	    q++;
        }
	*q = '\0';
        skip_blanks;
#       ifdef DEBUG
	    if (!(AT_END)) {
		dbgmsg("Junk: %s following RIC label: %s\n", p, result -> label);
            }
#       endif
	result -> label_arg = TRUE;
    } else {
        /* numeric arguments */
	result = (struct RIC_instr *) gc_malloc(sizeof(struct RIC_instr));
	result -> label_arg = FALSE;
	result -> op_code = op_code;
	result -> n_args = 0;
	result -> next_instr = (struct RIC_instr *)0;
	while (!(AT_END)) {
            if (isdigit(*p)) {
                int v = 0;
                while (isdigit(*p)) {
                    v = v * 10 + (*p++ - '0');
                }
		result -> arg[result -> n_args++] = v;
            } else if (*p == '$') {
                int v = 0;

                p++;
                while (isdigit(*p)) {
                    v = v * 10 + (*p++ - '0');
                }
#               ifdef DEBUG
                    if (v <= 0) {
                        dbgmsg("There is no parameter 0\n");
                    }
#               endif
		result -> arg[result -> n_args++] = v + ARG_BIT;
	    } else {
                p = extract_field(p);
                i = find_RIC_table(field_buf);
#               ifdef DEBUG
                    if (i == MISSING) {
                        dbgmsg ("Bad RIC loc: %s\n", field_buf);
                        abort();
                    }
#               endif
		result -> arg[result -> n_args++] = i;
	    }
            skip_blanks;
        }
    }
    *s = p;
    return(result);
}

/* Convert a string representing RIC code to internal form. The string */
/* is assumed to contain a number of newline or ; separated            */
/* instructions. The result is a linked list of RIC_instr nodes.       */
/* The original string is freed unless it was statically allocated.    */
struct RIC_instr * Ginline_cnvt(s)
char *s;
{
    char *p = s;
    struct RIC_instr *first = NIL;
    struct RIC_instr *last = NIL;
    struct RIC_instr *q = NIL;
    extern char end;

    while (*p != '\0') {
	q = parse_RIC(&p);
	if (first == NIL) {
	    first = last = q;
	} else {
	    last -> next_instr = q;
	    last = q;
	}
	while (*p == '\n' || *p == ';') { p++; }
    }
    last -> next_instr = NIL;
    if (s > &end) {
      free(s);
    }
    return(first);
}

/* Write out the RIC instruction on stream f.  Add n-1 to each argument */
/* number appearing in the instruction, except if n = 0.                */
/* If n = 0, map the first argument to the top of the stack, the second */
/* one to below the top of the stack, etc.  RS location by the value of */
/* rs.                                                                  */
void write_RIC(f, RIC_i, n, rs)
FILE * f;
struct RIC_instr * RIC_i;
int n;
int rs;
{
    char * result;

    putw(RIC_i -> op_code, f);
    if (RIC_i -> label_arg) {
	fputs(RIC_i -> label,f);
	putc('\0',f);
    } else {
	int i;
	int is_ldn = ((RIC_i -> op_code == LDN) || (RIC_i -> op_code == IDT));
        for (i = 0; i < 3; i++) {
	    if (i < RIC_i -> n_args) {
		int arg = RIC_i -> arg[i];

		if ((arg & ARG_BIT) && (!is_ldn || i != 0)) {
		    arg &= ~ARG_BIT;
		    arg = (n == 0? 1 - arg :  arg + n - 1);
		} else if (arg == RS && 
			   (!is_ldn || i != 0)) {
		    /* Must be real result reference */
		    arg = rs;
		}
		ASSERT(arg < MAXLOC || is_ldn,
		       "Bad opnd in inline code\n");
		putw(arg, f);
	    } else {
		putw(SK, f);
            }
        }
    }
}

/* Parse a sequence of RIC instructions, represented as above.      */
/* Translate arguments and RS references as above.                  */
void write_RIC_seq(f, RIC_seq, n, rs)
FILE * f;
struct RIC_instr * RIC_seq;
int n;
int rs;
{
    struct RIC_instr * p = RIC_seq;

    while (p != NIL) {
	write_RIC(f, p, n, rs);
	p = p -> next_instr;
    }
}

/* Write out a label operand instruction onto file f */
genl_RIC(f, op, label)
FILE * f;
int op;
char * label;
{
    putw(op, f);
    fputs(label, f);
    putc('\0', f);
}


/* Write out a location operand instruction onto file f */
gen_RIC(f, op, arg1, arg2, arg3)
FILE * f;
int op;
int arg1, arg2, arg3;
{
    ASSERT((arg1 <= MAXLOC || op == LDN || op == IDT)
	   && arg2 <= MAXLOC && arg3 <= MAXLOC,
	   "Bad operand to gen_RIC\n");
    putw(op, f);
    putw(arg1, f);
    putw(arg2, f);
    putw(arg3, f);
}

/* Copy a sequence of RIC instructions */
struct RIC_instr * copy_RIC(p)
struct RIC_instr * p;
{
    if (p == RIC_nil) {
	return (RIC_nil);
    } else {
	struct RIC_instr * result;

	if (!p -> label_arg) {
	    result = (struct RIC_instr *) gc_malloc(sizeof (struct RIC_instr));
	    *result = *p;
	} else {
	    int sz = (sizeof (struct RIC_instr)) + strlen(p -> label);
	    result = (struct RIC_instr *)gc_malloc(sz);
	    result -> op_code = p -> op_code;
	    result -> n_args = 0;
	    result -> label_arg = TRUE;
	    strcpy(result -> label, p -> label);
	}
	result -> next_instr = copy_RIC(p -> next_instr);
	return(result);
    }
}

/* Destructively concatenate two sequences of RIC instructions */
struct RIC_instr * cat_RIC(p,q)
struct RIC_instr * p;
struct RIC_instr * q;
{
    register struct RIC_instr * r;

    if (p == RIC_nil) {
	return(q);
    } else {
	for (r = p; r -> next_instr != RIC_nil; r = r -> next_instr);
	r -> next_instr = q;
    }
    return(p);
}

/* Replace arguments in RIC sequence by consecutively numbered locations */
/* (virtual registers) starting with location n.                         */
/* This is done destructively.                                           */
struct RIC_instr * RIC_inst_args(p, n)
struct RIC_instr * p;
long n;
{
    struct RIC_instr * r;
    int i;
    boolean is_ldn;     /* First argument to current instruction is a */
			/* potentially huge integer constant. Ignore  */
			/* its ARG_BIT.                               */
    int arg_no;

    for (r = p; r != RIC_nil; r = r -> next_instr) {
	is_ldn = (r -> op_code == LDN || r -> op_code == IDT);
	for (i = 0; i < r -> n_args && !(r -> label_arg); i++) {
	    if (!is_ldn || i >= 1) {
		if ((r -> arg[i]) & ARG_BIT) {
		    arg_no = (r -> arg[i]) & ~ ARG_BIT;
		    r -> arg[i] = arg_no + n - 1;
		}
	    }
	}
    }
    return(p);
}


/* Replace RS in RIC sequence by the location n. */
/* This is done destructively.                   */
struct RIC_instr * RIC_inst_rs(p, n)
struct RIC_instr * p;
long n;
{
    struct RIC_instr * r;
    int i;
    boolean is_ldn;     /* First argument to current instruction is a */
			/* potentially huge integer constant. Ignore. */

    for (r = p; r != RIC_nil; r = r -> next_instr) {
	is_ldn = (r -> op_code == LDN || r -> op_code == IDT);
	for (i = 0; i < r -> n_args && !(r -> label_arg); i++) {
	    if (!is_ldn || i >= 1) {
		if ((r -> arg[i]) == RS) {
		    r -> arg[i] = n;
		}
	    }
	}
    }
    return(p);
}

/* Return the length of an instruction sequence, discounting HINT */
/* instructions and optional code.                                */
long RIC_len(p)
struct RIC_instr * p;
{
    struct RIC_instr * r;
    int i = 0;

    for (r = p; r != RIC_nil; r = r -> next_instr) {
	i++;
	if (r -> op_code == HINT) {
	    i--;
	    if (r -> arg[1] == OPT) {
		i -= r -> arg[2];
	    }
	}
    }
    return(i);
}
