#include "parm.h"

#include "stree/ststructs.mh"

#include "pass4/sigs.h"

#include "codeutil.h"
#include <stdio.h>

#define UNDEFNAME 0

extern FILE * Voutfile;

extern int comp_index;

extern char * entry_name;

char *
Vnewlabel (name)
char * name;
{
    static int unique=0;
    char strbuff[128];
    char * result;

#ifdef DEBUG
    sprintf (strbuff, "l_%s_%d_%s", entry_name, unique++,name);
#else
    sprintf (strbuff, "L_%s_%d", entry_name, unique++);
#endif
    result = (char *)malloc (strlen(strbuff)+1);
    strcpy (result,strbuff);
    return (result);
}

char *
new_global_label (name)
char * name;
{
    static int unique=0;
    char strbuff[128];
    char * result;

    sprintf (strbuff, "G_%s_%d_%s", entry_name, unique++, name);
    result = (char *)malloc (strlen(strbuff)+1);
    strcpy (result,strbuff);
    return (result);
}

/* Generate a call to mcount to count function calls */
void
Vcall_mcount()
{
    char * count_ptr = Vnewlabel("mcount_tmp");

    /* Allocate temporary */
        fputs("\t.data\n\t.align\t2\n", Voutfile);
        fprintf(Voutfile, "%s:\t.long\t0\n", count_ptr);
        fputs("\t.text\n", Voutfile);
    /* Generate call */
        fprintf(Voutfile, "\tmovab\t%s,r0\n", count_ptr);
        fputs("\t.globl\tmcount\n\tjsb\tmcount\n", Voutfile);
}

/*
 *  Given a parameter node, return either a type checked selection
 * of put from the parameter type, or NIL if no appropriate function
 * exists.  (Used for trace code).  NIL is also returned if the
 * parameter type is not an identifier.  If Flag is true, only
 * global identifiers will be accepted.
 */
NODE *
mk_put_selection(p, Fflag)
NODE * p;
boolean Fflag;
{
    NODE * arg_id;
    NODE * arg_list;
    NODE * appl;
    NODE * type;
    NODE * comp_sig;  /* Signature of put */
    NODE * result;
    extern NODE * id_put;

    ASSERT(p -> kind == PARAMETER, "mk_put_selection: bad parameter\n");
    switch (p -> par_signature -> kind) {
        case VARSIGNATURE:
            type = p -> par_signature -> var_denotation;
            break;
        case VALSIGNATURE:
            type = p -> par_signature -> val_denotation;
            break;
        default:
            return(NIL);
    }
    if (type -> kind != LETTERID && type -> kind != OPRID
        || type -> sel_type != NIL) return(NIL);
    tl_findsig(type);
    if (Fflag) {
        if (type -> id_last_definition -> level != 0) {
            return (NIL);
        }
    }
    if (type -> signature == ERR_SIG) return(NIL);

    if (p -> par_id != NIL) {
        arg_id = copynode(p -> par_id);
    } else {
        arg_id = mknode(LETTERID, UNDEFNAME);
    }
    arg_id -> id_last_definition = p;
    arg_id -> id_def_found = TRUE;
    arg_id -> signature = p -> par_signature;
    arg_id -> sig_done = SIG_DONE;
    arg_list = mklist(arg_id, -1);
    result = copynode(id_put);
    result -> sel_type = type;
    appl = lock(mknode(APPLICATION, result, arg_list));
    comp_sig =
      getcomp(type -> signature, id_put, type, NIL, NIL, appl, TRUE);
    vfree(unlock(appl));
    if (comp_sig == NIL) {
        return(NIL);
    }
    result -> sel_index = comp_index;
    result -> id_def_found = TRUE;
    result -> signature = comp_sig;
    result -> sig_done = SIG_DONE;
    return(result);
}

char * Gbase;       /* implicit parameters to the following */
boolean GFflag;
int Goffset;

/*
 *  Generate call to trace an argument
 *         param is the parameter node
 *         Gbase is a base register for accessing argument
 *              - the first argument should be at an offset of 4
 *         Goffset is the current offset from Gbase; it is decremented
 *              by this routine
 *         GFflag is true iff we are generating jsb code.
 */
Varg_trace(param)
NODE * param;
{
    NODE * put_sel = mk_put_selection(param, GFflag);

    /* Push print function value */
      if (put_sel != NIL) {
        Vexpression(put_sel);
      } else {
        fputs("\t.globl\t_FV_put_any\n\tpushl\t$_FV_put_any\n", Voutfile);
      }

    /* Push argument value */
      fprintf(Voutfile, "\tpushl\t%d(%s)\n", Goffset, Gbase);
      Goffset -= 4;

    /* Call add_arg_entry */
      fputs("\t.globl\t_add_arg_entry\n\tcalls\t$2, _add_arg_entry\n",
            Voutfile);
}

/*
 *  Generate calls to trace function entry
 *         name is the name of the routine being entered
 *         params is the parameter list to the function construction
 *         Fflag is true iff we are generating jsb code.
 */
Ventry_trace(name, params, Fflag)
char * name;
NODE * params;
boolean Fflag;
{
    char * name_loc = Vnewlabel("rn");

    /* Add argument information in reverse order */
        if (Fflag) {
            Gbase = "sp";
            Goffset = 4 * length(params) + 4 /* compensate for print fn */;
        } else {
            Gbase = "ap";
            Goffset = 4 * length(params);
        }
        GFflag = Fflag;
        maprlist(params, Varg_trace);

    fputs("\t.data\n", Voutfile);
    fprintf(Voutfile, "%s:\t.ascii\t\"%s\\0\"\n", name_loc, name);
    fputs("\t.text\n", Voutfile);
    fprintf(Voutfile, "\tpushl\t$%s\n", name_loc);
    fputs("\t.globl\t_add_func_entry\n\tcalls\t$1,_add_func_entry\n",
          Voutfile);
}

/* Generate call to trace function exit */
Vexit_trace()
{
    fputs("\t.globl\t_rem_func_entry\n\tcalls\t$0,_rem_func_entry\n",
          Voutfile);
}

/*
 *  allocate temporary register and return the register name
 *
 *  "register name" may be a label if there are no registers
 * available.
 *
 *   Vreg_bit is set to the mask bit corresponding to the register,
 * or 0 if a memory location is used.
 */

# define NREGS 3

static char * regs[NREGS] = {"r6","r7","r8"};

static int mask_bit[NREGS] = {R6, R7, R8};

static boolean reg_free[NREGS] = {TRUE, TRUE, TRUE};
				 /* reg_free[i] is TRUE if */
				 /* regs[i] is available   */

int Vreg_bit;

char * Vnewreg()
{
    register int i;
    for (i = 0; i < NREGS && !reg_free[i];) i++;
    if (i < NREGS) {
	reg_free[i] = FALSE;
	Vreg_bit = mask_bit[i];
	Ventry_mask |= Vreg_bit;
	return(regs[i]);
    } else {
	char * tmp = Vnewlabel("tmp");
	fprintf(Voutfile, "\t.lcomm\t%s,4\n", tmp);
	Vreg_bit = 0;
	return(tmp);
    }
}

/*
 *   Return register to pool of available registers
 * Argument must be a pointer to a string returned by
 * Vnewreg.
 *   The register is removed from Vgc_mask.
 */   

void Vretreg(r)
char *r;
{
    register int i;
    
    for (i = 0; i < NREGS && r != regs[i];) i++;
    if (i < NREGS) {
	reg_free[i] = TRUE;
	Vgc_mask &= ~mask_bit[i];
    }
}
