/* Routines to generate RIC trace code for function entry and exit */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

extern int avail_loc;

extern FILE * Goutfile;

int GGbase;       /* implicit parameters to the following */
boolean GGFflag;
int GGoffset;

/*
 *  Generate call to trace an argument
 *         param is the parameter node
 *         GGbase is a base location for accessing argument
 *              - the first argument should be at an offset of 1
 *         GGoffset is the current offset from GGbase; it is decremented
 *              by this routine
 *         GGFflag is true iff we are generating code which assumes no s.l.
 */
Garg_trace(param)
NODE * param;
{
    NODE * put_sel = (NODE *)mk_put_selection(param, GGFflag);
    int put_loc = avail_loc++;
    int arg_loc = avail_loc++;

    /* Load print function value */
      gen2(DCL, put_loc, DCL_ADDR);
      if (put_sel != NIL) {
        Gexpression(put_sel, put_loc);
      } else {                      
	gen2(HINT, ET, DCL_INT);
        genl(EXT, "_FV_put_any");
        genl(LBA, "_FV_put_any");
        gen1(LDL, put_loc);
      }

    /* Push argument value */
      gen2(DCL, arg_loc, DCL_INT);
      gen3(LDI, GGbase, GGoffset, arg_loc);
      GGoffset -= 1;

    /* Make trace call */
      gen2(TAR, arg_loc, put_loc);
      gen2(UDC, arg_loc, 1);
      gen2(UDC, put_loc, 1);
}

/*
 *  Generate calls to trace function entry
 *         name is the name of the routine being entered
 *         params is the parameter list to the function construction
 *         Fflag is true iff we are generating jsb code.
 */
Gentry_trace(name, params, Fflag)
char * name;
NODE * params;
boolean Fflag;
{
    /* Add argument information in reverse order */
        if (Fflag) {
            GGbase = SP;
            GGoffset = length(params) + 1 /* compensate for print fn */;
        } else {
            GGbase = AR;
	    GGoffset = length(params);
        }
        GGFflag = Fflag;
        maprlist(params, Garg_trace);
        genl(TFB, name);
}

/* Generate call to trace function exit */
Gexit_trace(name)
{
    genl(TFE, name);
}

