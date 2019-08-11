# define DEBUG

# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"
# include <ctype.h>

extern int yydebug;
extern int yynerrs;

extern FILE * unparse_file;

extern FILE * Goutfile;

extern char str_code_buf[];

extern int avail_loc;

extern int Glevel;

extern NODE * Gcurrent;  /* Current function construction */

extern boolean Vflag;

extern boolean Nflag;

extern boolean Oflag;

extern boolean xflag;

/* Check whether s is a valid C identifier */
boolean valid_C_id(s)
char * s;
{
    register char * t = s;

    if (!isalpha(*t) && *t != '_') {
	return(FALSE);
    };
    for (t++; *t != '\0'; t++) {
	if (!isalnum(*t) && *t != '_') {
	    return(FALSE);
	}
    };
    return(TRUE);
}
      
/* Generate a C callable stup for the i th type component with signature */
/* sig and name nm of the type stored at location identified by lbl.     */
/* Assumes that a pointer to the correct global a.r. is stored at ar_lbl.*/
/* The code is largely a greatly simplified version of appl.c.           */
void compile_stub(nm, sig, lbl, ar_lbl, i)
char * nm;
NODE * sig;
char * lbl;
char * ar_lbl;
int i;
{
    int size_loc = avail_loc++;
    int ar_loc = avail_loc++;
    NODE * params = sig -> fsig_param_list;
    int n_args = length(params) - n_vacuous_params(params);
    int first_arg_loc;
    int closure_loc, tp_loc;
    NODE * constr = sig -> fsig_construction;
    boolean need_closure = (constr == NIL
			    || (constr -> fc_complexity & SL_ACC));
    boolean discard_ar = (constr != NIL
			  && (constr -> fc_complexity & NO_AR_REFS));

    ASSERT(sig -> kind == FUNCSIGNATURE, "compile_stub: bad sig\n");

    sprintf(str_code_buf, "_%s", nm);
    genl(EXT, str_code_buf);
    genl(BSF, str_code_buf);
    
    /* Copy arguments to registers.  This is required by the ILOC */
    /* translator.  It doesn't hurt too much otherwise.           */
      first_arg_loc = avail_loc;
      avail_loc += n_args;
      {
	register int arg_count;

	for (arg_count = 0; arg_count < n_args; arg_count++) {
	    gen2(DCL, first_arg_loc + arg_count, DCL_INT);
	    gen2(GAR, arg_count+1, first_arg_loc + arg_count);
	}
      }


    /* Get closure, if necessary */
      if (need_closure) {
	tp_loc = avail_loc++;
	closure_loc = avail_loc++;
	gen2(DCL, tp_loc, DCL_ADDR);
	gen2(DCL, closure_loc, DCL_ADDR);
	genl(LBA, lbl);
	gen1(LDL, tp_loc);
	gen3(LDI, tp_loc, 0, tp_loc);
	gen3(LDI, tp_loc, i, closure_loc);
	gen1(UDC, tp_loc);
      }
    /* Load activation record size into size_loc */
      gen2(DCL, size_loc, DCL_INT);
      if (constr != NIL) {
	gen2(LDN, constr -> ar_size, size_loc);
      } else {
	gen3(LDI, closure_loc, FO_SIZE, size_loc);
      }
    /* Allocate activation record */
      gen2(DCL, ar_loc, DCL_ADDR);
      gen2(ALH, size_loc, ar_loc);
      gen1(UDC, size_loc);

    /* Copy arguments to activation record.  Ignore vacuous arguments */
      {
	register int arg_count;

	for (arg_count = 0; arg_count < n_args; arg_count++) {
	    gen3(STI, ar_loc, arg_count+1, first_arg_loc + arg_count);
	    gen1(UDC, first_arg_loc + arg_count);
	}
      }

    /* store environment ptr */
      if (need_closure) {
	gen3(LDI, closure_loc, FO_EP, T1);
	gen3(STI, ar_loc, 0, T1);
      }

    /* Load correct value of global frame pointer */
    /* This doesn't matter if we generate C code, since the */
    /* code will retrieve GF whenever it needs it, anyway.  */
#     ifndef GEN_C
        genl(LBA, ar_lbl);
        gen1(LDL, T1);
        gen1(HINT, GFU);
        gen3(LDI, T1, 0, GF);
#     endif

    if (!xflag) {
      /* Store global frame pointer in global_ar, so call_russell works */
        gen2(DCL, T2, DCL_ADDR);
        genl(LBA, "_global_ar");
        gen1(LDL, T2);
        gen3(STI, T2, 0, GF);
        gen1(UDC, T2);
    } else {
      /* This is useless and unnecessary in pcr world.  Whatever value */
      /* is already there will do fine.				       */
    }

    /* Pass activation record as the only argument */
      gen2(ARG, 1, ar_loc);

    /* Call the Russell function */
      if (need_closure) {
	gen2(CLI, closure_loc, FO_IP);
	gen1(UDC, closure_loc);
      } else {
	genl(CLL, constr -> fc_code_label);
      }

    /* Allow deallocation of activation record if appropriate */
      if (discard_ar) {
	gen3(HINT, DEA, ar_loc, constr -> ar_size);
      }
      gen1(UDC, ar_loc);

    gen0(RTN);
}

/* Generate C callable stubs for all the components of the type */
/* signature tsig.  Assumes that the type value is stored in    */
/* the location labelled by lbl.                                */
/* Assumes that a pointer to the correct global a.r. is stored  */
/* at ar_lbl.                                                   */
void compile_stubs(tsig, lbl, ar_lbl)
NODE * tsig;
char * lbl;
char * ar_lbl;
{
    unsigned * s;
    int i;
    int component_count = 0;
    char * cname;
    NODE * csig;

    maplist(t, tsig -> ts_clist, {
	switch(t -> kind) {
	    case DEFCHARSIGS:
		s = &(t -> dcs_0);
		for (i = 0; i < NVECTORS; i++) {
		    if (s[i] != 0) {
			errmsg0(t, "Can't generate stub for quoted identifier");
			break;
		    }
		}
		break;
	    case TSCOMPONENT:
		csig = sig_structure(t -> tsc_signature);
		if (csig -> kind != FUNCSIGNATURE){
		    errmsg0(csig, "Can't generate stup for non-function");
		    break;
		}
		cname = (char *)getname(t -> tsc_id -> id_str_table_index);
		if (!valid_C_id(cname)) {
		    errmsg1(csig, "Can't generate stub named %s", cname);
		    break;
		}
		compile_stub(cname, t -> tsc_signature,
			     lbl, ar_lbl, component_count++);
		break;
	}
    });
}
