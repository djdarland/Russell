# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

extern int yydebug;
extern int yynerrs;

extern FILE * Goutfile;

extern NODE * Gcurrent;

extern boolean Pflag;  /* Generate profiling code */
extern boolean Tflag;  /* Generate trace code */
extern boolean Vflag;  /* Optimization info */

extern int avail_loc;

/* Generate code corresponding for identifier binding corr. to declaration v */
compile_decl(v)
NODE * v;
{
      int i = avail_loc++;

      if (!v -> decl_needed) {
	  if (Vflag) {
	      printf("Suppressing code to build %s:%s\n",
		     Gcurrent -> fc_code_label,
		     getname(v -> decl_id -> id_str_table_index));
	  }
	  /* Generate code for nested function       */
	  /* constructions or modified primary nodes */
	  /* that may be evaluated.  */
	    Gtraverse (v -> decl_denotation);
	  return;
      }
      if (v -> decl_special & VAR_ON_STACK) {
	ASSERT(v -> decl_signature -> kind == VARSIGNATURE,
	       "codegen.c: bad stack allocated variable");
	/* initialize location */
	if (v -> decl_special & SIMPLE_VAR_ON_STACK) {
	    gen3(STI, AR, v -> displacement, C0);
	} else if (v -> decl_special & PTR_VAR_ON_STACK) {
	    gen2(HINT, OPT, 1);
	    gen3(STI, AR, v -> displacement, UN);
	} else /* Initialized */ {
	    NODE * appl = v -> decl_denotation;
	    NODE * arg = first(appl -> ap_args);

	    ASSERT(appl -> kind == APPLICATION,
		   "codegen.c: bad New application");
	    gen2(DCL, i, DCL_INT);
	    Gexpression (arg, i, FALSE);
            gen3(STI, AR, v -> displacement, i);
	    gen1(UDC, i);
	}
      } else if (v -> decl_special & VAR_IN_REG) {
	ASSERT(v -> decl_signature -> kind == VARSIGNATURE,
	       "codegen.c: bad register allocated variable");
	/* initialize location */
	if (v -> decl_special & SIMPLE_VAR_IN_REG) {
	    gen2(MOV, C0, v -> displacement);
	} else if (v -> decl_special & PTR_VAR_IN_REG) {
	    /* gen2(HINT, OPT, 1);  -- reduces effectiveness of -OO */
	    gen2(MOV, UN, v -> displacement);
	} else /* Initialized */ {
	    NODE * appl = v -> decl_denotation;
	    NODE * arg = first(appl -> ap_args);

	    ASSERT(appl -> kind == APPLICATION,
		   "codegen.c: bad New application");
	    Gexpression (arg, v -> displacement, FALSE);
	}
      } else if (v -> decl_special & ARRAY_CONTIG) {
	NODE * op;
	int size;

	ASSERT(v -> decl_denotation -> kind == APPLICATION,
	       "bad ARRAY_CONTIG flag");
	if (Vflag) {
	    printf("Allocating contiguous array for %s:%s\n",
		    Gcurrent -> fc_code_label,
		    getname(v -> decl_id
			      -> id_str_table_index));
	}
	op = v -> decl_denotation -> ap_operator;
	size = special_val(op -> signature -> fsig_special);
	if (!Gpush_size(size, op)) {
	    if (Vflag) {
		printf("Couldn't get size of %s:%s\n",
		       Gcurrent -> fc_code_label,
		       getname(v -> decl_id
				 -> id_str_table_index));
	    }
	    goto standard_decl;
	}
	if (special_tp(op -> signature -> fsig_special)
	    == ARRAY_PTR_NEW) {
	    genl(EXT, "_contig_pArray_New");
	    genl(LBA, "_contig_pArray_New");
	    gen1(CLC, 1);
	    if (v -> decl_special & ID_IN_REG) {
		gen2(MOV, RL, v -> displacement);
	    } else {
		gen3(STI, AR, v -> displacement, RL);
	    }
	} else /* Non-pointer array */ {
	    genl(EXT, "_contig_Array_New");
	    genl(LBA, "_contig_Array_New");
	    gen1(CLC, 1);
	    if (v -> decl_special & ID_IN_REG) {
		gen2(MOV, RL, v -> displacement);
	    } else {
		gen3(STI, AR, v -> displacement, RL);
	    }
	}
      } else {
	standard_decl:
	    if (v -> decl_special & ID_IN_REG) {
		Gexpression(v -> decl_denotation,
			    v -> displacement, FALSE);
	    } else {
		gen2(DCL, i, DCL_INT);
		Gexpression (v-> decl_denotation, i, FALSE);
		gen3(STI, AR, v -> displacement, i);
		gen1(UDC, i);
	    }
      }
}
