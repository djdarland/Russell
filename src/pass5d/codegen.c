# define DEBUG

/* 
 *  This is an attempt at a portable Russell code generator.
 *  It is based on the VAX specific version (pass5c).
 *
 *      Hans Boehm (started 2 July 1985)
 *
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codegen.h"
# include "op_codes.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

extern int yydebug;
extern int yynerrs;

extern boolean Pflag;  /* Generate profiling code */
extern boolean Tflag;  /* Generate trace code */
extern boolean Vflag;  /* Optimization info */
extern boolean Oflag;  /* Optimize for execution speed */
extern boolean Xflag;  /* Generate externally callable code */
extern boolean xflag;  /* Generate code for Xerox Portable Common Runtime */
extern boolean Fflag;  /* Load arguments into temporaries */

extern char * entry_name; /* "" for main compilation */

boolean sl_available = TRUE;  /* Static link available in      */
			      /* current function construction */

boolean compile_fcs = TRUE;   /* Also compile nested function constructions */
			      /* as they are encountered.		    */

boolean copied_globals;     /* Current function uses closure with copied */
			    /* non-local identifier bindings             */

int n_globals;              /* Number of non-local identifiers referenced */
			    /* in current function.  Valid only if        */
			    /* copied_globals applies.                    */

int avail_loc = FIRST_AVAIL_LOC;     /* next available location number */

int first_param_loc;        /* First parameter location for current fn.  */
			    /* Applies only if parameters were copied.   */
			    /* This is done only if the DIR_REC bit is   */
			    /* set for the current function construction */
			    /* Otherwise first_param_loc is 0.           */

void type_expr();  /* generate code for type in record element */

boolean Gpush_size();  /* Push the size of an array onto the stack */

char str_code_buf[MAXSTRCODELEN]; /* used for strings, "special" fns */
                                  /* and by find_inline              */
                                  /* and for object file names       */
                                  /* and to assemble labels          */
char str_code_buf2[MAXSTRCODELEN]; /* used for strings */

extern FILE * objfilelist;  /* List of object files which must be loaded */

extern FILE * unparse_file;

#ifdef UNDEFINED
void add_objfile(fn)
char * fn;
{
    char *s;

    if (objfilelist == NULL) {
        objfilelist = fopen(OBJFILELIST, "w");
        if (objfilelist == NULL) {
            fprintf(stderr, "Can't open %s\n", OBJFILELIST);
            exit(1);
        }
    }
    for (s = fn; *s != '\0'; s++) {
        putc(*s, objfilelist);
    }
    putc('\n', objfilelist);
}
#endif

/* Definitions for list of function constructions for whose body
 * we still need to generate code
 */

typedef struct Fc_Entry {
	NODE * fc_pointer;
	int fc_level;
	int fc_fast_only;  /* Only simplified version needed */
			   /* Not really needed anymore ...  */
	struct Fc_Entry * fc_next;
} fc_entry;

fc_entry * Gfc_list = NIL;

/* Add an entry to the front of Gfc_list, provided compile_fcs is TRUE */
void Gfc_add(ptr,lvl,fast_only)
NODE * ptr;    /* pointer to function construction */
int lvl;       /* level of function construction   */
int fast_only; /* generate only BSF version        */
{
    fc_entry * p;

    if (!compile_fcs) { return; }
    p = (fc_entry *)malloc(sizeof (fc_entry));
    p -> fc_pointer = ptr;
    p -> fc_level = lvl;
    p -> fc_fast_only = fast_only;
    p -> fc_next = Gfc_list;
    Gfc_list = p;
}

/* remove an entry from the head of Gfc_list */
void Gfc_delete()
{
    fc_entry * p = Gfc_list;

    Gfc_list = Gfc_list -> fc_next;
    free(p);
}

int Glevel = -1;   /* current static nesting level */

NODE * Gcurrent;
FILE * Goutfile;

boolean finished_accessible = FALSE;    /* Finished BOTH accessibility    */
					/* check and static link analysis */

/* Compile all function constructions on Gfc_list */
void Gcompile_funcs()
{
    NODE * fc_ptr;
    int fc_lvl;
    int fc_fast_only;
    int first_avail_loc;

    first_avail_loc = avail_loc;
    while (Gfc_list != NIL) {
	fc_ptr = Gfc_list -> fc_pointer;
	fc_lvl = Gfc_list -> fc_level;
        fc_fast_only = Gfc_list -> fc_fast_only;
        Gfc_delete();
	if(!fc_fast_only) {
	    /* Reset location counter so numbers don't grow too */
	    /* rapidly.                                         */
	      avail_loc = first_avail_loc;
	    Gfunc_body(fc_ptr, fc_lvl);
	}
	if((fc_ptr -> fc_complexity & NO_SL) && fc_lvl != 0) {
	    boolean old_sl_avail = sl_available;

	    sl_available = FALSE;
            if (!fc_fast_only) { compile_fcs = FALSE; }
	    avail_loc = first_avail_loc;
	    Gfunc_body(fc_ptr,fc_lvl);
	    sl_available = old_sl_avail;
	    compile_fcs = TRUE;
	}
    }
}


/* generate code for syntax tree p into file f */
Ggeneratecode ( f , p )
NODE * p;
FILE * f;
{
    Goutfile = f;
    p -> fc_code_label = "_russell_top_level";
    analyze(p);     /* Compute fc_complexity fields */
    accessible(p);  /* find accessible code                 */
    bld_analyze(&p);  /* Find blocks requiring activation records */
    Gallocate (p,TRUE);   /* allocate space in activation records */
    sl_analyze(p);  /* Decide which functions reference free ids  */
    finished_accessible = TRUE;
    if (Oflag) {
	/* Redo fc_complexity analysis to take advantage of new info */
	if (Vflag) {
	    printf("Repeating basic analysis:\n");
	}
	analyze(p);
    }
    cl_analyze(p, TRUE);  /* Decide on closure rep.               */
    if (yydebug) prtree(p);
    Gfc_add(p,0,FALSE); /* add main function to list of functions that need */
			/* compiling.                                       */
    Gcompile_funcs();
}

/* generate code for syntax tree p embedded in syntax tree q into file f */
/* invoked with -c compiler flag                                         */
Ggeneratepcode ( f, q, p )
NODE * p;
NODE * q;
FILE * f;
{
    extern char * ofname;  /* Name of assembly language file */

    Goutfile = f;
    q -> fc_code_label = "_russell_top_level";
    if (p -> kind != FUNCCONSTR) {
	errmsg0(p, "Outermost expression must be function construction");
        return;
    }
    analyze(q);     /* Compute fc_complexity fields */
    accessible(q);  /* find accessible code                 */
    bld_analyze(&q);  /* Find blocks requiring activation records */
    Gallocate (q,FALSE);  /* allocate space in activation records */
    sl_analyze(p);  /* Decide which functions reference free ids  */
    finished_accessible = TRUE;
    if (Oflag) {
	/* Redo fc_complexity analysis to take advantage of new info */
	if (Vflag) {
	    printf("Repeating basic analysis:\n");
	}
	analyze(q);
    }
    cl_analyze(p, TRUE);  /* Decide on closure rep.               */
#   ifdef DEBUG
	if (yydebug) prtree(p);
#   endif
    if (p -> ar_static_level != 1) {
        dbgmsg("user program must be at level 1");
    }
    /* Compile main function */
      sprintf(str_code_buf, "m_%s", entry_name);
      genl(EXT,str_code_buf);
      genl(BFN,str_code_buf);
      Glevel = 0;
      Gfuncconstructor(p, RL);
      gen0(RTN);

    Gcompile_funcs();
}

/* Generate code for syntax tree p embedded in syntax tree q into file f */
/* The expression p should have type signature.  Generate an externally  */
/* callable interface for p.                                             */
/* Invoked with -X compiler flag                                         */
GgenerateXcode ( f, q, p )
NODE * p;
NODE * q;
FILE * f;
{
    NODE * sig = sig_structure(p -> signature);
    char name_buf[120];
    char ar_name_buf[120];

    if (strlen(entry_name) > 100) {
	errmsg0(p, "file name too long");
	return;
    }
    Goutfile = f;
    q -> fc_code_label = "_russell_top_level";
    if (sig -> kind != TYPESIGNATURE) {
	errmsg0(p, "-X requires expression with type signature");
        return;
    }
    analyze(q);     /* Compute fc_complexity fields */
    accessible(q);  /* find accessible code                 */
    bld_analyze(&q);  /* Find blocks requiring activation records */
    Gallocate (q,FALSE);  /* allocate space in activation records */
    sl_analyze(q);  /* Decide which functions reference free ids  */
    finished_accessible = TRUE;
    if (Oflag) {
	/* Redo fc_complexity analysis to take advantage of new info */
	if (Vflag) {
	    printf("Repeating basic analysis:\n");
	}
	analyze(q);
    }
    cl_analyze(q, TRUE);  /* Decide on closure rep.               */
#   ifdef DEBUG
	if (yydebug) prtree(p);
#   endif
    /* Compile the type expression in the context of a C callable function */
	Gcurrent = q;
	Glevel = 0;
	copied_globals = FALSE;
	sl_available = TRUE;

	/* Generate prolog */
	  if (xflag) { 
	    sprintf(name_buf, "_XR_run");
	  } else {
	    sprintf(name_buf, "_%s_", entry_name);
	  }
	  genl(EXT, name_buf);
	  genl(BSF, name_buf);

	/* Generate a call to russell_set_up to set up main act. record */
	  gen2(LDN, q -> ar_size, T1);     /* activation record size */
	  if (xflag) {
	      gen2(ARG, 1, T1);
	  } else {
	      gen2(ARG, 3, T1);
	      gen2(GAR, 2, T1);     /* argv */
	      gen2(ARG, 2, T1);
	      gen2(GAR, 1, T1);     /* argc */
	      gen2(ARG, 1, T1);
	  }
	  genl(EXT, "_russell_set_up");
	  genl(LBA, "_russell_set_up");
	  if (xflag) {
	      gen1(CLC, 1);
	  } else {
	      gen1(CLC, 3);
	  }
	  gen1(HINT, GFU);
	  gen2(MOV, RL, GF);
	  gen2(MOV, GF, AR);

	/* Save the result in a named global location */
	  sprintf(ar_name_buf, "_%s_ar_save_loc", entry_name);
	  genl(LBA, ar_name_buf);
	  gen1(IDT, 0);
	  gen2(DCL, T2, DCL_ADDR);
	  genl(LBA, ar_name_buf);
	  gen1(LDL, T2);
	  gen3(STI, T2, 0, GF);
	  gen1(UDC, T2);

	/* Also save the result in global_ar, so that the various */
	/* call_russell routines will work correctly.             */
	/* For C code generation, this is needed by every routine */
	/* in a nested file that references GF.			  */
	  gen2(HINT, ET, DCL_INT);
	  genl(EXT, "_global_ar");
#	  ifdef GEN_C
	    /* The C code has to declare global_ar in each file anyway. */
	    /* Furthermore, there is an initialized declaration in      */
	    /* startup.c.  Skip it here. 				*/
#	  else
	    genl(LBA, "_global_ar");
	    gen1(IDT, 0);
#	  endif
	  if (xflag) {
	    /* Global_ar is set up by the startup code.  Furthermore, it */
	    /* shouldn't matter which global_ar I get, since nobody      */
	    /* relies on anything in the global frame except initenv.    */
	    /* Separately compiled functions don't see anything else, 	 */
	    /* and functions in this file will lokk in the right place.  */
	    /* If I load things separately, pcr should actually          */
	    /* guarantee that each Russell program sees the right one,   */
	    /* but we don't rely on that.				 */
	  } else {
	    gen2(DCL, T2, DCL_ADDR);
	    genl(LBA, "_global_ar");
	    gen1(LDL, T2);
	    gen3(STI, T2, 0, GF);
	    gen1(UDC, T2);
	  }

	Gexpression( p, T1, FALSE );

	/* Generate an epilog that stores the type value */
	  sprintf(name_buf, "_%s_save_loc", entry_name);
	  genl(LBA, name_buf);
	  gen1(IDT, 0);
	  gen2(DCL, T2, DCL_ADDR);
	  genl(LBA, name_buf);
	  gen1(LDL, T2);
	  gen3(STI, T2, 0, T1);
	  gen1(UDC, T2);
	  gen0(RTN);

    /* Generate the necessary C callable procedure stubs  */
    /* These access both the stored type value and stored */
    /* global activation record.                          */
	compile_stubs(sig, name_buf, ar_name_buf);

    Gcompile_funcs();
}


/* generate code for function constructor p.  Return location holding object */
Gfuncconstructor(p, rloc)
NODE * p;
int rloc;

/*
 * Generate code to put the function object for the function construction
 * p into rloc.  Queue the function body for later code generation.
 */
{
    int n_args;
    boolean contains_globals;  /* Closure contains non-local bindings */
    int cl_size;
    int fv_len;         /* number of non-locals */
    int tloc = avail_loc++;

    gen2(DCL, tloc, DCL_INT);
    if (p -> fc_body -> kind == EXTERNDEF) {
      int n_args = length(p -> signature -> fsig_param_list);

      /* allocate function object */
	ALLOC_FO(rloc);
#     ifdef UNDEFINED
	/* Generate new name for function stub */
	  p -> fc_code_label = new_global_label("fstub");
#     endif
      /* Fill in ip */
        genl(EXT, p -> fc_code_label);
        genl(LBA, p -> fc_code_label);
	gen1(LDL, tloc);
	gen3(STI, rloc, FO_IP, tloc);
      /* fill in arg count + 1 as size */
	gen2(LDN, n_args + 1, tloc);
	gen3(STI, rloc, FO_SIZE, tloc);
      /* use arg count as ep */
	gen2(LDN, n_args, tloc);
	gen3(STI, rloc, FO_EP, tloc);
    } else {
	/* Compute size of closure */
	  contains_globals = ((p -> fc_complexity & CP_GLOBALS) != 0);
	  if (contains_globals) {
	    fv_len = length(p -> fc_free_vars);
	    if (fv_len <= 1) {
	      cl_size = 3;
	    } else {
	      cl_size = 3 + fv_len;
	    }
	  } else {
	    cl_size = 3;
	  }
      /* Allocate function object */
	gen2(LDN, cl_size, tloc);
	gen2(ALH, tloc, rloc);

      /* Fill in ip */
	genl(EXT, p -> fc_code_label);
        genl(LBA, p -> fc_code_label);
	gen1(LDL, tloc);
	gen3(STI, rloc, FO_IP, tloc);
      /* fill in size */
	if (p -> fc_complexity & NO_AR_REFS) {
	  /* store positive size, indicating stack a.r. is OK */
	    gen2(LDN, p -> ar_size, tloc);
	} else {
	    gen2(LDN, -(p -> ar_size), tloc);
	}
	gen3(STI, rloc, FO_SIZE, tloc);
      /* Fill in a suitable value as environment pointer */
	if (contains_globals) {

	  if (fv_len == 1) {
	    /* tloc := binding of non-local id */
		Gident(first(p -> fc_free_vars), tloc);
	    /* ep := tloc */
		gen3(STI, rloc, FO_EP, tloc);
	  } else if (fv_len > 1) {
	    int cp;  /* Next position in closure to be filled in */

	    /* ep := closure itself */
		gen3(STI, rloc, FO_EP, rloc);
	    /* copy non-locals */
		cp = 3;
		maplist(s, p -> fc_free_vars, {
		    Gident(s, tloc);
		    gen3(STI, rloc, cp, tloc);
		    cp++;
		});
	  }
	} else {
	  /* Fill in current AR as ep */
	    gen3(STI, rloc, FO_EP, AR);
	}
    }
    gen1(UDC, tloc);
    /* Add the function body to the queue */
	Gfc_add(p, Glevel + 1, FALSE);
}


/*
 * Generate code for the body of the function construction p at level l.
 * The actual function value is computed by Gfuncconstructor above.
 */
Gfunc_body(p, l)
NODE * p;
int l;
{
    boolean is_extern = (p -> fc_body -> kind == EXTERNDEF);
/* 
 *	Enter scope of new function construction
 */
        Gcurrent = p;
        Glevel = l;
	copied_globals = ((Gcurrent -> fc_complexity & CP_GLOBALS) != 0);
	if (copied_globals) {
	    n_globals = length(Gcurrent -> fc_free_vars);
	}

	if (sl_available) {
	    genl(EXT, p -> fc_code_label);
	    genl(BFN, p -> fc_code_label);
	} else {
	  /* This is "fast" version of routine */
	    sprintf(str_code_buf, "F%s", p -> fc_code_label);
	    genl(EXT, str_code_buf);
	    genl(BSF, str_code_buf);
	}

        if (Glevel == 0) {
	  /* save global frame pointer */
	    gen1(HINT, GFU);
	    gen2(MOV,AR,GF);
        }

        if (Pflag) {
          /* generate profiling code */
	    genl(PRO, (is_extern? p -> fc_body -> ext_name
				: p -> fc_code_label));
        }

        if (Tflag) {
          /* generate calls to stack trace routines */
	    Gentry_trace((is_extern? p -> fc_body -> ext_name
				   : p -> fc_code_label),
			   p -> signature -> fsig_param_list, FALSE);
	}

	if ((Fflag || (p -> fc_complexity & DIR_REC)) && !sl_available) {
	    int i, n_args;

	    /* Copy arguments into temporaries */
	      first_param_loc = avail_loc;
	      n_args = p -> ar_size - 1;  /* ar_size = # args + 1 */
	      avail_loc += n_args;
	      for (i = 0; i < n_args; i++) {
		gen2(DCL, first_param_loc + i, DCL_INT);
		gen2(GAR, i+1, first_param_loc + i);
	      }
	} else {
	    first_param_loc = 0;
	}
	if (p -> fc_complexity & DIR_REC) {
	    if (sl_available) {
	      sprintf(str_code_buf, "R%s", p -> fc_code_label);
	    } else {
	      sprintf(str_code_buf, "RF%s", p -> fc_code_label);
	    }
	    genl(LBL, str_code_buf);
	}

    if (is_extern) {
      /* Generate stub */
	NODE * params = p -> signature -> fsig_param_list;
	int n_args = length(params) - n_vacuous_params(params);
	int i;

      ASSERT(sl_available, "Can't compile stub without activation record\n");
      /* Push arguments */
	{
	    int j = avail_loc++;

	    gen2(DCL, j, DCL_INT);
	    for (i = n_args; i >= 1; i--) {
		gen3(LDI, AR, i, j);
		gen2(ARG, i, j);
	    }
	    gen1(UDC, j);
	}
      /* Call procedure */
	genl(EXT, p -> fc_body -> ext_name);
	genl(LBA, p -> fc_body -> ext_name);
	gen1(CLC, n_args);
      
    } else {
      /* recursively descend */
	Gexpression( p -> fc_body, RL, TRUE );
    }

    if ((Fflag || (p -> fc_complexity & DIR_REC)) && !sl_available) {
	int i, n_args;

	/* Undeclare argument temporaries */
	  n_args = p -> ar_size - 1;  /* ar_size = # args + 1 */
	  for (i = 0; i < n_args; i++) {
	       gen1(UDC, first_param_loc + i);
	  }
    }

    /* Generate the epilog */
        if (Tflag) {
          /* generate call to stack trace routine */
	    Gexit_trace(is_extern? p -> fc_body -> ext_name
				 : p -> fc_code_label);
	}
	gen0(RTN);

    if (Glevel == 0) {
	gen2(HINT, ET, DCL_INT);
	genl(EXT, "_entry_ar_sz");
        genl(LBA, "_entry_ar_sz");
	gen1(IDT, p -> ar_size);
    }
}

/*
 * Generate code for the expression tree headed by p.  The value of the
 * expression is left in the location passed as a parameter.
 * The location rloc is presumed to have been declared.
 * Last_expr is TRUE only if it is known that this is the last subexpression
 * to be executed as a part of the current function body.  (This
 * is used to improve handling of tail recursion.)
 */
Gexpression (p, rloc, last_expr)
register NODE * p;
int rloc;
boolean last_expr;
{
    int i;
    
    ASSERT( !last_expr || rloc == RL, "Gexpression: bad last_expr value\n");

    if (p -> signature -> kind == SIGNATURESIG) {
	/* signatures don't evaluate to anything interesting. */
	gen2(MOV, UN, rloc);
	return;
    }

    switch ( p -> kind ) {

	case OPRID :
	case LETTERID :
		Gident(p, rloc);
		break;

	case QSTR:
	case UQSTR:
		{
		    NODE * sig = p -> sel_type -> signature;
		    char * code;  /* Actually list of RIC instrs */
		    int maxlen;  /* Maximum length for validity of */
				 /* ts_string_code                 */
		    boolean know_inline;

		    ASSERT(sig -> kind == TYPESIGNATURE,
			   "codegen: bad string type\n");
		    if (sig -> ts_string_max == -1) {
			maxlen = MAXSTRLEN;
		    } else {
			maxlen = sig -> ts_string_max;
		    }
		    know_inline = (sig -> ts_string_code != NIL 
				   && sig -> ts_element_code != NIL
				   && strlen(p -> str_string) <= maxlen);
		    if (know_inline
			&& ! calls_put(p -> sel_type)) {
			/* build body of in-line expansion */
			  char *r = p -> str_string;
                          char *q = str_code_buf;

			  *q = '\0';
			  while (*r != '\0') {
			    sprintf(q, sig -> ts_element_code, *r);
			    /* position q at trailing 0 */
			      q += strlen(q);
			    r++;
			  }
			sprintf(str_code_buf2, sig -> ts_string_code,
				str_code_buf);
			code = (char *)Ginline_cnvt(str_code_buf2);
			write_RIC_seq(Goutfile, code, 0, rloc);
			free_RIC(code);
		    } else {
			if (Vflag) {
			    printf("Compiling expansion of %s\n",
				   p -> str_string);
			}
			/* Should consider ts_meta_concat here */
			Gexpression(p -> str_expansion, rloc, last_expr);
		    }
		}
		break;

        case APPLICATION :
		Gappl(p, rloc, last_expr);
                break;

        case BLOCKDENOTATION :
		{
		    if ( p -> bld_flags & REQUIRES_AR ) {
		      /* Allocate activation record on heap.  Stack */
		      /* allocation doesn't make any sense, since   */
		      /* we only allocate a separate a.r. if refs   */
		      /* to the environment can escape.             */
			int sz_loc = avail_loc++;
			int ar_loc = avail_loc++;

			ASSERT(sl_available, "Block a.r. inside simple fn");
			Glevel++;
			gen2(DCL, sz_loc, DCL_INT);
			gen2(DCL, ar_loc, DCL_ADDR);
			gen2(LDN, p -> ar_size, sz_loc);
			gen2(ALH, sz_loc, ar_loc);
			gen3(STI, ar_loc, 0, AR);
			gen2(MOV, ar_loc, AR);
			gen1(UDC, sz_loc);
			gen1(UDC, ar_loc);
		    }
		    /* fill in undefined values so that forward refs */
		    /* can be checked.                               */
		    /* Declare virtual registers used for bindings.  */
                      maplist (v, p -> bld_declaration_list, {
			ASSERT (v->kind == DECLARATION,
                                "codegen.c: decl expected");
			if (v -> decl_needed &&
			    (v -> decl_special & (ID_IN_REG | VAR_IN_REG))) {
			    genl(LBR, getname(v -> decl_id
						-> id_str_table_index));
			    if (v -> decl_special & PTR_VAR_IN_REG) {
				gen2(DCL, v -> displacement, DCL_ADDR);
			    } else {
				gen2(DCL, v -> displacement, DCL_INT);
			    }
			}
			if (v -> decl_needed &&
			    v -> decl_can_be_refd <= v -> pre_num) {
			  /* possible forward reference to this decl */
			    gen2(HINT, OPT, 1);
			    if (v -> decl_special & (ID_IN_REG | VAR_IN_REG)) {
				gen2(MOV, UN, v -> displacement);
			    } else {
				gen3(STI, AR, v -> displacement, UN);
			    }
                        }
		      });
		    maplist (v, p -> bld_declaration_list, {
		      extern compile_decl();

		      compile_decl(v);
		    });
		    maplist (v,p->bld_den_seq, {
                        if (v != last(p -> bld_den_seq)) {
			    Gexpression(v, SK, FALSE);
                        } else {
			    Gexpression(v, rloc, last_expr);
			}
		    });
		    /* Undeclare locations holding id bindings */
		      maplist (v, p -> bld_declaration_list, {
			if (v -> decl_needed &&
			    (v -> decl_special & (ID_IN_REG | VAR_IN_REG))) {
			    if (v -> decl_special & ARRAY_CONTIG) {
				/* References to the interior of the array */
				/* can be live up to this point.  Thus     */
				/* the pointer to the beggining of the     */
				/* array should be kept around.            */
				gen2(HINT, LIVE, v -> displacement);
			    }
			    gen1(UDC, v -> displacement);
			}
		      });
		    if ( p -> bld_flags & REQUIRES_AR ) {
			Glevel--;
			gen3(LDI, AR, 0, AR);
		    }
		    break;
		}
		
	case LOOPDENOTATION :
		if (length(p -> gl_list) == 1) {
		    compile_while_loop(first(p -> gl_list), rloc);
		    break;
		} /* else continue as with GUARDEDLIST: */

        case GUARDEDLIST :
		{
		    char * L0;
		    register NODE * v;
		    boolean saw_else = FALSE;

		    if (p->kind == LOOPDENOTATION) {
			L0=Gnewlabel("loop");
                        genl(LBL, L0);
		    } 
		    else {
			L0=Gnewlabel("guard_exit");
		    }
		    maplist (v,p->gl_list, {
                        char * L1;
                        
                        ASSERT (v->kind == GUARDEDELEMENT,
					"codegen.c: bad guard list");
			if (v -> ge_guard -> kind == WORDELSE)  {
			    saw_else = TRUE;
			} else {
			    L1 = Gnewlabel ("guard");
			    if (Oflag && LAST_ITER
				&& p -> kind == GUARDEDLIST) {
				/* No reason to perform the test, since */
				/* it better be true.                   */
				/* Evaluate guard for effect:           */
				Gexpression(v -> ge_guard, SK, FALSE);
			    } else {
				Gexpression(v->ge_guard, TL, FALSE);
				genl(BRF, L1);
			    }
			}
			if (p -> kind == LOOPDENOTATION) {
			    Gexpression(v -> ge_element, SK, FALSE);
			    genl(BR, L0);
                        } else {
			    Gexpression(v -> ge_element, rloc, last_expr);
			    if (saw_else) {
				/* Just fall off the end, dropping */
				/* any other else clauses          */
				    break;
			    } else {
				/* Skip over the rest */
				    genl(BR, L0);
			    }
			}
			genl(LBL, L1);
                    });
		    if (p -> kind == LOOPDENOTATION) {
                      /* return Void value "Null" */
                        gen2(MOV, UN, rloc);
		    } else {
		      if (!saw_else) {
			/* Fell through the end of else-less "if" */
			    gen2(HINT, OPT, 1);
			    genl(ERR, "_cond_error");
		      }
		      /* Exit label for "if" */
                        genl(LBL, L0);
                    }
		    break;
                }

	case WORDELSE :
                {
                    /* Use true value */
		      gen1(TRU, rloc);
		    break;
                }
		

	case FUNCCONSTR :
		{
		    Gfuncconstructor (p, rloc);
		    break;
                }

        case REXTERNDEF :
                {
		    int name_length = strlen(p -> r_ext_name);
		    char * q;

                    if (name_length + 3 > MAXSTRCODELEN) {
                        errmsg0(p, "File name too long");
		    }
		    if (Vflag) {
			printf("%s forces evaluation of externally defined object %s\n",
			       Gcurrent -> fc_code_label,
			       p -> r_ext_name);
		    }
		    strcpy(str_code_buf, p -> r_ext_name);
		    str_code_buf[name_length] = '.';
                    str_code_buf[name_length+1] = 'o';
                    str_code_buf[name_length+2] = 0;
		    add_objfile(str_code_buf);

		    strcpy(str_code_buf, "m_");
		    strcat(str_code_buf, p -> r_ext_name);
		    /* Replace slashes with periods */
			for (q = str_code_buf; *q != '\0'; q++) {
			    if (*q == '/') {
				*q = '.';
			    }
			}
		    gen2(ARG, 1, GF);
                    genl(CLL, str_code_buf);
		    gen2(MOV, RL, rloc);
                    break;
                }

	case USELIST :
		{
		    maplist (v,p->usl_den_seq, {
                        if (v != last(p -> usl_den_seq)) {
			  Gexpression(v, SK, FALSE);
                        } else {
			  Gexpression(v, rloc, last_expr);
                        }
		    });
		    break;
		}


	case MODPRIMARY :
		{
		    NODE * tm = p -> mp_type_modifier;
		    unsigned * delv = (unsigned *)(p -> mp_delete_v);
		    int orig = p -> mp_orig_length;
		    int final = 0;  /* size of modified type */
		    int i,j;
		    int res_pos;    /* current position in result    */
				    /* type                          */
		    DECLARE_ITER;   /* used for unusual traversal of */
		    NODE *s;        /* with list.  Note that with    */
				    /* list is initially ordered by  */
				    /* final component positions.    */
		    int *q;
		    boolean is_wl = (tm == NIL? FALSE
					      : (tm -> kind == WITHLIST));
		    int wl_length;

                    if (is_wl) {
			wl_length = length(tm -> wl_component_list);
		    } else {
			wl_length = 0;
		    }
		    /* calculate size of new type */
		      if (orig > 0) {
			q = (int *)delv; i = 0; j = *q;
			while (i < orig) {
			  if (j >= 0) /* not deleted */ final++;
			  i++; j <<= 1;
			  if (i % WORDLENGTH == 0) /* go on to next word */ {
			    j = *(++q);
			  }
			}
		      }
		      final += wl_length;
                    if (final == 0) {
                        gen2(LDN, 0, rloc);
                    } else {
                        int sz_loc = avail_loc++;
                                     /* size of new type              */
                                     /* also used as temp for copying */
                                     /* and wl components             */
			int primary_loc = avail_loc++; /* pointer to orig tp */
			int result_loc = avail_loc++;

			/* Set up result_loc, in case rloc is SK */
			    if (rloc == SK || rloc == RL){
				/* RL might interfere with subsidiary */
				/* code generation for WITH list      */
				gen2(DCL, result_loc, DCL_ADDR);
			    } else {
				result_loc = rloc;
			    }
			/* Evaluate original type */
			    if (orig > 0) {
			      gen2(DCL, primary_loc, DCL_ADDR);
			      Gexpression(p -> mp_primary, primary_loc, FALSE);
			    }

                        /* allocate type object, put ptr to it into tp_loc */
                        /* and rloc                                        */
                            gen2(DCL, sz_loc, DCL_INT);
			    gen2(LDN, final, sz_loc);
			    gen2(ALH, sz_loc, result_loc);
			    gen1(UDC, sz_loc);

			/* save new type in space reserved for local name */
			  if (is_wl) {
			    int display_entry;

                            DISPLAY (display_entry, p -> level);
			    gen3(STI, display_entry, p -> displacement,
				      result_loc);
                            UNDISPLAY(display_entry);
			  }

                        /* pointer to new object is in tp_loc      */
			/* copy selected fields, reserve new ones  */
			  
                          /* s := first element of with list, NIL if there */
			  /* are none.                                     */
			    if (is_wl && !is_empty(tm->wl_component_list)) {
			      INIT_ITER(s, tm -> wl_component_list);
			    } else {
			      s = NIL;
			    }
			  q = (int *)delv; i = res_pos = 0;
			  j = (orig > 0? *q : 0);
			  while (s != NIL || i < orig) {
			    /* i = position in original type             */
			    /* sign bit of j = corr. deletion vector bit */
			    /* s = next unprocessed entry in with list   */
			    ASSERT(res_pos < 5000 && i < 5000,
				   "Gexpression: bad type modification\n");
			    if (s != NIL && s -> decl_sel_index == res_pos) {
				res_pos++;
				NEXT_ITER(s);
				continue;
			    } else if (j >= 0) /* not deleted */ {
				/* Copy this element and increment pointers */
				  gen3(LDI, primary_loc, i, T1);
				  gen3(STI, result_loc, res_pos, T1);
				res_pos++;
				i++; j <<= 1;
			    } else /* deleted */ {
				i++; j <<= 1;
			    }
			    if (i % WORDLENGTH == 0) /*go on to next word*/ {
				j = *(++q);
			    }
                          }
                        /* new type, with with-list components missing, */
			/* is in result_loc.                            */
			if (orig > 0) {
			    gen1(UDC, primary_loc);
			}
			if (is_wl) {
			  NODE * decl_l = (NODE *)
			  		     decl_sort(p -> mp_type_modifier
						       -> wl_component_list);
					      /* declaration list in original*/
					      /* order, with forward refs    */
					      /* marked.                     */
			  int comp_loc = avail_loc++;

                          /* fill in undefined values so that forward refs */
                          /* can be checked.                               */
                            maplist (v, decl_l, {
                              ASSERT (v -> kind == DECLARATION,
                                      "codegen.c: decl expected");
                              if (v -> decl_can_be_refd <= v -> pre_num) {
                                /* possible forward reference to this decl */
				  gen3(STI, result_loc, v -> decl_sel_index, UN);
                              }
                            });
			  /* Fill in with list components */
			    gen2(DCL, comp_loc, DCL_ADDR);
                            maplist(s, decl_l, {
			      Gexpression(s -> decl_denotation, comp_loc, FALSE);
			      gen3(STI, result_loc,
				   s -> decl_sel_index, comp_loc);
			    });
			    gen1(UDC, comp_loc);
                        }
			/* result is in result_loc */
			if (rloc == SK) {
			    gen1(UDC, result_loc);
			} else if (rloc == RL) {
			    gen2(MOV, result_loc, rloc);
			    gen1(UDC, result_loc);
			}  
		    }
		    break;
		}

        case RECORDCONSTRUCTION:
	case PRODCONSTRUCTION :
	case UNIONCONSTRUCTION :
	case ENUMERATION:
        case EXTENSION :
                type_constr(p, rloc);
                break;


        default :
            findvl( p -> vlineno );

            dbgmsg( "Gexpression: Unimplemented construct (kind = %s) in file %s at line %d\n",
                    kindname(p->kind), getname(getfn()), getrl() );
            dbgmsg( "Gexpression:  p is 0x%x\n",p);
            fprintf( Goutfile, "?" );
            fflush (Goutfile);
            abort();
    }
}

/* Compile a loop consisting of a single guarded command efficiently */
/* Could probably be generalized ...                                 */
compile_while_loop(p, rloc)
NODE *p;
int rloc;
{
    extern boolean OOOflag;     /* Unroll once */
    char * cond_label = Gnewlabel("guard");
    char * start_label = Gnewlabel("loop");
    char * end_label;
    boolean Ocompile_fcs = compile_fcs;

    if (OOOflag) {
      /* Compile unrolled loop body */
	compile_fcs = FALSE;  /* ignore nested functions; we'll get them */
			      /* later.                                  */
	end_label = Gnewlabel("loop_end");
	Gexpression(p -> ge_guard, TL, FALSE);
	genl(BRF, end_label);
	Gexpression(p -> ge_element, SK, FALSE);
	compile_fcs = Ocompile_fcs;
    }
    genl(BR, cond_label);
    genl(LBL, start_label);
    Gexpression(p -> ge_element, SK, FALSE);
    genl(LBL, cond_label);
    Gexpression(p -> ge_guard, TL, FALSE);
    genl(BRT, start_label);
    if (OOOflag) {
	genl(LBL, end_label);
    }
    /* Expression value is Null */
	gen2(MOV, UN, rloc);
}
