/* 
 *  This is the zeroth approximation to the Russell code generator.
 *  I have started with printastx.c from pass5a and have begun to hack.
 *      Jim Hook  27 March 1982 
 *  Trying to revive this code.  Switched from display to static chain.
 *  Adding stack based calls, in-line calls, and type modification.
 *      Hans Boehm 7 Feb 1984
 *  Adding type constructions, etc.
 *      Hans Boehm 15 June 1984
 *  Added jsb calls, etc.
 *      Hans Boehm 6 Oct 1984
 *  Adding stack based variable allocation.
 *      Hans Boehm 4 June 1986 
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codeutil.h"
# include "pass4/sigs.h"
# include "pass3/is_local.h"

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

# define UNDEF (0x87654321)  /* runtime rep of undefined value */
# define MAXOBJSZ 512	/* Must agree with the (obsolete) VAX runtime */

extern int yydebug;
extern int yynerrs;

extern boolean Pflag;  /* Generate profiling code */
extern boolean Tflag;  /* Generate trace code */

extern char * entry_name; /* "" for main compilation */

boolean mentions_r11();

void type_expr();  /* generate code for type in record element */

extern boolean is_int_const();

char str_code_buf[MAXSTRCODELEN]; /* used for strings, "special" fns */
                                  /* and by find_inline              */
                                  /* and for object file names       */

FILE * objfilelist = NULL;  /* List of object files which must be loaded */

/* Add an object file to the list of files needed for linking */
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

/* Definitions for list of function constructions for whose body
 * we still need to generate code
 */

typedef struct Fc_Entry {
	NODE * fc_pointer;
	int fc_level;
        int fc_fast_only;  /* Only jsb version needed */
	struct Fc_Entry * fc_next;
} fc_entry;

fc_entry * fc_list = NIL;

/* Add an entry to the front of fc_list */
void fc_add(ptr,lvl,fast_only)
NODE * ptr;    /* pointer to function construction */
int lvl;       /* level of function construction   */
int fast_only; /* generate only jsb version	   */
{
    fc_entry * p = (fc_entry *)malloc(sizeof (fc_entry));

    p -> fc_pointer = ptr;
    p -> fc_level = lvl;
    p -> fc_fast_only = fast_only;
    p -> fc_next = fc_list;
    fc_list = p;
}

/* remove an entry from the head of fc_list */
void fc_delete()
{
    fc_entry * p = fc_list;

    fc_list = fc_list -> fc_next;
    free(p);
}

int Ventry_mask,Vgc_mask;
int Vlevel = -1;   /* current static nesting level */

NODE * Vcurrent;   /* Function currently being compiled */
FILE * Voutfile;


/* generate code for syntax tree p into file f */
Vgeneratecode ( f , p )
NODE * p;
FILE * f;
{
    NODE * fc_ptr;
    int fc_lvl;
    int fc_fast_only;

    Voutfile = f;
    p -> fc_code_label = "russell_top_level";
    analyze(p);     /* Set up fc_complexity fields, etc. */
    accessible(p);  /* find accessible code                 */
    bld_analyze(&p);  /* Find blocks requiring activation records */
    Vallocate (p, TRUE);  /* allocate space in activation records */
    if (yydebug) prtree(p);
    ASM_HEADER;
    fc_add(p,0,FALSE);  /* add main function to list of functions that need */
                        /* compiling.                                       */
    while (fc_list != NIL) {
	fc_ptr = fc_list -> fc_pointer;
        fc_lvl = fc_list -> fc_level;
 	fc_fast_only = fc_list -> fc_fast_only;
	fc_delete();
	if(!fc_fast_only) Vfuncbody(fc_ptr, fc_lvl);
	if((fc_ptr -> fc_complexity & NO_SL) && fc_lvl != 0) {
	    Ffuncbody(fc_ptr);
	    /* These have no nested function constructions */
	}
    }
}

/* generate code for syntax tree p embedded in syntax tree q into file f */
/* invoked with -c compiler flag                                         */
Vgeneratepcode ( f, q, p )
NODE * q;
NODE * p;
FILE * f;
{
    NODE * fc_ptr;
    int fc_lvl;
    int fc_fast_only;
    extern char * ofname;  /* Name of assembly language file */

    Voutfile = f;
    q -> fc_code_label = "russell_top_level";
    if (p -> kind != FUNCCONSTR) {
	errmsg0(p, "Outermost expression must be function construction");
        return;
    }
    analyze(q);     /* Set up fc_complexity fields */
    accessible(q);  /* find accessible code                 */
    bld_analyze(&q);  /* Find blocks requiring activation records */
    Vallocate (q, TRUE);  /* allocate space in activation records */
    if (yydebug) prtree(p);
    if (p -> ar_static_level != 1) {
        dbgmsg("user program must be at level 1");
    }
    /* Compile main function */
      fprintf(Voutfile, "\t.globl\tm_%s\n", entry_name);
      fprintf(Voutfile, "m_%s:\n", entry_name);
      Vlevel = 0;
      Vfuncconstructor(p);
      fprintf(Voutfile, "\tmovl\t(sp)+,r0\n");
      fprintf(Voutfile, "\trsb\n");

    while (fc_list != NIL) {
	fc_ptr = fc_list -> fc_pointer;
        fc_lvl = fc_list -> fc_level;
 	fc_fast_only = fc_list -> fc_fast_only;
	fc_delete();
	if(!fc_fast_only) Vfuncbody(fc_ptr, fc_lvl);
	if((fc_ptr -> fc_complexity & NO_SL) && fc_lvl != 0) {
	    Ffuncbody(fc_ptr);
	    /* These have no nested function constructions */
	}
    }
}


/* generate code for function constructor p */
Vfuncconstructor(p)
NODE * p;

/*
 * Generate code to push the function object for the function construction
 * p onto the stack.  Queue the function body for later code generation.
 */
{
	if (p -> fc_body -> kind == EXTERNDEF) {
	  int n_args = length(p -> signature -> fsig_param_list);
          /* compute the function "value" */
            putcomment ("# request new function object");
            FXD_NEWOBJ(FO_OBJ_SIZE);
	    fprintf(Voutfile,"\tmovl\t$%d,%d(r0)", n_args, FO_EP);
	    putcomment("# environment pointer");
	    fprintf(Voutfile,"\t.globl\t%s\n",p -> fc_code_label);
	    fprintf(Voutfile,"\tmovab\t%s,%d(r0)",p -> fc_code_label, FO_IP);
	    putcomment("# instruction pointer");
	    fprintf(Voutfile,"\tmovl\t$%d,%d(r0)", n_args+1 ,FO_SIZE);
	    putcomment("# size of activation record");
	    PUSH ("r0","# push function value");
	    return;
	}

      /* compute the function "value" for non-external */
        putcomment ("# request new function object");
        FXD_NEWOBJ(FO_OBJ_SIZE);
	fprintf(Voutfile,"\tmovl\tap,%d(r0)",FO_EP);
	putcomment("# environment pointer");
	fprintf(Voutfile,"\tmovab\t%s,%d(r0)",p -> fc_code_label, FO_IP);
	putcomment("# instruction pointer");

		/* Note that there is an implicit assumption that
			small integers will be sufficient for stack
			frame size.  */
	fprintf(Voutfile,"\tmovzwl\t$%d,%d(r0)",p -> ar_size,FO_SIZE);
	putcomment("# size of activation record");
	PUSH ("r0","# push function value");

    /* Add the function body to the queue */
	fc_add(p, Vlevel + 1, FALSE);
}


/*
 * Generate code for the body of the function construction p at level l.
 * The actual function value is computed by Vfuncconstructor above.
 */
Vfuncbody(p, l)
NODE * p;
int l;
{
	char * M1;
	
/* 
 *	Enter scope of new function construction
 */
	Vcurrent = p;
	Ventry_mask = 0;
	Vgc_mask = 0;
	Vlevel = l;

	fprintf(Voutfile,"\t.globl\t%s\n",p -> fc_code_label);
        M1 = Vnewlabel ("mask");
	CODE ("\t.align  1");
        fprintf(Voutfile,"%s:\t.word\t%s",p -> fc_code_label,M1);
        putcomment("# code body");

	if (Vlevel == 0) {
	  /* save global frame pointer */
	    fprintf(Voutfile,"\tmovl\tap,%s\n", L0fp);
	    Ventry_mask |= L0FP;
	}

        if (Pflag) {
          /* generate profiling code */
            Vcall_mcount();
        }

        if (Tflag) {
          /* generate calls to stack trace routines */
            Ventry_trace(p -> fc_code_label,
                         p -> signature -> fsig_param_list, FALSE);
        }

    /* recursively descend */
        Vexpression( p -> fc_body );

    /* Generate the epilog:  r0 <- top of stack */
        if (Tflag) {
          /* generate call to stack trace routine */
            Vexit_trace();
        }
	POP ("r0","# function value to r0");
	fprintf(Voutfile,"\tret\n");
	fprintf(Voutfile,"\t.set\t%s,0x%x\n",M1,Ventry_mask);

    if (Vlevel == 0) {
        CODE("\t.data");
        fprintf(Voutfile,"_entry_ar_sz:\t.long\t%d\n",p -> ar_size);
        CODE("\t.text");
    }
}

/*
 * Generate code for the expression tree headed by p.
 */
Vexpression (p)
register NODE * p;
{
    int i;

    if (p -> signature -> kind == SIGNATURESIG) {
	/* signatures don't evaluate to anything interesting. */
	fprintf(Voutfile, "\tpushl\t$0\n");
	return;
    }

    switch ( p -> kind ) {

        case OPRID :
        case LETTERID :
		{
		    register NODE * v;
		    char * display_reg; /* name of reg used for a.r. pointer */
		    char * r10 = "r10";

		    if (is_int_const(p)) {
			extern long int_const_val;

			fprintf(Voutfile, "\tpushl\t$%d\n", int_const_val);
			return;
		    }

		    v = p -> id_last_definition;
		    if (p -> sel_type == NIL) {
			ASSERT2 (v != NIL, "Vexpression: id %s not declared\n", 
				 getname(p -> id_str_table_index)
			);
			ASSERT2 (v -> kind == DECLARATION 
			     || v -> kind == PARAMETER
			     || v -> kind == MODPRIMARY
				&& v -> mp_type_modifier -> kind == WITHLIST,
			      "Vexpression: id %x not declaration or parameter\n",v
			);
			putcomment1("\t\t\t# Identifier %s",
				     getname(p -> id_str_table_index));
			DISPLAY ( display_reg, v -> level, r10,
				"# display entry in place");
			if (display_reg == r10) Ventry_mask |= R10;
			if (v -> kind == DECLARATION
			    && (v -> decl_special & VAR_ON_STACK)) {
			  fprintf(Voutfile, "\tmoval\t%d(%s),-(sp)\n",
				  4 * v->displacement, display_reg);
			} else {
			  PUSH_DISP (display_reg, v->displacement,
						  "# referenced object");
			}
		    }
		    else {
			putcomment1("\t\t\t# Selection of %s",
				     getname(p->id_str_table_index));
			Vexpression (p -> sel_type);
			POP ("r0","# type value to r0");
			PUSH_DISP ("r0",p -> sel_index,
				"# push selected function value");
                    }
                    if (p -> id_forward_ref) {
                      /* Check that the value is defined */
                        fprintf(Voutfile, "\tcmpl\t(sp),$0x%X\t #---\n",
                                UNDEF);
                        fprintf(Voutfile, "\tbneq\t1f\t #---\n");
                        fprintf(Voutfile,
                                "\tcalls\t$0,_forward_error\t #---\n");
                        fprintf(Voutfile, "1:\n");
                    }
		    break;
		}

	case QSTR:
	case UQSTR:
		{
		    NODE * sig = p -> sel_type -> signature;
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
			fprintf(Voutfile, sig -> ts_string_code, str_code_buf);
			fprintf(Voutfile, "\n");
		    } else {
			/* Should consider ts_meta_concat here */
			Vexpression(p -> str_expansion);
		    }
		}
		break;
		
	case APPLICATION :
		Vappl(p);
		break;

        case BLOCKDENOTATION :
		{
		    if ( p -> bld_flags & REQUIRES_AR ) {
		      /* Allocate activation record on heap.  Stack */
		      /* allocation doesn't make any sense, since   */
		      /* we only allocate a separate a.r. if refs   */
		      /* to the environment can escape.             */ 
			Vlevel++;
			FXD_NEWOBJ(p -> ar_size);
			CODE("\tmovl\tap,(r0)");
			CODE("\tmovl\tr0,ap");
		    }
		    /* fill in undefined values so that forward refs */
                    /* can be checked.                               */
                      maplist (v, p -> bld_declaration_list, {
			ASSERT (v->kind == DECLARATION,
                                "codegen.c: decl expected");
			if (v -> decl_needed &&
			    v -> decl_can_be_refd <= v -> pre_num) {
                          /* possible forward reference to this decl */
                            fprintf(Voutfile,"\tmovl\t$0x%X,%d(ap)\t #---\n",
                                    UNDEF, ObjSize * (v -> displacement));
                            putcomment("# store undefined value");
                        }
                      });
		    maplist (v, p -> bld_declaration_list, {
		      if (!v -> decl_needed) {
			  /* Generate code for nested function       */
			  /* constructions or modified primary nodes */
			  /* that may be evaluated.		     */
			    Vtraverse (v -> decl_denotation);
		      } else {
			if (!(v -> decl_special & VAR_ON_STACK)) {
			    Vexpression (v-> decl_denotation);
			    POP_DISP ("ap",v->displacement,
			              "# store declared value");
			} else {
			  /* Initialize location */
			    if (v -> decl_special & SIMPLE_VAR_ON_STACK) {
				fprintf(Voutfile,"\tmovl\t$0,%d(ap)\n",
					4 * v->displacement);
			    } else if (v -> decl_special & PTR_VAR_ON_STACK) {
				fprintf(Voutfile,"\tmovl\t$%d,%d(ap)\n",
					UNDEF, 4 * v->displacement);
			    } else /* explicitly initialized */ {
				NODE * appl = v -> decl_denotation;
				NODE * arg = first(appl -> ap_args);

				ASSERT(appl -> kind == APPLICATION,
				       "codegen.c: bad New application");
				Vexpression (arg);
				POP_DISP ("ap",v->displacement,
					  "# store initial value");
			    }
			}
		      }
		    });
		    maplist (v,p->bld_den_seq, {
			Vexpression(v);
			if (v != last(p -> bld_den_seq)) {
			    POP ("r0","# trash value");
			}
		    });
		    if ( p -> bld_flags & REQUIRES_AR ) {
			Vlevel--;                                      
			CODE("\tmovl\t(ap),ap");                          
		    }
		    break;
		}
		
        case GUARDEDLIST :
        case LOOPDENOTATION :
		{
		    char * L0;
		    register NODE * v;

		    if (p->kind == LOOPDENOTATION) {
			L0=Vnewlabel("loop");
			fprintf(Voutfile,"%s:\n",L0);
		    } 
		    else {
			L0=Vnewlabel("guard_exit");
		    }
		    maplist (v,p->gl_list, {
			char * L1;
			
			ASSERT (v->kind == GUARDEDELEMENT,
					"codegen.c: bad guard list");
			Vexpression(v->ge_guard);
			L1 = Vnewlabel ("guard");
			POP ("r0","# value of guard");
			fprintf(Voutfile,"\tjeql\t%s", L1);
			putcomment ("# branch on false");
			Vexpression(v->ge_element);
			if (p -> kind == LOOPDENOTATION) {
			    POP("r0","# trash element value");
			} else {
			    /* only one element execd */
			}
 			fprintf(Voutfile,"\tjbr\t%s",L0);
			putcomment ("# leave guarded list");
			fprintf(Voutfile,"%s:",L1);
			putcomment ("# next case");
		    });
		    if (p -> kind == LOOPDENOTATION) {
		      PUSH ("$0","# Value of loop or default of else is void");
		    } else {
		      fprintf(Voutfile,"\tcalls\t$0,_cond_error\n");
		    }
		    if (p->kind == GUARDEDLIST) {
			fprintf (Voutfile,"%s:\n",L0);
		    }
		    break;
		}
	case WORDELSE :
		{
		    PUSH ("$1","# Else = constant 1");
		    break;
		}
		

	case FUNCCONSTR :
		{
                    Vfuncconstructor (p);
		    break;
                }

        case REXTERNDEF :
                {
		    int name_length = strlen(p -> r_ext_name);
		    char *q;

                    if (name_length + 3 > MAXSTRCODELEN) {
                        errmsg0(p, "File name too long");
                    }
                    strcpy(str_code_buf, p -> r_ext_name);
                    str_code_buf[name_length] = '.';
                    str_code_buf[name_length+1] = 'o';
                    str_code_buf[name_length+2] = 0;
                    add_objfile(str_code_buf);

		    strcpy(str_code_buf, p -> r_ext_name);
		    /* Replace slashes with periods */
			for (q = str_code_buf; *q != '\0'; q++) {
			    if (*q == '/') {
				*q = '.';
			    }                
			}
		    fprintf(Voutfile, "\t.globl\tm_%s\n", str_code_buf);
		    fprintf(Voutfile, "\tjsb\tm_%s\n", str_code_buf);
                    fprintf(Voutfile, "\tpushl\tr0\n");
                    break;
                }

	case USELIST :
		{
		    maplist (v,p->usl_den_seq, {
			Vexpression(v);
			if (v != last(p -> usl_den_seq)) {
			  POP ("r0","# trash value");
			}
		    });
		    break;
		}


	case MODPRIMARY :
		{
		    NODE * tm = p -> mp_type_modifier;
		    unsigned * delv = (unsigned *)p -> mp_delete_v;
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
		    int skipcnt;
		    int copycnt;
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
			CODE("\tclrl\t-(sp)");
		    } else {
			Vexpression(p -> mp_primary);
                        putcomment("# Get type object");
                        FXD_NEWOBJ(final);
                        CODE("\tclrl\t(r0)");

			/* pointer to new object is in r0          */
			/* copy selected fields, reserve new ones  */
			  
			  Ventry_mask |= R10;
			  POP("r10", "# original type");
			  PUSH("r0", "# new type");
			  /* s := first element of with list, NIL if there */
			  /* are none.                                     */
			    if (is_wl && !is_empty(tm->wl_component_list)) {
			      INIT_ITER(s, tm -> wl_component_list);
			    } else {
			      s = NIL;
			    }
			  q = (int *)delv; i = res_pos = 0;
			  j = (orig > 0? *q : 0);
			  skipcnt = 0; copycnt = 0;
			  while (s != NIL || i < orig) {
			    /* i = position in original type             */
			    /* sign bit of j = corr. deletion vector bit */
			    /* s = next unprocessed entry in with list   */
			    /* skipcnt = number of fields to be skipped  */
			    /* before next field is copied               */
			    /* copycnt = number of fields still to be    */
			    /* copied.                                   */
			    if (s != NIL && s -> decl_sel_index == res_pos) {
				/* first take care of postponed copies */
				  copy_r10_to_r0(copycnt);
				  copycnt = 0;
				fprintf(Voutfile,"\tclrl\t(r0)+");
				putcomment("# space for with list component");
				res_pos++;
				NEXT_ITER(s);
				continue;
			    } else if (j >= 0) /* not deleted */ {
				/* skip indicated number of slots */
				  if (skipcnt != 0) {
				      fprintf(Voutfile,"\taddl2\t$%d,r10",
						       4*skipcnt);
				      putcomment("# skip slots");
				      skipcnt = 0;
				  }
				copycnt++;
				res_pos++;
				i++; j <<= 1;
			    } else /* deleted */ {
				/* copy indicated number of slots */
				  copy_r10_to_r0(copycnt);
				  copycnt = 0;
				skipcnt++;
				i++; j <<= 1;
			    }
			    if (i % WORDLENGTH == 0) /*go on to next word*/ {
				j = *(++q);
			    }
			  }
			  /* take care of any remaining copies */
			    copy_r10_to_r0(copycnt);
			    copycnt = 0;
			/* new type, with with-list components missing, */
			/* is on top of the stack                       */
			if (is_wl) {
			  char * display_reg; /* name of reg used for a.r. */
					      /* pointer                   */
			  char * r10 = "r10";
			  NODE * decl_l = (LIST)
			  		    decl_sort(p -> mp_type_modifier
						        -> wl_component_list);
                                              /* declaration list in original*/
                                              /* order, with forward refs    */
                                              /* marked.                     */
			  char * nt_tmp;  /* temporary for new type object  */
			  boolean in_memory;
					    
			  /* allocate temporary for new type and save it    */
			  /* there.                                         */
			    nt_tmp = Vnewreg();
			    in_memory = (Vreg_bit == 0);
			    fprintf(Voutfile, "\tmovl\t(sp),%s", nt_tmp);
			    putcomment("# new type to temporary");
			  /* save new type in space reserved for local name */
			    DISPLAY (display_reg, p -> level, r10,
				     "# display entry for wlc");
			    if (display_reg == r10) Ventry_mask |= R10;
			    fprintf(Voutfile, "\tmovl\t%s,%d(%s)",
					      nt_tmp,
					      4 * (p -> displacement),
					      display_reg);
			    putcomment("# save for local id references");

                          /* fill in undefined values so that forward refs */
                          /* can be checked.                               */
                            maplist (v, decl_l, {
                              ASSERT (v -> kind == DECLARATION,
                                      "codegen.c: decl expected");
                              if (v -> decl_can_be_refd <= v -> pre_num) {
                                /* possible forward reference to this decl */
                                  if (in_memory) {
                                    fprintf(Voutfile,
                                            "\tmovl\t%s,r0\t #---\n", nt_tmp);
                                  }
                                  fprintf(Voutfile,
                                          "\tmovl\t$0x%X,%d(%s)\t #---",
                                          UNDEF,
                                          ObjSize * (v -> decl_sel_index),
                                          in_memory? "r0" : nt_tmp);
                                  putcomment("# store undefined value");
                              }
                            });
			  /* Fill in with list components */
                            maplist(s, decl_l, {
			      Vexpression(s -> decl_denotation);
			      if (in_memory) {
				fprintf(Voutfile, "\tmovl\t%s,r0\n", nt_tmp);
				POP_DISP("r0", s -> decl_sel_index,
					 "# store with list component");
			      } else {
				POP_DISP(nt_tmp, s -> decl_sel_index,
					 "# store with list component");
			      }
			    });
			  Vretreg(nt_tmp);
			}
			/* result is on top of the stack */
		    }
		    break;
		}

        case RECORDCONSTRUCTION:
                {
                    int n_components = length(p -> rec_component_list);
                    int i;

                    /* Allocate "environment" object for New, := and V */
                    /* This is a vector of these 3 functions for each  */
                    /* component.                                      */
                      if (3 * n_components > MAXOBJSZ) {
                          errmsg0(p, "Record too big\n");
                      }
                      putcomment ("# request pseudo-environment object");
                      FXD_NEWOBJ(3 * n_components);
                      CODE("\tclrl\t(r0)");
                    /* pointer to new type object is in r0   */
                    /* push it onto the stack                */
                      CODE("\tpushl\tr0");
                    /* evaluate type expressions in reverse order */
                      maprlist(p -> rec_component_list, type_expr);
                    /* Fill in fields in "environment"       */
                      i = 0;  /* position in "environment" */
                      maplist(s, p -> rec_component_list, {
                        fprintf(Voutfile, "\tmovl\t(sp)+,r1");
                        putcomment("# get component type");
                        fprintf(Voutfile, "\tmovl\t%d(r1),(r0)+",
                                4 * (s -> re_assign_index));
                        putcomment("# component := operator");
                        fprintf(Voutfile, "\tmovl\t%d(r1),(r0)+",
                                4 * (s -> re_New_index));
                        putcomment("# component New operator");
                        fprintf(Voutfile, "\tmovl\t%d(r1),(r0)+",
                                4 * (s -> re_ValueOf_index));
                        putcomment("# component ValueOf operator");
                      });
                    /* Only a pointer to the pseudo-env object is  */
                    /* on the stack.  Proceed as with other constr */
                }

	case PRODCONSTRUCTION :
	case UNIONCONSTRUCTION :
	case ENUMERATION:
		{
		    NODE * clist = p -> signature -> ts_clist;
                    int len = tsig_length(p -> signature);

		    if (len > MAXOBJSZ) {
                        errmsg0(p, "Constructed type too big");
		    }
		    /* allocate an object of the right size */
                      putcomment ("# request new type object");
                      FXD_NEWOBJ(len);
		    /* pointer to new type object is in r0   */
		    /* Push it onto the stack and copy to r2 */
		      CODE("\tpushl\tr0");
		      CODE("\tmovl\tr0,r2");
		      Ventry_mask |= R2;
		    /* Fill in individual fields. r2 points to */
		    /* next field to be filled in.             */
		      /* First take care of 1 character constants in enumerations */
			{                              
			  NODE * dcs = first(clist); 
					   
			  ASSERT(dcs -> kind == DEFCHARSIGS,
				 "codegen: type constr: bad DCS node\n");
			  if (dcs -> dcs_exceptions != NIL) {
			    maplist(s, dcs -> dcs_exceptions, {
			      gen_special(s -> dcse_special);
			      CODE("\tmovl\tr0,(r2)+");
			    });
			  }
			}

                      maplist(s, clist, {
			switch(s -> kind) {
			    case TSCOMPONENT:
			      gen_special(s -> tsc_signature -> fsig_special);
			      CODE("\tmovl\tr0,(r2)+");
			      break;
			  IFDEBUG(
			    case DEFCHARSIGS:
			      /* no constants */
			      break;
			    default:
			      dbgmsg("codegen: bad type constr. sig\n");
			  )
			}
                      });
                    if (p -> kind == RECORDCONSTRUCTION) {
                      /* pop pseudo-environment from under type */
                        fprintf(Voutfile,"\tmovl\t(sp)+,(sp)");
                        putcomment(" # remove pseudo-environment");
                    }
		    break;
                }

        case EXTENSION :
		{
                    int len = tsig_length(p -> signature);

		    if (len > MAXOBJSZ) {
                        errmsg0(p, "Constructed type too big");
		    }
		    /* allocate an object of the right size */
                      putcomment ("# request new extension type object");
                      FXD_NEWOBJ(len);
                      CODE("\tclrl\t(r0)");
		    /* pointer to new type object is in r0   */
                    /* Push it.                              */
                      CODE("\tpushl\tr0");
                    /* put "argument" value into r3 */
                      Vexpression(p -> ext_denotation);
                      fprintf(Voutfile, "\tmovl\t(sp)+,r3");
                      putcomment("# Extension argument");
                      Ventry_mask |= R3;
                      Vgc_mask |= R3;
                    /* Copy new type object form top of stack into r2 */
                      CODE("\tmovl\t(sp),r2");
                      Ventry_mask |= R2;
                    /* put identity function value in r0       */
                      gen_special(special(IDENTITY, 0));
		    /* Fill in individual fields. r2 points to */
                    /* next field to be filled in.             */
                    /* r3 points to next unused field in arg   */
                      Vgc_mask &= ~R3; /* no allocation before its discarded */
                      ASSERT(p -> In_index < p -> Out_index,
                             "Vexpression: bad In, Out indicees\n");
                      copy_r3_to_r2(p -> In_index);
                      fprintf(Voutfile, "\tmovl\tr0,(r2)+");
                      putcomment("# In function");
                      copy_r3_to_r2(p->Out_index - p->In_index - 1);
                      fprintf(Voutfile, "\tmovl\tr0,(r2)+");
                      putcomment("# Out function");
                      copy_r3_to_r2(len - p->Out_index - 1);
                    /* new function value is left on the stack */
                    break;
                }


        default :
            findvl( p -> vlineno );

            dbgmsg( "Vexpression: Unimplemented construct (kind = %s) in file %s at line %d\n",
                    kindname(p->kind), getname(getfn()), getrl() );
            dbgmsg( "Vexpression:  p is 0x%x\n",p);
            fprintf( Voutfile, "?" );
	    fflush (Voutfile);
            abort();
    }
}


/* Compute the function value associated with the given special value */
/* Leave the result in r0.  Also affects r1 and r10.                  */
/* If a nontrivial pseudo-environment is needed, it is presumed to be */
/* immediately below the top of the stack.                            */
gen_special(spcl)
unsigned spcl;
{
    char * routine_name;  /* name of routine for each operation */
    int n_args;           /* number of arguments to routine     */
    boolean ep_on_stack = FALSE;  /* pseudo-env to be obtained from stack */

    /* Find routine name and n_args */
	switch(special_tp(spcl)) {
	    case PROD_PROJ:
	    case RECORD_VAL_FIELD:
	    case RECORD_VAR_FIELD:
		routine_name = "_P_R_ith";
		n_args = 1;
		break;

            case RECORD_MK:
	    case PROD_MK:
                routine_name = "_P_R_Make";
		n_args = special_val(spcl);
		break;

	    case PROD_NEW:
	    case UNION_NEW:
		routine_name = "_P_U_New";
		n_args = 0;
                break;

            case RECORD_NEW:
                routine_name = "_Record_New";
                n_args = 0;
                ep_on_stack = TRUE;
                break;

	    case ENUM_NEW:
		routine_name = "_E_New";
		n_args = 0;
		break;

	    case PROD_ASSIGN:
	    case UNION_ASSIGN:
            case ENUM_ASSIGN:
		routine_name = "_P_U_E_Assign";
		n_args = 2;
                break;

            case RECORD_ASSIGN:
                routine_name = "_Record_Assign";
                n_args = 2;
                ep_on_stack = TRUE;
                break;
    
	    case PROD_VALUEOF:
	    case UNION_VALUEOF:
	    case ENUM_VALUEOF:
		routine_name = "_P_U_E_ValueOf";
		n_args = 1;
		break;
                      
            case RECORD_VALUEOF:
                routine_name = "_Record_ValueOf";
                n_args = 1;
                ep_on_stack = TRUE;
                break;

	    case UNION_PROJ:
		routine_name = "_Union_Proj";
		n_args = 1;
		break;
    
	    case UNION_INJ:
		routine_name = "_Union_Inj";
		n_args = 1;
		break;

	    case UNION_INQ:
		routine_name = "_Union_Inq";
		n_args = 1;
		break;

	    case ENUM_EQ:
		routine_name = "_Enum_eq";
		n_args = 2;
		break;

	    case ENUM_NE:
		routine_name = "_Enum_ne";
		n_args = 2;
		break;

	    case ENUM_ELEMENT:
		routine_name = "_Enum_Element";
		n_args = 0;
		break;

	    case ENUM_CARD:
		routine_name = "_Enum_Card";
		n_args = 0;
		break;

	    case ENUM_PRED:
		routine_name = "_Enum_Pred";
		n_args = 1;
		break;

	    case ENUM_SUCC:
		routine_name = "_Enum_Succ";
		n_args = 1;
		break;

	    case IDENTITY:
		routine_name = "_Identity";
		n_args = 1;
		break;

#         ifdef DEBUG
	    default:
                dbgmsg("gen_special: Unknown special function\n");
#         endif
	}
    /* allocate new function object */
        putcomment ("# request new function object");
        FXD_NEWOBJ(FO_OBJ_SIZE);
        if (!ep_on_stack) {
          /* Use special value as ep */
            fprintf(Voutfile,"\tmovl\t$%d,%d(r0)",special_val(spcl), FO_EP);
        } else {
            fprintf(Voutfile,"\tmovl\t4(sp),%d(r0)", FO_EP);
        }
	putcomment("# dummy environment pointer");
    /* Set up ip */
	fprintf(Voutfile,"\t.globl\t%s\n", routine_name);
	fprintf(Voutfile,"\tmovab\t%s,%d(r0)",routine_name, FO_IP);
	putcomment("# instruction pointer");
    /* set activation record size to number of arguments plus 1 */
	fprintf(Voutfile,"\tmovzwl\t$%d,%d(r0)", n_args + 1, FO_SIZE);
	putcomment("# size of activation record");
}


/* Copy copycnt longwords from the source pointed to by r0 to the    */
/* destination pointed to by r10.  r0 and r10 point to the addresses */
/* past the last word moved after the operation is finished.         */
copy_r10_to_r0(copycnt)
int copycnt;
{
# ifdef EXTENDED_RANGE
    while (copycnt >= 4) {
	CODE("\tmovo\t(r10)+,(r0)+");
	copycnt -= 4;
    }
# endif
    while (copycnt >= 2) {
	CODE("\tmovq\t(r10)+,(r0)+");
	copycnt -= 2;
    }
    while (copycnt >= 1) {
	CODE("\tmovl\t(r10)+,(r0)+");
	copycnt -= 1;
    }
}

/* Same as above, but r3 to r2 */
copy_r3_to_r2(copycnt)
int copycnt;
{
# ifdef EXTENDED_RANGE
    while (copycnt >= 4) {
        CODE("\tmovo\t(r3)+,(r2)+");
	copycnt -= 4;
    }
# endif
    while (copycnt >= 2) {
        CODE("\tmovq\t(r3)+,(r2)+");
	copycnt -= 2;
    }
    while (copycnt >= 1) {
        CODE("\tmovl\t(r3)+,(r2)+");
	copycnt -= 1;
    }
}

/* Convert the special function descriptor from function signature to */
/* inline code.							      */
/* Clobbers str_code_buf					      */
char * Vspcl_to_inline(spcl)
unsigned spcl;
{
#   define MAX_PROD_EXP_LEN 5
    int tp = special_tp(spcl);
    int val = special_val(spcl);
    int i;
    char * result;

    switch(tp) {
	case PROD_NEW:
	case UNION_NEW:
	    sprintf(str_code_buf, "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0x%X,(r0)\n\tpushl\tr0", UNDEF);
	    break;
        case ENUM_NEW:
	    sprintf(str_code_buf, "\tmovl\t_objfreelist+4,r0\n\tjneq\t1f\n\tmovl\t$0x%%X,r11\n\tpushl\t$1\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+4\n\tmovl\t$0,(r0)\n\tpushl\tr0", 0);
	    break;
	case PROD_ASSIGN:
	case UNION_ASSIGN:
	case ENUM_ASSIGN:
	    return("\tmovl\t4(sp),*(sp)+");
	case PROD_VALUEOF:
	case UNION_VALUEOF:
	case ENUM_VALUEOF:
	    return("\tmovl\t*(sp),(sp)");
	case PROD_MK:
	    if (val > MAX_PROD_EXP_LEN) return(NIL);
	    sprintf(str_code_buf, "\tmovl\t_objfreelist+%d,r0\n\tjneq\t1f\n\tpushl\t$%d\n\tmovl\t$0x%%X,r11\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+%d\n\tmovl\tr0,r1", 4*val, val, 4*val);
	    for(i = 0; i < val; i++) {
		strcat(str_code_buf, "\n\tmovl\t(sp)+,(r1)+");
	    }
	    strcat(str_code_buf, "\n\tpushl\tr0");
	    break;
        case PROD_PROJ:
        case RECORD_VAL_FIELD:
        case RECORD_VAR_FIELD:
	    if (val == 0) {
	        return("\tmovl\t*(sp),(sp)");
	    } else {
		sprintf(str_code_buf, "\tmovl\t(sp),r0\n\tmovl\t%d(r0),(sp)",
                                      4*val);
	    }
	    break;
	case UNION_INJ:
	    sprintf(str_code_buf, "\tmovl\t_objfreelist+8,r0\n\tjneq\t1f\n\tpushl\t$2\n\tmovl\t$0x%%X,r11\n\tcalls\t$1,_allocobj\n1:\tmovl\t(r0),_objfreelist+8\n\tmovl\t$%d,4(r0)\n\tmovl\t(sp),(r0)\n\tmovl\tr0,(sp)", val);
	    break;
	case UNION_INQ:
	    sprintf(str_code_buf, "\tmovl\t(sp)+,r0;\tcmpl\t4(r0),$%d #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)", val);
	    break;
	case UNION_PROJ:
	    sprintf(str_code_buf, "\tmovl\t(sp),r0 #---\n\tcmpl\t$%d,4(r0) #---\n\tbeql\t1f #---\n\t.globl\t_union_err #---\n\tcalls\t$0,_union_err #---\n1:\tmovl\t*(sp),(sp)",val);
	    break;
	case ENUM_EQ:
	    sprintf(str_code_buf, "\tcmpl\t(sp)+,(sp)+ #COMP jneq 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\t$0xfffffffe,r0,-(sp)", val);
	    break;
	case ENUM_NE:
	    sprintf(str_code_buf, "\tcmpl\t(sp)+,(sp)+ #COMP jeql 4\n\tmovpsl\tr0\n\trotl\t$-2,r0,r0\n\tbicl3\tr0,$0x1,-(sp)", val);
	    break;
	case ENUM_CARD:
	case ENUM_ELEMENT:
	    sprintf(str_code_buf, "\tpushl\t$%d", val);
	    break;
	case IDENTITY:
            return("# application of In, Out, Ord or OrdInv");
	case ENUM_PRED:
	    return("\tdecl\t(sp)\n\tbgeq\t1f #---\n\t.globl\t_pred_error #---\n\tcalls\t$0,_pred_error #---\n1:\t\t #---");
	case ENUM_SUCC:
	    sprintf(str_code_buf, "\tincl\t(sp)\n\tcmpl\t(sp),$%d #---\n\tbleq\t1f #---\n\t.globl\t_succ_error #---\n\tcalls\t$0,_succ_error #---\n1:\t\t #---",val);
	    break;
	default:
	    return(NIL);
    }
    /* code is in str_code_buf */
    result = (char *) malloc(strlen(str_code_buf) + 1);
    strcpy(result, str_code_buf);
    return(result);
}

/* Return TRUE iff the argument has "r11" as a substring */
boolean mentions_r11(string)
char *string;
{
    register char * s;

    s = string;
    while (*s != '\0') {
	if (*s++ == 'r') {
	    if (*s == '1') {
		s++;
		if (*s == '1') {
		    return(TRUE);
		}
	    }
	}
    }
    return(FALSE);
}

/* generate code to push the value of the type expression in a record */
/* component.                                                         */
void type_expr(re)
NODE * re;
{
  Vexpression(re -> re_denotation);
}
