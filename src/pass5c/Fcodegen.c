# define DEBUG
/* 
 *  This is a highly simplified code generator.  It works only
 * for functions referring only to  local or level 0 identifiers,
 * containing no type modifications, declarations, loops,
 * type constructions, or function constructions, and
 * containing only applications of functions which can be
 * expanded inline, or are just as simple.
 *  The generated code uses a JSB calling sequence.  No registers
 * other than r0 and r1 are used.
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codeutil.h"
# include "pass4/sigs.h"
# include "../runtime/runtime.h"

extern int yydebug;
extern int yynerrs;
extern boolean Pflag;  /* generate profiling code */
extern boolean Tflag;  /* generate trace code */

extern long int_const_val;

char str_code_buf[MAXSTRCODELEN]; /* used for strings, "special" fns */
				  /* and by find_inline		     */

NODE * Vcurrent;
FILE * Voutfile;

int SP_offset;  /* current offset of sp from the return address in long words */

/*
 * Generate code for the body of the function construction p.
 */
Ffuncbody(p)
NODE * p;
{
/* 
 *	Enter scope of new function construction
 */
	
        fprintf(Voutfile,"\t.globl\tF%s\n",p -> fc_code_label);
        CODE ("\t.align  1");
	fprintf(Voutfile, "F%s:\n",p -> fc_code_label);
        SP_offset = 0;

        if (Pflag) {
          /* generate profiling code */
            Vcall_mcount();
        }

        if (Tflag) {
          /* Generate calls to entry trace routines */
            Ventry_trace(p -> fc_code_label,
                         p -> signature -> fsig_param_list, TRUE);
        }

    /* recursively descend */
	Fexpression( p -> fc_body );

    /* Generate the epilog:  r0 <- top of stack */
        if (Tflag) {
          /* Generate call to exit trace routine */
            Vexit_trace();
        }
	POP ("r0","# function value to r0");
	fprintf(Voutfile,"\trsb\n");
}

/*
 * Generate code for the expression tree headed by p.
 */
Fexpression (p)
register NODE * p;
{
    int i;
    switch ( p -> kind ) {

        case OPRID :
        case LETTERID :
		{
		    register NODE * v;
		    char * display_reg; /* name of reg used for a.r. pointer */
		    char * r10 = "r10";
		    
		    v = p -> id_last_definition;
		    if (p -> sel_type == NIL) {
			ASSERT2 (v != NIL, "Fexpression: id %s not declared\n", 
				 getname(p -> id_str_table_index)
			);
			ASSERT2 (v -> kind == DECLARATION 
			     || v -> kind == PARAMETER
			     || v -> kind == MODPRIMARY
				&& v -> mp_type_modifier -> kind == WITHLIST,
			      "Fexpression: id %x not declaration or parameter\n",v
			);
			putcomment1("\t\t\t# Identifier %s",
				     getname(p -> id_str_table_index));
			if (is_int_const(p)) {
			    fprintf(Voutfile, "\tmovzwl\t$%d,-(sp)\n",
                                              (short) int_const_val);
			} else if (v -> level == 0) {
			    PUSH_DISP (L0fp, v->displacement,
					      "# referenced object");
			} else {
			    fprintf(Voutfile,"\tpushl\t%d(sp)\n",
					     4*(SP_offset + v->displacement));
			}
			SP_offset++;
		    }
		    else {
			putcomment1("\t\t\t# Selection of %s",
				     getname(p->id_str_table_index));
			Fexpression (p -> sel_type);
			POP ("r0","# type value to r0");
			PUSH_DISP ("r0",p -> sel_index,
				"# push selected function value");
		    }
		    break;
		}

	case QSTR:
	case UQSTR:
		{
		    NODE * sig = p -> sel_type -> signature;

		    ASSERT(sig -> kind == TYPESIGNATURE,
			   "codegen: bad string type\n");
		    if (sig -> ts_string_code != NIL 
			&& sig -> ts_element_code != NIL
			&& strlen(p -> str_string) <= MAXSTRLEN) {
			/* build body of in-line expansion */
			  char *r = p -> str_string;
			  char *q = str_code_buf;
			  while (*r != '\0') {
			    sprintf(q, sig -> ts_element_code, *r);
			    /* position q at trailing 0 */
			      q += strlen(q);
			    r++;
			  }
			fprintf(Voutfile, sig -> ts_string_code, str_code_buf);
			fprintf(Voutfile, "\n");
			SP_offset++;
		    } else {
			dbgmsg("Fexpression: bad string\n");
		    }
		}
		break;
		
        case APPLICATION :
		{
		    char * in_line;     /* in-line code, NIL if not known */
		    NODE * construction  =  p -> ap_operator -> signature
					      -> fsig_construction;
		    NODE * op_sig = p -> ap_operator -> signature;


		    /* determine type of calling sequence */
		      in_line = op_sig -> fsig_inline_code;
		    /* push arguments in reverse order */
		      maprlist(p -> ap_args, Fexpression);
		    if (in_line != NIL) {
		      fprintf(Voutfile, in_line, 0,0);
		      fprintf(Voutfile, "\n");
		    } else {
		      int l = length(p -> ap_args);
		      /* Construction known and we can use jsb calling seq */
		      fprintf(Voutfile, "\tjsb\tF%s\n",
                                        construction -> fc_code_label);
		      /* pop arguments */
			if (l > 0) {
			  fprintf(Voutfile,"\taddl2\t$%d,sp\n", 4*l);
			}
		      fprintf(Voutfile, "\tpushl\tr0\t# push return value\n");
		    }
		    SP_offset -= (length(p -> ap_args) - 1);
		    break;
		}

        case BLOCKDENOTATION :
		{
		    /* Declaration list is guaranteed to be empty */
		    /* see get_complexity.			  */
		    maplist (v,p->bld_den_seq, {
			Fexpression(v);
			if (v != last(p -> bld_den_seq)) {
			    POP ("r0","# trash value");
			    SP_offset--;
			}
		    });
		    break;
		}
		
        case GUARDEDLIST :
		{
		    char * L0;
		    register NODE * v;
		    int save_SP_offset = SP_offset;

		    L0=Vnewlabel("guard_exit");
		    maplist (v,p->gl_list, {
			char * L1;
			
			ASSERT (v->kind == GUARDEDELEMENT,
					"Fcodegen.c: bad guard list");
			SP_offset = save_SP_offset;
			Fexpression(v->ge_guard);
			L1 = Vnewlabel ("guard");
			POP ("r0","# value of guard");
			SP_offset--;
			fprintf(Voutfile,"\tjeql\t%s", L1);
			putcomment ("# branch on false");
			Fexpression(v->ge_element);
 			fprintf(Voutfile,"\tjbr\t%s",L0);
			putcomment ("# leave guarded list");
			fprintf(Voutfile,"%s:",L1);
			putcomment ("# next case");
		    });
		    fprintf(Voutfile,"\tcalls\t$0,_cond_error\n");
		    fprintf (Voutfile,"%s:\n",L0);
		    break;
		}

	case WORDELSE :
		{
		    PUSH ("$1","# Else = constant 1");
		    SP_offset++;
		    break;
		}
		

	case USELIST :
		{
		    maplist (v,p->usl_den_seq, {
			Fexpression(v);
			if (v != last(p -> usl_den_seq)) {
			  POP ("r0","# trash value");
			  SP_offset--;
			}
		    });
		    break;
		}

#	ifdef DEBUG
          default :
	    dbgmsg("Fcodegen: bad kind\n");
            abort();
#       endif
    }
}
