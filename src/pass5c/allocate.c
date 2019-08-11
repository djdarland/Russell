#define DEBUG
/*
 *	Static Storage Allocator.
 *
 *	This routine walks the tree, associating with declarations and 
 *  parameters a level and displacement.  It also calculates the size of
 *  the activation record needed for a procedure.  
 *      Allocate expects a node of kind FUNCCONSTR.     
 *      Note that static nesting levels are reset here, to reflect the
 *  fact that some blocks actually require activation records.
 */

# ifdef VERBOSE
#   define IFVERBOSE(x) x
# else
#   define IFVERBOSE(x)
# endif

# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "codeutil.h"
# include "../pass4/sigs.h"

# define MAXOBJSZ 512 /* Should agree with runtime */

extern FILE * unparse_file;

NODE * Vcurrent_func;       /* Immediately surrounding function construction */
NODE * Vcurrent_ar;         /* Immediately surrounding construct (block */
			    /* or function) requiring an act. record.   */

int Vstatic_level = -1;
int Vnext_free;             /* Next location in activation record to be */
			    /* allocated.                               */
int Vhigh_water;            /* max(Vnext_free, Vhigh_water) = highest   */
			    /* numbered location allocated so far.  At  */
			    /* the end of a fn this is the required a.r.*/
			    /* size.                                    */

boolean Vextent_limited;     /* Variables can't outlive current function  */
			     /* activation & current block is executed    */
			     /* only once in current fn activation.       */
boolean Vallocate_on_stack;  /* It is safe to allocate variables directly */
			     /* in the activation record in the current   */
			     /* context.                                  */
			     /* This implies Vextent_limited and callcc   */
			     /* can't be called.                          */
			     /* On the VAX it also means that a.r. must   */
			     /* always be stack allocated, since pointers */
			     /* to the middle of objects may o.w. result  */
			     /* Even if this is false, individual vars    */
			     /* may be allocated directly in a.r. if no   */
			     /* references to that particular             */
			     /* variable can be created.                  */
boolean Vcallcc;             /* Current function may save a continuation  */

extern boolean Nflag; /* No Callcc calls */
extern boolean Vflag;
extern boolean Gflag;
extern boolean Rflag; /* Use registers as much as possible. */
extern boolean hflag; /* Don't stack allocate variables.    */

extern long max_addr_regs; /* Try to allocate this many non-parameter */
			   /* represented by pointers to registers    */
extern long max_int_regs;  /* Number of non-pointers to allocate to regs */
long n_addr_regs = 0;      /* Number of virtual registers allocated to */
long n_int_regs = 0;       /* variables so far.                        */
			   /* Note that the distinction between int and  */
			   /* addr is very approximate.  All initialized */
			   /* variables, as well as all non-variables    */
			   /* count as ints.                             */

extern int yynerrs;

int n_decls;        /* Number of declarations in current block */

/* List of blocks in current function */
struct blocks {
    struct blocks * bl_next;
    NODE * bl_block;
} * Vcurrent_blocks = NIL;

/* Free nodes on current block list */
void Vfree_blocks()
{
    struct blocks *p;

    while (Vcurrent_blocks != NIL) {
	p = Vcurrent_blocks;
	Vcurrent_blocks = Vcurrent_blocks -> bl_next;
	free(p);
    }
}

/* Make sure variables declared in blocks parallel to block     */
/* are not allocated directly in a.r., since their locations    */
/* are no longer guaranteed to be retained until function exit. */
void realloc_blocks(block)
NODE * block;
{
    struct blocks *p;

    /* Process preceding parallel blocks. */
      for(p = Vcurrent_blocks; p != NIL; p = p -> bl_next) {
	/* Blocks are added in preorder.                     */
	/* Thus p -> bl_block -> pre_num < block -> pre_num  */
	if (p -> bl_block -> post_num < block -> post_num /* not ancestor */) {
	    switch (p -> bl_block -> kind) {
		case BLOCKDENOTATION:
		    maplist(s, p -> bl_block -> bld_declaration_list, {
		      if (s -> decl_special & VAR_NONTR_REF) {
			if (Vflag && (s -> decl_special & VAR_ON_STACK)) {
			    printf("Parallel blocks: Undoing stack allocation of %s:%s\n",
				   Vcurrent_func -> fc_code_label,
				   getname(s -> decl_id -> id_str_table_index));
			}
			s -> decl_special &= ~VAR_ON_STACK;
		      }
                    });
                    break;
		default:
		    dbgmsg("realloc_blocks: bad list entry\n");
	    }
	}
      }
}

/* Add a block to block list */
void add_block(block)
NODE * block;
{
    struct blocks *p;

    if (block -> kind == MODPRIMARY) {
	dbgmsg("add_block: MODPRIMARY in block list\n");
	return;
    }
    if (block -> kind == BLOCKDENOTATION
	&& (block -> bld_declaration_list == NIL
	    || is_empty(block -> bld_declaration_list))) {
	/* No local declarations - ignore */
	return;
    }
    /* Add block to list */
      p = (struct blocks *) malloc(sizeof (struct blocks));
      p -> bl_block = block;
      p -> bl_next = Vcurrent_blocks;
      Vcurrent_blocks = p;
}

Vallocwalk(p)
register NODE * p;
{
register NODE * v;
int tp;

    if (p == NIL) return;

    /* Take care of nonexpression nodes first: */
    switch (p -> kind) {
	case DECLARATION:
		{
		    /* level field has been set by import to be function */
		    /* nesting level.  It has been used by "accessible". */
		    /* It is reset here to a.r. nesting depth.           */
		    NODE * den = p -> decl_denotation;
		    boolean no_ref = (p -> decl_signature -> kind
				      == VARSIGNATURE
				      && !(p -> decl_special & VAR_NONTR_REF));
		    boolean allocate_in_ar =
		       (Vallocate_on_stack || (no_ref && !Vcallcc))
			&& den -> kind == APPLICATION
			&& ((tp = special_tp(den -> ap_operator -> signature
						  -> fsig_special))
			     == STD_NEW
			    || tp == PROD_NEW || tp == UNION_NEW
			    || tp == PTR_NEW  || tp == INIT_NEW  );
		    boolean allocate_in_reg =
			   allocate_in_ar
			   && (Rflag
			       || ((tp == STD_NEW || tp == INIT_NEW) ?
				   (n_int_regs < max_int_regs)
				   : (n_addr_regs < max_addr_regs)))
			    && !(p -> decl_special & (ID_IMPORTED
						      | VAR_NONTR_REF))
			    && (special_val(den -> ap_operator
					       -> signature -> fsig_special)
				== 1
				|| tp == PROD_NEW || tp == UNION_NEW);

		    if (hflag || allocate_in_reg) {
			allocate_in_ar = FALSE;
		    }
		    p -> level = Vstatic_level;
		    if (allocate_in_reg) {
			  /* Note: Both Gflag and !Vcallcc hold at this   */
			  /* point. Store variable value in register.     */
			    extern int avail_loc;

			    if (Vflag) {
			      printf("Allocating value of %s:%s to v. register\n",
				     Vcurrent_func -> fc_code_label,
				     getname(p -> decl_id -> id_str_table_index));
			    }
			    p -> displacement = avail_loc++;
			    if (tp == STD_NEW) {
				p -> decl_special |= SIMPLE_VAR_IN_REG;
				n_int_regs++;
			    } else if (tp == INIT_NEW) {
				p -> decl_special |= INIT_VAR_IN_REG;
				n_int_regs++;
			    } else {
				p -> decl_special |= PTR_VAR_IN_REG;
				n_addr_regs++;
			    }
		    } else if (allocate_in_ar) {
			  /* Allocate variable directly in activation record */
			    if (Vflag) {
			      printf("Allocating %s:%s directly in act. record\n",
				     Vcurrent_func -> fc_code_label,
				     getname(p -> decl_id -> id_str_table_index));
			    }
			    p -> displacement = Vnext_free;
			    Vnext_free +=
				 special_val(den -> ap_operator
						 -> signature -> fsig_special);
			    if (tp == STD_NEW) {
				p -> decl_special |= SIMPLE_VAR_ON_STACK;
			    } else if (tp == INIT_NEW) {
				p -> decl_special |= INIT_VAR_ON_STACK;
			    } else {
				p -> decl_special |= PTR_VAR_ON_STACK;
			    }
		    } else {
			if (Vextent_limited
			    && den -> kind == APPLICATION
			    && ((tp = special_tp(den -> ap_operator -> signature
						     -> fsig_special))
			       == ARRAY_STD_NEW
			       || tp == ARRAY_PTR_NEW)) {
			    /* Allocate array contiguously */
			      if (Vflag && Gflag) {
				printf("Allocating %s:%s contiguously\n",
				       Vcurrent_func -> fc_code_label,
				       getname(p -> decl_id -> id_str_table_index));
			      }
			      p -> decl_special |= ARRAY_CONTIG;
			  }
			if (p -> decl_needed) {
			  /* If ((Rflag || messy, ad hoc heuristic) && ... */
			  if ((Rflag || (n_int_regs < max_int_regs)
					&& n_decls < max_int_regs + max_addr_regs
					&& !is_int_const(p -> decl_denotation))
			      && !(p -> decl_special & ID_IMPORTED)) {
			    extern int avail_loc;

			    if (Vflag) {
			      printf("Binding of %s:%s stored in v. register\n",
				     Vcurrent_func -> fc_code_label,
				     getname(p -> decl_id -> id_str_table_index));
			    }
			    p -> decl_special |= ID_IN_REG;
			    p -> displacement = avail_loc++;
			    n_int_regs++;
			  } else {
			    p -> displacement = Vnext_free++;
			  }
			}
		    }
		    if (p -> decl_denotation -> kind == FUNCCONSTR
			&& !(p -> decl_needed)) {
			Vallocate(p -> decl_denotation, TRUE);
		    } else {
			Vallocwalk(p -> decl_denotation);
		    }
		    return;
		}

	case PARAMETER:
		if (Vflag && p -> par_id != NIL
		    && is_real_def(p -> par_only_def)) {
		    printf( "Parameter %s:%s is known to be bound to: ",
			    Vcurrent_func -> fc_code_label,
			    getname(p -> par_id -> id_str_table_index));
		    unparse_file = stdout;
		    unparse(p -> par_only_def);
		    printf("\n");
		}
		p -> displacement = Vnext_free++;
		p -> level = Vstatic_level;
		return;

        case GUARDEDELEMENT:
		Vallocwalk(p->ge_guard);
		Vallocwalk(p->ge_element);
		return;

	case EXTERNDEF:
		return;
    }

    if (p -> signature -> kind == SIGNATURESIG) {
	/* signatures don't require storage for evaluation */
	return;
    }

    switch ( p -> kind ) {

	case BLOCKDENOTATION:
		{
		    long old_int_regs = n_int_regs;
		    long old_addr_regs = n_addr_regs;
		    int old_n_decls = n_decls;
		    boolean escaping_env_ptrs
		      = (p->bld_flags & (CONTAINS_CLOSURE | CALLCC_CALL)) != 0;
		    boolean needs_ar = (p -> bld_flags & REQUIRES_AR) != 0;
		    boolean old_ccc = Vcallcc;
		    boolean old_aos;
		    boolean old_el;
		    struct blocks * old_Vcurrent_blocks;
		    NODE * old_ar;
		    int old_Vnext_free = Vnext_free;
		    int old_Vhigh_water = Vhigh_water;

		    if (p -> bld_declaration_list != NIL) {
			n_decls = length(p -> bld_declaration_list);
		    } else {
			n_decls = 0;
		    }
		    Vcallcc = (p -> bld_flags & CALLCC_CALL) != 0;
		    if (needs_ar) {
			old_aos = Vallocate_on_stack;
			old_el = Vextent_limited;
			old_Vcurrent_blocks = Vcurrent_blocks;
			old_ar = Vcurrent_ar;
			Vcurrent_ar = p;
			Vcurrent_func -> fc_complexity |= NESTED_AR_BLOCK;
			Vextent_limited = (p -> signature -> kind
					   == TYPESIGNATURE);
			/* In this case, the import rule guarantees the */
			/* desired property.                            */
			Vallocate_on_stack = Vextent_limited && !Vcallcc;
			Vcurrent_blocks = NIL;
			Vhigh_water = Vnext_free = AR_FIRST_PARM;
			Vstatic_level++;
		    } else {
		      /* If there is no potential for a pointer to the */
		      /* environment surviving block exit, we recycle  */
		      /* locations at the end of the block.  This      */
		      /* requires that potentially surviving variables */
		      /* (references) may not be allocated directly    */
		      /* in the activation record if there is a        */
		      /* subsequent parallel block.  Here we undo such */
		      /* allocations for preceding blocks.             */
			realloc_blocks(p);
			if (!escaping_env_ptrs) {
			  /* Remember this block for possible future fix-up */
			    add_block(p);
			}
		    }

		    maplist(v,p->bld_declaration_list,Vallocwalk(v));
		    maplist(v,p->bld_den_seq,Vallocwalk(v));
		    if (needs_ar) {
			Vallocate_on_stack = old_aos;
			Vextent_limited = old_el;
			Vcurrent_blocks = old_Vcurrent_blocks;
			Vcallcc = old_ccc;
			if (Vnext_free >= Vhigh_water) {
			  p -> ar_size = Vnext_free;
			} else {
			  p -> ar_size = Vhigh_water;
			}
			if (p -> ar_size > MAXOBJSZ) {
			    errmsg0(p, "Too many local variables");
			}
			p -> ar_static_level = Vstatic_level;
			p -> ar_static_link = old_ar;
			Vnext_free = old_Vnext_free;
			Vhigh_water = old_Vhigh_water;
			Vcurrent_ar = old_ar;
			Vstatic_level--;
		    } else if (!escaping_env_ptrs) {
			if (Vnext_free > Vhigh_water) {
			    Vhigh_water = Vnext_free;
			}
			Vnext_free = old_Vnext_free;    
		    }
		    n_int_regs = old_int_regs;
		    n_addr_regs = old_addr_regs;
		    n_decls = old_n_decls;
		    break;
		}

	case APPLICATION:
		{
		    Vallocwalk(p->ap_operator);
		    maplist(v,p->ap_args,Vallocwalk(v));
		    break;
		}

	case LOOPDENOTATION:
		{
		    boolean old_aos = Vallocate_on_stack;
		    boolean old_el = Vextent_limited;
		    
		    Vallocate_on_stack = FALSE;
		    Vextent_limited = FALSE;
		    maplist(v,p->gl_list,Vallocwalk(v));
		    Vallocate_on_stack = old_aos;
		    Vextent_limited = old_el;
		    break;
                }

        case QSTR:
        case UQSTR:
                {
                    /* Selection type is handled as though it were */
                    /* in a loop, since it may be evaluated more   */
                    /* than once.                                  */
                    boolean old_aos = Vallocate_on_stack;
                    boolean old_el = Vextent_limited;

                    Vallocate_on_stack = FALSE;
                    Vextent_limited = FALSE;
                    Vallocwalk(p -> sel_type);
                    Vallocate_on_stack = old_aos;
                    Vextent_limited = old_el;
                    break;
                }

        case GUARDEDLIST:
		maplist(v,p->gl_list,Vallocwalk(v));
		break;

        case OPRID:
        case LETTERID:
		if (p -> sel_type != NIL) {
		    Vallocwalk(p->sel_type);
		}
		break;

	case FUNCCONSTR:
		{
		    Vallocate (p, FALSE);
		    break;
		}

        case REXTERNDEF:
                break;

	case USELIST:
		maplist(q, p -> usl_den_seq, Vallocwalk(q));
		break;

	case MODPRIMARY:
		if (p -> mp_type_modifier != NIL
		    && p -> mp_type_modifier -> kind == WITHLIST) {
		   /* Treat it as a block since it can overlay */
		   /* previously allocated locations           */
		       realloc_blocks(p);
		}
		Vallocwalk(p -> mp_primary);
		if (p -> mp_type_modifier != NIL
		    && p -> mp_type_modifier -> kind == WITHLIST) {
		    /* reserve space for local type identifier */
			p -> displacement = Vnext_free++;
			p -> level = Vstatic_level;
		    maplist (q, p -> mp_type_modifier -> wl_component_list, {
			Vallocwalk(q -> decl_denotation);
		    });
		}
		break;

	case ENUMERATION:
	case PRODCONSTRUCTION:
        case UNIONCONSTRUCTION:
		/* Subexpressions are not evaluated, thus */
		/* no storage needs to be allocated.      */
                break;

        case EXTENSION:
                Vallocwalk(p -> ext_denotation);
                break;

        case RECORDCONSTRUCTION:
                maplist(s, p -> rec_component_list, {
                  Vallocwalk(s -> re_denotation);
                });
                break;

	case WORDELSE:
		break;

        case WORDCAND:
	case WORDCOR:
		dbgmsg("Vallocate: cand or cor\n");
		break;

        case RECORDELEMENT:
        case FUNCSIGNATURE:
	case LISTHEADER: /* should never get here */
	case VARSIGNATURE:
	case VALSIGNATURE:
	case TYPESIGNATURE:
	case TSCOMPONENT:
        case DEFCHARSIGS:
	case WITHLIST:
        case EXPORTLIST:
        case EXPORTELEMENT:
        case ALLCONSTANTS:
	case HIDELIST:
	case PARAMETER:
	case DECLARATION:
	case GUARDEDELEMENT:
	default:
		dbgmsg("Vallocwalk: bad kind\n");
		break;
    };
    return;
}

/* Compute the number of known vacuous parameters appearing at the */
/* end of a a parameter list.                                      */
static boolean found_non_vacuous; /* Saw a real argument */
static n_vacuous;                 /* Number of vacuous arguments found so far */

static void check_vacuous(p)
NODE * p;
{
    if (!found_non_vacuous) {
	if (vacuous_arg(p -> par_signature)) {
	    n_vacuous++;
	} else {
	    found_non_vacuous = TRUE;
	}
    }
}

int n_vacuous_params(p)
NODE *p;    /* parameter list */
{
    if (!Gflag) return(0);
		 /* Vax implementation passes everything explicitly */
    found_non_vacuous = FALSE;
    n_vacuous = 0;
    maprlist(p, check_vacuous);
    return(n_vacuous);
}

Vallocate (p, unused_decl)
register NODE * p;
boolean unused_decl;  /* Function object is never needed explicitly */
{
	register NODE * v;
	boolean old_aos = Vallocate_on_stack;
	boolean old_el = Vextent_limited;
	boolean old_ccc = Vcallcc;
	long old_addr_regs = n_addr_regs;
	long old_int_regs = n_int_regs;
	struct blocks * old_Vcurrent_blocks = Vcurrent_blocks;
	NODE * old_func = Vcurrent_func;
	NODE * old_ar = Vcurrent_ar;
	int old_Vnext_free = Vnext_free;
	int old_Vhigh_water = Vhigh_water;
	NODE * op_sig = p -> signature;
	NODE * result_sig = p -> signature -> fsig_result_sig;

	ASSERT (p->kind == FUNCCONSTR,"Vallocate.c: arg not FUNCCONSTR\n");
	if (p->kind != FUNCCONSTR) {dbgmsg ("p is %x\n",p);};
	Vstatic_level++;
	Vcurrent_func = Vcurrent_ar = p;
	Vcurrent_blocks = NIL;
	Vhigh_water = Vnext_free = AR_FIRST_PARM;
	n_int_regs = n_addr_regs = 0;
	/* Determine whether stack allocation of variables is safe */
	  {
	      if (impure(op_sig)) {
		  /* Who knows what it does? */
		  Vextent_limited = FALSE;
#                 ifdef VERBOSE
		    printf("Vallocate: impure operator signature\n");
#                 endif
	      } else if (result_sig -> kind == VARSIGNATURE) {
		  /* Can obviously return a variable */
		  Vextent_limited = FALSE;
#                 ifdef VERBOSE
		    printf("Vallocate: variable result signature\n");
#                 endif
	      } else if (result_sig -> kind == VALSIGNATURE &&
			 !result_sig -> val_denotation -> signature
				     -> ts_simple_type) {
		  ASSERT (has_sig(result_sig -> val_denotation),
			  "Missing result type signature");
		  Vextent_limited = FALSE;
#                 ifdef VERBOSE
		      printf("Vallocate: bad value result\n");
#                 endif
	      } else if (result_sig -> kind == FUNCSIGNATURE
			 && impure(result_sig)) {
		  Vextent_limited = FALSE;
#                 ifdef VERBOSE
		    printf("Vallocate: impure result signature\n");
#                 endif
	      } else {
		  /* Can only return a variable through arguments */
		  Vextent_limited = TRUE;
#                 ifdef VERBOSE
		    printf("Vallocate: so far - so good\n");
#                 endif
		  if (p -> ar_static_level != 0) {
		    maplist(q, op_sig -> fsig_param_list, {
		      NODE * sig = q -> par_signature;

		      if (sig -> kind == VARSIGNATURE) {
			  ASSERT (has_sig(sig -> var_denotation),
				  "Missing parameter type signature");
			  if (!sig -> var_denotation -> signature
				   -> ts_simple_type) {
			      Vextent_limited = FALSE;
			      IFVERBOSE(
				printf("Vallocate: bad parameter\n");
			      )
			  }
		      }
		    });
		  }  
	      }
	  }
	Vcallcc = FALSE;
	Vallocate_on_stack = Vextent_limited;
	if (!Gflag
	     && (result_sig -> kind == FUNCSIGNATURE || !unused_decl)) {
	    /* VAX runtime system & a.r. may be heap allocated */
	    /* Could result in bad pointers                    */
	    Vallocate_on_stack = FALSE;
#           ifdef VERBOSE
		printf("Vallocate: possible heap a.r.\n");
#           endif
	} 
	if (!Nflag && (p -> fc_complexity & NO_CALLCC) == 0) {
	    /* Must be able to copy environment w/o getting */
	    /* part of the state                            */
	    Vallocate_on_stack = FALSE;
	    Vcallcc = TRUE;
#           ifdef VERBOSE                             
		printf("Vallocate: possible saved continuation\n"); 
#           endif
	}
#       ifdef VERBOSE
	    printf("Vallocate: Vallocate_on_stack = %d\n",
		   Vallocate_on_stack);
#       endif
	{
	    NODE * params = p -> signature -> fsig_param_list;
	    int n_params = length(params) - n_vacuous_params(params);
	    register int i = 0;

	    maplist (v,p->signature->fsig_param_list, {
		if (i < n_params || Vstatic_level == 0) {
		  /* May be passed explicitly, reserve space */
		  Vallocwalk(v);
		} else {
		  /* Map it onto the last real parameter */
		  v -> displacement = Vnext_free - 1;
		  v -> level = Vstatic_level;
		}
		i++;
	    });
	}
	Vallocwalk (p->fc_body);
	p -> ar_static_level = Vstatic_level;
	if (Vnext_free >= Vhigh_water) {
	    p -> ar_size = Vnext_free;
	} else {
	    p -> ar_size = Vhigh_water;
	}
	if (p -> ar_size > MAXOBJSZ) {
	    errmsg0(p, "Too many local variables");
	}
	p -> ar_static_link = old_ar;
	Vcurrent_func = old_func;
	Vcurrent_ar = old_ar;
	Vstatic_level--;
	Vnext_free = old_Vnext_free;
	Vhigh_water = old_Vhigh_water;
	Vallocate_on_stack = old_aos;
	Vextent_limited = old_el;
	Vcallcc = old_ccc;
	Vfree_blocks();
	Vcurrent_blocks = old_Vcurrent_blocks;
	n_int_regs = old_int_regs;
	n_addr_regs = old_addr_regs;
}

