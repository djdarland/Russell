#define DEBUG

#define TRACE
#undef TRACE
/*
 *  Set the SL_ACC bits in FUNCCONSTR nodes.  (See stree/streedefs.h
 * for definition.)
 *  This affects cl_analyze1, and is thus done first.
 */
# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "pass3/is_local.h"
# include "codegen.h"

extern FILE * unparse_file;

extern boolean Vflag;

static int nestlevel;     /* Current nesting level */

static NODE * current_blk; /* Innermost construction or block          	*/
			   /* currently being analyzed.                	*/
			   /* blocks count only if they require an	*/
			   /* activation record.			*/
static NODE * current_fc;  /* Innermost function.  Blocks don't count.  */

/* Data structure declarations for hash table representing sl access  */
/* dependency graph.  An edge i -> j is in the graph if it is known   */
/* that a static link access by i forces a static link access by j    */

/* List of function constructions.  Used in adjacency lists. */
struct constr_list {
    NODE * cl_constr;
    struct constr_list * cl_next;
};
# define cl_nil ((struct constr_list *)0)

/* Adjacency list for a single source vertex: */
struct adj_list {
    NODE * adj_source;
    struct constr_list * adj_dests;
    struct adj_list * adj_next;     /* Next adjacency list in hash table */
				    /* chain.                            */
};
# define adj_nil ((struct adj_list *)0)

/* Hash table of adjacency lists */
# define HSIZE 317
# define hash(p) (((int)p) % HSIZE)

struct adj_list * sl_deps[HSIZE];

/* Deallocate all memory associated with the adjacency list p */
void adj_free(p)
struct adj_list * p;
{
    struct constr_list * q, *nq;
    
    for (q = p -> adj_dests; q != cl_nil; q = nq) {
	nq = q -> cl_next;
	free(q);
    }
    free(p);
}

/* Find i's adjacency list.  Create one if it's not there. */
struct adj_list * find_adj(i)
NODE * i;
{
    int h = hash(i);
    struct adj_list *p;

    for (p = sl_deps[h]; p != adj_nil && p -> adj_source != i;
	 p = p -> adj_next);
    if (p == adj_nil) {
	p = (struct adj_list *)malloc(sizeof (struct adj_list));
	p -> adj_source = i;
	p -> adj_next = sl_deps[h];
	p -> adj_dests = cl_nil;
	sl_deps[h] = p;
    }
    return(p);
}

/* Add an edge i -> j to the graph */
void sl_dep_add(i, j)
NODE *i, *j;
{
    struct adj_list *p = find_adj(i);
    struct constr_list * q = (struct constr_list *)
			     malloc(sizeof (struct constr_list));
    q -> cl_next = p -> adj_dests;
    q -> cl_constr = j;
    p -> adj_dests = q;
}

/* Set SL_ACC bits for all nodes reachable from i */
sl_mark_reachable(i)
NODE * i;
{
    struct adj_list * p;
    struct constr_list * q;
    NODE * cfc;             /* current function construction */

    p = find_adj(i);
    for (q = p -> adj_dests; q != cl_nil; q = q -> cl_next) {
	cfc = q -> cl_constr;
#       ifdef DEBUG
	    if (cfc -> kind != FUNCCONSTR) {
		dbgmsg("Sl_mark_reachable: bad construction\n");
		abort(p,q,cfc);
	    }
#       endif
	if (!(cfc -> fc_complexity & SL_ACC)) {
#           ifdef TRACE
		printf("Sl_mark_reachable: setting SL_ACC for %s\n",
		       cfc -> fc_code_label);
		printf("\tpropagated from %s\n",
		       i -> fc_code_label);
#           endif
	    cfc -> fc_complexity |= SL_ACC;
	    sl_mark_reachable(cfc);
	}
    }
}

/* Set SL_ACC bits for all constructions in the graph that are */
/* reachable from a construction with the SL_ACC bit set.      */
/* Subsequently delete the graph.                              */
void sl_solve()
{
    int i;
    struct adj_list * p, *np;
    struct constr_list * q;

    for (i = 0; i < HSIZE; i++) {
	for (p = sl_deps[i]; p != adj_nil; p = p -> adj_next) {
	  if (p -> adj_source -> fc_complexity & SL_ACC) {
#           ifdef TRACE
		printf("Sl_solve: propagating SL_ACC from %s\n",
		       p -> adj_source -> fc_code_label);
#           endif
	    sl_mark_reachable(p -> adj_source);
	  }
	}
    }
    /* Now deallocate the whole thing */
      for (i = 0; i < HSIZE; i++) {
	for (p = sl_deps[i]; p != adj_nil; p = np) {
	    np = p -> adj_next;
	    adj_free(p);
	}
      }
}

/* Set SL_ACC bits in accordance with the fact that we have just seen */
/* a level i identifier.                                              */
void process_global(i)
int i;
{
    int j;
    NODE * blk = current_blk;

    for (j = nestlevel; j > i; j--) {
	if (blk -> kind == BLOCKDENOTATION) {
#	  ifdef DEBUG
	      if (!(blk -> bld_flags & REQUIRES_AR)) {
	          dbgmsg("process_global: bad block\n");
	      }
#	  endif
#	  ifdef TRACE
	    printf("Skipping level %d block, decl lvl = %d\n", j, i);
#	  endif
	} else {
#         ifdef TRACE
	    printf("Setting SL_ACC for %s (levels:%d,%d)\n",
                   blk -> fc_code_label, i,j);
#         endif
	  blk -> fc_complexity |= SL_ACC;
	}
	blk = blk -> ar_static_link;
    }
}

/* Analogous to process_global, but SL_ACC bit should be set only if */
/* the SL_ACC bit in fc either is set, or eventually gets set.       */
void cond_process_global(i, fc)
int i;
NODE * fc;
{
    int j;
    NODE * blk = current_blk;

    if (current_fc -> fc_complexity & SL_ACC) {
	process_global(i);
	return;
    }
    for (j = nestlevel; j > i; j--) {
	if (blk -> kind != BLOCKDENOTATION) {
#         ifdef TRACE
	    printf("Entering dependency for %s on %s (levels:%d,%d)\n",
		   blk -> fc_code_label, fc -> fc_code_label, i,j);
#         endif
	  sl_dep_add(fc, blk);
	}
	blk = blk -> ar_static_link;
    }
}

/*
 *  Set fields in the tree headed by p.
 */
sl_analyze(p)
NODE * p;
{
    sl_analyze1(p);
    sl_solve();
}
 
/* Call sl_analyze1 on p if non_vac is true or p is not an identifier */
/* Used in conjunction with maprlist_non_vacuous.                     */
cond_sl_analyze1(p, needed)
NODE * p;
boolean needed;
{
    extern boolean is_id();

    if (needed || !is_id(p)) {
	sl_analyze1(p);
    }
}

sl_analyze1(p)
register NODE * p;
{
    NODE * v;
    int i;

    if (p == NIL) return;

    if (p -> signature -> kind == SIGNATURESIG) {
	/* signature evaluation doesn't reference anything. */
	return;
    }

    switch ( p -> kind ) {
	case LETTERID:
	case OPRID:
		if (p -> sel_type != NIL) {
		    sl_analyze1(p -> sel_type);
		} else {
		    int id_lev = p -> id_last_definition -> level;

		    if (id_lev != 0 && id_lev != nestlevel) {
			if(p -> id_last_definition -> kind == DECLARATION
			   && !(p -> id_last_definition
				  -> decl_special & ID_IMPORTED)) {
			   /* The accessibility check decided that this access was impossible; */
			   /* We'll believe it.                                                */
#                           ifdef TRACE
			      printf("Discarding bogus global reference: ");
			      unparse_file = stdout;                  
			      unparse(p);                             
			      printf("\n");
#                           endif
			} else {
#                           ifdef TRACE
				printf("Processing identifier: ");
				unparse_file = stdout;
				unparse(p);
				printf("\n");
#			    endif
			    process_global(id_lev);
			}
		    }
		}
		break;

	case BLOCKDENOTATION :
	    {
	    	NODE * old_blk = current_blk;
	    	
		if (p -> bld_flags & REQUIRES_AR) {
		    nestlevel++;
		    current_blk = p;
		}
		maplist (v, p->bld_declaration_list, {
		    ASSERT (v->kind == DECLARATION,
			    "sl_analyze1: decl expected");
		    sl_analyze1(v-> decl_denotation);
		});
		maplist (v,p->bld_den_seq, {
		    sl_analyze1(v);
		});
		if (p -> bld_flags & REQUIRES_AR) {
		    nestlevel--;
		    current_blk = old_blk;
		}
		break;
	    }
	    
	case USELIST:
		maplist(s, p -> usl_den_seq, {
		    sl_analyze1(s);
		});
		break;
		
	case APPLICATION:
		{
		  NODE * op_sig = p -> ap_operator -> signature;
		  NODE * constr = op_sig -> fsig_construction;

		  /* Check if this call could require operator value */
		  /* sl_analyze1 the operator if necessary.           */
		    {
		      extern boolean is_id();
		      boolean is_ident = is_id(p -> ap_operator);
		      boolean no_op_val =
			     is_ident
			     && (op_sig -> fsig_inline_code != NIL
				 || (constr != NIL &&
				     (constr -> ar_static_level == 1
				      || constr -> fc_complexity & NO_SL)));
		      if (!no_op_val) {
#                         ifdef TRACE
			    printf("Examining op: ");
			    unparse_file = stdout;
			    unparse(p -> ap_operator);
			    printf("\n");
#                         endif
			  if (is_ident
			      && constr != NIL && op_sig -> fsig_slink_known) {
			      /* May need to be able to get to its env */
			      cond_process_global(constr -> ar_static_level - 1,
						  constr);
			  } else {
			      sl_analyze1(p -> ap_operator);
			  }
		      }
#                     ifdef TRACE
			if (no_op_val) {
			    printf("Op value not needed (level %d): ",
				   constr == NIL? -1 : constr -> ar_static_level);
			    unparse_file = stdout;
			    unparse(p -> ap_operator);
			    printf("\n");
			}
#                     endif
		    }
		}
		maprlist_non_vacuous(p -> ap_args, cond_sl_analyze1);
		break;

        case LOOPDENOTATION:
	case GUARDEDLIST:
		maplist(v,p->gl_list, {
		    sl_analyze1(v);
		});
		break;

	case GUARDEDELEMENT:
		sl_analyze1(p -> ge_guard);
		sl_analyze1(p -> ge_element);
		break;

	case FUNCCONSTR:
		{
		    NODE * old_fc = current_fc;
		    NODE * old_blk = current_blk;

		    current_fc = p;
		    current_blk = p;
#		    ifdef DEBUG
			if (old_blk != NIL
			    && nestlevel != old_blk -> ar_static_level) {
			    dbgmsg("sl_analyze: nesting level confusion\n");
			}
#		    endif
		    nestlevel = current_fc -> ar_static_level;
		    sl_analyze1(p -> fc_body);

		    current_fc = old_fc;
		    current_blk = old_blk;
		    if (current_fc != NIL) {
			nestlevel = current_blk -> ar_static_level;
		    }
		}
		break;

	case MODPRIMARY:
		sl_analyze1(p -> mp_primary);
		if (p -> mp_type_modifier != NIL
		    && p -> mp_type_modifier -> kind == WITHLIST) {
		    maplist (q, p -> mp_type_modifier -> wl_component_list, {
			sl_analyze1(q -> decl_denotation);
		    });
		}
		break;

	case ENUMERATION:
	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
		/* Subexpressions are not evaluated */
		break;

	case QSTR:
	case UQSTR:
		{
		    NODE * tsig = p -> sel_type -> signature;

		    ASSERT(tsig -> kind == TYPESIGNATURE,
			   "sl_analyze1: bad string type");
		    if (tsig -> ts_string_code != NIL
			&& tsig -> ts_element_code != NIL
			&& strlen(p -> str_string) <= MAXSTRLEN) {
			    break;
			    /* May be dubious on VAX ? */
		    } else {
			sl_analyze1(p -> str_expansion);
		    }
		    break;
		}

	case WORDELSE:
		break;

	case EXTERNDEF:
		break;

	case REXTERNDEF:
		break;

	case RECORDCONSTRUCTION:
		maplist(s, p -> rec_component_list, {
		    sl_analyze1(s -> re_denotation);
                });
		break;

        case EXTENSION:
		sl_analyze1(p -> ext_denotation);
		break;

        case RECORDELEMENT:
	case DECLARATION:
	case PARAMETER:
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
	case WORDCAND:
	case WORDCOR:
	default:
		dbgmsg("sl_analyze1: bad kind, kind = %d\n", p -> kind);
		abort();
    };
    return;
}
