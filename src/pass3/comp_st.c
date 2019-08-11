# define TRACE
# undef TRACE
# include "parm.h"

# include <stdio.h>

# include "stree/ststructs.mh"

# include "decl_pairs.h"

# include "is_local.h"

# include "../pass4/sigs.h"

extern int stplinks[];
extern int stsigs[];

extern FILE * unparse_file;

/*
 *   Compare two syntax trees, imposing an arbitrary order, except that
 * WORDELSE is infinitely large.
 *   If tsigp and tsigq are given, they are assumed to be the next enclosing
 * type signatures.  Tsigp and tsigq are taken to be corresponding
 * declarations; an identifier in p referring to tsigp matches an identifier
 * in q referring to tsigq.
 *  The variables diff_p and diff_q are set to the first corresponding
 * subtrees that differ, if there are any, and if one of them is an
 * identifier which appears with NIL sel_type in an expression context.
 * They are set to NIL if this is not the case.
 * (This is used for missing argument inference.)
 *  The imposed ordering is consistent in that two sets of matching
 * signatures with all identifiers pointing to their correct declarations
 * will get ordered consistently.  This means that a string is considered
 * equivalent to its expansion, and a MODPRIMARY node indicating forgetting
 * is ignored.
 *   The left-hand-side of a signature transparent declaration is
 * identified with its right-hand-side.
 *
 *  The precise ordering is as follows:
 *
 *      0.  NIL is less than anything else.
 *      1.  If either tree consists of a WORDELSE node, it is greater.
 *      2.  If the kind fields differ, the ordering is that of the kind
 *          values.  OPRID and LETTERID are treated as identical.
 *          If 2 lists have different lengths they are ordered
 *          by length. 
 *      3.  If both nodes are identifier nodes (other than operation 
 *          names) then
 *          (a)  if they point to the same declaration they are 
 *               equal.
 *          (b)  if they point to corresponding declarations inside
 *               p and q they are equal
 *          (c)  if one points to a declaration inside the subtree
 *               and one points outside, the inside one is less.
 *          (d)  if none of the above applies the nodes are
 *               ordered by the preorder numbers of the declarations.
 *      4.  If none of the above applies the ordering is the
 *          lexicographic ordering (derived from this ordering) of
 *          the subtrees.  (The ordering of the fields within a
 *          node is arbitrary.)
 */
NODE * outer_p, * outer_q;

NODE * diff_p, * diff_q;

comp_st(p,q,tsigp,tsigq)
NODE *p, *q , *tsigp, *tsigq;
{
    register int i;

    outer_p = p;
    outer_q = q;
    diff_p = NIL;
    diff_q = NIL;
    clr_dlist;
    if (tsigp != NIL && tsigq != NIL) {
      add_dlist(tsigp, tsigq);
    }
    i = comp1_st(p, q, tsigp, tsigq, FALSE);
#   ifdef TRACE
      unparse_file = stdout;
      printf("comp_st: comparing %x and %x (", p, q);
      unparse(p);
      printf(" and ");
      unparse(q);
      printf(")\n");
      if (tsigp != NIL && tsigq != NIL) {
	printf("\tinside types %x and %x (", tsigp, tsigq);
	unparse(tsigp);
	printf(" and ");
	unparse(tsigq);
	printf(")\n");
      }
      printf("comp_st: returning %d\n", i);
#   endif
    return(i);
}

/* The following is used both by the above and called directly by amatch */
/* in pass4       */

comp1_st(p, q, tsigp, tsigq, exact) 
			    /* exact indicates declarations , parameters, */
                            /* and identifiers at the top level should    */
			    /* match exactly, i.e. id names should match. */
			    /* exact should be true only if one of these  */
			    /* kinds of nodes or a list thereof appears   */
			    /* at the top level of both trees.            */

NODE *p, *q, *tsigp, *tsigq;
boolean exact;
{
    register NODE **r;  /* pointer to next field of p to be recursively     */
                        /* examined.                                        */
    register NODE **s;  /* corresponding pointer for q                      */
    register int plinkv;/* bit vector specifying primary link fields of *p  */
                        /* shifted so that the most significant bit         */
			/* corresponds to q.                                */
    register int sigv;  /* vector specifying links leading from expressions */
			/* to signatures.                                   */
    int i,j;
    if (q == NIL) return(p != NIL /* 0 if both NIL, 1 o.w. */);
    if (p == NIL) return(-1);
    if (p == q) return(0);        /* only an optimization */
    if (p == ERR_SIG || q == ERR_SIG) return(0);
    if (p -> kind == WORDELSE) return(q -> kind != WORDELSE);
    if (q -> kind == WORDELSE) return(-1);
    /* make sure strings match their expansions */
        if ((p -> kind == QSTR || p -> kind == UQSTR)
            && p -> sel_type != NIL) {
            if (p -> str_expansion == NIL) {
                chgfld(&(p -> str_expansion), expand_str(p));
            }
	    p = p -> str_expansion;
	}
        if ((q -> kind == QSTR || q -> kind == UQSTR)
            && q -> sel_type != NIL) {
            if (q -> str_expansion == NIL) {
                chgfld(&(q -> str_expansion), expand_str(q));
            }
	    q = q -> str_expansion;
	}
    /* Don't consider MODPRIMARY nodes representing forgetting   */
    /* Map left-hand-sides of signature transparent declarations */
    /* to their right-hand-sides.                                */
      for (;;) {
	if (p -> kind == MODPRIMARY && p -> mp_type_modifier == NIL) {
	    p = p -> mp_primary;
	} else if (q -> kind == MODPRIMARY && q -> mp_type_modifier == NIL) {
	    q = q -> mp_primary;
	} else if (!exact
		   && (p -> kind == LETTERID || p -> kind == OPRID)
		   && p -> sel_type == NIL
		   && p -> id_last_definition != NIL
		   && p -> id_last_definition -> kind == DECLARATION
		   && p -> id_last_definition -> decl_sig_transp
		   && p -> id_last_definition -> post_num < p -> post_num) {
	    p = p -> id_last_definition -> decl_denotation;
	} else if (!exact
		   && (q -> kind == LETTERID || q -> kind == OPRID)
		   && q -> sel_type == NIL
		   && q -> id_last_definition != NIL
		   && q -> id_last_definition -> kind == DECLARATION
		   && q -> id_last_definition -> decl_sig_transp
		   && q -> id_last_definition -> post_num < q -> post_num) {
	    q = q -> id_last_definition -> decl_denotation;
	} else {
	    break;
	}
      }
    if (p -> kind != LETTERID && p -> kind != OPRID
	|| q -> kind != LETTERID && q -> kind != OPRID) {
	if (p -> kind == LETTERID || p -> kind == OPRID
	    || q -> kind == LETTERID || q -> kind == OPRID) {
	    diff_p = p;
	    diff_q = q;
	}
	if (p -> kind > q -> kind) return(1);
	if (p -> kind < q -> kind) return(-1);
    }
    /* kind fields are equal */
        switch(p->kind) {
            case LETTERID:
	    case OPRID:
	      {
		int p_indx;     /* string table index for p */
		int q_indx;
		NODE * p_decl;  /* declaration for p        */
		NODE * q_decl;

		p_indx = p -> id_str_table_index;
		q_indx = q -> id_str_table_index;
		if(exact
		   || p -> sel_type != NIL
		   || q -> sel_type != NIL) {
#                   ifdef DEBUG
			if (p_indx == -1 || q_indx == -1) {
			    dbgmsg("comp_st: local type id in wrong context\n");
			}
#                   endif
		    if(p_indx < q_indx)
                        return(-1);
		    if(p_indx > q_indx)
                        return(1);
		    return(comp1_st(p -> sel_type, q -> sel_type), tsigp, tsigq, FALSE);
		} else {
		    p_decl = p -> id_last_definition;
		    q_decl = q -> id_last_definition;
#                   ifdef DEBUG
			if(p_indx==-1 && tsigp==NIL || q_indx==-1 && tsigq==NIL) {
                            dbgmsg("comp_st: bad use of local type id\n");
                            abort();
			}
#                   endif
		    if (p_indx == -1) p_decl = tsigp;
		    if (q_indx == -1) q_decl = tsigq;
		    if (p_decl != NIL && q_decl != NIL
			&& p_decl -> pre_num == q_decl -> pre_num) {
			return(0);
		    }
		    if (p_decl != NIL && q_decl != NIL &&
			dl_match(p_decl, q_decl)) {
			return(0);
		    }
		    if (p_decl == NIL && q_decl == NIL
			&& p_indx == q_indx) {
			return(0);
		    }
		    /* They're different */
		    diff_p = p;
		    diff_q = q;
		    if (p_decl == NIL && q_decl == NIL) {
		      if(p_indx < q_indx) {
			  return(-1);
		      } else {
			  return(1);
		      }
		    }
		    if (q_decl == NIL) return (1);
		    if (p_decl == NIL) return (-1);
		    if (is_descendant(q_decl, outer_q)
			&& !is_descendant(p_decl, outer_p)) {
			return(1);
		    }
		    if (is_descendant(p_decl, outer_p)
			&& !is_descendant(q_decl, outer_q)) {
			return(-1);
		    }
		    if (p_decl -> pre_num  > q_decl -> pre_num) {
			return(1);
		    } else {
			return(-1);
		    }
		}
	      }

	    case QSTR:
            case UQSTR:
		i = strcmp(p -> str_string, q -> str_string);
		if (i != 0)
		    return(i);
		else
		    return(comp1_st(p -> sel_type, q -> sel_type), tsigp, tsigq, FALSE);

	    case FUNCCONSTR:
		i = length(p->signature->fsig_param_list);
		j = length(q->signature->fsig_param_list);
		if (i > j) return(1);
		if (i < j) return(-1);
		map2lists (s, p -> signature -> fsig_param_list,
			   r, q -> signature -> fsig_param_list, {
		    add_dlist(s, r);
		});
		map2lists (s, p -> signature -> fsig_param_list,
			   r, q -> signature -> fsig_param_list, {
		    if ((i = comp1_st(s -> par_signature, r -> par_signature,
				      tsigp, tsigq, FALSE)) != 0) {
			return(i);
		    }
		});
		return(
		    comp1_st(p -> fc_body, q -> fc_body, tsigp, tsigq, FALSE)
                );

            case EXTERNDEF:
		return(strcmp(p -> ext_name, q -> ext_name));

            case REXTERNDEF:
		return(strcmp(p -> r_ext_name, q -> r_ext_name));

	    case FUNCSIGNATURE:
		i = length(p->fsig_param_list);
		j = length(q->fsig_param_list);
		if (i > j) return(1);
		if (i < j) return(-1);
		map2lists (s, p -> fsig_param_list,
			   r, q -> fsig_param_list, {
		    add_dlist(s, r);
		});
		map2lists (s, p -> fsig_param_list,
			   r, q -> fsig_param_list, {
		    i = comp1_st(s->par_signature, r->par_signature,
				 tsigp,tsigq,FALSE);
		    if (i != 0) {
			return(i);
		    }
		});
		return(comp1_st(p -> fsig_result_sig, q-> fsig_result_sig,
                                tsigp, tsigq, FALSE));

	    case TYPESIGNATURE:
		add_dlist(p,q);
		return(comp1_st(p->ts_clist, q->ts_clist, p, q, FALSE));

	    case PARAMETER:
#               ifdef DEBUG
		if(!exact) {
		    /* should never get here */
			dbgmsg("comp_st: bad parameter\n");
			prtree(p);
			abort();
		}
#               endif
		return(comp1_st(p -> par_id, q -> par_id, tsigp, tsigq, TRUE));

	    case DECLARATION:
#               ifdef DEBUG
		if(!exact) {
		    /* should have been processed in BLOCKDENOTATION */
			dbgmsg("comp_st: bad declaration\n");
		}
#               endif
		{
		    int i;
		    i = comp1_st(p -> decl_id, q -> decl_id, tsigp, tsigq, tsigq, TRUE);
                    if (i != 0) return(i);
		}
                return(comp1_st(p -> decl_denotation,
				q -> decl_denotation, tsigp, tsigq, FALSE));

	    case BLOCKDENOTATION:
		i = length(p -> bld_declaration_list);
		j = length(q -> bld_declaration_list);
		if(i > j) return(1);
		if(i < j) return(-1);
		map2lists(s, p -> bld_declaration_list,
			  r, q -> bld_declaration_list, {
		    add_dlist(s, r);
		});
		map2lists(s, p -> bld_declaration_list,
			  r, q -> bld_declaration_list, {
		    i = comp1_st(s -> decl_denotation, 
				 r -> decl_denotation, tsigp, tsigq, FALSE);
		    if(i != 0) return(i);
		});
		return(comp1_st(p -> bld_den_seq,
				q -> bld_den_seq, tsigp,tsigq,FALSE));

            case RECORDELEMENT:
                {
                    int i;
		    i = comp1_st(p -> re_id, q -> re_id, tsigp, tsigq, TRUE);
                    if (i != 0)
                        return(i);
                    else
                        return(comp1_st(p -> re_denotation,
					q -> re_denotation, tsigp, tsigq, FALSE));
		}

	    case TSCOMPONENT:
                {
                    int i;
		    i = comp1_st(p -> tsc_id, q -> tsc_id, tsigp, tsigq, TRUE);
                    if (i != 0)
                        return(i);
                    else
                        return(comp1_st(p -> tsc_signature,
					q -> tsc_signature, tsigp, tsigq, FALSE));
		}

	    case DEFCHARSIGS:
		{
		    unsigned * vp;  /* pointers to current bitvectors */
		    unsigned * vq;

		    vp = (unsigned *) &(p -> dcs_0);
		    vq = (unsigned *) &(q -> dcs_0);
		    for (i = 0; i < NVECTORS; i++) {
			if (*vp < *vq) return (-1);
			if (*vp > *vq) return (1);
			vp++; vq++;
		    }
		    return(0);
		}

	    case UNIONCONSTRUCTION:
	    case PRODCONSTRUCTION:
		/* check to make sure that field names match */
		    map2lists(s, p -> prod_components,
			      r, q -> prod_components, {
			i = comp1_st(s -> par_id, r -> par_id,
				     tsigp, tsigq, TRUE);
			if (i != 0) return(i);
		    });

                if (p -> kind == PRODCONSTRUCTION) {
                  /* add identifiers to definitions list */
                    map2lists(s, p -> prod_components,
			      r, q -> prod_components, {
			add_dlist(s, r);
		    });
                    add_dlist(p,q);
                }
		
		/* check that signatures match */
		    map2lists(s, p -> prod_components,
			      r, q -> prod_components, {
			i = comp1_st(s -> par_signature, r -> par_signature,
				     tsigp, tsigq, FALSE);
			if (i != 0) return(i);
		    });

                /* both products or unions are identical */
		return(0);

            case MODPRIMARY:
		add_dlist(p,q);
		goto lex_order;

            case WITHLIST:
                exact = TRUE;
		/* and now continue: */
            default:
            lex_order:
                /* recursively examine subtrees */
                    if (is_list(p)) {
#                       ifdef DEBUG
                            if(!is_list(q)) {
                                dbgmsg("comp_st: inconsistent lists\n");
                            }
#                       endif
                        i = length(p);
                        j = length(q);
                        if (i > j) return(1);
                        if (i < j) return(-1);
			map2lists(e1, p, e2, q, {
			   if ((i = comp1_st(e1, e2, tsigp, tsigq, exact)) != 0)
			       return(i);
                        });
                    } else {
			plinkv = stplinks[p -> kind];
			sigv = stsigs[p -> kind];
                        r = (NODE **) p;
                        s = (NODE **) q;
			while ( plinkv != 0 ) {
			    if ( plinkv < 0 /* msb is set */ && sigv >= 0) {
				if ((i = comp1_st(*r, *s, tsigp, tsigq, exact)) != 0)
                                    return(i);
			    }
			    r++;
			    s++;
			    plinkv <<= 1;
			    sigv <<= 1;
                        }
		    }
		    return(0);
        }
}
