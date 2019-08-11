# include "parm.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "stree/Array.h"
# include "pass5c/codeutil.h"

extern int yynerrs;     /* Incremented by errmsg macros */

Array * (list_to_array());

/* Return -1, 0, or 1, depending on whether the preorder number of p */
/* is less than, equal to, or greater than, that of q.               */
static int pre_order(p, q)
NODE **p, **q;
{
    if ((*p) -> pre_num < (*q) -> pre_num) {
        return(-1);
    } else if ((*p) -> pre_num > (*q) -> pre_num) {
        return(1);
    } else {
        return(0);
    }
}

/* Return a copy of the list of declarations in its original order,   */
/* that is, sort it by pre-order number.                              */
NODE * decl_sort(decl_l)
NODE * decl_l;
{
    Array * a = list_to_array(decl_l);
    NODE * result = emptylist();
    NODE **p;
    int i;

    qsort(a -> a_body, a -> a_size, sizeof(NODE *), pre_order);

    /* Convert back to list form */
        for (p = a -> a_body; p < &(a -> a_body[a -> a_size]); p++) {
            addright(result, *p);
        }

    return(result);
}

#define NOT_REFD 0x7fffffff

void label_refd_decls();
void find_forward_refs();

static NODE * current_scope;  /* pointer to current block of sorting decls */
static NODE * current_mp;     /* pointer to current MODPRIMARY node if     */
			      /* sorting with list. NIL o.w.               */


/*
 *  Fill in decl_can_be_refd fields in the the list of declarations
 * decl_l.  These should contain the least preorder number of
 * any declaration in the list whose elaboration can require the
 * value produced by the declaration under consideration.
 *  Then fill in the id_forward_ref fields to label those identifiers
 * whose evaluation could result in a reference to a not yet computed
 * value.  id_forward_ref fields assume that function abstractions
 * appearing directly on the rhs of a declaration are NOT implemented
 * by copying globals into the closure.  (Directly means by themselves
 * or in a WITH list appearing at the outermost level.)
 */

NODE * label_decls(decl_l)
NODE * decl_l;
{
    int len = length(decl_l);
    register int i;

    if (len == 0) return(decl_l);
    ASSERT(decl_l -> kind == LISTHEADER, "label_decls: bad declaration list\n");
    /* initialize static variables */
	current_scope = first(decl_l) -> decl_scope;
	current_mp = NIL;
    /* initialize decl_can_be_refd fields */
	maplist(p ,decl_l, {
            p -> decl_can_be_refd = NOT_REFD;
        });
    maplist(p, decl_l, {
        /* update decl_can_be_refd fields to reflect references from */
        /* within p -> decl_denotation.  Initially ignore function   */
	/* constructions.                                            */
	    switch(p -> decl_denotation -> kind) {
	      case FUNCCONSTR:
		/* Not generally evaluated during declaration elaboration. */
		/* cl_analyze has to compensate if it wants to copy        */
		/* globals into a closure.                                 */
		break;
	      case MODPRIMARY:
		{
		  NODE * den = p -> decl_denotation;

		  /* Mark the type expression as referenced, but */
		  /* omit function constructions in WITH lists.  */
		    while (den -> kind == MODPRIMARY) {
			if (den -> mp_type_modifier == NIL
			    || den -> mp_type_modifier -> kind == EXPORTLIST
			    || den -> mp_type_modifier -> kind == HIDELIST) {
			    den = den -> mp_primary;
			} else /* with list */ {
			    ASSERT(den -> mp_type_modifier -> kind
				   == WITHLIST, "label_decls: bad mp\n");
			    maplist(s, den -> mp_type_modifier
					   -> wl_component_list, {
				ASSERT(s -> kind == DECLARATION,
				       "label_decls: bad wl\n");
				if (s -> decl_denotation -> kind != FUNCCONSTR) {
				    label_refd_decls(s -> decl_denotation,
						     p -> pre_num);
				}
			    });
			    den = den -> mp_primary;
			}
		    }
		    label_refd_decls(den, p -> pre_num);
		  break;
		}
	      default:
		label_refd_decls(p -> decl_denotation, p -> pre_num);
		break;
	    }
    });
    /* Fill in id_forward_ref fields */
      maplist(p, decl_l, {
        if (p -> decl_denotation -> kind == FUNCCONSTR) {
            if (p -> decl_can_be_refd != NOT_REFD) {
                find_forward_refs(p -> decl_denotation, p -> decl_can_be_refd);
            }
        } else {
            if (p -> decl_can_be_refd < p -> pre_num) {
                find_forward_refs(p -> decl_denotation, p -> decl_can_be_refd);
            } else {
                find_forward_refs(p -> decl_denotation, p -> pre_num);
            }
        }
      });
    return(decl_l);
}

/*
 *  Fill in decl_can_be_refd and id_forward_ref fields in the declaration
 * list associated with the with list of the given MODPRIMARY node.
 * Mp must refer to a with list.  The list of declarations is returned.
 */

LIST
label_wl(mp)
NODE * mp;
{
    int len;
    NODE * decl_l;
	   
    ASSERT(mp -> kind == MODPRIMARY 
	   && mp -> mp_type_modifier -> kind == WITHLIST,
           "label_wl: bad with list");
    decl_l =  mp -> mp_type_modifier -> wl_component_list;
    len = length(decl_l);
    if (len == 0) return(decl_l);
    ASSERT(decl_l -> kind == LISTHEADER, "wl_sort: bad declaration list\n");
    /* initialize static variables */
	current_scope = NIL;
	current_mp = mp;
    /* initialize decl_can_be_refd fields */
	maplist(p ,decl_l, {
            p -> decl_can_be_refd = NOT_REFD;
        });
    maplist(p, decl_l, {
        /* update decl_can_be_refd fields to reflect references from */
        /* within p -> decl_denotation.  Initially ignore function   */
        /* constructions.                                            */
            if (p -> decl_denotation -> kind != FUNCCONSTR) {
                label_refd_decls(p -> decl_denotation, p -> pre_num);
            }
    });
    /* Fill in id_forward_ref fields */
      maplist(p, decl_l, {
        if (p -> decl_denotation -> kind == FUNCCONSTR) {
            if (p -> decl_can_be_refd != NOT_REFD) {
                find_forward_refs(p -> decl_denotation, p -> decl_can_be_refd);
            }
        } else {
            if (p -> decl_can_be_refd < p -> pre_num) {
                find_forward_refs(p -> decl_denotation, p -> decl_can_be_refd);
            } else {
                find_forward_refs(p -> decl_denotation, p -> pre_num);
            }
        }
      });
    return(decl_l);
}


/*
 *  Label all declarations whose value may be needed in the evaluation
 * of p as being needed by a declaration with pre-order number prenum.
 */
void label_refd_decls(p, prenum)
NODE * p;
int prenum;
{
    switch(p -> kind) {
	case BLOCKDENOTATION:
	    maplist(s, p -> bld_declaration_list, {
                label_refd_decls(s -> decl_denotation, prenum);
	    });
	    maplist(s, p -> bld_den_seq, {
                label_refd_decls(s, prenum);
	    });
	    break;

	case USELIST:
	    maplist(s, p -> usl_type_list, {
                label_refd_decls(s, prenum);
	    });
	    maplist(s, p -> usl_den_seq, {
                label_refd_decls(s, prenum);
	    });
	    break;

	case APPLICATION:
            label_refd_decls(p -> ap_operator, prenum);
	    maplist(s, p -> ap_args, {
                label_refd_decls(s, prenum);
	    });
	    break;

	case EXTENSION:
	    /* The compiler causes the argument of an extension */
	    /* to be evaluated.                                 */
            label_refd_decls(p -> ext_denotation, prenum);
            break;

        case RECORDCONSTRUCTION:
            maplist(s, p -> rec_component_list, {
                label_refd_decls(s -> re_denotation, prenum);
            });
            break;

	case WORDELSE:
	case ENUMERATION:
	case PRODCONSTRUCTION:
        case UNIONCONSTRUCTION:
        case EXTERNDEF:
        case REXTERNDEF:
	    /* The "arguments" are not evaluated */
	    break;

	case MODPRIMARY:
	    {
		NODE * tm = p -> mp_type_modifier;

                label_refd_decls(p -> mp_primary, prenum);
		if (tm != NIL && tm -> kind == WITHLIST) {
		    maplist(s, tm -> wl_component_list, {
                        label_refd_decls(s -> decl_denotation, prenum);
		    });
		}
	    }
	    break;

	case GUARDEDLIST:
	case LOOPDENOTATION:
	    maplist(s, p -> gl_list, {
                label_refd_decls(s -> ge_guard, prenum);
                label_refd_decls(s -> ge_element, prenum);
	    });
            break;

        case OPRID:
	case LETTERID:
	    ASSERT(p ->  id_str_table_index != -1,
                   "label_refd_decls: Funny identifier\n");
	    if (current_mp == NIL) /* normal declarations */ {
		NODE * def = p -> id_last_definition;

                if (p -> sel_type != NIL) {
                    label_refd_decls(p -> sel_type, prenum);
                } else if (def -> kind == DECLARATION
		    && def -> decl_scope == current_scope) {
		    /* This is a reference to an identifier in the */
		    /* current block.                              */
                    if (prenum < def -> decl_can_be_refd) {
                        def -> decl_can_be_refd = prenum;
                        label_refd_decls(def -> decl_denotation, prenum);
                    }
		}
	    } else /* with list */ {
		NODE * sel_t = p -> sel_type;

		if (sel_t == NIL) {
		    if (p -> id_last_definition == current_mp) {
		    /* reference to new type as a whole */
                      errmsg0(p, "Warning - forward reference to local type identifier - no runtime check inserted");
                      yynerrs--;  /* only a warning */
		    }
		} else /* sel_type not NIL */ {

		    if ((   sel_t -> kind == LETTERID
			 || sel_t -> kind == OPRID)
			 && sel_t -> sel_type == NIL
			 && sel_t -> id_last_definition == current_mp) {
			/* obvious selection from local type id          */
                        /* update with list component which defines this */
			/* component.                                    */
			  maplist(s, current_mp -> mp_type_modifier
					      -> wl_component_list, {
                            if (s -> decl_sel_index
                                == p -> sel_index) {
                                /* It refers to this decl */
                                if (prenum < s -> decl_can_be_refd) {
                                    s -> decl_can_be_refd = prenum;
                                    label_refd_decls(s -> decl_denotation,
                                                     prenum);
                                }
                            }
			  });
		    } else {
                        label_refd_decls(sel_t, prenum);
		    }
		}
	    }
	    break;

	case QSTR:
	case UQSTR:
	    if (current_mp != NIL) {
		/* In the case of a with list make sure that constants */
                /* and concatenation operators in string are computed  */
                /* first.                                              */
                  if (p -> sel_type -> signature -> ts_string_code != NIL &&
                      p -> sel_type -> signature -> ts_element_code != NIL) {
                      /* No subexpressions will be evaluated */
                  } else {
                      label_refd_decls(p -> str_expansion, prenum);
                  }
	    } else {
		/* For declarations we don't care about individual type */
		/* components.  Just process selection type.            */
                label_refd_decls(p -> sel_type, prenum);
	    }
	    break;

	case FUNCCONSTR:
            /* function is referenced from within decl and may be applied */
            label_refd_decls(p -> fc_body, prenum);
	    break;

#   ifdef DEBUG
	default:
            dbgmsg("label_refd_decls: bad kind encountered\n");
	    abort();
#   endif
    }
}


/*
 *  Find any possible references within p to declarations with pre-order
 * number >= prenum.
 */
void find_forward_refs(p, prenum)
NODE * p;
int prenum;
{
    switch(p -> kind) {
	case BLOCKDENOTATION:
	    maplist(s, p -> bld_declaration_list, {
                find_forward_refs(s -> decl_denotation, prenum);
	    });
	    maplist(s, p -> bld_den_seq, {
                find_forward_refs(s, prenum);
	    });
	    break;

	case USELIST:
	    maplist(s, p -> usl_type_list, {
                find_forward_refs(s, prenum);
	    });
	    maplist(s, p -> usl_den_seq, {
                find_forward_refs(s, prenum);
	    });
	    break;

	case APPLICATION:
            find_forward_refs(p -> ap_operator, prenum);
	    maplist(s, p -> ap_args, {
                find_forward_refs(s, prenum);
	    });
	    break;

	case EXTENSION:
	    /* The compiler causes the argument of an extension */
	    /* to be evaluated.                                 */
            find_forward_refs(p -> ext_denotation, prenum);
            break;

        case RECORDCONSTRUCTION:
            maplist(s, p -> rec_component_list, {
                find_forward_refs(s -> re_denotation, prenum);
            });
            break;

	case WORDELSE:
	case ENUMERATION:
	case PRODCONSTRUCTION:
        case UNIONCONSTRUCTION:
        case EXTERNDEF:
        case REXTERNDEF:
	    /* The "arguments" are not evaluated */
	    break;

	case MODPRIMARY:
	    {
		NODE * tm = p -> mp_type_modifier;

                find_forward_refs(p -> mp_primary, prenum);
		if (tm != NIL && tm -> kind == WITHLIST) {
		    maplist(s, tm -> wl_component_list, {
                        label_refd_decls(s -> decl_denotation, prenum);
		    });
		}
	    }
	    break;

	case GUARDEDLIST:
	case LOOPDENOTATION:
	    maplist(s, p -> gl_list, {
                find_forward_refs(s -> ge_guard, prenum);
                find_forward_refs(s -> ge_element, prenum);
	    });
            break;

        case OPRID:
	case LETTERID:
	    ASSERT(p ->  id_str_table_index != -1,
                   "find_forward_refs: Funny identifier\n");
	    if (current_mp == NIL) /* normal declarations */ {
		NODE * def = p -> id_last_definition;

                if (p -> sel_type != NIL) {
                    find_forward_refs(p -> sel_type, prenum);
                } else if (def -> kind == DECLARATION
		    && def -> decl_scope == current_scope) {
		    /* This is a reference to an identifier in the */
		    /* current block.                              */
                    if (prenum <= def -> pre_num) {
                        p -> id_forward_ref = TRUE;
                    }
		}
	    } else /* with list */ {
                NODE * sel_t = p -> sel_type;

                if (sel_t != NIL) {

		    if ((   sel_t -> kind == LETTERID
			 || sel_t -> kind == OPRID)
			 && sel_t -> sel_type == NIL
			 && sel_t -> id_last_definition == current_mp) {
			/* obvious selection from local type id          */
                        /* Check for forward reference.                  */
                          maplist(s, current_mp -> mp_type_modifier
					      -> wl_component_list, {
                            if (s -> decl_sel_index
                                == p -> sel_index) {
                                /* It refers to this decl */
                                if (prenum <= s -> pre_num) {
                                    p -> id_forward_ref = TRUE;
                                }
                            }
			  });
		    } else {
                        find_forward_refs(sel_t, prenum);
		    }
		}
	    }
	    break;

	case QSTR:
	case UQSTR:
	    if (current_mp != NIL) {
		/* In the case of a with list make sure that constants */
                /* and concatenation operators in string are not       */
                /* forward references.                                 */
                  if (p -> sel_type -> signature -> ts_string_code != NIL &&
                      p -> sel_type -> signature -> ts_string_code != NIL) {
                      /* No subexpressions will be evaluated */
                  } else {
                      find_forward_refs(p -> str_expansion, prenum);
                  }
	    } else {
		/* For declarations we don't care about individual type */
		/* components.  Just process selection type.            */
                find_forward_refs(p -> sel_type, prenum);
	    }
	    break;

	case FUNCCONSTR:
            /* function is referenced from within decl and may be applied */
            find_forward_refs(p -> fc_body, prenum);
	    break;

#   ifdef DEBUG
	default:
            dbgmsg("find_forward_refs: bad kind encountered\n");
	    abort();
#   endif
    }
}
