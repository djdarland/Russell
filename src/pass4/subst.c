# define TRACE
# undef TRACE
# define DEBUG

# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "stree/stsigs.mh"

# include "sigs.h"

# include "pass3/decl_pairs.h"
# include "pass3/is_local.h"

extern unsigned stplinks[];

extern int stsize[];

extern int yynerrs;  /* number of errors encountered in this pass */

extern struct cn * dontsubst;

extern NODE * substerr;

extern FILE * unparse_file;

/* Return an expression on the dontsubst list if s contains one.
 * Otherwise return NIL.
 */
NODE * on_dontsubst(s)
NODE *s;
{
    struct cn *c;

    c = dontsubst;
    while(c != NIL) {
	if( is_descendant(((NODE *)(cn_head(c))), s) ) {
#           ifdef TRACE
		printf("on_dontsubst: returning 0x%X\n", cn_head(c));
		fflush(stdout);
#           endif
            return((NODE *)(cn_head(c)));
        }
        c = cn_tail(c);
    };
    return(NIL);
}

/*
 * subst(p, parameter list, argument list)
 *
 * Returns a structure which is a copy of the first argument except
 * that all identifiers declared in the parameter list are replaced
 * by the corresponding denotations in the argument list.
 * An attempt is made to copy as little of the signature
 * as possible.
 *  It is assumed that all identifiers in p point to their
 * declaration.
 *  Back pointers in the copy point to the original nodes.
 *  If an attempt is made to substitute an expression which has
 * a subexpression on the dontsubst list, substerr is set to point 
 * to the subexpression.
 */

NODE * subst1();

NODE * subst(p,params,args)
NODE * p, * params, * args;
{
    NODE * result;

    if (is_empty(params)) {
	return(p);
    }
    clr_dlist;
    result = subst1(p, params, args);
#   ifdef TRACE
        if (result != p) {
            unparse_file = stdout;
            printf("subst: replaced\n");
            unparse(p);
            printf("\nwith\n");
            unparse(result);
            printf("\n");
        }
#   endif
    return(result);
}

NODE * subst1(p,params,args)
NODE * p, *params, *args;
{
    boolean mod_flag = FALSE; /* One of the descendants was modified */
    register int knd;         /* kind field of node being examined   */
    int plinkv;               /* bit vector indicating primary links */
                              /* to be followed to recursively       */
			      /* substitute in subtrees              */
    int sigv;                 /* vector of signature pointers        */
    register NODE ** q;       /* pointer to link field to be         */
                              /* recursively examined.               */
    int idpos;                /* position of identifier in params    */
    NODE * tmpcopy[MAXFIELDS];/* temporary version of result         */
    register NODE ** s;
    register int i;
    int lim;
    register struct cn * c;
    int j;
    NODE * v, * w;

    if (p == ERR_SIG || p == NIL) return(p);

    switch ( knd = (p -> kind) ) {
        case LISTHEADER:
            i = 0;
            j = length(p);
            /* get sufficiently large chunk of memory to temporarily copy */
	    /* the list, thus minimizing number of allocations.           */
                if (j <= MAXFIELDS) {
                    s = tmpcopy;
                } else {
                    s = (NODE **) malloc(j * sizeof(NODE *));
                }
            maplist(q, p, {
		s[i] = subst1(q, params, args);
                        /* should be locked but ... */
                if(s[i] != q) {
                    mod_flag = TRUE;
                }
                i++;
	    });
            if (mod_flag) {
              /* convert the temporary copy in s to a real list structure */
              /* and return it */
                NODE * result;
                result = emptylist();
                for (i = 0; i < j; i++) {
                    addright(result,s[i]);
		}
		if (j > MAXFIELDS) free(s);
                return(result);
	    } else {
		if (j > MAXFIELDS) free(s);
                return(p);
            }
        case OPRID:
	case LETTERID:
	  if (p -> id_str_table_index == -1) return(p);
#         ifdef DEBUG
	    if (!(p -> id_def_found) && yynerrs == 0) {
		dbgmsg("subst: id declaration unresolved: %s\n",
		       getname(p -> id_str_table_index));
	    }
#         endif
	  if (!p -> id_def_found) {
	    /* undeclared identifier */
	    return(p);
	  }
          if (p -> sel_type != NIL) {
              NODE * new_sel_type = subst1(p -> sel_type, params, args);

              if (new_sel_type == p -> sel_type) {
		  /* id_appl may be wrong, but inside a signature it's */
                  /* not used.                                         */
                  return(p);
              } else {
                  NODE * q = copynode(p);
                  q -> sig_done = SIG_UNKNOWN;
                  chgfld(&(q -> signature), NIL);
                  chgfld(&(q -> sel_type), new_sel_type);
		  q -> id_appl = NIL;
                  return(q);
              }
          }

          /* check for identifier in parameter list and */
	  /* substitute if appropriate.                 */
	  /* Replace signature transparent ids by r.h.s. */
	      i = 0;
	      idpos = 0;
	      if (p -> id_last_definition -> kind == PARAMETER) {
		maplist(s, params, {
                  i++;
		  if(is_declared_by(p,s)) {
                      idpos = i;
                  }    
		});
	      } else if (p -> id_last_definition != NIL
			 && p -> id_last_definition -> kind == DECLARATION
			 && p -> id_last_definition -> decl_sig_transp) {
		NODE * tmp = subst1(p -> id_last_definition -> decl_denotation,
				    params, args);
		if (tmp != p -> id_last_definition -> decl_denotation) {
		    return(tmp);
		}
		/* else treat it like any other identifier, so we don't */
		/* make extra copies.                                   */
	      }
              if (idpos != 0) {
		  /* return the idpos th element of args */
		      i = 0;
		      maplist(s, args, {
                          i++;
			  if(i == idpos) {
			      /* check if it includes an expression on the */
                              /* dontsubst list.                           */
                                if (on_dontsubst(s) != NIL) {
                                  substerr = on_dontsubst(s);
                                }
                              return(s);
                          }
                      });
	      } else {
		  if (dl_new_decl(p) != p -> id_last_definition) {
		    /* declaration changed - need to copy node */
		    NODE * q = copynode(p);
		    q -> id_last_definition = dl_new_decl(p);
		    if (q -> signature != ERR_SIG) {
		      q -> sig_done = SIG_UNKNOWN;
		      chgfld(&(q -> signature), NIL);
                    }
		    q -> id_appl = NIL;
		    return(q);
		  } else {
		      /* id_appl may be wrong, but inside a signature it's */
                      /* not used.                                         */
                      return(p);
                  }
	      }

	case MODPRIMARY:
	    /* Need to copy now so that local type id references get changed */
	    if (p -> mp_type_modifier == NIL) {
		/* forgetting node.  Can and should be discarded */
		return (subst1(p -> mp_primary, params, args));
	    }
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> mp_primary), subst1(p -> mp_primary, params, args));
	    chgfld(&(v -> mp_type_modifier),
	           subst1(p -> mp_type_modifier, params, args));
	    if (p -> mp_primary == v -> mp_primary &&
		p -> mp_type_modifier == v -> mp_type_modifier) {
		return(v);
	    } else if (p -> signature != ERR_SIG) {
		chgfld(&(v -> signature), NIL);
		v -> sig_done = SIG_UNKNOWN;
	    }
	    return(v);

        case RECORDCONSTRUCTION:
	    /* Need to copy now so that local type id references get changed */
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> rec_component_list),
                   subst1(p -> rec_component_list, params, args));
	    if (p -> signature != ERR_SIG) {
		chgfld(&(v -> signature), NIL);
		v -> sig_done = SIG_UNKNOWN;
	    }
	    return(v);

        case UNIONCONSTRUCTION:
            /* Need to copy now so that local type id references get changed */
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> prod_components),
                   subst1(p -> prod_components, params, args));
	    if (p -> signature != ERR_SIG) {
		chgfld(&(v -> signature), NIL);
		v -> sig_done = SIG_UNKNOWN;
	    }
	    return(v);

        case PRODCONSTRUCTION:
          /* treat parameter nodes as declarations, as for FUNCCONSTR below */
	  {
            NODE * new_prod = copynode(p);
            NODE * new_params = emptylist();

	    /* Build new list of parameters and add them to decl map */
              maplist(s, p -> prod_components, {
		v = copynode(s);
		add_dlist(s, v);
		addright(new_params, v);
	      });
	    /* Substitute into parameter signatures */
	      maplist(s, new_params, {
		chgfld(&(s -> par_signature),
		       subst1(s -> par_signature, params, args));
	      });
            /* replace components and clear signature */
              chgfld(&(new_prod -> prod_components), new_params);
              if (new_prod -> signature != ERR_SIG) {
                chgfld(&(new_prod -> signature), NIL);
                new_prod -> sig_done = SIG_UNKNOWN;
              }
            return(new_prod); 
	  }

        case TYPESIGNATURE:
	    /* Need to copy now so that local type id references get changed */
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> ts_clist),
		   subst1(p -> ts_clist, params, args));
	    return(v);

	/* The following constructs are treated specially to avoid */
	/* trying to substitute for declaring instances of ids     */
        /* Declarations are added to the list by the enclosing     */
        /* constructs.                                             */
	case TSCOMPONENT:
	    v = subst1(p -> tsc_signature, params, args);
	    if (v != p -> tsc_signature) {
		w = copynode(p);
		chgfld(&(w -> tsc_signature), v);
		return(w);
	    } else {
		return(p);
	    }

	case WITHLIST:
	    /* Don't try to substitute for local id */
	    v = subst1(p -> wl_component_list, params, args);
	    if (v != p -> wl_component_list) {
		w = copynode(p);
		chgfld(&(w -> wl_component_list), v);
		return(w);
	    } else {
		return(p);
	    }

	case EXPORTLIST:
	case HIDELIST:
	    v = subst1(p -> el_export_element_list, params, args);
	    if (v != p -> el_export_element_list) {
		w = copynode(p);
		chgfld(&(w -> el_export_element_list), v);
		return(w);
	    } else {
		return(p);
	    }


	case RECORDELEMENT:
	    v = subst1(p -> re_denotation, params, args);
	    if (v != p -> re_denotation) {
		w = copynode(p);
		chgfld(&(w -> re_denotation), v);
		return(w);
	    } else {
		return(p);
	    }

	case PARAMETER:
	    v = subst1(p -> par_signature, params, args);
	    if (v != p -> par_signature) {
		w = copynode(p);
		chgfld(&(w -> par_signature), v);
		return(w);
	    } else {
		return(p);
	    }

	case DECLARATION:
	  { NODE * new_sig, * new_den;

	    new_sig = subst1(p -> decl_signature, params, args);
	    new_den = subst1(p -> decl_denotation, params, args);
	    if (new_sig != p -> decl_signature
		|| new_den != p -> decl_denotation) {
		w = copynode(p);
		chgfld(&(w -> decl_signature), new_sig);
		chgfld(&(w -> decl_denotation), new_den);
		return(w);
	    } else {
		return(p);
	    }
	  }

	/* In the following two cases it is necessary to first      */
	/* add all declared identifiers to the list of declarations */
	/* This code will probably be executed at most once a 	    */
	/* century.  Therefore no attempt is made to avoid	    */
	/* unnecessary copies.					    */
	case BLOCKDENOTATION:
	  {
	    NODE * new_block = copynode(p);
   	    NODE * new_decls = emptylist();

	    /* Build new list of declarations and add them to map */
	      maplist(s, p -> bld_declaration_list, {
		v = copynode(s);
		add_dlist(s, v);
		addright(new_decls, v);
	      });
	    /* Substitute into declarations */
	      maplist(s, new_decls, {
		chgfld(&(s -> decl_signature),
		       subst1(s -> decl_signature, params, args));
		chgfld(&(s -> decl_denotation),
		       subst1(s -> decl_denotation, params, args));
	      });
	    /* substitute into body and replace declaration list */
	      chgfld(&(new_block -> bld_den_seq),
		     subst1(p -> bld_den_seq, params, args));
	      chgfld(&(new_block -> bld_declaration_list), new_decls);
	    /* Clear signature */
	      if (new_block -> signature != ERR_SIG) {
		chgfld(&(new_block -> signature), NIL);
		new_block -> sig_done = SIG_UNKNOWN;
	      }
	    return(new_block); 
	  }

	case FUNCCONSTR:
	  /* Check whether params corresponds to this function  */
	  /* construction.  If so, substitute no further, since */
	  /* a subsequent mention of an identifier in params    */
	  /* actually corresponds to the local declaration.     */
	  /* This can actually happen, as in                    */
	  /*    (Short with { I == func ...})$I [ ... ]         */
	  /* We check identity of first parameter, since others */
	  /* could have been added by inferargs.                */
	    if (!is_empty(p -> signature -> fsig_param_list)
		&& first(params) -> pre_num == 
		   first(p -> signature -> fsig_param_list) -> pre_num) {
#               ifdef TRACE
		    printf("Truncating substitution\n");
#               endif
		return(p);
	    }
	  {
	    NODE * new_func = copynode(p);
	    NODE * new_sig = copynode(p -> signature);
   	    NODE * new_params = emptylist();

	    /* Build new list of parameters and add them to decl map */
	      maplist(s, p -> signature -> fsig_param_list, {
		v = copynode(s);
		add_dlist(s, v);
		addright(new_params, v);
	      });
	    /* Substitute into parameter signatures */
	      maplist(s, new_params, {
		chgfld(&(s -> par_signature),
		       subst1(s -> par_signature, params, args));
	      });
            /* substitute into body, result sig, and replace signature */
              if (new_sig -> fsig_result_sig != ERR_SIG) {
                chgfld(&(new_sig -> fsig_result_sig),
                       subst1(new_sig -> fsig_result_sig, params, args));
              }
	      chgfld(&(new_sig -> fsig_param_list), new_params);
	      chgfld(&(new_func -> fc_body),
		     subst1(p -> fc_body, params, args));
	      chgfld(&(new_func -> signature), new_sig);
	    return(new_func); 
	  }

        case FUNCSIGNATURE:
	  {
            NODE * new_sig = copynode(p);
   	    NODE * new_params = emptylist();

	    /* Build new list of parameters and add them to decl map */
              maplist(s, p -> fsig_param_list, {
		v = copynode(s);
		add_dlist(s, v);
		addright(new_params, v);
	      });
	    /* Substitute into parameter signatures */
	      maplist(s, new_params, {
		chgfld(&(s -> par_signature),
		       subst1(s -> par_signature, params, args));
	      });
            /* substitute into body, result sig, and replace signature */
              if (new_sig -> fsig_result_sig != ERR_SIG) {
                chgfld(&(new_sig -> fsig_result_sig),
                       subst1(new_sig -> fsig_result_sig, params, args));
              }
	      chgfld(&(new_sig -> fsig_param_list), new_params);
            return(new_sig); 
	  }

	default:
	deflt:
            i = 0;
	    q = (NODE **) p;
	    sigv = stsigs[knd];
	    plinkv = stplinks[knd];
	    lim = stsize[knd];
	    for(; i < lim;
		(plinkv <<= 1, sigv <<= 1, q++, i++)) {
		if (plinkv < 0 && sigv >= 0 /* non-sig primary link */) {
		    tmpcopy[i] = subst1(*q, params, args);
                            /* should be locked but ... */
                    if(tmpcopy[i] != *q) {
                        mod_flag = TRUE;
                    }
                } else {
                    tmpcopy[i] = *q;
                            /* again ... */
                }
            }
	    if (mod_flag) {
                /* zero out signatures and clear sig_done fields */
		    for((sigv = stsigs[knd], i = 0); sigv != 0;
			(sigv <<= 1, i++)) {
			if (sigv < 0) {
                            tmpcopy[i] = 0;
                            tmpcopy[i+1] = SIG_UNKNOWN;
			}
		    }
	        return(copynode((NODE *)tmpcopy));
            } else {
                return(p);
            }
    }
}

/* Returns TRUE if the expression p is safe to substitute even if its */
/* signature can't be found.                                          */
/* Assumes that an attempt has been made to find the signature of p.  */
/* Thus at least some partial information is known.                   */
boolean trivial(p)
NODE * p;
{
    switch(p -> kind) {
        case LETTERID:
        case OPRID:
            if (!p -> id_def_found) {
                return(FALSE);
            }
            if (p -> sel_type == NIL) {
                return(TRUE);
            } else {
                return(trivial(p -> sel_type));
            }
        default:
            return(FALSE);
    }
}

/* Return a structure identical to p, but insure that nodes that will */
/* contain context sensitive information are copied, so that this     */
/* information can be distinct for different contexts.                */
/* Such information is relevant only for expressions that will be     */
/* evaluated.  Others are ignored.                                    */

NODE * unshare1();

NODE * unshare(p)
NODE * p;
{
    NODE * result;

    clr_dlist;
    result = unshare1(p);
    return(result);
}

NODE * unshare1(p)
NODE * p;
{
    boolean mod_flag = FALSE; /* One of the descendants was modified */
    register int knd;         /* kind field of node being examined   */
    int plinkv;               /* bit vector indicating primary links */
                              /* to be followed to recursively       */
			      /* substitute in subtrees              */
    int sigv;                 /* vector of signature pointers        */
    register NODE ** q;       /* pointer to link field to be         */
                              /* recursively examined.               */
    NODE * tmpcopy[MAXFIELDS];/* temporary version of result         */
    register NODE ** s;
    register int i;
    int lim;
    register struct cn * c;
    int j;
    NODE * v, * w;


    if (p == NIL) { return(NIL); }

    knd = p -> kind;
    switch(knd) {
        case LISTHEADER:
            i = 0;
            j = length(p);
            /* get sufficiently large chunk of memory to temporarily copy */
	    /* the list, thus minimizing number of allocations.           */
                if (j <= MAXFIELDS) {
                    s = tmpcopy;
                } else {
                    s = (NODE **) malloc(j * sizeof(NODE *));
                }
            maplist(q, p, {
		s[i] = unshare1(q);
                        /* should be locked but ... */
                if(s[i] != q) {
                    mod_flag = TRUE;
                }
                i++;
	    });
            if (mod_flag) {
              /* convert the temporary copy in s to a real list structure */
              /* and return it */
                NODE * result;
                result = emptylist();
                for (i = 0; i < j; i++) {
                    addright(result,s[i]);
		}
		if (j > MAXFIELDS) free(s);
                return(result);
	    } else {
		if (j > MAXFIELDS) free(s);
                return(p);
	    }

	case FUNCCONSTR:
	  /* Force a copy to be made */
	  {
	    NODE * new_func = copynode(p);
	    NODE * new_sig = copynode(p -> signature);
	    char * new_lbl = (char *) malloc(strlen(p -> fc_code_label)+8);
	    char buf[7];
	    static int fn_count = 0;
	    
	    chgfld(&(new_sig -> fsig_param_list),
		   unshare1(new_sig -> fsig_param_list));
	    chgfld(&(new_func -> signature), new_sig);
	    new_sig -> fsig_construction = new_func;
	    chgfld(&(new_func -> fc_body), unshare1(p -> fc_body));
	    /* Give it a new name.  We may need to generate code for it */
	    /* in different contexts.  Such code can be different,      */
	    /* because it could appear at a different level.            */
#             ifdef DEBUG
		if (p -> fc_code_label == NIL) {
		    dbgmsg("Unshare: missing label\n");
		    abort();
		}
#             endif
	      strcpy(new_lbl, p -> fc_code_label);
	      sprintf(buf, ".%d", fn_count++);
	      strcat(new_lbl, buf);
	      new_func -> fc_code_label = new_lbl;
	    return(new_func); 
	  }

	case DECLARATION:
	  /* Force a copy */
	  { NODE * new_sig, * new_den;
	    extern NODE * clear_construction();

	    new_sig = clear_construction(p -> decl_signature);
	    new_den = unshare1(p -> decl_denotation);
	    w = copynode(p);
	    chgfld(&(w -> decl_signature), new_sig);
	    chgfld(&(w -> decl_denotation), new_den);
	    w -> decl_sig_done = SIG_UNKNOWN;
	    return(w);
	  }

	case PARAMETER:
	  /* Force a top level copy.  Signature may be shared. */
	    v = copynode(p);
	    add_dlist(p, v);
	    return(v);

	case BLOCKDENOTATION:
	  /* First add to declaration list, then unshare subexpressions. */
	  {
	    NODE * new_block = copynode(p);
	    NODE * new_decls = emptylist();

	    /* Build new list of declarations and add them to map */
	      maplist(s, p -> bld_declaration_list, {
		v = copynode(s);
		add_dlist(s, v);
		addright(new_decls, v);
	      });
	    /* unshare declarations */
	      maplist(s, new_decls, {
		chgfld(&(s -> decl_denotation), unshare1(s -> decl_denotation));
	      });
	    /* Unshare body, and replace declaration list */
	      chgfld(&(new_block -> bld_den_seq), unshare1(p -> bld_den_seq));
	      chgfld(&(new_block -> bld_declaration_list), new_decls);

	    new_block -> bld_flags &= ~NO_SURR_LOOP;
	    return(new_block);
	  }

	case MODPRIMARY:
	    /* Need to copy */
	    v = copynode(p);
	    add_dlist(p,v);
	    chgfld(&(v -> mp_primary), unshare1(p -> mp_primary));
	    if (v -> mp_type_modifier != NIL) {
	      chgfld(&(v -> mp_type_modifier), unshare1(p -> mp_type_modifier));
	    }
	    v -> mp_no_surr_loop = FALSE;
	    return(v);

	case PRODCONSTRUCTION:
	case UNIONCONSTRUCTION:
	    /* No evaluable subexpressions.  No need to look further. */
	    return(p);

	case LETTERID:
	case OPRID:
	    if (p -> sel_type == NIL) {
		if ((w = dl_new_decl(p)) != p -> id_last_definition) {
		    /* Create a copy pointing to the copied declaration */
		    v = copynode(p);
		    v -> id_last_definition = w;
		    return(v);
		} else {
		    return(p);
		}
	    } else {
		v = unshare1(p -> sel_type);
		if (v != p -> sel_type) {
		    w = copynode(p);
		    chgfld(&(w -> sel_type), v);
		    return(w);
		} else {
		    return(p);
		}
	    }

	default:
	    i = 0;
	    q = (NODE **) p;
	    sigv = stsigs[knd];
	    plinkv = stplinks[knd];
	    lim = stsize[knd];
	    for(; i < lim;
		(plinkv <<= 1, sigv <<= 1, q++, i++)) {
		if (plinkv < 0 && sigv >= 0 /* non-sig primary link */) {
		    tmpcopy[i] = unshare1(*q);
                            /* should be locked but ... */
                    if(tmpcopy[i] != *q) {
                        mod_flag = TRUE;
                    }
                } else {
                    tmpcopy[i] = *q;
                            /* again ... */
                }
            }
	    if (mod_flag) {
#               ifdef VERBOSE
		  printf("Unshare copied node: ");
		  unparse_file = stdout;
		  unparse(p);
		  printf("\n");
#               endif
		/* Clear fsig_construction pointers. */
		    for((sigv = stsigs[knd], i = 0); sigv != 0;
			(sigv <<= 1, i++)) {
			if (sigv < 0) {
			    extern NODE * clear_construction();
			    NODE * s = tmpcopy[i];

			    tmpcopy[i] = clear_construction(s);
			}
		    }
		return(copynode((NODE *)tmpcopy));
            } else {
                return(p);
            }
    }
}
