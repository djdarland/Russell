# define DEBUG

# ifdef DEBUG
#   define IFDEBUG(x) x
# else
#   define IFDEBUG(x)
# endif

# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "pass3/decl_pairs.h"

char * (* inline_cnvt)();

char * getname();

extern unsigned indx_subscr; /*DEBUG*/

extern int stplinks[];

extern int stsize[];

extern int stsigs[];

extern int yynerrs;  /* number of errors encountered in this pass */

# include "pass3/is_local.h"
# include "sigs.h"

int comp_index;

extern FILE * unparse_file;

NODE * tsubst();

extern NODE * sig_const;

/*
 *  getcomp(type_signature, id_node, type_expression,
 *          component_signature, enclosing_type, application, exact)
 * 
 *  Returns the signature of the component of the type_signature which
 * is pointed to by the second parameter (NIL if there is none).
 *  Either a signature or an application may be given to resolve
 * overloading.  Exact specifies whether argument signatures must
 * match exactly, i.e. whether coercions are allowed.
 *  The type expression (if any) is substituted for the local type identifier.
 * (The component_signature is matched after the substitution.)
 *  If no substitution is performed, an enclosing type may be given
 * for the component signature.  The argument list is ignored in this case.
 *  comp_index is set to the index of the component in the type
 * signature.
 */


NODE * getcomp(sig,id,exp,csig,ctype,appl,exact)
NODE * sig, * exp;
NODE * id;
NODE * csig, * appl;
boolean exact;
{
    register NODE * p;
    register boolean is_char;
                            /* ind refers to a single character quoted id */
    char * name;            /* identifier name */
    char character;         /* identifier name (non-quote char) when      */
			    /* is_char is true.                           */
    NODE * exp_sig;         /* expanded signature of component            */
    int ci;                 /* local version of comp_index                */
    int ind = id -> id_str_table_index;
    int i;
#   ifdef DEBUG
	boolean trace = FALSE;
#   endif

    if (sig == ERR_SIG) return(ERR_SIG);
#   ifdef DEBUG
#     ifdef VAX
	if (nargs() != 7) {
	    dbgmsg("getcomp: wrong number of args\n");
	    abort();
	}
#     endif
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("getcomp: bad type signature\n");
            abort();
	}
	if (appl != NIL && appl -> kind != APPLICATION) {
	    dbgmsg("getcomp: bad application\n");
	}
#   endif

    ci = 0;
    name = getname(ind);
    if(is_char = (name[0] == '\'' && name[2] == '\'')) character = name[1];
#   ifdef DEBUG
        if (trace) {
            unparse_file = stdout;
            printf("Looking in:\n");
            unparse(sig);
	    printf ("\nname = %s, is_char = %d, character = %c, appl = %X\n",
		     name, is_char, character, appl);
	}
#   endif
    begin_maplist(p, sig -> ts_clist) {
	if (p -> kind == TSCOMPONENT) {
	    if (p -> tsc_id -> id_str_table_index == ind) {
#               ifdef DEBUG
		  if (trace) {
		    printf("found matching id\n");
		  }
#               endif
		if (exp == NIL) {
#                   ifdef DEBUG
                      if (trace) {
                        printf ("No type expression\n");
                        unparse_file = stdout;
                        printf("Actual component signature:\n");
                        unparse(p -> tsc_signature);
                        printf("\n");
                        printf("\nSpecified component signature:\n");
                        unparse(csig);
                        printf("\n");
                        printf("\ntype signature:\n");
                        unparse(sig);
                        printf("\nlocal id refers to:\n");
                        unparse(ctype);
                        printf("\n");
                      }
#                   endif
		    if (csig == NIL
			|| comp_st(p -> tsc_signature,
				   csig,
				   sig,
				   ctype) == 0) {
#                       ifdef DEBUG
                            if (trace) {
                                printf ("Found it\n");
                                printf ("comp_index = %d\n", ci);
                            }
#                       endif
                        comp_index = ci;
			return(p -> tsc_signature);
		    } else {
#                       ifdef DEBUG
			    if (trace) printf ("Failed signature comp\n");
#                       endif
                        ci++;
		    }
		} else {
		  /* do substitution and then check */
#                   ifdef DEBUG
			if (trace) {
			    printf ("Substituting %X for type %X\n", exp, sig);
			}
#                   endif
		    exp_sig = tsubst(p -> tsc_signature, sig, exp, TRUE);
#                   ifdef DEBUG
                        if (trace) {
                            unparse_file = stdout;
			    printf("Type component signature:\n");
                            unparse(exp_sig);
                            printf("\nExpected signature:\n");
                            unparse(csig);
			    if (appl != NIL) {
			      printf("\nApplication:\n");
			      unparse(appl);
			      printf("\nargument signatures:\n");
			      maplist(s, appl -> ap_args, {
                                printf(",");
                                unparse(s -> signature);
                              });
                            }
                            printf("\n");
			}
#                   endif
		    if (def_match(exp_sig, csig, appl, id, exact)) {
#                       ifdef DEBUG
                            if (trace) {
                                printf ("Found it\n");
                                printf ("comp_index = %d\n", ci);
                            }
#                       endif
                        comp_index = ci;
			return(exp_sig);
		    } else {
#                       ifdef DEBUG
			    if (trace) printf ("Failed signature comp\n");
#                       endif
                        ci++;
		    }
		}
	    } else {
                ci++;
	    }
	}
	/* Check whether it is a character with a default signature */
	    if(p -> kind == DEFCHARSIGS) {

                unsigned word;
		int bitno;
		int wordno;
		unsigned * base = &(p -> dcs_0);
		unsigned * s;

		if (is_char) {
		    wordno = ((int) character) / WORDLENGTH;
		    word = *(base + wordno);
		    bitno = ((int) character) - wordno * WORDLENGTH;
		}

		if ( is_char && (((int) word) << bitno) < 0
		      && (exp == NIL ?
			    (exp_sig = sig_const, csig == NIL)
			    || (comp_st(sig_const, csig, sig, ctype) == 0)
			  : def_match(exp_sig = tsubst(sig_const,sig,exp,TRUE),
				      csig, appl, id, exact)) )  {
		    /* type component appears in this node */
		    /* update comp_index                   */
			for (s = base; s < base + wordno; s++){
                            ci += bitcnt(*s);
			}
			for (i = 0; i < bitno; i++) {
			    if ((((int) word) << i) < 0) 
                                ci++;
			}
		    if (sig -> ts_const_code != NIL) {
			char *t;
			NODE * s = exp_sig;
			exp_sig = copynode(s);
			vfree(s);
			t = (char *)malloc(strlen(sig -> ts_const_code)+NINCR);
			sprintf(t, sig -> ts_const_code, character, character);
			exp_sig -> fsig_inline_code = (* inline_cnvt)(t);
		    } else if (p -> dcs_exceptions != NIL) {
			/* See if this is an "exceptional" constant.  */
			/* If so, update special and in_line fields   */
			maplist(r, p -> dcs_exceptions, {
			    if (r -> dcse_char == character) {
				NODE * s = exp_sig;
				exp_sig = copynode(s);
				vfree(s);
				exp_sig -> fsig_inline_code = r -> dcse_inline;
				exp_sig -> fsig_special = r -> dcse_special;
				exp_sig -> fsig_construction = r -> dcse_construction;
			    } else if (r -> dcse_char > character) {
				break;
			    }
			});
		    }
                    comp_index = ci;
		    return(exp_sig);
		} else {
		    for (s = base; s < base + NVECTORS; s++) {
                        ci += bitcnt(*s);
		    }
		}
	    }
#       ifdef DEBUG
	    if(p -> kind != TSCOMPONENT && p -> kind != DEFCHARSIGS) {
		dbgmsg("getcomp: bad tsc\n");
	    }
#       endif
    } end_maplist;
    /* no matching component */
    return(NIL);
}


/*
 * tsig_length(type_signature)
 *  
 * returns the number of components in a type signature, and thus the
 * length of the runtime representation of that type.  Like everything
 * else here it expects the signature to be normalized.
 */
int
tsig_length(tsig)
NODE * tsig;
{
    int result = 0;
    NODE * p;
    unsigned * base, * s;

#   ifdef DEBUG
	if (tsig -> kind != TYPESIGNATURE) {
	    dbgmsg("tsig_length: bad type signature\n");
            abort();
        }
#   endif
    /* count number of constants */
	p = first(tsig -> ts_clist);
#       ifdef DEBUG
	    if (p -> kind != DEFCHARSIGS) {
		dbgmsg("tsig_length: abnormal type list\n");
	    }
#       endif
	base = &(p -> dcs_0);
	for (s = base; s < base + NVECTORS; s++) {
	     result += bitcnt(*s);
	}
    /* add the number of other components */
	result += length(tsig -> ts_clist) - 1;
	return(result);
}

extern NODE * substerr;


extern struct cn * dontsubst;

/*
 * tsubst (p, type_definition, expression, substm1)
 *
 *  Substitute expression for the local type identifier corresponding
 * to type_definition inside p.  Type_definition is either a type signature
 * or construction.
 * If substm1 is true then any id node
 * with a string table index of -1 refers to the local type id.
 *  We do not substitute inside a PROPER subexpression of p which is a copy
 * of type_definition.
 */

NODE * tsubst1();

int tsubst_count;   /* The number of tsubst1 calls started since the */
		    /* last tsubst call.  Used to handle top level   */
		    /* call slightly differently.                    */

NODE * tsubst(p,tsig,expr,substm1)
NODE * p, *tsig, *expr;
boolean substm1;
{
    NODE * result;

    clr_dlist;
    tsubst_count = 0;
    result = tsubst1(p, tsig, expr, substm1);
#   ifdef TRACE
	if (result != p) {
            unparse_file = stdout;
            printf("tsubst: replaced\n");
            unparse(p);
            printf("\nwith 0x%X\n", result);
            unparse(result);
            printf("\n");
	} else {
	    unparse_file = stdout;
	    printf("tsubst: no place to substitute ");
	    unparse(expr);
	    printf(" into ");
	    unparse(p);
	    printf("\n");
	}
#   endif
    return(result);
}

NODE * tsubst1(p,tsig,expr,substm1)
NODE * p, *tsig, *expr;
boolean substm1;
{
    boolean mod_flag = FALSE; /* One of the descendants was modified */
    register int knd;         /* kind field of node being examined   */
    int plinkv;               /* bit vector indicating primary links */
                              /* to be followed to recursively       */
			      /* substitute in subtrees              */
    int sigv;                 /* bit vector indicating sig links     */
    register NODE ** q;       /* pointer to link field to be         */
                              /* recursively examined.               */
    NODE ** s;                /* temporary copy of list.             */
    NODE * tmpcopy[MAXFIELDS];/* temporary version of result         */
    NODE *v, *w;              /* temporaries                         */
    register int i;
    register struct cn * c;
    int j;
    int lim;

    if (p == ERR_SIG || p == NIL) return(p);

    tsubst_count++;

    switch ( knd = (p -> kind) ) {
        case LISTHEADER:
            i = 0;
            j = length(p);
            /* get sufficiently large chunk of memory to temporarily copy */
            /* the list minimizing number of allocations.                 */
                if (j <= MAXFIELDS) {
                    s = tmpcopy;
                } else {
                    s = (NODE **) malloc(j * sizeof(NODE *));
                }
            maplist(q, p, {
		s[i] = tsubst1(q, tsig, expr, substm1);
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
                return(p);
            }
	case MODPRIMARY:
	    if (p -> pre_num == tsig -> pre_num) {
		/* Nested references refer to local definition */
#               ifdef TRACE
		    printf("Truncating subst at MODPRIMARY\n");
#               endif
		return(p);
	    }
	    /* Need to copy now so that local type id references get changed */
	    if (p -> mp_type_modifier == NIL) {
		/* forgetting node.  Can and should be discarded */
		return (tsubst1(p -> mp_primary, tsig, expr, substm1));
	    }
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> mp_primary),
		   tsubst1(p -> mp_primary, tsig, expr, substm1));
	    chgfld(&(v -> mp_type_modifier),
		   tsubst1(p -> mp_type_modifier, tsig, expr, substm1));
	    if (p -> mp_primary == v -> mp_primary &&
		p -> mp_type_modifier == v -> mp_type_modifier) {
	        return(v);
	    } else if (p -> signature != ERR_SIG) {
		chgfld(&(v -> signature), NIL);
		v -> sig_done = SIG_UNKNOWN;
	    }
	    return(v);

        case RECORDCONSTRUCTION:
	    if (p -> pre_num == tsig -> pre_num) {
		/* Nested references refer to local definition */
#               ifdef TRACE
		    printf("Truncating subst at RECORDCONSTRUCTION\n");
#               endif
		return(p);
	    }
	    /* Need to copy now so that local type id references get changed */
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> rec_component_list),
		   tsubst1(p -> rec_component_list, tsig, expr, substm1));
	    if (p -> signature != ERR_SIG) {
		chgfld(&(v -> signature), NIL);
		v -> sig_done = SIG_UNKNOWN;
	    }
	    return(v);

        case UNIONCONSTRUCTION:
	    if (p -> pre_num == tsig -> pre_num) {
		/* Nested references refer to local definition */
#               ifdef TRACE
		    printf("Truncating subst at UNIONCONSTRUCTION\n");
#               endif
		return(p);
	    }
            /* Need to copy now so that local type id references get changed */
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> prod_components),
                   tsubst1(p -> prod_components, tsig, expr, substm1));
	    if (p -> signature != ERR_SIG) {
		chgfld(&(v -> signature), NIL);
		v -> sig_done = SIG_UNKNOWN;
	    }
	    return(v);

        case PRODCONSTRUCTION:
	  if (p -> pre_num == tsig -> pre_num) {
	      /* Nested references refer to local definition */
#             ifdef TRACE
		  printf("Truncating subst at PRODCONSTRUCTION\n");
#             endif
	      return(p);
	  }
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
                       tsubst1(s -> par_signature, tsig, expr, FALSE));
	      });
            /* replace components and clear signature */
              chgfld(&(new_prod -> prod_components), new_params);
              if (new_prod -> signature != ERR_SIG) {
                chgfld(&(new_prod -> signature), NIL);
                new_prod -> sig_done = SIG_UNKNOWN;
              }
            return(new_prod); 
          }

	/* The following constructs are treated specially to avoid */
	/* trying to substitute for declaring instances of ids     */
        /* Declarations are added to the list by the enclosing     */
        /* constructs.                                             */
	case TSCOMPONENT:
	    v = tsubst1(p -> tsc_signature, tsig, expr, substm1);
	    if (v != p -> tsc_signature) {
		w = copynode(p);
		chgfld(&(w -> tsc_signature), v);
		return(w);
	    } else {
		return(p);
	    }

	case WITHLIST:
	    /* Don't try to substitute for local id */
	    v = tsubst1(p -> wl_component_list, tsig, expr, substm1);
	    if (v != p -> wl_component_list) {
		w = copynode(p);
		chgfld(&(w -> wl_component_list), v);
		return(w);
	    } else {
		return(p);
	    }

	case EXPORTLIST:
	case HIDELIST:
	    if (p -> pre_num == tsig -> pre_num) {
		/* Nested references refer to local definition */
#               ifdef TRACE
		    printf("Truncating subst at export or hide list\n");
#               endif
		return(p);
	    }
	    v = tsubst1(p -> el_export_element_list, tsig, expr, substm1);
	    if (v != p -> el_export_element_list) {
		w = copynode(p);
		chgfld(&(w -> el_export_element_list), v);
		return(w);
	    } else {
		return(p);
	    }

	case RECORDELEMENT:
	    v = tsubst1(p -> re_denotation, tsig, expr, substm1);
	    if (v != p -> re_denotation) {
		w = copynode(p);
		chgfld(&(w -> re_denotation), v);
		return(w);
	    } else {
		return(p);
	    }

	case PARAMETER:
	    v = tsubst1(p -> par_signature, tsig, expr, substm1);
	    if (v != p -> par_signature) {
		w = copynode(p);
		chgfld(&(w -> par_signature), v);
		return(w);
	    } else {
		return(p);
	    }

	case DECLARATION:
	  { NODE * new_sig, * new_den;

	    new_sig = tsubst1(p -> decl_signature, tsig, expr, substm1);
	    new_den = tsubst1(p -> decl_denotation, tsig, expr, substm1);
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
		       tsubst1(s -> decl_signature, tsig, expr, substm1));
		chgfld(&(s -> decl_denotation),
		       tsubst1(s -> decl_denotation, tsig, expr, substm1));
	      });
	    /* substitute into body and replace declaration list */
	      chgfld(&(new_block -> bld_den_seq),
		     tsubst1(p -> bld_den_seq, tsig, expr, substm1));
	      chgfld(&(new_block -> bld_declaration_list), new_decls);
	    /* Clear signature */
	      if (new_block -> signature != ERR_SIG) {
		chgfld(&(new_block -> signature), NIL);
		new_block -> sig_done = SIG_UNKNOWN;
	      }
	    return(new_block); 
	  }

	case FUNCCONSTR:
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
		       tsubst1(s -> par_signature, tsig, expr, substm1));
	      });
            /* substitute into body, result sig, and replace signature */
              if (new_sig -> fsig_result_sig != ERR_SIG) {
                chgfld(&(new_sig -> fsig_result_sig),
                    tsubst1(new_sig -> fsig_result_sig, tsig, expr, substm1));
              }
	      chgfld(&(new_sig -> fsig_param_list), new_params);
	      chgfld(&(new_func -> fc_body),
		     tsubst1(p -> fc_body, tsig, expr, substm1));
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
		       tsubst1(s -> par_signature, tsig, expr, substm1));
	      });
            /* substitute into result sig and change parameter list */
              if (new_sig -> fsig_result_sig != ERR_SIG) {
                chgfld(&(new_sig -> fsig_result_sig),
                    tsubst1(new_sig -> fsig_result_sig, tsig, expr, substm1));
              }
	      chgfld(&(new_sig -> fsig_param_list), new_params);
            return(new_sig); 
	  }

	case TYPESIGNATURE:
	    if (tsubst_count > 1) {
	      if (p == tsig) {
		/* Nested references refer to local definition */
#               ifdef TRACE
		    printf("Truncating subst at TYPESIGNATURE\n");
#               endif
		return(p);
	      }
	      substm1 = FALSE; /* -1 no longer refers to the right type sig */
	    }
	    /* Need to copy now so that local type id references get changed */
	    v = copynode(p);
	    add_dlist(p, v);
	    chgfld(&(v -> ts_clist),
		   tsubst1(p -> ts_clist, tsig, expr, substm1));
	    return(v);

        case OPRID:
        case LETTERID:
#           ifdef DEBUG
		if(!p -> id_def_found && p -> id_str_table_index != -1
		   && yynerrs == 0) {
		    dbgmsg("tsubst: unresolved identifier reference:%s\n",
			   getname(p -> id_str_table_index));
		    abort();
		}
#           endif
	    if(!p -> id_def_found && p -> id_str_table_index != -1) {
		/* undeclared identifier */
		return(p);
	    }
	    if (p -> id_last_definition != NIL
		&& p -> id_last_definition -> kind == DECLARATION
		&& p -> id_last_definition -> decl_sig_transp) {
		NODE * tmp = tsubst1(p -> id_last_definition -> decl_denotation,
				     tsig, expr, substm1);                    
		if (tmp != p -> id_last_definition -> decl_denotation) {    
		    return(tmp);                                           
		}                                                          
		/* else treat it like any other identifier, so we don't */ 
		/* make extra copies.                                   */
	    }
	    if(   p -> sel_type == NIL && p -> id_str_table_index != -1
		  && is_declared_by(p,tsig)
	       || (substm1 && p -> id_str_table_index == -1)) {
		/* substitute the expression */
		    /* check whether substitution is safe */
			c = dontsubst;
			while(c != NIL) {
			    if( is_descendant((NODE *)(cn_head(c)), expr) ) {
				substerr = (NODE *)cn_head(c);
			    }
			    c = cn_tail(c);
			}
                    return(expr);
	    }
	    if (p -> sel_type == NIL && p -> id_str_table_index != -1
		&& dl_new_decl(p) != p -> id_last_definition) {
                /* declaration changed - need to copy node */
                NODE * q = copynode(p);
                q -> id_last_definition = dl_new_decl(p);
		if (q -> signature != ERR_SIG) {
		    q -> sig_done = SIG_UNKNOWN;
		    chgfld(&(q -> signature), NIL);
		}
		q -> id_appl = NIL;
		return(q);
            }
            if (p -> sel_type == NIL) {
		/* id_appl may be wrong, but inside a signature it's */
                /* not used.                                         */
                return(p);
            } else {
                NODE * new_sel_type = tsubst1(p -> sel_type, tsig,
                                              expr, substm1);

                if (new_sel_type == p -> sel_type) {
		    /* id_appl may be wrong, but inside a signature it's */
                    /* not used.                                         */
                    return(p);
                } else {
                    NODE * q = copynode(p);
                    if (q -> signature != ERR_SIG) {
                        q -> sig_done = SIG_UNKNOWN;
                        chgfld(&(q -> signature), NIL);
                    }
                    chgfld(&(q -> sel_type), new_sel_type);
		    q -> id_appl = NIL;
                    return(q);
                }
            }

        /* else do the copy: */
    }
    i = 0;
    q = (NODE **) p;
    plinkv = stplinks[knd];
    sigv = stsigs[knd];
    lim = stsize[knd];
    for(; i < lim; (plinkv <<= 1, sigv <<= 1, q++, i++)) {
	if (plinkv < 0 && sigv >= 0 /* non-sig primary link */) {
	    tmpcopy[i] = tsubst1(*q, tsig, expr, substm1);
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
        /* clear signature and sig_done fields */
	    for((sigv = stsigs[knd], i = 0); sigv != 0;
		(sigv <<= 1, i++)) {
		if (sigv < 0) {
                    tmpcopy[i] = NIL;
                    tmpcopy[i+1] = SIG_UNKNOWN;
		}
	    }
	return(copynode(tmpcopy));
    } else {
        return(p);
    }
}


/*
 *  inscomp(type_signature, id_node, component_signature, enclosing_type)
 * 
 *  Insert a component with name given by id_node and the given signature
 * into the given type signature.  This is done in place, i.e. it
 * CLOBBERS THE ORIGINAL TYPE SIGNATURE.  (The dcs_exceptions list
 * is copied if it needs to be updated.)
 *  The appropriate substitutions of local type identifiers are made.
 *
 *  Sets comp_index to the position of the inserted component.
 */


void
inscomp(sig,id,csig,ctype)
NODE * sig, * id;
NODE * csig;
{
    register NODE * dcs;  /* DEFCHARSIGS component */
    register boolean is_char;
                            /* ind refers to a single character quoted id */
    char * name;            /* identifier name */
    char character;         /* identifier name (non-quote char) when      */
			    /* is_char is true.                           */
    unsigned ind = id -> id_str_table_index;
    int i;
    NODE * ntid;            /* new local type identifier node to substitute */
			    /* in csig                                      */

    if (sig == ERR_SIG) return;
#   ifdef DEBUG
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("inscomp: bad type signature\n");
            abort();
        }
#   endif

    dcs = first(sig -> ts_clist);
#   ifdef DEBUG
	if (dcs -> kind != DEFCHARSIGS) {
	    dbgmsg("inscomp: abnormal type list\n");
	}
#   endif
    comp_index = 0;
    name = getname(ind);
    if(is_char = (name[0] == '\'' && name[2] == '\'' && is_const(csig,ctype))) {
	character = name[1];
	/* invalidate inline info for constants */
	  sig -> ts_const_code = sig -> ts_string_code
			       = sig -> ts_element_code = NIL;
	{
	    unsigned * word;
	    int bitno;
	    int wordno;
	    unsigned * base = &(dcs -> dcs_0);
	    unsigned * s;

	    wordno = ((int) character) / WORDLENGTH;
	    word = base + wordno;
	    bitno = ((int) character) - wordno * WORDLENGTH;
	    /* update comp_index */
		for (s = base; s < base + wordno; s++){
		    comp_index += bitcnt(*s);
		}
		for (i = 0; i < bitno; i++) {
		    if ((((int) *word) << i) < 0) 
			comp_index++;
		}
	    /* set appropriate bit */
		*word |= 1 << (WORDLENGTH-1 - bitno);
	}
	/* Add it to exception list, if appropriate */
	if (special_tp(csig -> fsig_special) != NOT_SPECIAL
	    || csig -> fsig_inline_code != NIL
	    || csig -> fsig_construction != NIL) {
	    if (dcs -> dcs_exceptions == NIL) {
		initfld(&(dcs -> dcs_exceptions), emptylist());
	    } else {
		chgfld(&(dcs -> dcs_exceptions),
		       copylist(dcs -> dcs_exceptions));
	    }
	    add_dcse(dcs -> dcs_exceptions, character,
		     csig -> fsig_inline_code,
		     csig -> fsig_special, csig -> fsig_construction);
	}
    } else {
	unsigned * base = &(dcs -> dcs_0);
	unsigned * s;
	NODE * new_tsc;

	/* build new local type id to substitute in */
	    ntid = mknode(LETTERID, 0);
	    ntid -> id_last_definition = sig;
	    ntid -> id_def_found = TRUE;

	/* build new signature component */
	    new_tsc = mknode(TSCOMPONENT, id, NIL);
	    if (csig != ERR_SIG) {
	       if (ctype == NIL) {
		   initfld(&(new_tsc -> tsc_signature),
			   csig);
	       } else {
		   initfld(&(new_tsc -> tsc_signature),
			   tsubst(csig, ctype, ntid, TRUE));
	       }
	    } else {
		new_tsc -> tsc_signature = ERR_SIG;
	    }

	mapinslist(p, sig -> ts_clist, {
	    if (p == NIL) { 
		INSERT(new_tsc);
		return;
	    }
	    switch (p -> kind) {
	      case TSCOMPONENT:
		if ((i = strcmp(name, 
				getname(p->tsc_id->id_str_table_index))) < 0
		    || i == 0 && comp_st(csig,p->tsc_signature,ctype,sig) < 0) {
		    INSERT(new_tsc);
		    return;
		} else {
		    comp_index++;
		}
		break;
	      case DEFCHARSIGS:
		/* count number of constants */
		    for (s = base; s < base + NVECTORS; s++) {
			comp_index += bitcnt(*s);
		    }
		break;
	      IFDEBUG(
		default:
		    dbgmsg("getcomp: bad tsc\n");
	      )
	    }
	});
    }
}


/*
 *  delcomp(type_signature, deletion_vector)
 *
 *  Return a copy of type_signature with the components indicated by the
 * deletion vector (a bit vector given as a sequence of ints) deleted from
 * the type signature.
 *  Set changed_strings if either a constant or concatenation
 * is replaced.  Reset it otherwise.
 */

boolean changed_strings;

NODE *
delcomp(sig, delv)
NODE * sig;
int * delv;
{
    NODE * p;
    int delword, delbit;    /* current word and bit in deletion vector */
#   define INCPOS if (++delbit >= WORDLENGTH) { delword++; delbit = 0; }
#   define BITSET ((delv[delword] << delbit) < 0)

    changed_strings = FALSE;
    if (sig == ERR_SIG) return;
#   ifdef DEBUG
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("delcomp: bad type signature\n");
            abort();
        }
#   endif
    /* Make a new copy of the signature */
	sig = copynode(sig);
	{   NODE * clist = emptylist();
	    maplist(s, sig -> ts_clist, {
                if (s -> kind == DEFCHARSIGS) {
		    addright(clist, copynode(s));
		} else {
		    addright(clist, s);
		}
	    });
	    chgfld(&(sig->ts_clist), clist);
	}
    if (sig == ERR_SIG) return;
#   ifdef DEBUG
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("delcomp: bad type signature\n");
            abort();
        }
#   endif

    delword = delbit = 0;
    /* clear appropriate bits in DEFCHARSIGS node */
	p = first(sig -> ts_clist);
#       ifdef DEBUG
	    if (p -> kind != DEFCHARSIGS) {
		dbgmsg("delcomp: abnormal type list\n");
	    }
#       endif
	{
	    unsigned * word;
	    int bitno;
	    int wordno;
	    unsigned * base = &(p -> dcs_0);
	    long s;

	    for (wordno = 0; wordno < NVECTORS; wordno++) {
		word = base + wordno;
		for (bitno = 0; bitno < WORDLENGTH; bitno ++) {
		    s = (long)(*word << bitno);
		    if (s == 0) break; /* only an optimization */
		    if (s < 0 /* component present in signature */) {
			if (BITSET) {
			    /* delete this component */
				*word &= ~(1 << (WORDLENGTH-1 - bitno));
			    changed_strings = TRUE;
			}
			/* move to next component in deletion vector */
			INCPOS;
		    }
		}
	    }
	}
    /* delete components from rest of signature */
	mapinslist(p, sig -> ts_clist, {
	    if (p == NIL) { 
		break;
	    }
	    if (p -> kind == TSCOMPONENT) {
		if (BITSET) {
		    extern long indx_pconc;
		    extern long indx_sconc;

		    /* delete this component */
			DELETE;
		    if (p -> tsc_id -> id_str_table_index == indx_pconc
			|| p -> tsc_id -> id_str_table_index == indx_sconc) {
			changed_strings = TRUE;
		    }
		}
		/* move to next component in deletion vector */
		    INCPOS;
	    }
	    IFDEBUG(
		if(p -> kind != TSCOMPONENT && p -> kind != DEFCHARSIGS) {
		    dbgmsg("getcomp: bad tsc\n");
		}
	    )
	});
    return(sig);
}


/* Returns true if type signature sig contains at most one component with */
/* name corresponding to string table index ind.                          */
boolean
is_unique(sig,ind)
NODE * sig;
int ind;
{
    register NODE * p;
    register boolean is_char;
                            /* ind refers to a single character quoted id */
    char * name;            /* identifier name */
    char character;         /* identifier name (non-quote char) when      */
			    /* is_char is true.                           */
    int nfound = 0;         /* number of components with this name found  */
			    /* so far.                                    */

    if (sig == ERR_SIG) return(TRUE);
#   ifdef DEBUG
#     ifdef VAX
	if (nargs() != 2) {
	    dbgmsg("is_unique: wrong number of args\n");
	    abort();
	}
#     endif
        if (sig -> kind != TYPESIGNATURE) {
	    dbgmsg("is_unique: bad type signature\n");
            abort();
        }
#   endif

    name = getname(ind);
    if(is_char = (name[0] == '\'' && name[2] == '\'')) character = name[1];
    maplist(p, sig -> ts_clist, {
	if (p -> kind == TSCOMPONENT) {
            if (p -> tsc_id -> id_str_table_index == ind) {
                nfound++;
	    }
	}
	/* Check whether it is a character with a default signature */
	    if(p -> kind == DEFCHARSIGS) {

                unsigned word;
		int bitno;
		int wordno;
		unsigned * base = &(p -> dcs_0);
		unsigned * s;

		if (is_char) {
		    wordno = ((int) character) / WORDLENGTH;
		    word = *(base + wordno);
		    bitno = ((int) character) - wordno * WORDLENGTH;
		}

		if ( is_char && (((int) word) << bitno) < 0 ) {
                    /* type component appears in this node */
                    nfound++;
		}
	    }
	IFDEBUG(
	    if(p -> kind != TSCOMPONENT && p -> kind != DEFCHARSIGS) {
		dbgmsg("getcomp: bad tsc\n");
	    }
	)
    });
    return(nfound <= 1);
}
