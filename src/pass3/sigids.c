# define DEBUG

# define TRACE
# undef TRACE
/* Fix up references to identifiers within signatures.  Note          */
/* that by our specially contrived restrictions this can be done      */
/* before the signature deduction phase.                              */

/* This pass also fixes up the def_found                              */
/* of identifiers explicitly selected from a type.                    */

/* String expansions are added inside signatures.                     */

/* fc_code_label fields are filled in.                                */

/* Ap_void_decl fields are filled in.                                 */

/* fsig_slink_known fields are set for signature of function */
/* constructions.  They are then propagated by pass4.        */

/* NO_SURR_LOOPS flags are filled in for BLOCKDENOTATIONs.   */
/* no_surr_loop fields in MODPRIMARY's are set appropriately */


# include <stdio.h>
# include <ctype.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "../pass4/sigs.h"

# include "is_local.h"

char * getname();

extern FILE * unparse_file;
extern int stplinks[];
extern int yynerrs;
extern char * entry_name;

static int fn_count = 0;

static boolean insig = FALSE;     /* currently looking at signature         */

static NODE * curr_void_decl = NIL;

static NODE * curr_sig_transp = NIL;
				  /* Innermost signature transparent       */
				  /* declaration, whose r.h.s is currently */
				  /* being processed.                      */

static boolean in_loop = FALSE;

extern NODE * var_Void;

extern NODE * sig_Signature;

void check_sig();       /* Checks that an expression has the form of a */
			/* signature.                                  */


/* Fix up the subtree headed by p */
sigids(p)
NODE * p;
{
    register int * q;   /* pointer to next field of p to be recursively     */
                        /* examined.                                        */
    register int v;     /* bit vector specifying primary link fields of *p  */
                        /* shifted so that the most significant bit         */
                        /* corresponds to q.                                */
    boolean old_insig = insig;
    boolean old_in_loop = in_loop;
    NODE * old_void_decl = curr_void_decl;
    NODE * old_sig_transp = curr_sig_transp;

    if (p == NIL) return;
    switch( p -> kind ) {
        case OPRID:
        case LETTERID:
            if ( p -> sel_type != NIL ) {
		p -> id_last_definition = NIL;
		p -> id_def_found = TRUE;
		sigids( p -> sel_type);
	    } else if (insig) {
#               ifdef TRACE
		    printf("Finding declaration of %s, curr_sig_transp = %X, def = %X\n",
			   getname(p -> id_str_table_index), curr_sig_transp,
			   p -> id_last_definition);
#               endif
		if (p -> signature != NIL && p -> sig_done != SIG_DONE) {
		    /* user specifed signature */
		    errmsg0(p,
			    "Warning - signatures in signatures are ignored");
		    yynerrs--;
		}
		if (curr_sig_transp != NIL
		    && p -> id_last_definition != NIL) {
		    NODE * def = p -> id_last_definition;
		    NODE * imid = curr_sig_transp -> decl_innermost_id;

		    /* Update innermost id of curr_sig_transp */
		      if (def -> kind == DECLARATION) {
			NODE * id_def;

			/* Check that it's not a forward reference */
			  if (def -> post_num >= p -> post_num) {
			    errmsg1(p, "Forward reference to %s in === declaration",
				    getname(p -> id_str_table_index));
			  }
			if (def -> decl_sig_transp) {
			    id_def = def -> decl_innermost_id;
			} else {
			    id_def = def;
			}
			if (id_def != NIL &&
			    (imid == NIL
			     || is_descendant(id_def, imid -> decl_scope))) {
			    curr_sig_transp -> decl_innermost_id = id_def;
#                           ifdef TRACE
				unparse_file = stdout;
				printf("Setting innermost id of ");
				unparse(curr_sig_transp -> decl_id);
				printf(" to ");
				unparse(id_def -> decl_id);
				printf("\n");
#                           endif
#                           ifdef DEBUG
				if (id_def -> kind != DECLARATION) {
				    dbgmsg("Sigids: Bad decl_innermost_id\n");
				}
#                           endif
			}
		      }
		}
		/* MAY NEED TO BE SMARTER EVENTUALLY */
		if (p -> id_last_definition == NIL
		    && p -> id_str_table_index != -1) {
		    /* This hopefully excludes defining instances */
		    NODE * usl = p -> id_use_list;
		    NODE * sig, * def;

		    while (usl != NIL) {
			maplist(s, usl -> usl_type_list, {
			    if ((s -> kind != LETTERID && s -> kind != OPRID)
			     || s -> signature != NIL
			     || s -> sel_type != NIL) {
				initfld(&(p -> sel_type), s);
				goto fixed_it;
			    } else {
				def = s -> id_last_definition;
				if (def == NIL ||
				    (def -> kind != DECLARATION &&
				    def -> kind != PARAMETER) ||
				    (def -> kind == DECLARATION &&
				    def ->decl_signature == NIL)) {
				    initfld(&(p -> sel_type),s);
				    goto fixed_it;
				}
				sig = (def -> kind == PARAMETER) ?
				      def -> par_signature :
				      def -> decl_signature;
				if (sig -> kind == TYPESIGNATURE &&
				    hascomp(sig, p -> id_str_table_index)) {
				    initfld(&(p -> sel_type), s);
				    goto fixed_it;
				}
			    }
			});
			usl = usl -> usl_previous_list;
		    }
		    errmsg1(p,"%s undeclared",
			getname(p -> id_str_table_index));
		    /* make sure the next pass doesn't run into it again */
			p -> signature = ERR_SIG;
			p -> sig_done = SIG_DONE;
		    break;
		}
		fixed_it:
		    p -> id_def_found = TRUE;
		    sigids(p -> sel_type);
	    }
	    check_sig(p -> signature);
            sigids( p -> signature );
	    break;

	case QSTR:
	case UQSTR:
            if ( p -> sel_type != NIL ) {
		sigids( p -> sel_type);
		initfld(&(p -> str_expansion), expand_str(p));
	    } else if (insig) {
		/* MAY NEED TO BE SMARTER EVENTUALLY */
		NODE * usl = p -> str_use_list;
		NODE * sig, * def;

		while (usl != NIL) {
		    maplist(s, usl -> usl_type_list, {
			if ((s -> kind != LETTERID && s -> kind != OPRID)
			     || s -> signature != NIL
			     || s -> sel_type != NIL) {
			    initfld(&(p -> sel_type), s);
			    goto str_fixed_it;
			} else {
			    def = s -> id_last_definition;
			    if (def == NIL ||
				(def -> kind != DECLARATION &&
				 def -> kind != PARAMETER) ||
				(def -> kind == DECLARATION &&
				 def ->decl_signature == NIL)) {
				initfld(&(p -> sel_type),s);
				goto str_fixed_it;
			    }
			    sig = (def -> kind == PARAMETER) ?
				  def -> par_signature :
				  def -> decl_signature;
			    if (sig -> kind == TYPESIGNATURE &&
				hasstring(sig, p)) {
				initfld(&(p -> sel_type), s);
				goto str_fixed_it;
			    }
			}
		    });
		    usl = usl -> usl_previous_list;
		}
		/* no suitable type for the implied selection was found */
		switch(p -> kind) {
		    case QSTR:
			errmsg1(p, "No appropriate type for \"%s\" inside signature",
				p -> str_string);
			break;
		    case UQSTR:
			errmsg1(p, "No appropriate type for %s inside signature",
				p -> str_string);
			break;
		}
		p -> signature = ERR_SIG;
		p -> sig_done = SIG_DONE;
		break;
	    str_fixed_it:
		initfld(&(p -> str_expansion), expand_str(p));
            }
	    break;

	case RECORDCONSTRUCTION:
                maplist(s, p -> rec_component_list, {
		    sigids(s -> re_denotation);
		});
	    break;

	case FUNCCONSTR:
            /* give it a reasonable name */
              if (p -> fc_code_label == NIL) {
#               define FN_LN_LEN 16
                char * fn_name =
                  (char *) malloc(strlen(entry_name)+FN_LN_LEN);

                findvl(p -> vlineno);
		sprintf(fn_name,"fn_%s.ln%d_%d",entry_name,getrl(),fn_count++);
                p -> fc_code_label = fn_name;
	      }
	    in_loop = FALSE;
	    /* look for var Void parameters, check that par. sigs are legit. */
		maplist(s, p -> signature -> fsig_param_list, {
		    check_sig(s -> par_signature);
		    if (comp_st(s -> par_signature,
				var_Void, NIL, NIL) == 0) {
                        curr_void_decl = s;
                    }
		});
		insig = TRUE;
		maplist(s, p -> signature -> fsig_param_list, {
		    sigids(s -> par_signature);
		});
		check_sig(p -> signature -> fsig_result_sig);
		sigids(p -> signature -> fsig_result_sig);
		insig = old_insig;
		sigids(p -> fc_body);
		p -> signature -> fsig_slink_known = TRUE;
		    /* static link is available in this context  */
		    /* will be cleared if function is used in    */
		    /* a context in which it's not available.    */
	    break;

	case APPLICATION:
            sigids(p -> ap_operator);
	    sigids(p -> ap_args);
	    p -> ap_void_decl = curr_void_decl;
	    break;

	case BLOCKDENOTATION:
	    if (!in_loop) {
		p -> bld_flags |= NO_SURR_LOOP;
	    }
	    sigids(p -> bld_declaration_list);
	    sigids(p -> bld_den_seq);
	    break;

	case DECLARATION:
            if (p -> decl_denotation -> kind == FUNCCONSTR
		&& p -> decl_id -> kind == LETTERID
		&& p -> decl_denotation -> fc_code_label == NIL) {
	      /* try to give it a reasonable name */
#               define FN_NAME_LEN 10
		char * id_name = getname(p -> decl_id -> id_str_table_index);
		if (id_name[0] != '\'') {
		  char * fn_name =
		    (char *) malloc(strlen(id_name)
				    +strlen(entry_name)+FN_NAME_LEN);
		  sprintf(fn_name,"fn_%s.%s_%d",entry_name,id_name,fn_count++);
		  p -> decl_denotation -> fc_code_label = fn_name;
		}
	    }
	    insig = TRUE; 
	    check_sig(p -> decl_signature);
	    sigids(p -> decl_signature);
	    if (!p -> decl_sig_transp) {
		insig = old_insig;
	    } else {
		/* Otherwise treat the denotation as a signature */
		curr_sig_transp = p;
	    }
	    sigids(p -> decl_denotation);
            break;

	case TSCOMPONENT:
	    check_sig(p -> tsc_signature);
            sigids(p -> tsc_signature);
            /* skip tsc_id */
	    break;

	case EXPORTELEMENT:
	    sigids(p -> ee_export_list);
	    insig = TRUE;
	    check_sig(p -> ee_signature);
	    sigids(p -> ee_signature);
	    break;

	case REXTERNDEF:
	    /* Don't bother with the signature */
	    break;

	case PARAMETER:
	    insig = TRUE;
	    check_sig(p -> par_signature);
            sigids(p -> par_signature);
            /* skip par_id */
	    break;

	case LOOPDENOTATION:
	    in_loop = TRUE;
	    goto dflt;
	    
	case MODPRIMARY:
	    p -> mp_no_surr_loop = !in_loop;
	    goto dflt;

	case TYPESIGNATURE:
	case FUNCSIGNATURE:
	case VALSIGNATURE:
	case VARSIGNATURE:
            insig = TRUE;
            /* and now fix up subtrees */

	default:
	dflt:
            /* recursively examine subtrees */
                if (is_list(p)) {
                    maplist(e, p, {
                        sigids(e);
                    });
                } else {
                    v = stplinks[p -> kind];
                    q = (int *) p;
                    while ( v != 0 ) {
                        if ( v < 0 /* msb is set */) {
                            sigids(*q);
                        }
                        q++;
                        v <<= 1;
                    }
                }
    }
    insig = old_insig;
    curr_void_decl = old_void_decl;
    curr_sig_transp = old_sig_transp;
}

/* Check whether q is an explicit signature node, or an identifier bound, */
/* via signature transparent declarations, to a signature.                */
void check_sig(q)
NODE *q;
{
    register NODE *p = q;

    if (p == NIL) return;
    while (   (p -> kind == LETTERID || p -> kind == OPRID)
	   && p -> sel_type == NIL
	   && p -> id_last_definition != NIL
	   && p -> id_last_definition -> kind == DECLARATION
	   && p -> id_last_definition -> decl_sig_transp
	   && p -> id_last_definition -> post_num < p -> post_num) {
	    p = p -> id_last_definition -> decl_denotation;
    }
    switch (p -> kind) {
	case SIGNATURESIG:
	case VALSIGNATURE:
	case VARSIGNATURE:
	case FUNCSIGNATURE:
	case TYPESIGNATURE:
	    break;
	default:
	    if (p -> kind == LETTERID || p -> kind == OPRID) {
		if ( p -> id_last_definition == NIL
		     || p -> id_last_definition -> kind != PARAMETER
		     || comp_st(p -> id_last_definition -> par_signature,
				sig_Signature, NIL, NIL) != 0) {
		  errmsg1(q,
			  "Identifier %s not meaningfully bound to a signature",
			  getname(q -> id_str_table_index));
		}
	    } else {
		errmsg0(q, "Signature expected");
	    }
    }
}
