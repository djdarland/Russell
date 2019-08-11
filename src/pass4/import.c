#define DEBUG
# include <stdio.h>
# include "parm.h"

# include "stree/ststructs.mh"

# include "sigs.h"
# include "../pass3/is_local.h"

boolean impure();

extern FILE * unparse_file;

extern int stplinks[];
extern int stsigs[];
extern int yynerrs;
extern boolean Lflag;
extern NODE * var_Void;

NODE * infunc = NIL;     /* innermost enclosing known type or function  */
			 /* denotation, or signature node.              */
			 /* NIL if there is none.                       */

NODE * insig = NIL;      /* enclosing signature, if any                 */

static int Clevel = -1;   /* Current procedure nesting level */


/* make sure that there are no import rule violations within the tree headed */
/* by p, and make sure that none of the identifiers are declared in          */
/* scope.   (scope may be NIL.)                                              */
/* Also set level fields for DECLARATION and MODPRIMARY nodes to reflect     */
/* procedure nesting depth.  (Other related fields are set by allocate in    */
/* pass5.)                                                                   */

import(p, scope)
NODE * p, * scope;
{
    register int * q;   /* pointer to next field of p to be recursively     */
                        /* examined.                                        */
    register int plinkv;/* bit vector specifying primary link fields of *p  */
                        /* shifted so that the most significant bit         */
                        /* corresponds to q.                                */
    register int sigv;  /* corresponding vector for signature pointers.     */
    NODE * old_infunc = infunc;
    NODE * old_insig = insig;
    int sig_kind;       /* kind of signature of p                           */
    boolean is_expression;
    NODE * def;

    if (p == NIL || p == ERR_SIG) return;

    switch(p -> kind) {
        case BLOCKDENOTATION:
        case USELIST:
        case APPLICATION:
        case ENUMERATION:
        case EXTENSION:
        case PRODCONSTRUCTION:
        case RECORDCONSTRUCTION:
        case UNIONCONSTRUCTION:
        case MODPRIMARY:
        case WORDELSE:
        case GUARDEDLIST:
        case LOOPDENOTATION:
        case OPRID:
        case LETTERID:
        case QSTR:
        case UQSTR:
        case FUNCCONSTR:
        case REXTERNDEF:
            is_expression = TRUE;
            break;

        default:
            is_expression = FALSE;
            break;
    }

    if (is_expression && has_sig(p) /* o.w. error or already in signature */) {
        sig_kind = p -> signature -> kind;
        if (sig_kind == TYPESIGNATURE) {
	    infunc = p;
	}
	if (p -> kind == FUNCCONSTR) {
	    if (impure(p -> signature)) {
		infunc = NIL;
	    } else {
                infunc = p;
            }
	}
    }

    switch( p -> kind ) {
        case OPRID:
        case LETTERID:
            if ( p -> sel_type != NIL ) {
		import( p -> sel_type, scope);
	    } else {
	      if ((def = p -> id_last_definition) == NIL) {
		  break;
	      }
              if (scope != NIL
		  && def -> kind == DECLARATION
		  && (   !(def -> decl_sig_transp)
			 && def -> decl_scope == scope
		      || def -> decl_sig_transp
			 && def -> decl_innermost_id != NIL
			 && def -> decl_innermost_id -> decl_scope == scope)) {
		errmsg1(p,
			"Identifier %s contained in signature outside its scope",
			def -> decl_sig_transp?
			getname(def -> decl_innermost_id -> decl_id
				    -> id_str_table_index)
			: getname(p -> id_str_table_index));
	      }
	      if (infunc != NIL) {
		if (insig == NIL && has_sig(p) &&
		    sig_kind == VARSIGNATURE  && !is_local(p, infunc)) {
                    errmsg1(p,
                            "Variable %s imported into function or type:",
                            getname(p -> id_str_table_index)
                    );
                    fprintf(stderr, "\t");
                    unparse_file = stderr;
                    unparse(infunc);
                    fprintf(stderr,"\n\tfunction or type signature:\n\t");
                    unparse(infunc -> signature);
                    fprintf(stderr, "\n");
		}
	      }
            }
	    break;

	case FUNCCONSTR:
	    Clevel++;
	    p -> ar_static_level = Clevel;
	    /* Don't look at signature */
	    import(p -> fc_body, scope);
	    Clevel--;
	    break;

	case APPLICATION:
	    import(p -> ap_operator, scope);
	    import(p -> ap_args, scope);
	    if (insig == NIL) {
	      /* Make sure that we didn't substitute a variable into */
	      /* the signature.                                      */
		insig = p -> signature;
		import(p -> signature, scope);
	    }
	    break;

	case BLOCKDENOTATION:
	    import(p -> bld_declaration_list, scope);
	    import(p -> bld_den_seq, scope);
	    if (insig == NIL) {
	      /* Check export rule */
		insig = p -> signature;
		import(p -> signature, p);
	    }
	    break;

	case DECLARATION:
	    p -> level = Clevel;
	    import(p -> decl_denotation, scope);
	    /* All expressions inside signatures produce types;  */
	    /* they may however not have signatures computed.    */
	    /* Illegally importing a variable inside a signature */
	    /* that's declared in the signature is benign and    */
	    /* not checked.                                      */
	    infunc = p -> decl_signature;
	    import(p -> decl_signature, scope);
	    break;

	case MODPRIMARY:
	    p -> level = Clevel;
	    import(p -> mp_primary, scope);
	    import(p -> mp_type_modifier, scope);
	    break;

	case QSTR:
	case UQSTR:
	    /* dont bother checking expansion */
	    break;

        default:
            /* recursively examine subtrees */
                if (is_list(p)) {
                    maplist(e, p, {
			import(e, scope);
                    });
                } else {
                    plinkv = stplinks[p -> kind];
                    sigv = stsigs[p -> kind];
                    q = (int *) p;
                    while ( plinkv != 0 ) {
                        if ( plinkv < 0 /* msb is set */ && sigv >= 0) {
			    import(*q, scope);
                        }
                        q++;
                        plinkv <<= 1;
                        sigv <<= 1;
                    }
                }
    }
    infunc = old_infunc;
    insig = old_insig;
}

/* determine whether function signature p refers to an impure function */
boolean impure(p)
NODE *p;
{
#   ifdef DEBUG
        if (p -> kind != FUNCSIGNATURE) {
            dbgmsg("Bad call to impure\n");
        }
#   endif
    if (!Lflag) {
        return(FALSE);
    } else {
        NODE * plist = p -> fsig_param_list;
    
        if (is_empty(plist)
            || comp_st(last(plist) -> par_signature,
                       var_Void,
                       NIL, NIL) != 0) {
            return(FALSE);
        } else {
            return(TRUE);
        }
    }
}
