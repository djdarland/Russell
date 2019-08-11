/*
 * Syntax Tree Structure Definitions
 *
 * This file is a collection of macro invocations
 *
 * The calling environment must contain definitions for
 *
 * TOP          called at the beginning
 * START        called to start each structure declaration
 * INT, UNSIGNED, REFCNT, NODEKIND, NODESTAR, VLINENO, STTINDX,
 *      LISTPTR, BACKREF,CNSTAR(= pointer to Cons Node),STRPTR,
 *	BITVECTOR, NBPTR, HNODESTAR (identical to NODESTAR except hidden,
 *      i.e. not passed to mknode ), SIG, and HSIG.
 *      (The latter two are used for pointers from expression
 *      nodes to signature nodes.)
 *              called to define individual fields
 * FINISH       called to finish each structure
 * BOTTOM       called to finish everything
 */

/* 
 *	Note that certain fields are not really part of the syntax tree,
 * but rather comprise the symbol table.  In particular each identifier
 * structure contains a field which eventually points to the last
 * declaration in the innermost enclosing scope of that identifier.
 * All constructs which can define a new identifier contain fields
 * used for chaining all definitions of a given identifier together.
 * If several of these constructs can occur in a given scope, i.e.
 * in the case of declarations and parameters, another field is
 * included to point back to some structure which uniquely identifies
 * that scope, so that illegal multiple declarations can subsequently
 * be detected.
 *  Identifier nodes contain an additional field identifying the
 * surrounding use list.  Use lists themselves are chained together
 * like identifier declarations.  Thus the list of types from which
 * selections can be inferred is easily identifiable.
 *  Also the first field of any structure which can possibly represent
 * a denotation is one for the signature of that denotation.  In general
 * this field is hidden and not used until the signature deduction phase.
 *  Any node which can declare an identifier (outside a signature)
 * contains displacement and level fields in the third and fourth positions
 * following the prefix.  The level number refers to function nesting
 * depth before the storage allocation pass of the code generator, and
 * is then changed to be activation record nesting depth.  (The two may
 * differ, since blocks occasionally require activation records.)
 *  Any signature field must be followed by a corresponding sig_done field.
 */


#define PREFIX \
  /* REFCNT(refcount) - we now garbage collect */ \
  NODEKIND(kind) \
  VLINENO(vlineno) \
  INT(pre_num) \
  INT(post_num) 

@ifndef STREEDEFS
@  define SZSTANDARDPREFIX 4
/* used to be 5, before we dropped refcount */
@endif

/*
 * Notes:
 *  
 */

TOP

START(LISTHEADER,ListHeader)
  PREFIX
  CNSTAR(lh_first)
  CNSTAR(lh_last)
FINISH

START(DECLARATION,Declaration)
  PREFIX
  NODESTAR(decl_id)
  NODESTAR(decl_denotation)
  INT(displacement)
  INT(level)
  SIG(decl_signature)
  INT(decl_sig_done)
  INT(decl_sig_transp)        /* This declaration is transparent to */
			      /* signature checking.                */
  BACKREF(decl_innermost_id)  /* decl of an innermost let-declared identifier*/
			      /* declared by r.h.s.  Used only for signature */
			      /* transparent declarations.  References       */
			      /* to signature transparent ids are counted    */
			      /* as refs to the right side of their          */
			      /* declarations for export rule checks.        */
			      
  BACKREF(decl_previous_definition)
  BACKREF(decl_scope)

  INT(decl_can_be_refd)       /* Smallest pre-order number of declaration  */
                              /* in this block which may require the value */
                              /* of the declared identifier.               */
			      /* Used to detect possible forward refs.     */
  INT(decl_special)           /* This is a variable which was unusually    */
			      /* allocated.                                */
@ ifndef STREEDEFS
@   define VAR_ON_STACK 7     /* Variable was directly stack allocated */
@       define SIMPLE_VAR_ON_STACK 1
@       define PTR_VAR_ON_STACK 2
@       define INIT_VAR_ON_STACK 4
@   define ARRAY_CONTIG 8     /* Array is contiguously allocated.      */
@   define ID_IMPORTED  16    /* Imported into nested scope            */
@   define VAR_NONTR_REF 32   /* Variable is referenced other than as  */
			      /* argument to V or :=                   */
		/* With the -R option, the -G code generator keeps     */
		/* identifier bindings or variables in v. registers:   */
@   define ID_IN_REG    64      /* displacement is v. register number  */
@   define SIMPLE_VAR_IN_REG   128
@   define PTR_VAR_IN_REG      256
@   define INIT_VAR_IN_REG     512
@   define VAR_IN_REG          896
	       /* r.h.s always evaluates to integer constant decl_const_val: */
@   define DECL_CONST         1024
@   define NOT_DECL_CONST     2048
@ endif
  INT(decl_sel_index)           /* index for with list components       */
  INT(decl_needed)              /* The rhs really needs to be evald     */
  INT(decl_const_val)
FINISH

START(PARAMETER,Parameter)
  PREFIX
  NODESTAR(par_id)
  NODESTAR(par_signature)
  INT(displacement)
  INT(level)
  BACKREF(par_previous_definition)
  BACKREF(par_scope)
  BACKREF(par_only_def)         /* The only argument passed as this  */
				/* parameter.  NIL indicates no call */
				/* found so far. MULTIPLE_DEFS       */
				/* indicates that different arguments*/
				/* are passe, or we don't know the   */
				/* identity of the argument.         */
				/* MULTIPLE_TP_DEFS indicates that   */
				/* different arguments are passed,   */
				/* but they are all types with       */
				/* standard := and V operations.     */
@ define MULTIPLE_DEFS ((NODE *) 1)
@ define MULTIPLE_TP_DEFS ((NODE *) 2)
@ define is_real_def(x) (((unsigned) (x)) > 2)
FINISH

START(RECORDELEMENT,RElement)
			/* also used for unions */
  PREFIX
  NODESTAR(re_id)
  NODESTAR(re_denotation)
  INT(re_New_index)
  INT(re_ValueOf_index)
  INT(re_assign_index)
FINISH

START(VARSIGNATURE,VarSignature)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  NODESTAR(var_denotation)
FINISH

START(VALSIGNATURE,ValSignature)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  NODESTAR(val_denotation)
FINISH

START(FUNCSIGNATURE,FSignature)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  BACKREF(fsig_construction)   /* Corresponding function construction */
			       /* (if known) - used for optimization  */
  INT(fsig_special)            /* Look at ststructs for definitions   */
  INT(fsig_slink_known)        /* ep can be found by tracing static   */
			       /* chain from environment in which     */
			       /* expression occurs.                  */
  STRPTR(fsig_inline_code)
  LISTPTR(fsig_param_list)
  NODESTAR(fsig_result_sig)
FINISH

START(TYPESIGNATURE,TSignature)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  INT(ts_simple_type)   /* Representation can't point (indirectly) to   */
			/* activation records.  (Used for optimization) */
  NODESTAR(ts_local_type_id)
  LISTPTR(ts_clist)
  BACKREF(ts_previous_definition)
  STRPTR(ts_const_code)            /* inline code for quoted single char  */
  STRPTR(ts_string_code)           /* inline code for string as a whole   */
  STRPTR(ts_element_code)          /* inline code for each string element */
  INT(ts_string_max)               /* maximum length for above string     */
				   /* expansion.  -1 ==> default          */
  STRPTR(ts_meta_concat)           /* inline code to concatenate string   */
				   /* sections of long string.            */
FINISH

START(TSCOMPONENT,TSComponent)
  PREFIX
  NODESTAR(tsc_id)
  NODESTAR(tsc_signature)
FINISH

START(DEFCHARSIGS,DefCharSigs)
/* Gives list of quoted characters with */
/* default signatures. Also used as a	*/
/* type signature component.                    */
/* Assumes ASCII, 32 bits per word              */
  PREFIX
  BITVECTOR(dcs_0)
  BITVECTOR(dcs_1)
  BITVECTOR(dcs_2)
  BITVECTOR(dcs_3)
@ ifndef STREEDEFS
@   define NVECTORS 4
@ endif
  HLISTPTR(dcs_exceptions)        /* List of DCSEXCEPTION nodes         */
				  /* for constants with non-nil special */
				  /* or in-line code values.            */
FINISH

START(SIGNATURESIG, SignatureSig)
				/* The signature of a signature */
  PREFIX
  HSIG(signature)
  INT(sig_done)
FINISH

START(BLOCKDENOTATION,BlDenotation)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  /* Activation record info, used only if INSIDE_LOOP and CONTAINS_CLOSURE */
  /* are both set, and the declaration list is not empty.                  */
    INT(ar_size)
    BACKREF(ar_static_link)
    INT(ar_static_level)
  LISTPTR(bld_declaration_list)
  LISTPTR(bld_den_seq)
  INT(bld_precedence)
  INT(bld_flags)
@   define INSIDE_LOOP       1   /* There is a sourrounding loop with no */
				 /* intervening blocks or function       */
				 /* abstractions.                        */
				 /* Also set if there is a possibility   */
				 /* that the block may be reexecuted     */
				 /* without leaving the surrounding      */
				 /* function due to Call/cc calls.       */
@   define CONTAINS_CLOSURE  2   /* A closure value is built inside the  */
				 /* block.                               */
@   define CALLCC_CALL       4   /* There may be an embedded Call/cc     */
				 /* call.                                */
@   define REQUIRES_AR       8   /* We need an a.r. if this is inside a  */
				 /* loop, and it either the block builds */
				 /* a closure, or there is a Call/cc     */
				 /* call embedded in the block.          */
				 /* These imply that it is not safe to   */
				 /* promote bindings to surrounding      */
				 /* function activation record.          */
@   define NO_SURR_LOOP     16   /* It is known that there is no         */
				 /* explicit loop between this block     */
				 /* and the surrounding function         */
				 /* construction.  Implies not           */
				 /* INSIDE_LOOP, but can be computed     */
				 /* much earlier.                        */
FINISH                                                                     


START(USELIST,UseList)
  PREFIX
  HSIG(signature)
  INT(sig_done)     /* signature checking done flag */
  LISTPTR(usl_type_list)
  LISTPTR(usl_den_seq)
  BACKREF(usl_previous_list)
  INT(usl_precedence)
FINISH

START(APPLICATION,Application)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  NODESTAR(ap_operator)
  LISTPTR(ap_args)
  BACKREF(ap_void_decl)   	/* enclosing var Void parameter */
FINISH

START(ENUMERATION,Enumeration)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  LISTPTR(enum_id_list)
FINISH

START(EXTENSION,Extension)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  NODESTAR(ext_denotation)
  INT(In_index)                 /* Position of In operation in type sign. */
  INT(Out_index)
FINISH

START(PRODCONSTRUCTION,Product)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  NODESTAR(prod_local_type_id)
  LISTPTR(prod_components)      /* a list of parameter nodes */
  BACKREF(prod_previous_definition)
FINISH

START(RECORDCONSTRUCTION,Record)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  LISTPTR(rec_component_list)
  BACKREF(rec_previous_definition)
FINISH

START(UNIONCONSTRUCTION,Union)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  NODESTAR(prod_local_type_id)
  LISTPTR(prod_components)       /* list of parameter nodes */
  BACKREF(prod_previous_definition)
FINISH

START(WITHLIST,WithList)
  PREFIX
  NODESTAR(wl_local_type_id)
  LISTPTR(wl_component_list)
  BACKREF(wl_previous_definition)
FINISH

START(MODPRIMARY,MPrimary)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  INT(displacement)
  INT(level)
  NODESTAR(mp_primary)
  NODESTAR(mp_type_modifier)    /* NIL if this represents a coercion */
  STRPTR(mp_delete_v)           /* bit vector specifying components  */
                                /* to be deleted.                    */
  INT(mp_orig_length)           /* number of type components before  */
                                /* deletions.                        */
  INT(mp_needed)		/* Need to actually construct value  */
  INT(mp_no_surr_loop)
FINISH

START(EXPORTLIST,Elist)
  PREFIX
  NODESTAR(el_local_type_id)
  LISTPTR(el_export_element_list)
  BACKREF(el_previous_definition)
FINISH

START(HIDELIST,Hlist)
  PREFIX
  NODESTAR(el_local_type_id)
  LISTPTR(el_export_element_list)
  BACKREF(el_previous_definition)
FINISH

START(EXPORTELEMENT,EElement)
  PREFIX
  NODESTAR(ee_id)
  NODESTAR(ee_signature)
  NODESTAR(ee_export_list)
FINISH

START(ALLCONSTANTS,ConstsKeyWord)
  PREFIX
FINISH

START(WORDELSE,ElseKeyWord)
  PREFIX
  HSIG(signature)
  INT(sig_done)
FINISH

START(WORDCAND,CandKeyWord)
  PREFIX
FINISH

START(WORDCOR,CorKeyWord)
  PREFIX
FINISH

START(GUARDEDLIST,GList)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  LISTPTR(gl_list)
FINISH

START(LOOPDENOTATION,LDenotation)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  LISTPTR(gl_list)
FINISH

START(GUARDEDELEMENT,GElement)
  PREFIX
  NODESTAR(ge_guard)
  NODESTAR(ge_element)
FINISH

START(OPRID,OpId)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  HNODESTAR(sel_type)         /* optional type denotation from which    */
			      /* identifier is to be selected.          */
  STTINDX(id_str_table_index) /* -1 ==> imm. surrounding local type id	*/
			      /* Note that the local type id is in fact */
			      /* always defined by a TYPESIGNATURE node */
			      /* since this convention is used only for */
			      /* id's with a default signature.         */
  BACKREF(id_use_list)
  BACKREF(id_last_definition) /* innermost definition of identifier     */
  INT(id_def_found)           /* TRUE ==> last_definition points to     */
			      /* actual definition rather than last     */
			      /* overloading of the identifier and the  */
			      /* sel_type field has its final value.    */
  INT(sel_index)
  BACKREF(id_appl)            /* An application of the identifier       */
  INT(id_forward_ref)         /* Need to check for erroneous forward ref */
FINISH

START(LETTERID,LetterId)
  PREFIX
  HSIG(signature)
  INT(sig_done)
  HNODESTAR(sel_type)
  STTINDX(id_str_table_index)
  BACKREF(id_use_list)
  BACKREF(id_last_definition)
  INT(id_def_found)
  INT(sel_index)
  HNODESTAR(id_appl)          /* An application of the identifier         */
			      /* Useful for overload resolution           */
  INT(id_forward_ref)       /* Need to check for erroneous forward ref */
FINISH

START(QSTR,QStr)
  /* quoted string */
  PREFIX
  HSIG(signature)
  INT(sig_done)
  HNODESTAR(sel_type)
  STRPTR(str_string)
  BACKREF(str_use_list)
  HNODESTAR(str_expansion)   
FINISH

START(UQSTR,UQStr)
  /* unquoted string */
  PREFIX
  HSIG(signature)
  INT(sig_done)
  HNODESTAR(sel_type)
  STRPTR(str_string)
  BACKREF(str_use_list)
  HNODESTAR(str_expansion)
FINISH

START(FUNCCONSTR,FConstruction)
  PREFIX
  NODESTAR(signature)   /* completed by signature deduction pass if */
			/* necessary.  */
  INT(sig_done)

  /* Activation record info */
    INT(ar_size)
    BACKREF(ar_static_link)
    INT(ar_static_level)

  NODESTAR(fc_body)
  INT(fc_complexity)    /* used to hold the following bits of dataflow */
			/* information:                                */
@ ifndef STREEDEFS
@   define COMPLICATED  0 /* No special properties */
@   define NO_SL        1 /* Static link not needed,       */
			  /* no "complicated" constructs,  */
			  /* no accesses through AR.       */
@   define NO_PUT       2 /* No calls to an output routine */
@   define NO_CALLCC    4 /* Environment can't be saved by   */
			  /* call/cc call.  This implies     */
			  /* that it's safe to allocate      */
			  /* variables directly on the stack */
@   define NO_CONSTR    8 /* There are no nested fn constrs.  */
			  /* that could conceivably be        */
			  /* exported.                        */
			  /* Thus it is safe to copy id       */
			  /* bindings into the closure itself */
@   define NEED_CL     16 /* Closure must be explicitly represented */
@   define CP_GLOBALS  32 /* Nonlocal bindings WILL BE copied to */
			  /* closure. Requires that NO_CONSTR    */
			  /* holds, that there aren't too many   */
			  /* non-locals, that the closure needs  */
			  /* to be explicitly built, that        */
			  /* none of the non-locals constitute   */
			  /* forward references, and that there  */
			  /* are no calls to the procedure that  */
			  /* could otherwise bypass evaluation   */
			  /* of the operator.                    */
@   define NO_AR_REFS  64 /* All nested fn constructions that require  */
			  /* a closure copy identifier bindings into   */
			  /* the closure itself.  Thus it is           */
			  /* automatically safe to allocate            */
			  /* activation record on the stack.           */
@   define DIR_REC    128 /* Known to call itself directly and */
			  /* tail-recursively.                 */   
@   define DIR_CALL   256 /* There is a direct call to this  */
			  /* procedure that does not require */
			  /* an explicit closure.            */
@   define SL_ACC     512 /* Procedure may indirect through   */
			  /* static link.  Thus it better be  */
			  /* there.  (Accesses through AR are */
			  /* possible even if SL_ACC is false)*/
@   define NESTED_AR_BLOCK 1024
			  /* Procedure contains a nested block that */
			  /* requires its own activation record.    */
@ endif
  HSTRPTR(fc_code_label)

  LBACKREF(fc_free_vars)        /* List of occurrences of distinct   */
				/* non-local identifiers inside the  */
				/* function.  Meaningful only in the */
				/* presence of CP_GLOBALS.  Ordering */
				/* in list determines ordering in    */
				/* closure.                          */
				/* References to identifiers that    */
				/* are global to the entire program  */
				/* are excluded.                     */

  INT(fc_body_needed)           /* need to generate code for body */
FINISH

START(FREEVARNODE,FreeVarNode)
				/* A list of these is pointed to	*/
  PREFIX			/* by each funcconstructor (i.e.	*/
				/* lambda abstraction)		  	*/
  BACKREF(fv_last_definition)	/* Pointer to declaration		*/
  INT(fv_surr_class)		/* Local/Global in surrounding scope	*/
  INT(fv_surr_index)		/* Variable Index in surrounding scope	*/
  INT(fv_index)			/* Free Variable Index (in closure)	*/
FINISH

START(EXTERNDEF,ExternDef)
				/* used as body of externally defined  */
				/* functions.                          */
  PREFIX
  STRPTR(ext_name)
FINISH

START(REXTERNDEF,RExternDef)
				 /* used as body of separately compiled  */
                                 /* Russell functions.                   */
  PREFIX
  SIG(signature)
  INT(sig_done)
  STRPTR(r_ext_name)
FINISH

START(DCSEXCEPTION, DCSException)
				   /* Single character type component with */
				   /* in-line code or special field.       */
  PREFIX
  INT(dcse_char)
  HSTRPTR(dcse_inline)
  INT(dcse_special)              /* Same conventions as fsig_special  */
  BACKREF(dcse_construction)     /* Must be BACKREF so that subst etc.*/
				 /* don't try to follow it.           */
FINISH

BOTTOM

@define STREEDEFS
