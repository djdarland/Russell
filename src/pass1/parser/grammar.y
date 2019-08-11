/*        YACC INPUT TO GENERATE FIRST PASS OF RUSSELL PARSER           */

/* Error recovery assumes modified parser. The parser variable yychar   */
/* is referenced but not altered.                                       */

/* The syntax tree is built during the parse. No other semantic         */
/* actions are performed.                                               */

/* Denotations are only incompletely parsed using the yacc parser.      */
/* Mkappl is then called to refine the parse.                           */

/* Note: Nonterminals consisting only of lower case letters generate    */
/* the empty string. They are introduced only to perform appropriate     */
/* semantic actions.                                                    */


%token
CAND CHARACTERS CONSTANTS COR DO ELSE ELSIF END ENUM EXPORT 
FI FIELD FUNC HIDE IF IN EXTEND LET NI OD 
READONLY RECORD THEN TYPE UNION USE VAL VAR 
WITH RIGHT_ARROW EQUALS_EQUALS EQUALS_EQUALS_EQUALS COLON WORDID OPID
PROD QSTRING UQSTRING LEFT_ANGLE_BRACKET RIGHT_ANGLE_BRACKET EXTERN
SIGNATURE

%left COR
%left CAND
%{
#   define YYDEBUG 1

#   include <stdio.h>
#   include "parm.h"
#   include "pass1/applinfer/precedence.h"
#   include "stree/ststructs.mh"
#   include "rcs.h"    /* bit vectors defining RUSSELL character set */
#   include "../stt/sttdefs.h"
#   include "../../pass4/sigs.h"
#   include <a.out.h>

/* The following should really be handled with %union, 	*/
/* but that would be lots of work to fix.							*/
typedef NODE * NodeStar;
#   define YYSTYPE NodeStar

#   define maxskip 8 /* maximum number of tokens to be skipped in error */
                     /* recovery, other than in denotation sequences    */

#   ifdef BAD
        extern boolean BADflag;
#   endif

#   ifdef DEBUG
	int yydebug;
#   endif

#   define LIST_END 0x7fff
    int paramstop []   /* stop symbols for parameter error recovery */
    =   { ';',
          ']',
          '{',
          VAR,
          VAL,
          FUNC,
	  TYPE,
	  SIGNATURE,
	  EOF,
	  LIST_END     };
    int denseqstop []   /* stop symbols for statement list error recovery */
    =   { ';',
	  '#',
	  ELSIF,
	  ELSE,
          FI,
          OD,
          ')',
          NI,
          '}',
          IF,
          DO,
          LET,
          FUNC,
          '(',
          WITH,
          IN,
	  EOF,
	  LIST_END       };
    int condstop []     /* stop symbols for error recovery in various       */
                        /* conditional constructs                           */
    =   {  OD,
           FI,
           RIGHT_ARROW,
           '#',
           ';',
           LET,
           DO,
	   IF,
	   THEN,
	   ELSE,
           '(',
	   EOF,
	   LIST_END       };
    int declstop []     /* stop symbols for error recovery in declarations  */
    =   { EQUALS_EQUALS,
	  EQUALS_EQUALS_EQUALS,
          ';',
          '}',
          IN,
          NI,
          IF,
          DO,
          LET,
          '(',
	  EOF,
	  LIST_END        };

    NODE * sig_in();    /* Read signature info from object file */

    extern boolean initflag;    /* This is a compiler initialization run */
    extern boolean OOflag;      /* Save info for intermediate code optimizer */
    NODE * insrtptr = NIL;  /* pointer to dummy BLOCKDENOTATION vertex into */
			    /* which the syntax tree is to be inserted.     */
			    /* Set up by a previous initialization run of   */
			    /* compiler.                                    */
    extern NODE * id_Boolean;  /* LETTERID node in which the parameter node  */
                               /* for Boolean must be inserted.              */
    extern NODE * id_Void;     /* Analogous node for Void                    */
    extern NODE * id_Integer;  /* Analogous node for Integer                 */
    extern NODE * id_Null;     /* Analogous for Null                         */
    extern NODE * appl_Null;   /* Node corresponding to application of Null  */
    extern sttrelptr indx_Boolean; /* string table index of 'Boolean' */
    extern sttrelptr indx_Integer;
    extern sttrelptr indx_Void;
    extern sttrelptr indx_simple;
    extern sttrelptr indx_standard;
    extern sttrelptr indx_inline;

    extern sttrelptr indx_put;
    extern sttrelptr indx_Callcc;
    extern sttrelptr indx_Array;
    extern sttrelptr indx_Null;

    NODE * stxtree = NIL; /* pointer to root of syntax tree */

    unsigned i; /* temporary */
    extern sttrelptr     /* indices in string table of type components with   */
			 /* predefined signatures.                            */
	indx_assign,
	indx_passign,
	indx_massign,
        indx_equals,   
        indx_less,
        indx_greater,
        indx_le,
        indx_ge,
        indx_ne,
	indx_New,
	indx_ptr_New,
	indx_init_New,
	indx_ValueOf,
	indx_sconc,
	indx_pconc;

    NODE * sig_assign, /* pointers to trees representing predefined */
         * sig_equals, /* signatures.                               */
         * sig_less,
         * sig_greater,
         * sig_New,
         * sig_const,
	 * sig_ValueOf,
	 * sig_sconc,
	 * sig_pconc,
	 * sig_Signature;

extern char * (getname());

extern NODE * mkappl();

extern char * (* inline_cnvt)();

boolean has_externs; /* Mentions separately compiled Russell programs */

/* Defines to establish and test the presence of an optional element */
#   define PRESENT ((NodeStar)1)
#   define NOTPRESENT 0
#   define is_present(x)  ((x) == PRESENT)
%}

%%
Program :
	{ $$ = insrtptr; /* Remember original value */ }
    Denotation =
        { if (stxtree != NIL) {
            /* insert new syntax tree in standard prologue */
		if ($1 != NIL)
		    chgfld(&($1 -> bld_den_seq),mklist($2, -1));
                else
		    yyperror("No insertion marker in standard prologue");
          } else {
	    stxtree = lock($2);
          }
        }
    | error
	{  yyperror("Syntax error");  }
    | error
	{  yyperror("Syntax error");  } 
      Denotation.seq
    ;
Parameters :
    Id.list  Opt.Colon Signature  =
	{ 
	    NODE *p;

	    if (!$2) {
		yyperror("Missing colon in parameter specification");
	    }
            lock($1); lock($3);
            if (first($1) -> id_str_table_index == indx_Boolean 
                && (id_Boolean -> id_last_definition == NIL)) {
                    $$ = p = split(PARAMETER, $1, $3);
		    chgfld ( &(id_Boolean -> id_last_definition), first(p) );
		    /* this is the actual definition */
                    id_Boolean -> id_def_found = TRUE;
                    if (!initflag) {
                        yywarn ("No builtin type Boolean");
                    }
            } else if (first($1) -> id_str_table_index == indx_Void 
                && (id_Void -> id_last_definition == NIL)) {
                    $$ = p = split(PARAMETER, $1, $3);
                    chgfld ( &(id_Void -> id_last_definition), first(p) );
                    id_Void -> id_def_found = TRUE;
                    if (!initflag) {
                        yywarn ("No builtin type Void");
                    }
            } else if (first($1) -> id_str_table_index == indx_Integer 
                && (id_Integer -> id_last_definition == NIL)) {
                    $$ = p = split(PARAMETER, $1, $3);
                    chgfld ( &(id_Integer -> id_last_definition), first(p) );
                    id_Integer -> id_def_found = TRUE;
                    if (!initflag) {
                        yywarn ("No builtin type Integer");
                    }
	    } else if (first($1) -> id_str_table_index == indx_Null 
		&& (id_Null -> id_last_definition == NIL)) {
                    $$ = p = split(PARAMETER, $1, $3);
		    chgfld ( &(id_Null -> id_last_definition), first(p) );
		    id_Null -> id_def_found = TRUE;
                    if (!initflag) {
			yywarn ("No builtin constant Null");
                    }
            } else {
                $$ = split(PARAMETER, $1, $3);
            }
            vfree(unlock($1));  vfree(unlock($3));
        }
    | Signature  =
        {  $$ = mklist( mknode(PARAMETER,NIL,$1), -1 ); }
    | error
	      { int i = 0;
                int c;

		yyperror("Error in Parameter Specification");
                for ( c = yychar; !in(c, paramstop); c = yylex() )
                    if ( ++i > maxskip ) break;
		yyclearin;
		switch (c) {
                    case VAL:
                    case VAR:
                    case FUNC:
                        yyunlex(c); yyunlex(';'); break;
                    case '{':
                        yyunlex(c); yyunlex(WORDID); yyunlex(VAL);
                        yyunlex(']'); break;
                    case ';':
                    case ']':
			yyunlex(c); break;
		    case EOF:
			exit(1);
                    default: /* recover at outer level */
                        yyunlex(c); yyunlex(']'); 
                }
#               ifdef DEBUG
                    if ( yydebug ) rec_on(c);
#               endif
		$$ = emptylist();
	      }

    ;
Opt.Colon :
    COLON
	{ $$ = (NodeStar)TRUE; }
    |
	{ $$ = (NodeStar)FALSE; }
    ;
Opt.Parameters :
    Parameters  =
        { $$ = $1; }
    |  =
        { $$ = emptylist(); }
    ;
Opt.Parameter.list :
    Opt.Parameters =
        { $$ = $1; }
    | Opt.Parameter.list ';' Opt.Parameters =
        { $$ = conc($1,$3); }
    ;
Id.list :
    ID  =
        { $$ = mklist($1, -1); }
    | Id.list ',' ID  =
        { $$ = addright($1,$3); }
    ;
Signature : 
    ID
	{ $$ = $1; }
    | ManifestSignature
	{ $$ = $1; }
    ;
ManifestSignature :
    SIGNATURE
	{
	  $$ = sig_Signature;
	}
    | VAR BasicDenotation  = 
	{
	  $$ = mknode(VARSIGNATURE,$2);
	}
    | VAL BasicDenotation =
	{
	  $$ = mknode(VALSIGNATURE,$2);
	}
    | FuncSignature  =
        { $$ = $1; }
    | TYPE '(' Opt.Simple CONSTANTS QSTRING QSTRING QSTRING UQSTRING QSTRING ')'
      Opt.Id  '{' Opt.TypeSignatureComponent.list '}'  =
	{ 
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  $$ = mknode(TYPESIGNATURE,$11,$13,$5,$6,$7,$9);
	  $$ -> ts_string_max = atoi($8);
	  $$ -> ts_simple_type = (int)$3;
	}
    | TYPE '(' Opt.Simple CONSTANTS QSTRING QSTRING QSTRING UQSTRING ')'
      Opt.Id  '{' Opt.TypeSignatureComponent.list '}'  =
	{ 
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  $$ = mknode(TYPESIGNATURE,$10,$12,$5,$6,$7,NIL);
	  $$ -> ts_string_max = atoi($8);
	  $$ -> ts_simple_type = (int)$3;
	}
    | TYPE '(' Opt.Simple CONSTANTS QSTRING QSTRING QSTRING ')'
      Opt.Id  '{' Opt.TypeSignatureComponent.list '}'  =
	{ 
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  $$ = mknode(TYPESIGNATURE,$9,$11,$5,$6,$7,NIL);
	  $$ -> ts_string_max = -1;
	  $$ -> ts_simple_type = (int)$3;
	}
    | TYPE '(' Opt.Simple CONSTANTS QSTRING ')'
      Opt.Id  '{' Opt.TypeSignatureComponent.list '}'  =
	{ 
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  $$ = mknode(TYPESIGNATURE,$7,$9,$5,NIL,NIL,NIL);
	  $$ -> ts_string_max = -1;
	  $$ -> ts_simple_type = (int)$3;
	}
    | TYPE '(' Opt.Simple ')'
      Opt.Id  '{' Opt.TypeSignatureComponent.list '}'  =
	{ 
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  $$ = mknode(TYPESIGNATURE,$5,$7,NIL,NIL,NIL,NIL);
	  $$ -> ts_string_max = -1;
	  $$ -> ts_simple_type = (int)$3;
	}
    | TYPE  Opt.Id  '{' Opt.TypeSignatureComponent.list '}'  =
	{ 
	  $$ = mknode(TYPESIGNATURE,$2,$4,NIL,NIL,NIL,NIL);
	  $$ -> ts_string_max = -1;
	  $$ -> ts_simple_type = FALSE;
	}
    ;
Opt.Simple :
    ID  =
	{
	  if ($1 -> id_str_table_index == indx_simple) {
	    $$ = (NodeStar)TRUE;  /* simple type, no pointers to activation records */
	  } else {
	    yywarn("Compiler directive not understood");
	    $$ = (NodeStar)FALSE;
	  }
	  vfree($1);
	}
    |     =
	{ $$ = FALSE; }
    ;
Opt.InLine :
    '(' ID QSTRING ')'
	{
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  if ($2 -> id_str_table_index == indx_inline) {
	    $$ = (NodeStar)((*inline_cnvt)($3));
	  } else {
	    yywarn("Compiler directive not understood");
	    $$ = NIL;
	  }
	  vfree($2);
	}
    |      =
	{ $$ = NIL; }
    ;   
Special :
    '(' ID ID UQSTRING ')'
	{
	  if (!initflag) {
	    yywarn("Compiler directive outside initialization");
	  }
	  if ($2 -> id_str_table_index == indx_standard) {
	    int num_arg = atoi($4);

	    if ($3 -> id_str_table_index == indx_New) {
		$$ = (NodeStar)special(STD_NEW, num_arg);
	    } else if ($3 -> id_str_table_index == indx_ptr_New) {
		$$ = (NodeStar)special(PTR_NEW, num_arg);
	    } else if ($3 -> id_str_table_index == indx_init_New) {
		$$ = (NodeStar)special(INIT_NEW, num_arg);
	    } else if ($3 -> id_str_table_index == indx_ValueOf) {
		$$ = (NodeStar)special(STD_VALUEOF, num_arg);
	    } else if ($3 -> id_str_table_index == indx_assign) {
		$$ = (NodeStar)special(STD_ASSIGN, num_arg);
	    } else if ($3 -> id_str_table_index == indx_put) {
		$$ = (NodeStar)special(STD_PUT, num_arg);
	    } else if ($3 -> id_str_table_index == indx_Callcc) {
		$$ = (NodeStar)special(STD_CALLCC, num_arg);
	    } else if ($3 -> id_str_table_index == indx_Array) {
		$$ = (NodeStar)special(STD_ARRAY, num_arg);
	    } else if ($3 -> id_str_table_index == indx_passign) {
		$$ = (NodeStar)special(STD_PASSIGN, num_arg);
	    } else if ($3 -> id_str_table_index == indx_massign) {
		$$ = (NodeStar)special(STD_MASSIGN, num_arg);
	    } else {
		yywarn("Bad standard directive");
		$$ = 0;
	    }
	  } else {
	    yywarn("Compiler directive not understood");
	    $$ = NIL;
	  }
	  vfree($2); vfree($3);
	}
    ;   
FuncSignature :
    FUNC Special Opt.InLine '[' Opt.Parameter.list ']' Signature =
	{
	  $$ = mknode(FUNCSIGNATURE,$3,$5,$7);
	  $$ -> fsig_special = (int)$2;
	}
    | FUNC Opt.InLine '[' Opt.Parameter.list ']' Signature =
	{
	  $$ = mknode(FUNCSIGNATURE,$2,$4,$6);
	  if ($2 != NIL) {
	    /* Remember that this is a primitive other than put or callcc */
	    $$ -> fsig_special = special(OTHER_BUILTIN, 0);
	  }
	}
    ;
Opt.TypeSignatureComponents :
    TypeSignatureComponents  =
        { $$ = $1; }
    |      =
        { $$ = emptylist(); }
    ;
Opt.TypeSignatureComponent.list :
    Opt.TypeSignatureComponents =
        { $$ = $1; }
    | Opt.TypeSignatureComponent.list ';' Opt.TypeSignatureComponents =
        { $$ = conc($1,$3); }
    ;
Opt.Id :
    ID =
        { $$ = $1; }
    |  =
        { $$ = NIL; }
    ;
TypeSignatureComponents :
    Id.list Opt.Colon Signature  =
        { 
	    if (!$2) {
		yyperror("Missing colon in type component");
	    }
	  lock($1); lock($3);
	  if ($3 -> kind == VARSIGNATURE) {
	    yyperror("Variable as type signature component");
	  }
          $$ = split(TSCOMPONENT, $1, $3);
          vfree(unlock($1)); vfree(unlock($3));
        }
    | Id.list Opt.Colon Opt.Readonly FIELD BasicDenotation  =
        {
          NODE * vl_field_sig;
          NODE * vr_field_sig;

	  if (!$2) {
	      yyperror("Missing colon in field specification");
	  }
          vl_field_sig = mknode(FUNCSIGNATURE,
                                NIL,  /* in-line code */
                                mklist(mknode(PARAMETER,
                                              NIL,
                                              mknode(VALSIGNATURE,
                                                     mknode(LETTERID,-1))),
                                       -1),
                                mknode(VALSIGNATURE, $5));
          lock($1); lock($5);
          if ( is_present($3) ) {
            $$ = split(TSCOMPONENT,$1,vl_field_sig);
          } else {
            vr_field_sig = mknode(FUNCSIGNATURE,
                                  NIL,  /* in-line code */
                                  mklist(mknode(PARAMETER,
                                                NIL,
                                                mknode(VARSIGNATURE,
                                                       mknode(LETTERID,-1))),
                                         -1),
                                  mknode(VARSIGNATURE, $5));
            $$ = conc(split(TSCOMPONENT,$1,vl_field_sig),
                      split(TSCOMPONENT,$1,vr_field_sig));
          }
          vfree(unlock($1)); vfree(unlock($5));
        }
    | ID =
        { 
            NODE * result;
            switch ($1 -> kind) {
                case OPRID:
                    i = $1 -> id_str_table_index;
                    if ( i == indx_assign )
                        result = mknode(TSCOMPONENT,$1,sig_assign);
                    else if (i == indx_less || i == indx_le )
                        result = mknode(TSCOMPONENT,$1,sig_less);
                    else if ( i == indx_greater || i == indx_ge )
                        result = mknode(TSCOMPONENT,$1,sig_greater);
                    else if ( i == indx_equals || i == indx_ne )
			result = mknode(TSCOMPONENT,$1,sig_equals);
		    else if ( i == indx_sconc )
			result = mknode(TSCOMPONENT,$1,sig_sconc);
		    else if ( i == indx_pconc )
			result = mknode(TSCOMPONENT,$1,sig_pconc);
                    else 
			yyperror("Missing signature");
                    break;

                case LETTERID:
                    i = $1 -> id_str_table_index;
                    if ( i == indx_New )
                        result = mknode(TSCOMPONENT,$1,sig_New);
                    else if ( i == indx_ValueOf )
                        result = mknode(TSCOMPONENT,$1,sig_ValueOf);
                    else if ( *(getname(i)) == '\'' ) /* quoted id */
                        result = mknode(TSCOMPONENT,$1,sig_const);
                    else
			yyperror("Missing signature");
                    break;

#             ifdef DEBUG
                default:
                        dbgmsg("parser: incorrect id structure");
#             endif
            }
            $$ = mklist(result,-1);
        }
    | CHARACTERS  =
        {
            /* return a singleton list containing a DEFCHARSIGS node with */
            /* all bits corresponding to valid RUSSELL characters set. */
            /* Note machine dependence. */
		$$ = mklist(mknode(DEFCHARSIGS, RCS0, RCS1, RCS2, RCS3), -1);
	}
    | error
	{
	    yyperror("Error in type signature component");
	    $$ = emptylist();
	}
    ;
Opt.Readonly :
    READONLY =
        { $$ = PRESENT; }
    |   =
        { $$ = NOTPRESENT; }
    ;
Denotation :
    ManifestSignature
	{ $$ = $1; }
    | BasicDenotation
    ;
BasicDenotation:
    Primary.list  =
        {
#           ifdef BAD
                if (BADflag)
                    flcheck(0,0);
                else
                    flcheck(0,1);
#           endif
            lock($1);
            $$ = mkappl($1);
            lock($$);
            vfree(unlock($1));
            unlock($$);
#           ifdef BAD
                if (BADflag)
                    flcheck(0,0);
                else
                    flcheck(0,1);
#           endif
        }
    | '\002' /* STX (marks the insertion place in a compiler initialization */
             /* run) */   =
        {
            if (!initflag) {
		yyperror("use of insertion character in non-initialization run");
            } else {
		$$ = insrtptr = mknode(BLOCKDENOTATION,emptylist(),NIL);
		insrtptr -> bld_precedence = INFINITE;
            }
        }
    ;
Primary.list :
    Primary =
        { $$ = mklist($1, -1); }
    | ArgList
        { $$ = mklist($1, -1); }
    | Primary.list Primary 
	{ $$ = addright($1,$2); }
    | Primary.list ArgList
	{ $$ = addright($1,$2); }
    ;
Primary :
    FuncConstruction
        { $$ = $1; }
    | Selection
	{ $$ = $1; }
    | CAND
        { $$ = mknode(WORDCAND); }
    | COR
        { $$ = mknode(WORDCOR); }
    | TypeConstruction
        { $$ = $1; }
    | Primary TypeModifier
	{ $$ = mknode(MODPRIMARY,$1,$2,NIL); }
    | '(' Denotation.seq ')'
        { int prec;
	  NODE *p;
#         ifdef DEBUG
            if ( is_empty($2) ) {
                dbgmsg("parser: empty denotation seq\n");
                abort();
            }
#         endif
          if (first($2) == last($2)) {
              /* just return the single denotation in the sequence */
                $$ = p = last($2);
                lock(p);
                vfree($2);
                unlock(p);
          }
          else {
              prec = precedence( last($2) );
	      $$ = p = mknode(BLOCKDENOTATION,emptylist(),$2);
	      p -> bld_precedence = prec;
          }
        }
    | IF GuardedDenotation.list FI =
        { $$ = mknode(GUARDEDLIST,$2); }
    | DO GuardedDenotation.list OD =
        { $$ = mknode(LOOPDENOTATION,$2); }
    | LET Opt.Declaration.list IN Denotation.seq NI =
        { int prec;  NODE *p;
	  prec = precedence( last($4) );
	  $$ = p = mknode(BLOCKDENOTATION,$2,$4);
	  p -> bld_precedence = prec;
	}
    | USE Opt.Denotation.list IN Denotation.seq NI =
        { int prec;  NODE *p;
	  prec = precedence( last($4) );
	  $$ = p = mknode(USELIST,$2,$4);
	  p -> usl_precedence = prec;
        }
    | EXTERN '{' QSTRING '}' =
	{
	  has_externs = TRUE;
	  $$ = mknode(REXTERNDEF, read_signature($3), $3);
	  $$ -> sig_done = SIG_DONE;
	}
    ;
Denotation.seq :
    Denotation.seq ';' Opt.Denotation =
        { $$ = addright($1,$3); }
    | Denotation =
        { $$ = mklist($1, -1); }
    | error 
	       {    int c;

		    yyperror("Error in Expression Sequence");
                    for (c = yychar; !in(c, denseqstop); c = yylex());
		    yyclearin;
		    switch(c)  {
                        case ';':
                        case '#':
			case FI:
			case ELSIF:
			case ELSE:
                        case OD:
                        case ')':
                        case 'NI':
                        case '}':
                            yyunlex(c);
                            break;
                        case IF:
                        case DO:
                        case LET:
                        case FUNC:
                        case '(':
                            yyunlex(c);
                            yyunlex(';');
                            break;
                        case WITH:
                            yyunlex(c);
                            yyunlex(WORDID);
                            yyunlex(';');
                            break;
                        case IN:
                            yyunlex(c);
                            yyunlex(LET);
                            yyunlex(';');
			    break;
			case EOF:
			    exit(1);
                        default:
                            yyunlex(c);
                    }
#                   ifdef DEBUG
                        if (yydebug) rec_on(c);
#                   endif
		    $$ = mklist(id_Integer, -1);  /* fake it */
	       }
    ;
Opt.Denotation :
    Denotation  =
        { $$ = $1; }
    |  =
        { $$ = NIL; }
    ;
FuncConstruction :
    IncompleteFuncSignature '{' Denotation.seq '}' =
	{ 
	  if (length($3) == 1) {
	      $$ = mknode(FUNCCONSTR,$1,first($3));
	  } else {
	      $$ = mknode(FUNCCONSTR,$1,
			      mknode(BLOCKDENOTATION,emptylist(),$3));
	      $$->fc_body->bld_precedence = INFINITE; /* doesn't matter */
	  }
	  $1 -> fsig_construction = $$;
	  if (initflag) {
	    /* Make sure that global functions receive the same name */
	    /* in all compilations.                                  */
	    char * name = (char *)malloc(15);
	    static int global_fn_count;
	    sprintf(name, "fn_global_%d", ++ global_fn_count);
	    $$ -> fc_code_label = name;
	  }
	}
    | IncompleteFuncSignature '{' EXTERN QSTRING '}' =
	{
	  if ($1 -> fsig_result_sig == NIL) {
	    yyperror("Must specify result signature for extern");
	  }
	  $$ = mknode(FUNCCONSTR, $1, mknode(EXTERNDEF, $4));
	}
    ;
IncompleteFuncSignature :
    FuncSignature  =
        { $$ = $1; }
    | FUNC Special Opt.InLine '[' Opt.Parameter.list ']'  =
	{ 
	  $$ = mknode(FUNCSIGNATURE, $3, $5, NIL);
	  $$ -> fsig_special = (int)$2;
	}
    | FUNC Opt.InLine '[' Opt.Parameter.list ']'  =
	{ $$ = mknode(FUNCSIGNATURE, $2, $4, NIL); }
    ;
Selection :
    Primary '$' ID Opt.SigClause =
        { 
            initfld(&($3->sel_type), $1);
            initfld(&($3->signature), $4);
            $$ = $3;
        }
    | Primary '$' STRING =
        {
            initfld(&($3->sel_type), $1);
            $$ = $3;
        }
    | ID Opt.SigClause =
        { 
            initfld(&($1->signature), $2);
            $$ = $1;
        }
    | STRING =
        {
            $$ = $1;
        }
    ;
TypeConstruction :
    Enumeration =
        { $$ = $1; }
    | Record =
        { $$ = $1; }
    | Extension =
        { $$ = $1; }
    | Product =
        { $$ = $1; }
    | Union =
        { $$ = $1; }
    ;
Guard :
    Denotation =
        { $$ = $1; }
    | ELSE =
        { $$ = mknode(WORDELSE); }
    | error
	{ yyperror("Error while trying to parse guard"); }
    ;
ArgList :
    '[' Opt.Denotation.list ']' =
        { $$ = $2; }
    ;
Opt.Denotation.list :
    Denotation =
        { $$ = mklist($1, -1); }
    | Denotation ',' Opt.Denotation.list =
        { $$ = addleft($3,$1); }
    |   =
        { $$ = emptylist(); }
    ;
Opt.Declaration.list :
    Opt.Declarations =
        { $$ = $1; }
    | Opt.Declaration.list ';' Opt.Declarations =
        { $$ = conc($1,$3); }
    ;
Opt.Declarations :
    Id.list Opt.ColonSignature EQUALS_EQUALS Denotation =
        { 
            lock($1); lock($2); lock($4);
            $$ = split(DECLARATION,$1,$4,$2);
            vfree(unlock($1)); vfree(unlock($2)); vfree(unlock($4));
        }
    | Id.list Opt.ColonSignature EQUALS_EQUALS_EQUALS Denotation =
        { 
	    NODE *q;
            lock($1); lock($2); lock($4);
	    $$ = q = split(DECLARATION,$1,$4,$2);
	    maplist(p, q, {
		p -> decl_sig_transp = TRUE;
	    });
            vfree(unlock($1)); vfree(unlock($2)); vfree(unlock($4));
        }
    |   =
        { $$ = emptylist(); }
    | error   { int c;
		int i = 0;

		yyperror("Error in declaration");
                for ( c = yychar; !in(c, declstop); c = yylex() )
                    if ( ++i > maxskip ) break;
                yyclearin;
                switch (c)  {
                    case EQUALS_EQUALS:
                        yyunlex(c);
                        yyunlex(WORDID);
                        yyunlex(';');
                        break;
                    case ';' :
                    case '}' :
                    case IN:
                        yyunlex(c);
                        break;
                    case NI:
                        yyunlex(c);
                        yyunlex(WORDID);
                        yyunlex(IN);
                        break;
                    case IF:
                    case DO:
                    case LET:
                    case '(':
                        yyunlex(c);
                        yyunlex(EQUALS_EQUALS);
                        yyunlex(WORDID);
			break;
		    case EOF:
			exit(1);
                    default :
                        yyunlex(c);
                        yyunlex(NI);
                        yyunlex(IN);
                }
#               ifdef DEBUG
                    if (yydebug) rec_on(c);
#               endif
		$$ = emptylist();
	      }
    ;
Opt.ColonSignature :
    Opt.Colon Signature  =
        { $$ = $2; }
    |  =
        { $$ = NIL; }
    ;
Enumeration :
    ENUM '{' Id.list '}' =
        { $$ = mknode(ENUMERATION,$3); }
    ;
Extension :
    EXTEND '{' Denotation '}' =
        { $$ = mknode(EXTENSION,$3); }
    ;
Product :
    PROD Opt.Id '{' Opt.Parameter.list '}'  =
        { 
          maplist (s, $4, {
            if (s -> par_id == NIL) {
		yyperror("Anonymous fields not allowed in product");
            }
          });
	  $$ = mknode(PRODCONSTRUCTION,$2,$4); 
	}
    | PROD error
	{ yyperror("Error in product construction"); }
    ;
Record :
    RECORD '{' Opt.RecordElement.list '}' =
        { $$ = mknode(RECORDCONSTRUCTION,$3); }
    | RECORD error
	{ yyperror("Error in record construction"); }
    ;
Union :
    UNION Opt.Id '{' Opt.Parameter.list '}' =
        {
          maplist (s, $4, {
            if (s -> par_id == NIL) {
		yyperror("Anonymous fields not allowed in union");
            }
          });
          $$ = mknode(UNIONCONSTRUCTION,$2,$4);
        }
    | UNION error
	{ yyperror("Error in union construction"); }
    ;
Opt.RecordElement.list :
    Opt.RecordElements =
        { $$ = $1; }
    | Opt.RecordElement.list ';' Opt.RecordElements =
        { $$ = conc($1,$3); }
    ;
Opt.RecordElements :
    RecordElements =
        { $$ = $1; }
    |  =
        { $$ = emptylist(); }
    ;
RecordElements :
    Id.list Opt.Colon Denotation =
        { 
	    if (!$2) {
		yyperror("Missing colon in record element specification");
	    }
            lock($1); lock($3);
            $$ = split(RECORDELEMENT,$1,$3);
            vfree(unlock($1)); vfree(unlock($3));
        }
    ;
TypeModifier :
    WithList =
        { $$ = $1; }
    | ExportList =
        { $$ = $1; }
    ;
WithList :
    WITH Opt.Id '{' Opt.Declaration.list '}'  =
	{ $$ = mknode(WITHLIST,$2,$4); }
    | WITH error
	{ yyperror("Error in \"with\" type modification"); }
    ;
ExportList :
    Start.ExportList Opt.Id '{' ExportElement.list '}' =
	{ $$ = mknode($1,$2,$4);  }
    | EXPORT error
	{ yyperror("Error in \"export\" type modification"); }
    | HIDE error
	{ yyperror("Error in \"hide\" type modification"); }
    ;
Start.ExportList :
    EXPORT =
        { $$ = (NODE *)EXPORTLIST; }
    | HIDE =
        { $$ = (NODE *)HIDELIST; }
    ;
ExportElement.list :
    ExportElement =
        { $$ = mklist($1, -1); }
    | ExportElement ';' ExportElement.list =
        { $$ = addleft($3,$1); }
    |   =
        { $$ = emptylist(); }
    ;
ExportElement :
    ID Opt.SigClause Opt.ExportList =
        { $$ = mknode(EXPORTELEMENT,$1,$2,$3); }
    | CONSTANTS =
        { $$ = mknode(ALLCONSTANTS); }
    | error
	{ yyperror("Error in \"export\"ed or hidden element specification"); }
    ;
Opt.ExportList :
    ExportList =
        { $$ = $1; }
    |   =
        { $$ = NIL; }
    ;
Opt.SigClause :
    LEFT_ANGLE_BRACKET Signature RIGHT_ANGLE_BRACKET  =
        { $$ = $2; }
    |   =
        { $$ = NIL; }
    ;
Opt.ElseList :
    ElseList
	{ $$ = $1; }
    |
	{
	    extern NODE * appl_Null;
	    $$ = appl_Null;
	}
    ;
ElseList :
    ELSE Denotation.seq
	{
	    if (length($2) == 1) {
		$$ = first($2);
	    } else {
		NODE *p = mknode(BLOCKDENOTATION, emptylist(), $2);
		$$ = p;
		p -> bld_precedence = INFINITE; /* doesnt matter */
	    }
	}
    | ELSIF Denotation THEN Denotation.seq Opt.ElseList
	{
	    NODE * ge1;  /* First guarded element */
	    NODE * ge2;  /* Second guarded element */

	    if (length($4) == 1) {
		ge1 = mknode(GUARDEDELEMENT,$2,first($4));
	    } else {
		ge1 = mknode(GUARDEDELEMENT,$2,
			    mknode(BLOCKDENOTATION,emptylist(),$4));
		ge1->ge_element->bld_precedence = INFINITE; /* doesn't matter */
	    }
	    /* $5 is a denotation to be executed if orignal */
	    /* guard does not apply                         */
	    ge2 = mknode(GUARDEDELEMENT, mknode(WORDELSE), $5);
	    $$ = mknode(GUARDEDLIST, mklist(ge1, ge2, -1));
	}
    ;
GuardedDenotation.list :
    GuardedDenotation =
        { $$ = mklist($1, -1); }
    | GuardedDenotation.list '#' GuardedDenotation =
	{ $$ = addright($1,$3); }
    | Denotation THEN Denotation.seq Opt.ElseList
	{
	    NODE * ge1;  /* First guarded element */
	    NODE * ge2;  /* Second guarded element */

	    if (length($3) == 1) {
		ge1 = mknode(GUARDEDELEMENT,$1,first($3));
	    } else {
		ge1 = mknode(GUARDEDELEMENT,$1,
			    mknode(BLOCKDENOTATION,emptylist(),$3));
		ge1->ge_element->bld_precedence = INFINITE; /* doesn't matter */
	    }
	    /* $4 is a denotation to be executed if orignal */
	    /* guard does not apply                         */
	    ge2 = mknode(GUARDEDELEMENT, mknode(WORDELSE), $4);
	    $$ = mklist(ge1, ge2, -1);
	}
    | error 
        {   
	  int i = 0;
          int c;

	  yyperror("Error in Loop or If");
          for (c = yychar; !in(c,condstop); c = yylex() )
              if ( ++i > maxskip ) break;
          yyclearin;
          switch(c) {
              case '#' :
              case FI :
              case OD :
                  yyunlex(c); break;
	      case RIGHT_ARROW :
	      case ELSE :
	      case THEN :
              case ';' :
		  yyunlex(RIGHT_ARROW); yyunlex(WORDID); yyunlex('#'); break;
              case LET :
              case '(' :
              case DO :
              case IF :
                  yyunlex(c);
                  yyunlex(RIGHT_ARROW);
                  yyunlex(WORDID);
                  yyunlex('#');
		  break;
	      case EOF:
		  exit(1);
              default :
                  yyunlex(c); yyunlex(END);
          }
#         ifdef DEBUG
              if (yydebug) rec_on(c);
#         endif
	  $$ = emptylist();
        }
    ;
GuardedDenotation :
    Guard RIGHT_ARROW Denotation.seq =
	{   if (length($3) == 1) {
		$$ = mknode(GUARDEDELEMENT,$1,first($3));
	    } else {
		NODE *p = mknode(GUARDEDELEMENT,$1,
				mknode(BLOCKDENOTATION,emptylist(),$3));
		$$= p;
		p->ge_element->bld_precedence = INFINITE; /* doesn't matter */
	    }
	}
    ;
ID :
    WORDID =
        { $$ = mknode(LETTERID,$1); }
    | OPID =
        { $$ = mknode(OPRID,$1); }
    ;
STRING :
    QSTRING =
        { $$ = mknode(QSTR,$1); }
    | UQSTRING =
        { $$ = mknode(UQSTR,$1); }
    ;
%%
 
in(element,list)
int element;
int list[];
{   int i;
    i = 0;
    while (element != list[i] && list[i] != LIST_END) ++i;
    return (element == list[i]);
}
 

# ifdef DEBUG
    rec_on(c)
    int c;
    {
        if ( c < 0400 ) {
            dbgmsg ( "recovery attempted on %c\n", c);
        }
        else {
            dbgmsg ( "recovery attempted on token number %d\n", c);
        }
    }
# endif

