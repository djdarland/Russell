# include "parm.h"
# include "stree/ststructs.mh"
# include "../stt/sttdefs.h"

# include "../applinfer/precedence.h"

/*
 *   Initialize the string table to contain various identifiers with
 * builtin meanings and/or precedences.  Then build trees to correspond
 * to default signatures, as well as certain other standard subtrees.
 */

 
sttrelptr  indx_assign,      /* precedence 0 */
           indx_passign,
           indx_massign,
           /*   cor             precedence 1 */
           /*   cand            precedence 2 */
           /* other operators   precedence 3 */
           indx_or,          /* precedence 4 */
           indx_and,         /* precedence 5 */
           indx_not,         /* precedence 6 */
           indx_equals,      /* precedence 7 */
           indx_less,
	   indx_le,
	   indx_ge,
	   indx_ne,
           indx_greater,
           indx_plus,        /* precedence 8 */
           indx_minus,
           indx_times,       /* precedence 9 */
           indx_divide,      
           indx_mod,
	   indx_pconc,       /* precedence 10 */
	   indx_sconc,
           indx_exp,         /* precedence 11 */
           indx_deref,       /* precedence 12 */
           indx_subscr,      /* precedence 13 */
           indx_New,         /* no precedence */
	   indx_ValueOf,
	   indx_First,
	   indx_Last,
	   indx_Pred,
	   indx_Succ,
           indx_Ord,
	   indx_OrdInv,
           indx_Card,
           indx_In,
           indx_Out,
	   indx_put,
	   indx_Callcc,
	   indx_size,
	   indx_ptr_New,
	   indx_init_New,
	   indx_empty,
	   indx_standard,
	   indx_inline;

int     pend[NLEVELS];   /* pend[n] = last string table entry with precedence */
                         /* level n                                           */
sttrelptr indx_Boolean,
	  indx_Void,
	  indx_true,
	  indx_false;
	  indx_simple,
	  indx_Null,
	  indx_Mk;
sttrelptr indx_Integer,
	  indx_Void,
	  indx_Array;

extern   NODE  * sig_assign,
               * sig_equals,
               * sig_less,
               * sig_greater,
               * sig_New,
               * sig_const,
	       * sig_ValueOf,
	       * sig_pconc,
	       * sig_sconc,
	       * sig_Signature;
NODE           * val_Boolean,
	       * val_Integer,
               * val_Void;
NODE 	       * var_Void;
NODE   * sel_true,  /* Trees for Boolean$true[] and Boolean$false[] */
       * sel_false,
       * id_Boolean, /* Note that the last definition field needs to be */
                     /* set later */
       * id_Integer,
       * id_Void,
       * id_put,
       * id_ValueOf,
       * id_New,
       * id_size,
       * id_Null,
       * id_times,
       * id_plus,
       * appl_Null;

extern int  yyvline;  /* initialized here since it is incorporated into */
                      /* the structures created here.                   */

initids()
{
    NODE *   var_param,
         *   val_param,
         *   as_params,
         *   two_params,
         *   al_params;

    yyvline = 9999;

    /* initialize string table indicees */
        stt_enter("?",2);   /* Make sure index 0 is not used */
        indx_passign = stt_enter("+=",3);
        indx_massign = stt_enter("-=",3);
        indx_assign = pend[0] = stt_enter(":=",3);
        indx_or = pend[1] = pend[2] = pend[3] = pend[4] = stt_enter("or",3);
        indx_and = pend[5] = stt_enter("and",4);
        indx_not = pend[6] = stt_enter("not",4);
        indx_equals = stt_enter("=",2);
        indx_less = stt_enter("<",2);
	indx_le = stt_enter("<=",3);
	indx_ge = stt_enter(">=",3);
	indx_ne = stt_enter("<>",3);
        indx_greater = pend[7] = stt_enter(">",2);
        indx_plus = stt_enter("+",2);
        indx_minus = pend[8] = stt_enter("-",2);
        indx_times = stt_enter("*",2);
        indx_divide = stt_enter("/",2);
        indx_mod = pend[9] = stt_enter("%",2);
	indx_pconc = stt_enter("^+",3);
	indx_sconc = pend[10] = stt_enter("^*",3);
        indx_exp = pend[11] = stt_enter("**",3);
        indx_deref = pend[12] = stt_enter("^",2);
        indx_subscr = pend[13] = stt_enter(".",2);
	indx_New = stt_enter("New",4);
	indx_ptr_New = stt_enter("ptr_New",8);
	indx_init_New = stt_enter("init_New",9);
	indx_ValueOf = stt_enter("V",2);
	indx_First = stt_enter("First",6);
	indx_Last = stt_enter("Last",5);
	indx_Pred = stt_enter("Pred",5);
	indx_Succ = stt_enter("Succ",5);
        indx_Ord = stt_enter("Ord",4);
	indx_OrdInv = stt_enter("OrdInv",7);
        indx_Card = stt_enter("Card",5);
        indx_In = stt_enter("In",3);
        indx_Out = stt_enter("Out",4);
        indx_Boolean = stt_enter("Boolean",8);
        indx_true = stt_enter("True",5);
        indx_false = stt_enter("False",6);
        indx_Integer = stt_enter("Short",6);
	indx_Void = stt_enter("Void",5);
	indx_Array = stt_enter("Array",6);
	indx_put = stt_enter("put",4);
	indx_Callcc = stt_enter("Callcc",7);
	indx_size = stt_enter("size", 5);
	indx_empty = stt_enter("''",3);
	indx_simple = stt_enter("simple",7);
	indx_standard = stt_enter("standard",9);
	indx_inline = stt_enter("inline",7);
	indx_Mk = stt_enter("Mk",3);
	indx_Null = stt_enter("Null",5);

        id_Boolean = mknode(LETTERID, indx_Boolean);
        id_Void = mknode(LETTERID, indx_Void);
	id_Integer = mknode(LETTERID, indx_Integer);
	id_put = mknode(LETTERID, indx_put);
	id_ValueOf = mknode(LETTERID, indx_ValueOf);
	id_New = mknode(LETTERID, indx_New);
	id_size = mknode(LETTERID, indx_size);
	id_Null = mknode(LETTERID, indx_Null);
	id_times = mknode(LETTERID, indx_times);
	id_plus = mknode(LETTERID, indx_plus);

    /* initialize default signatures */
        /* Note that since these can and will be shared they should not */
        /* be touched by the symbol table routine */
        val_Boolean = lock( mknode(VALSIGNATURE,
                                   id_Boolean)
                          );
	val_Integer = lock( mknode(VALSIGNATURE,
				   id_Integer)
			  );
        val_Void    = lock( mknode(VALSIGNATURE,
                                   id_Void)
                          );
        var_Void    = lock( mknode(VARSIGNATURE,
                                   id_Void)
                          );

        val_param = lock( mknode(PARAMETER,
                                 NIL,
                                 mknode(VALSIGNATURE,
                                        mknode(LETTERID,-1)
                                       )
                                 ) );

        var_param = lock( mknode(PARAMETER,
                                 NIL,
                                 mknode(VARSIGNATURE,
                                        mknode(LETTERID,-1)
                                       )
                                 ) );

        as_params = lock( mklist(var_param, val_param, -1) );
        two_params = lock( mklist(val_param, val_param, -1) );
        al_params = lock(mklist(var_param, var_param, -1) );

	sig_assign = lock( mknode(FUNCSIGNATURE, NIL /* inline */, as_params,
				  val_param->par_signature) );

	sig_pconc =
	sig_sconc =
	    lock( mknode(FUNCSIGNATURE, NIL, two_params,
                         val_param -> par_signature));

        sig_equals =
        sig_less =
        sig_greater =
	    lock( mknode(FUNCSIGNATURE, NIL, two_params,val_Boolean) );
	sig_New = lock( mknode(FUNCSIGNATURE, NIL, emptylist(),
                               var_param -> par_signature) );
	sig_const = lock( mknode(FUNCSIGNATURE, NIL, emptylist(),
                                val_param -> par_signature) );
	sig_ValueOf = lock( mknode(FUNCSIGNATURE, NIL,
                             mklist(var_param, -1),
			     val_param -> par_signature) );
	sig_Signature = lock(mknode(SIGNATURESIG));

        sel_true = mknode(LETTERID,indx_true);
        initfld(&(sel_true->sel_type),id_Boolean);
        sel_true = mknode(APPLICATION, sel_true, emptylist());
        lock( sel_true );

        sel_false = mknode(LETTERID,indx_false);
        initfld(&(sel_false->sel_type),id_Boolean);
        sel_false = mknode(APPLICATION, sel_false, emptylist());
	lock( sel_false );

	appl_Null = mknode(APPLICATION, id_Null, emptylist());

        yyvline = 0;
}

