# include "defs.h"
(* This is a hash table of summary info for functions. 		*)
(* Vflag indicates that some information should be written out 	*)
(* in user readable form.				       	*)
func [numfns : val Short; Vflag : val Boolean] {

    let 
	put_list == func [l : val List [Short]] val Void {
		if not is_nil [l] then 
		  put [head l]; put [" "]; put_list [tail l]
		fi
	    };

	TwoBits === prod { mem, thro : val Boolean };

	FnSumm === prod {
			code : val Short;
			size_or_argnum : val Short;
			numargs : val Short;
			args_summ : val (List [TwoBits])
		    };

	print == func [name : val ChStr; fnsumm : val FnSumm; impure] 
								val Void {
			put "Summary for ";put name;put " : ";
			put [let c == code fnsumm in
				if c = ALCNIL ==> "unknown"
				# c = ANY ==> "unknown"
				# c = ALCSTAT ==> "ptr to data seg"
				# c = ALCSTACK ==> "ptr to stack"
				# c = ALLOC ==> "allocated object"
				# c = ASSIGN ==> ""
				# else ==> "unknown"
				fi
			      ni];
			if code[fnsumm] = ALLOC cand
			   size_or_argnum [fnsumm] <> 0 then 
				put " size = ";put [size_or_argnum fnsumm]
			elsif [code fnsumm] = ASSIGN then
				put " arg# ";put [size_or_argnum fnsumm]
			fi;
			put "\n";
			let i == Short$New [numargs fnsumm];
			    as == (List [TwoBits])$New []
			in
			    as := args_summ [fnsumm];
			    do i > 0 ==>
				if mem [head as] then
				    put "    arg # ";put i;
				    put " goes to memory;\n"
				fi;
				if thro [head as] then
				    put "    arg # ";put i;
				    put " passed through;\n"
				fi;
				as := tail as; i -= 1
			    od
			ni			
		}; (* print *)

	noInfoFn == FnSumm$Mk [NULLCAT, 0, 0, (List [TwoBits])$''];

	CallSumm === prod { category : val Short;
			    supplement : val Short };

	hash == func [sym : val ChStr] val Short {
		let l == len [sym]
		in
			(31 * ChStr$Out[sym] + 5 * sym.(l - 1) + l)
		ni
		};

	TTT == extern {"hash"} [numfns, ChStr, FnSumm, noInfoFn];

	Table == TTT  with T {

	    look == func [name : val ChStr; t : var T] val FnSumm {
			T$lookup [hash name, name, t]
		    }

	     } (* with *)

    in prod  P {
	table : val Ref [Table];
	arg_register : val Ref [Short]; (* to record special register 
					    for the only (last) arg passed 
					    to a call between categorizing 
					    the arg and the call *)
(* this forces you to call categorize_arg on all the args before categorizing
				the call *)
	   } with S {

	    Init == func [] val S {
		let table == Table$Init [];
		    arg_register == Short$New []
		in
			S$Mk [(Ref Table)$In [table],
			      (Ref Short)$In [arg_register]]
		ni
		};

	    (* insert a summary of function "name" into the table t *)
	    append == func [name : val ChStr; fnsumm : val FnSumm; 
						summ : val S; impure] {
			if Vflag then
			    print [name, fnsumm];
			fi;
			Table$insert [hash name, name, fnsumm, (table summ)^]
		    };

	    is_summarized == func [name : val ChStr; summ : val S; impure]
								val Boolean {
			(code [Table$look [name, (table summ)^]]) <> NULLCAT
		};

	    (* return a the category dictated by the result code for the
				function name in the table t *)
	    (* with this new scheme, it might be worth recording when a result
		can be an ALLOC or a STACK/STAT, since deallocation of such a
		result, and even one that could also be an arg, is allowable *)

	    categorize_call == func [name : val ChStr; summ : val S;impure] 
								val CallSumm {
OPEN
put "Categorize call to ";put name; put "\n";
CLOSE
			let s == Table$look [name, (table summ)^];
			    resultcode == if code s = NULLCAT then
					put name;put " NOT found in table\n";
						ANY
					  else  code s
					  fi;
			    other == if resultcode = ALLOC then 
					size_or_argnum s
				     elsif resultcode = ASSIGN then
				(* other is the special register in which the
				       arg that reaches thro is saved *)
				        (arg_register summ)^
			   	     else 0
				     fi
			in
OPEN
put " resultcode = ";put resultcode;
if resultcode = ALLOC cor resultcode = ASSIGN
then put " other field = ";put other
fi;put "\n";
CLOSE
			   CallSumm$Mk [resultcode, other]
			ni
		    };

	    (* return the category for an ARG op dictated by the bits in table 
		for the argnumth arg of function "name"; also record any arg
		that are passed thro - only needed if only one is passed through
		so overwrite previous value *)
	    categorize_arg == func [name : val ChStr; argnum : val Short;
		    			     summ : val S;impure] val Short {
OPEN
put "\nCategorize ARG#";put argnum;
put " cat = ";put [
CLOSE
		    if argnum >= MAXWATCHARGS then TOMEM
		    else
			let s == Table$look [name, (table summ)^]
			in
			    if code s = NULLCAT (* i.e. noInfo *)
			    then put name;put " NOT found in table\n";
				 TOMEM
			    else (* find the argnumth element on the list *)
				let i == Short$New [numargs s];
				    alist == (List [TwoBits])$New []
				in (* the head is the ith arg *)
				    alist := args_summ s;
				    do i > argnum ==>
					alist := tail alist;
					i -= 1
				    od;

				    let arginfo == head alist
				    in
					if mem arginfo then TOMEM
				        elsif thro arginfo then
(* code s is ASSIGN only if it is known that one and only one of the args 
   (one numbered less than MAXWATCHARGS)
  passes to result. The negative of that arg's number is the special register
  in which the arg value is assumed to be for the call to see. Record it now. 
  The arg is WATCHARG only if it is THE correct one *)
						if (code s = ASSIGN) cand
						   (size_or_argnum s = argnum)
						then 
						     (arg_register summ)^ := 
							- argnum;
						     WATCHARG
						     (* the arg only goes to
							result and will be treated
							as an ASSIGN *)
						else TOMEM
						fi
					else WATCHARG (* the arg goes nowhere *)
					fi
				    ni
				ni
			    fi (* code s = NULLCAT *)
			ni (* s *)
		    fi
OPEN
]
CLOSE
		}; (* categorize_arg *)

	   } (* with *) hide { Mk; New; table }

	

    ni
}
