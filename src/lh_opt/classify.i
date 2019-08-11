(* 		CLL		CLI		CLC
		----------------------------------------
		UNSAFE		UNSAFE		ALCNIL
AL		ALLOC		ALLOC		ALLOC
defined_fn	"		"		MAYBEALC
NSC		ALCNIL		ALCNIL		ALCNIL
NSC & def fn	MAYBEALC	ALCNIL		MAYBEALC
(interfile?)
		---------------------------------------
		ARG 
		-----
		TOMEM
NP		dull
defined_fn	ARGCAT
(interfile?)
		------------
							*)	
(* ERR is a special category, but it looks like a call to 
	next_call_num/fn_names, etc. The ARGs are TOMEM (since it doesn't 
 	matter anyway). After an ERR I can assume all variables point to
	TOP since that point is never reached.	*)

(* only going to look at non-dull quads; may make a few quads dull; 
	those don't get a more entry *)

(* when I have interfile summary info I will need to use it in here, to classify
	CLLs, CLCs, and ARGs *)

if not dull [basics.q] then
let b == basics.q;
    op == opc [b];
    cat == Short$New [NULLCAT];	(* for quads I need to see after classification
				but not during data flow *)
    x_var == Short$New [0];
    other == (Ref [Short])$In [Short$New [0]];
    size == (Ref [Short])$In [Short$New [0]];
    extra == Short$New [];
    became_dull == Boolean$New [False];

    make_dull == func [impure] val Void {
		basics.q := Basics$Mk [op, arg1 b, arg2 b, arg3 b, True];
		became_dull := True
	}
in
	(* categorise some quads now *)
	if (op = CLL) cor (op = CLI) ==>  cat := UNSAFE;
		(* may change to ALLOC  or ALCNIL or MAYBEALC *)
				(* record the index of the call with the quad *)
				extra := next_call_num;
				next_call_num += 1
	 # (op = ERR)	 ==> cat := ERRCAT;
			     next_call_num += 1
	 # (op = CLC)    ==> cat := if ONLYINTRA then ALCNIL
				    elsif is_defined_fn 
					      [fn_names.next_call_num, labels]
				    then
					MAYBEALC
				    else ALCNIL
				    fi;
				(* may change to real ALLOC *)
			      x_var := RL;
				(* other^ may be loc# later *)
			      (* size^ is 0 - unknown size/atomicity *)
			      extra := next_call_num;
			      next_call_num += 1

	 # (op = BFN) ==>
			(* AR := the loc numbered 1 (the first arg loc) *)
			cat := GARCAT;
			x_var := AR;
			other^ := 1
	 # else ==> Null []
	 fi;
	(* take care of any hints outstanding *)

	if hinted then
	(* note that a hint applies to the next applicable op *)
	    if  (hint_type = NSC) ==>
		(* the result register (RL) changes but does not get a loc *)
		(* makes CLLs into maybeALLOCs;
		   CLIs into ALCNILs - not interproc.ing them *)
		    if (op = CLI) then
				hinted := False;
				cat := ALCNIL;
				x_var := RL
		    elsif (op = CLL) then
				hinted := False;
				x_var := RL;
				cat := let name == to_str [arg1 [basics.q]]
					in
					    if ONLYINTRA then ALCNIL
					    elsif is_defined_fn [name, labels]
					    then 
						MAYBEALC
					    else ALCNIL (* may change if fn
							is summarized *)
					    fi
					ni;
				(* other may become loc# *)
				(* size^ is 0 - unknown size/atomicity *)
		    fi
	    #   (hint_type = AL) ==>      (* allocator *)
		    if ((op = CLL) cor (op = CLI) cor (op = CLC)) then
				hinted := False;
				cat := ALLOC;
				x_var := RL;	(* since RL := alloced obj *)
				size^ := hint_info
			fi
	    #	(hint_type = STSZ) ==>  (* hold on till later *)
					(* don't reset 'hinted' *)
				Null []

	    # (hint_type = NP) cor (hint_type = PT) ==>   
				Null [] (* hold on till args of ARG read *)
				
	    # (hint_type = ONS) cand (op = ALH cor op = ALA) ==>
	    			Null [] (* treated as an ALS below *)

	    # else ==>	hinted := False
	    (* ignore other hints *)
	    fi
	elsif (op = CLC) cand (PTregister <> 0) then
		(* CLC is an assign of PTregister into 6 *)
		cat := ASSIGN;
		x_var := RL;
		other^ := PTregister;
		PTregister := 0
	fi;



    if (op > MAX_LABEL_OP) then

	let
	  a1 == to_arg [arg1 b];
	  a2 == arg2 b;
	  a3 == arg3 b
	in

	  if (op = ALA) cor (op = ALH) ==>
			if (a2 = SK) then make_dull []
			else
			    let sz == if
					a1 = C1 ==> 1
				     #	a1 = C2 ==> 2
				     #  a1 = C3 ==> 3
				     #	a1 = C4 ==> 4
				     # else 	  ==> 0 (* unknown - may get
							   worked out later *)
				    fi
			    in
				cat := if hinted then ALCSTACK else ALLOC fi;
				    (* If this is preceded by an HINT ONS,  *)
				    (* then enough hints for stack 	    *)
				    (* allocation are already provided.  We *)
				    (* would only confuse the issue.	    *)
				hinted := False;   (* 1/31/91 - HB *)
				x_var := a2;
			    	(* other will be set when alloc nums are assigned *)
				size^ := if (op = ALH) then sz (* composite *)
					    else - sz fi
			    ni
			fi
	   # (op = HINT)   ==>
			if a1 = DEAD then
			    cat := ALCNIL;
			    x_var := a2
			else
			    hint_type := a1;
			    hinted := True;
			    if hint_type = AL then
				hint_info := if (a3 = 0) then a2 
							(* composite *)
						else - a2 fi 
			    elsif hint_type = STSZ
				then hint_info := a2
			    fi;
			    make_dull []
			fi

	   # (op = DCL)    ==>
			last_var := MAX (a1, last_var);
(* these next two stmts are left out because all registers are inited to none
	in the start node.
			cat := ALCNIL;
			x_var := a1
*)
(* there was no code here ^ before *)

	   # (op = LDL)    ==> (* alloc of a static object *)
			cat := ALCSTAT;
			x_var := a1

	   # (op = LDN) cor (op = LDS) cor
	     (op = UDC) cor (op = ADP) cor (op = TRU) cor (op = FLS)
			   ==>
			(* equivalent to x := nil *)
			cat := ALCNIL;
			x_var := if (op = ADP) then a3
				 elsif (op = LDN) then a2
				 else a1
				 fi;
			if (x_var = SK) then make_dull [] fi
	   # (op = GAR)  ==>
			(* a2 := the loc numbered arg1(param#) *)
			cat := GARCAT;
			x_var := a2;
			other^ := a1

	   # (op = MOV)   ==>
			(* arg2 := arg1 *)
			if (a2 = SK) then
			    make_dull []
			elsif a1 = a2 then
			    make_dull []
			elsif (a1 = UN) then
			    cat := ALCSTAT;
			    x_var := a2
			elsif (a1 = SP) then
			    cat := ALCSTACK;
			    x_var := a2
			else
			    cat := ASSIGN;
			    x_var := a2;
			    other^ := a1
			fi
	   # (op = ARG)    ==> 
			x_var := a1; (* arg# *)
			other^ := a2; (* var being passed *)
			extra := next_call_num;
			if hinted cand ((hint_type = NP) cor (hint_type = PT))
				cand (a1 < MAXWATCHARGS)
			then
			    hinted := False;
			    cat := WATCHARG; (* need to see it to avoid
					deallocing between ARG and Call *)
			    if (hint_type = PT) then
			(* set PTregister to a2, so that the call will know 
				to be an ASSIGN of special register*)
				PTregister := a2 (* non-zero *)
			    fi
			else
			    cat := if ONLYINTRA then TOMEM
				elsif is_defined_fn [fn_names.next_call_num, labels]
				then ARGCAT
				else TOMEM (* can't hope to find out more yet *)
				fi
			fi

	   # (op = PSH) cor (op = STI)
		    ==>
			if (a1 = SK) cand (op = STI) then 
				(* noop *)
				make_dull []
			else
			(* var is stored in memory *)
			cat := TOMEM;
		(* the var is put in other, because it is not used on LHS *)
		(* NB x_var is used to mark hinted STI's *)
			other^ := if (op = PSH) ==> a1
				  # else        ==> a3
				 fi;
			if hinted cand (op = STI) cand (hint_type = STSZ) then
				(* mark the quad *)
OPEN
put [q];DEBUG(" is hinted STI for loc ");put hint_info;DEBUG("\n");
CLOSE
				x_var := hint_info; (* non-zero to be seen *)
				extra := last_var;
				hinted := False
			fi
			fi
	   # (op = TAR)   ==>
			(* both args are stored in memeory *)
			cat := CATTAR;
			x_var := a1;
			other^ := a2
	   # (op = LDI) cor (op = LDC)  ==>
			(* var gets anything (any loc); doesn't affect RCs *)
			if (a3 = SK) then make_dull []
			else
				cat := ANY;
				x_var := a3
			fi
	   # else  ==> Null []
	  fi;

	ni (* a1, a2, a3 *)


    fi; (* non-string ops *)
	(* note that CLL is the only op of those that take string args,
	   that needs to be categorised, and that's been done *)

	(* use inputsummary to refine classification for many calls and args *)
	if ((op = CLL) cor (op = CLC)) cand (cat = ALCNIL) then 
	(* these are the only ones that could be interfile, and summarized;
		for UNSAFE calls can't use summary info *)
	    let name == fn_names.extra
	    in
		if is_summarized [name, inputsummary] then
		    (* refine all the args that were already classified *)
		(* had to wait till now till the call name was known *)
OPEN
put "Refine classification for interfile fn call\n";
CLOSE
		    let argq == Short$New [q-1];
			    nargs == if (op = CLL) then 1 
					else to_arg [arg1 [basics.q]]  fi;
			    nargseen == Short$New [0]
		    in
			    (* iterate over preceding arg instrs till highest
				numbered one, categorizing the call using
				interfile info  *)
			do nargseen < nargs ==>
			    do opc [basics.argq] <> ARG ==> argq -= 1 od;
			    nargseen := to_arg [arg1 [basics.argq]];
			    let argcat == SummaryIn$categorize_arg [name,
						nargseen, inputsummary]
				(* argcat = TOMEM or WATCHARG - since the arg
					must have been classified as TOTMEM
					before, only need to change stuff if 
					it became a WATCHARG *)
			    in
				  if argcat = WATCHARG then
				    let old_maq == more.argq
				    in
OPEN
    put "Interfile Info allows arg#";put nargseen;put " to fn ";put name;
    put " to be categorized as WATCHARG\n";
CLOSE
			    more.argq := More$Mk [WATCHARG, -nargseen,
					other [old_maq], size [old_maq],
					extra [old_maq]]
				    ni
				  fi
			    ni (* argcat *)
			od
		    ni; (* argq *)

		    (* refine this call quad information *)
		    let	callsumm == SummaryIn$categorize_call
						     [name, inputsummary]
		    in
			cat := category callsumm;
OPEN
put "Interfile Info allows ";put name;put " to be given cat = ";put cat;put "\n";
CLOSE
			if cat = ASSIGN then other^ := supplement callsumm fi;
			if cat = ALLOC then size^ := supplement callsumm fi
		    ni;
		fi (* if is_summarized *)
	    ni
	fi;


    if not became_dull then
	more.q := More$Mk [cat, x_var, other, size, extra]
    fi;

ni (* b, op, cat ... *)


fi; (* dull -> do no more *)
