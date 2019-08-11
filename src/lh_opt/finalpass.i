(* This stage takes the rc and locs values for each function and
	runs through working out
		 which derefs actually allow deallocation
		 which of those require that I move the DEA and UDC (or
			DEAD hint)  to after 
			the call (record list of locs passed to a
			function and the callnum for those ARG instrs.)
		 watch for hinted STIs and decide what to do.
*)





let
	tmpblist == (List [Short])$New [];

	argedlocs == LocList$New[];
	argtable == (Array [MAXWATCHARGS + 1, Short])$New [];

	DEA_decide == func [q : val Short; clob : val C; locs : val LOCS;
			    impure] val Void {
		    (* maintain array of currently passed args *)
		    (* keep a loclist of objects which may be the subject of
				an arg *)
		    if cat [more.q] = WATCHARG then
			let argnum == to_arg [arg1 [basics.q]];
			    source == (other [more.q])^
			in
				argtable.argnum := source;

(*put "WATCHARG register#";put source;put " gives set of arged locs ";*)
				argedlocs := LocList$merge [argedlocs, 
						LOCS$look [source, locs]]
(*;LocList$print [argedlocs]*)
			ni
		    fi;

		    if quad [clob] >= 0 then
			    (* tell UA about the DEA *)
			let sz == loctable.(loc [clob]);
			    xlocs == LOCS$look [xvar clob, locs]
			in
			    if Vflag then
				put "    quad "; put [q];
				put " clobbers the ";
				if sz < 0 then
				    put "atomic object of size "; put [-sz];
				elsif sz = 0 then
				    put "object";
				else
				    put "composite object of size "; put [sz];
				fi;
				put " pointed to by ";put [xvar clob]; put "\n";
				if any_member [xlocs, argedlocs] then
				    put "\tinsering delayed deallocation\n"
				else
				    put "\tinserting ordinary deallocation\n";
				fi;
				flush [File$stdout];
			    fi;

			    if any_member [xlocs, argedlocs] then
			      let CALLquad == (* search forward for CLC *)
				    (let i == Short$New [q]
				     in do (i < num_quads)
						cand ((opc [basics.i]) <> CLC)
								==>
						i += 1
					od;
					i
				     ni)
			      in
OPEN
put "CALLquad is ";put CALLquad;put "\n";flush [File$stdout];
CLOSE

				(* assume that CALLquad is the right one and is
					preceded by an LBA *)
				if (xvar clob) = RL then

				  if (opc [basics.q] <> CLC) then

				    UA$add [updates, q, KILL_update, 0,0,0,0]
				  fi;

				  (* put DCL, and MOV of RL at LBA quad *)
				  UA$add [updates, CALLquad - 1, RL_before,
							   avail_var, 0, 0, 0];

				   (* put DEA and UDC of avail_var after CLC *)
				   UA$add [updates, CALLquad + 1, RL_after,
							avail_var, sz, 0, 0];
				   avail_var += 1

				else (* HINT DEAD or UDC of xvar *)
				    (* delay this quad till after the CALL *)
				    UA$add [updates, q, KILL_update, 0,0,0,0];

				  let kind == if (opc [basics.q]) = UDC 
						then postponed_UDC 
						else postponed_DEAD fi
				  in
				    UA$add [updates, CALLquad + 1, kind, 
							xvar clob, sz, 0, 0]
				  ni
				fi
			      ni
			    else (* the deallocatable object is not currently
						the subject of an ARG *)
				let op == opc [basics.q];
				    qd == if (op = CLC) cor (op = LDS) cor 
								(op = LDL)
					then q - 1
					else q
				        fi
				   (* really need to put the hint info at the 
				     LBA quad if the opc is CLC, LDL, LDS *)
				in
					UA$add [updates, qd, DEA_update,
							xvar [clob], sz, 0,0]
				ni
			    fi
			ni (* sz *)
		    fi (* there is a clobberer *)
		}; (* DEA_decide *)


	STI_decide == func [q, testvar : val Short;
			    rc : val RC; locs : val LOCS; impure] val Void {
			(* testvar is the xvar field for q *)

	      if (opc [basics.q] = STI) cand (testvar <> 0) then
		(* this is a hinted STI *)
OPEN
put ["\ndecide STI quad "]; put [q];
CLOSE
		let entry == look [testvar, locs];
		    tloc == oneloc [entry];
		    doit ==
(* 'size' if I should do it; 0 otherwise *)
(* if tvar refers only to what we hoped and tvar is the only reference *)
(* note: if size actually is 0 there is no need to do the STI thing anyway *)

			if (tloc <> 0)
			    cand (((rc^).tloc) = RefNum$In[1])
			then loctable.tloc (* size *)
			else 0
			fi
		in
		    if doit > 0 then
		(* if doit represents size (of composite obj) *)
		(* tag the previous quad i.e. the HINT STSZ *)
OPEN
put "; the var ";put testvar;put " has the only ref to ";put tloc;
put " whose size is ";put doit;
CLOSE
			if Vflag then
			    put "\tClosure has single ref.  ";
			    put "Storing size at quad ";
			    put q;
			    put "\n"
			fi;
			UA$add [updates, q - 1, STI_update,
			        to_arg [arg1 [basics.q]], arg2 [basics.q],
							avail_var, doit];
			avail_var += 1

		    fi
OPEN
;put "\n"
CLOSE
		ni
	      fi (* hinted STI ? *)

	    }; (* STI_decide *)

	bbApply == func [bbnum : val Short; rc : val RC; locs : val LOCS;
								impure] {

		let qlist == QuadList $New [];
		    var_list == vlist [bb.bbnum]
		in
OPEN
put ["\nFinal pass bbApply "];
CLOSE

		    qlist := quads [bb.bbnum];
		    do not is_nil [qlist] ==>
			let q == head qlist;
			    moreq == more.q;
			    catq == cat [moreq];
			    xq == x_var [moreq];
			    yq == (other [moreq])^;
			    isCLC == (opc [basics.q] = CLC
			              cor opc[basics.q] = CLL);
			                    (* 1/31/91 - HB *)
			    clob == C$New []
			in

(*put " ";put [q];*)

			    STI_decide [q, xq, rc, locs];

			    clob := RC $apply [q, catq, xq, yq, rc, 
						locs, var_list, True];
				(* watches for DEALLOCs in deref *)

			    DEA_decide [q, clob, locs];
			    if isCLC then argedlocs := LocList$top [] fi;

			    LOCS $apply [catq, xq, yq, locs, var_list, True];

			ni;
			qlist := tail qlist
		    od
		ni
	} (* bbapply *)


in
OPEN
DEBUG("Final Pass, which looks for deallocation opportunities ...");
CLOSE

	(* for each bb in blist? meet all its preds and then bbapply -
		all the interpretation gets done *)

		    tmpblist := blist [fn_info];
		    argedlocs := LocList$top [];

		    do not is_nil [tmpblist]     ==>

		      let bbnum == head tmpblist;
			  var_list == vlist [bb.bbnum];
			  tmp_RC == RC$top [];
		  	  tmp_LOCS == if bbnum = 0 then 
					LOCS$none [var_list]
				      else
					LOCS $top [var_list]
				      fi;
		  	  ps == BBList $New [];	(* for preds *)
			  p == Short$New []
		      in
(* if I stored values for start of bbs I wouldn't have to do this meet stuff *)
			  (* meet over all preds of bbnum *)

			  ps := (bbpreds [fn_info]).bbnum;
		    	  do not is_nil [ps]   ==>
				p := head ps;
				ps := tail ps;
				RC$pair_meet [rc.p, tmp_RC];
				LOCS $pair_meet [locs.p, tmp_LOCS, var_list]
			  od;


OPEN
DEBUG("After meet tmp_LOCS is\n");
LOCS $print [tmp_LOCS, var_list];
CLOSE

OPEN DEBUG ("\n");
DEBUG("APPLYs to ");put[bbnum];DEBUG("\n");
put ["\nbb = "];put bbnum;
CLOSE


			  bbApply [bbnum, tmp_RC, tmp_LOCS];


OPEN
DEBUG("After APPLY tmp_LOCS is\n");
LOCS $print [tmp_LOCS, var_list];
CLOSE

		    ni; (* tmp_RC *)

		    tmpblist := tail tmpblist
		od; (* blist *)

ni
