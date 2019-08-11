(* this note was down at redefining MAYBEALCs and ARGCATs *)
(* If the result could also come from somewhere else (see categorize_call) then
   the call is NOT an assign, and the ARG is not NP (unless I code things more
   carefully). If result can be ALLOC and arg1, for example, then effectively
   deref RL, then l(RL) := l(argvar) union {loc#}. This is true even if the
   arg also goes to memory. (Perhaps the data flow for whence_result should
   cope with it).
   At least I must recognise that there IS a reference to the arg after the
   call, namely the one from RL. By making the ARG into a TOMEM (i.e. a not
   NPed ARG) I achieve this, but lose out.
   For a better analysis, I need to record 3/4/5 bits for result code : alloc,
   stack/static, mem/none. Somehow the RC$apply and LOCS$apply have to cope 
   with the new kind of quad, and I have to store the relevant info with the 
   call quads.
   Then ARG would become NP again, i.e. WATCHARG  *)


let fn_info == fn_array.fn_index;
    bb == bbs [fn_info];
    tmpblist == BBList $New [];

    avail_var == Short$New [last_var + 1]; 
		(* for code transformations that require a temporary register *)
    alloc_num == Short$New [1]; (* 0 is used to refer to nil *)
    MAXLOCSperDF == 500;
    LocTable == (Array [MAXLOCSperDF, Short]);
    loctable == LocTable$New [];

    CallSumm === prod { category : val Short;
			supplement : val Short };
in

    if Vflag then
        put "looking for deallocation opportunities in ";
        put [name fn_info]; put "\n";
    fi;

    loctable.0 := 0; (* default entry is 0 => unknown size *)
    tmpblist := blist [fn_info];
    do not is_nil [tmpblist] ==>

	    let qlist == QuadList $New [];
		q == Short$New []
	    in
		qlist := quads [bb.(head tmpblist)];

		do not is_nil [qlist] ==>
		    let
		      q == head qlist;
		    in
		      if not ONLYINTRA then
(* Change the categories of MAYBEALCs and ARGCATS (leaving none of these) *)
(* Any arg that passes to its result becomes TOMEM unless it is the only thing
   that reaches the result.
   If it is the only thing that reaches the result then the ARG is WATCHARG and
   the call is ASSIGN.
   Calls that could return an arg as well as something else become ANYs.
   Most of all this is coded in summary.r or resultsummary.r.
*)
		      if (cat [more.q]) = MAYBEALC then
			(* redefine the 'more' entry for this quad *)
			    let mq == V [more.q];
				name == V [fn_names.(extra [mq])];
				(* index == def_fn_index [name, labels];
				   fnnode == basenode [fn_array.index];*)

				callsumm == Summary$categorize_call
						     [name, file_summary];
				cat == category callsumm;
				oth == Short$New [
					 if cat = ASSIGN then
						(* special arg register *)
						supplement callsumm
					 else (other [mq])^ (* unchanged *)
					 fi];
				sz == Short$New [
					if cat = ALLOC then 
					    supplement callsumm
					else (size [mq])^ (* unchanged *)
					fi]
			    in
				more.q := More$Mk [cat, x_var [mq], 
					(Ref [Short])$In [oth],
					(Ref [Short])$In [sz], extra [mq]]
			    ni

		      elsif (cat [more.q]) = ARGCAT then

			    let mq == V [more.q];
				name == fn_names.(extra [mq]);
				argnum == x_var [mq];
				callnum == extra [more.q];
				callquad == fn_callquads.callnum;
			(* is it possible for the call to be UNSAFE ?? *)
				category == if cat [more.callquad] = UNSAFE
					    then TOMEM
					    else
					  Summary$categorize_arg [name, argnum,
						      file_summary]
					    fi;
				(* that records args that are passed thro and
					returns TOMEM or WATCHARG as approp. *)
				xv == argnum
			    in
				more.q := More$Mk [category, xv, other [mq],
							size [mq], extra [mq]]
					(* only the cat and x_var can change *)
			    ni
		      fi
		      fi (* not ONLYINTRA *);


(* Now record any real allocators encountered *)

		      let mq == more.q;
			  bq == basics.q
		      in
		      if (cat[mq] = ALLOC) ==>

OPEN
DEBUG ("\n");DEBUG("allocator :"); put [q];DEBUG(" # ");put [alloc_num];
CLOSE

			    loctable.alloc_num := (size [mq])^;
OPEN
put " size=";put [loctable.alloc_num];put "\n";
CLOSE
			    (other [mq])^ := alloc_num;
			    alloc_num += 1;
			    if alloc_num = (MAXLOCSperDF  - 1) then
			      put ["Too many allocators per component"]
			    fi

		      # else ==> Null []
		      fi
		      ni (* mq *)
		    ni;
		    qlist := tail qlist
	    
		od (* iterate over quads in this bb *)
	    ni;
	    tmpblist := tail tmpblist
    od; (* iterate through blist *)

OPEN
    DEBUG( "  nlocs :");
    put [alloc_num - 1];
    let l == Short$New [1]
    in do l < (alloc_num) ==>
			put ["\n"];
			put [loctable.l];
			l += 1
		   od
    ni;
CLOSE

(* end of computing that information *)








    if (alloc_num > 1) then (* there is at least one loc in the fn *)

	(********************************************************)
	(* set up the lattices for this component		*)
	(*************************		*****************)
	let
	    nbbs == length [fn_info];
	    C === prod {quad : val Short;
			xvar : val Short;	
			loc : val Short };    (* RC$apply needs an arg of this type *)
	    nlocs == alloc_num - 1;	(* locs numbered 1 .. nlocs *)

	(************************** R C ************************************)
	    maxrc == 4;
	    RefNum === extern {"refnum"} [maxrc];

	    RArray === (Array [nlocs + 1, RefNum]);   (* locs numbered 1..nlocs;
						       0 is used for nil    *)

	    RC == extern {"rc"} [nlocs, maxrc, C];


	(**************************** L O C S **********************)

	    LocList === (extern {"loclist"} [nlocs]);

	    LOCS  === (extern {"locs"} [nlocs]);


	    (* these arrays will be indexed by bb numbering *)
	    rc == (Array [nbbs, RC])$New [];
	    locs == (Array [nbbs, LOCS])$New [];

	    n_bbs == length fn_info;
	    bbnum == Short$New [];

	    bbApply == func [bbnum : val Short; rc : val RC; locs : val LOCS;
								impure] {

		let qlist == QuadList $New [];
		    var_list == vlist [bb.bbnum]
		in
OPEN
put ["\nbbApply "];
CLOSE
		    qlist := quads [bb.bbnum];
		    do not is_nil [qlist] ==>
			let q == head qlist;
			    moreq == more.q;
			    catq == cat [moreq];
			    xq == x_var [moreq];
			    yq == (other [moreq])^;
			    isCLC == (opc [basics.q] = CLC)
			in
			    RC $apply [q, catq, xq, yq, rc, locs, var_list, 
									False];
			    LOCS $apply [catq, xq, yq, locs, var_list, False] 
OPEN
;put "After APPLY ";
put q;put " op=";put [opc [basics.q]];put " cat = ";put catq;put " xq = ";put xq;
LOCS$print [locs, var_list];
RC$print [rc]
CLOSE
			ni;
			qlist := tail qlist
		    od
		ni
	} (* bbApply *)


	in

	    (* Initialise the entries for the basic blocks in this function to
		{},and then Apply the node, to give exit values    *)
	    (* go in number order to catch bbs which are not on the order list,
		because they have no preds, mysteriously *)
	    (* INITIALISE *)
	    locs.0 := LOCS$none [vlist [bb.0]];
	    rc.0 := RC$top [];
	    bbApply [0, rc.0, locs.0];

	    bbnum := 1;
	    do bbnum < n_bbs ==>
		let var_list == vlist [bb.bbnum]
		in  
			locs.bbnum := LOCS $top [var_list];
			rc.bbnum := RC$top [];

			bbApply [bbnum, rc.bbnum, locs.bbnum];

			bbnum += 1
		ni
	    od;


	(* This is the heart of the algorithm *)
	    let
		change == Boolean$New [True]
	    in
		(* Iterate until no change .... *)
		do change   ==>

OPEN
    DEBUG ("another pass...\n");
CLOSE


		    change := False;
		    (* go thro the bbs in blist order (i.e. DF order) *)
		    tmpblist := blist [fn_info];

		    do not is_nil [tmpblist]     ==>
OPEN
put [".\n"];
CLOSE
		      let bbnum == head tmpblist;
			  var_list == vlist [bb.bbnum];
			  tmp_RC == RC$top []; (* even if bbnum = 0 *)
		  	  tmp_LOCS == if bbnum = 0 then
					LOCS$none [var_list]
				      else
					LOCS $top [var_list]
				      fi;
		  	  ps == BBList $New [];	(* for preds *)
			  p == Short$New []
		      in

			  (* meet over all preds of bbnum *)

	(*******************************************************************)
	(* the tmp values must use separate repns. from the .p's	*)
	(* however when there is only one predecessor, the most common  *)
	(* situation, there may be a quicker way to achieve the result 	*)
	(* where the tmps are COPIES of the .p's - N.B. the lists of	*)
	(* hash table entries must be copied, not just the array entry  *)
	(*******************************************************************)
			  ps := (bbpreds [fn_info]).bbnum;
		    	  do not is_nil [ps]   ==>
				p := head ps;

				ps := tail ps;
				RC$pair_meet [rc.p, tmp_RC];
				LOCS $pair_meet [locs.p, tmp_LOCS, var_list]
			  od;
	(*******************************************************************)

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

		    (* test for equality between the old bbnum values and the 
			apply [meet of all preds of bbnum]   values *)
			  if (not change) then
			  if (not RC$equal [tmp_RC, rc.bbnum]) ==>
OPEN
DEBUG("\nCHANGED RC\n");
CLOSE
					   change := True

			  # (not LOCS $equal [tmp_LOCS, 
						locs.bbnum, var_list]) ==>
OPEN
DEBUG("\nCHANGED LOCS\n");
DEBUG("tmp_LOCS is\n");
LOCS $print [tmp_LOCS, var_list];
DEBUG("locs.bbnum is\n");
LOCS $print [locs.bbnum, var_list];
CLOSE
					   change := True
			  # else  ==>   Null []
			  fi
		    	  fi;

			  (* update the values *)
			  rc.bbnum := tmp_RC;
			  locs.bbnum := tmp_LOCS;

		      ni; (* tmp_RC *)

		      tmpblist := tail tmpblist

		    od; (* gone thro all bbs in this component's blist -
							 check for change *)

		od (* now no change; => interpret the results *)
	    ni (* change *);

	    (* Finished Data Flow for this Component *)
	    (* now interpret the results for this component before rc and locs
						disappear *)
(* finalpass (for this function) needs to see rc and locs values, 
							loctable, ... *)

# include "finalpass.i"


	ni  (* nbbs, ..  nlocs, .. RC, .. LOCS, *)


    fi (* else no point in considering it further *)


ni; (* fn_info, bb, tmpblist, alloc_num, .. LocTable  *)
