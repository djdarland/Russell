(* Compute interprocedural summary information *)
OPEN
	DEBUG( "\nSTART first pass, function by function; num_fns =");
	put num_fns; put "\n";
CLOSE

    fn_index := 0;
    do fn_index < num_fns ==> 

      let fn_info == fn_array.fn_index;
	  bb == bbs [fn_info];
	  tmpblist == BBList $New [];

	  alloc_num == Short$New [1];(* 0 is used to refer to nil ?? *)
	  MAXLOCSperDF === 500;

	  LocTable === (Array [500, Short]);(* gives node # for representative
						of the size of alloc, or the
						result node for MAYBEALC fns *)
	  loctable == LocTable$New [];

      in
OPEN
	put ["\n\n"];
	put [name fn_info];put "\n";
CLOSE

(* ?? *)loctable.0 := GENERICNODE; (* default alloc node unknown size/atomicity *)

	(* number the args as locs - the ith arg is the ith loc *)
	(* the graph node can be found from the basenode for this fn *)
	let base == basenode [fn_info]
	in
		do alloc_num <= numargs [fn_info] ==>
		    loctable.alloc_num := base + alloc_num;
		    (* that is the graph node for this arg *)
		    alloc_num += 1
		od
	ni;

	(* look through the quads for other allocs and maybeallocs to number *)
	tmpblist := blist [fn_info];
	do not is_nil [tmpblist] ==>

	    let qlist == QuadList $New [];
		q == Short$New []
	    in
		qlist := quads [bb.(head tmpblist)];

		do not is_nil [qlist] ==>
		    let
		      q == head qlist;
		      mq == more.q (*exists for all non-dull qds*)
		    in
		      if (cat[mq] = ALLOC)  ==>
(* do I have a node for allocs of this ((size [mq])^) type - if not make one;
	set the loctable.alloc_num to the node # *)
			    let locsize == (size [mq])^
			    in
OPEN
	put "quad ";put q;put " allocator # ";
	put alloc_num;put " of size ";put locsize;
	put "\n";
CLOSE
				loctable.alloc_num := alloc_node [locsize,
									graph];
			    ni;
			    (other [mq])^ := alloc_num;
			    alloc_num += 1;
			    if alloc_num = (MAXLOCSperDF  - 1) then
			      put ["Too many allocators per component\n"]
			    fi

		      # (cat [mq] = MAYBEALC) ==>
OPEN
	put "quad ";put q;put " maybealloc loc# ";
	put alloc_num;File$flush[File$stdout];
	put q;put " MAYBEALC extra is ";put [extra mq];put " name ";
	put [fn_names.(extra [mq])];put "\n";File$flush[File$stdout];
CLOSE


			    loctable.alloc_num :=
				    let name == fn_names.(extra [mq]);
					fn_defn == def_fn_index [name, labels]
				    in
OPEN
	put ". Call to ";put name;
	put "\n";File$flush[File$stdout];
CLOSE
					basenode [fn_array.fn_defn]
					(* that is the graph node for the
						result of the fn called *)
				    ni;
			    (other [mq])^ := alloc_num;
			    alloc_num += 1;
			    if alloc_num = (MAXLOCSperDF  - 1) then
			      put ["Too many allocators per component\n"]
			    fi

		      # else ==> Null []
		      fi
		    ni;
		    qlist := tail qlist
	    
		od (* iterate over quads in this bb *)
	    ni;
	    tmpblist := tail tmpblist
	od; (* iterate through blist *)


OPEN	DEBUG( "  nlocs :");
	put [alloc_num - 1];put " ; nodes ";
	let l == Short$New [1]
	in do l < (alloc_num) ==>
			put [" "];
			put [loctable.l];
			l += 1
		   od;
		   put "\n"
	ni;
CLOSE

(* end of computing that information *)



	if (alloc_num > 1) then (* there is at least one loc in the fn *)
	(* since there is a loc for each arg, and one for most calls, this
		is unlikely to help *)

	(********************************************************)
	(* set up the lattices for this component		*)
	(*************************		*****************)
let
    nbbs == length [fn_info];
    nlocs == alloc_num - 1;	(* locs numbered 1 .. nlocs *)

(**************************** L O C S **********************)

    LocList === (extern {"loclist"} [nlocs]);

    LOCS  === (extern {"flocs"} [nlocs]);


    (* these arrays will be indexed by bb numbering *)
    locs == (Array [nbbs, LOCS])$New [];

    Note === prod {
		(* think about recording the node for the arg - use the callnum
			and argnum to work it out *)
		callnum : val Short; 
		argnum : val Short;
		locs : val LocList (* the set of locs that could be referred
					to by the arg passed as argnum-th
					arg in the callnum-th call *)
	    };
    Notes === List [Note];
    notes == Notes$New [];

    n_bbs == length fn_info;
    bbnum == Short$New [];

    bbApply == func [bbnum : val Short; locs : val LOCS; impure] {

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
			    LOCS $apply [catq, xq, yq, extra [moreq], 
						locs, var_list, notes]
			ni;
			qlist := tail qlist
		    od
		ni
	}


in

	(* Initialise the entries for the basic blocks in this function to
	{}, and then Apply the node, to give exit values    *)
	(* go in number order to catch bbs which are not on the order list,
		because they have no preds, mysteriously  -
	   this weirdness should have gone away as of July 12 *)
	locs.0 := LOCS$none [vlist [bb.0]];
	bbApply [0, locs.0];




	bbnum := 1;
	do bbnum < n_bbs ==>
	    let	var_list == vlist [bb.bbnum]
	    in  
		locs.bbnum := LOCS $top [var_list];
		bbApply [bbnum, locs.bbnum]
OPEN
	;put "After init bbApply :";LOCS$print [locs.bbnum, var_list];put "\n";
CLOSE
	    ni;
	    bbnum += 1
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
		notes := (List [Note])$''; (* gets changed by ARGCAT quads 
						and ops that send stuff to mem
						in locs apply *)

		(* go thro the bbs in blist order (i.e. DF order) *)
		tmpblist := blist [fn_info];

		do not is_nil [tmpblist]     ==>
OPEN
	put [".\n"];
CLOSE
		  let bbnum == head tmpblist;
		      var_list == vlist [bb.bbnum];
		      tmp_LOCS == if bbnum = 0 then
					LOCS $none [var_list]
				   else
					LOCS $top [var_list]
				   fi;
		      ps == BBList $New []	(* for preds *)
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
			LOCS $pair_meet [locs.(head ps), tmp_LOCS, var_list];
			ps := tail ps
		    od;

	(*******************************************************************)
OPEN
	DEBUG("After meet tmp_LOCS is\n");
	LOCS $print [tmp_LOCS, var_list];
CLOSE

OPEN
	DEBUG ("\n");
	DEBUG("APPLYs to ");put[bbnum];DEBUG("\n");
	put ["\nbb = "];put bbnum;
CLOSE


		    bbApply [bbnum, tmp_LOCS];

OPEN
	DEBUG("After APPLY tmp_LOCS is\n");
	LOCS $print [tmp_LOCS, var_list];
CLOSE

		    (* test for equality between the old bbnum values and the 
			apply [meet of all preds of bbnum]   values *)
		    if (not change) then
			if (not LOCS $equal [tmp_LOCS, locs.bbnum, var_list])
			then
OPEN
	DEBUG("\nCHANGED LOCS\n");
	DEBUG("tmp_LOCS is\n");
	LOCS $print [tmp_LOCS, var_list];
	DEBUG("locs.bbnum is\n");
	LOCS $print [locs.bbnum, var_list];
CLOSE
					   change := True
			fi
		    fi;

		    (* update the values *)
		    locs.bbnum := tmp_LOCS;

		    tmpblist := tail tmpblist

		  ni (* bbnum, var_list, tmp_LOCS *)

		od; (* gone thro all bbs in this component's blist -
							 check for change *)

	    od (* now no change; => interpret the results *)
	ni (* change *);


	(* Finished Data Flow for this Component *)
	(* now interpret the results for this component before rc and locs
			disappear *)

OPEN
	(* perhaps output the full set of notes *)
	(* look at locs of RL at end of last basic block and print their rcs
		and their size or fn_name. Find all notes on which any of the 
		locs appears *)

	let ns == (List [Note])$New []
	in
		ns := notes;
		do not is_nil [ns] ==>
		    let n == head ns
		    in
			put [callnum n]; put " ";put [fn_names.(callnum n)];
			put " ";put [argnum n]; put " "; print [locs n];
			put "\n"
		    ni;
		    ns := tail ns
		od
	ni;
CLOSE

	(* interpret the RL value, the notes, and the mem_var value -
		make the necessary graph edges *)

	(* for each loc in RL, make an edge from the corresponding node to
		the result node for this function *)
	let resultnode == basenode [fn_info];
	    resultlocs == LOCS$look [RL, locs.(rtn_bb fn_info)];
	    locs_in_mem == LOCS$look [mem_var, locs.(rtn_bb fn_info)]
	in
OPEN
	put "the locs that reach result node (";put resultnode;put ") are ";
	LocList$print [resultlocs];
	put "\n";
CLOSE
(* need to replace any real locs that are also in mem_var's list by NONE_OBJ *)
	    let rlocs_not_in_mem == 
			LocList$removelocs [locs_in_mem, resultlocs]
	    in
		result_edges [resultnode, rlocs_not_in_mem, loctable, graph]
	    ni
	ni;

	(* for each arg appearing in mem_var at the end make an edge from
		that arg's node to the MEMNODE *)
	let memlocs == LOCS$look [mem_var, locs.(rtn_bb fn_info)]
	in
		args_edges [MEMNODE, memlocs, loctable, graph]
	ni;

	(* for the notes, compute the arg node for the call and arg nums,
		then make an edge from any arg on the loclist to that node *)
	do not is_nil [notes] ==>

	    let note == head notes;
		(* computation of argnode from callnum and argnum in the note*)
		(* I probably need a better map to facilitate this *)
		name == fn_names.(callnum note);
		index == def_fn_index [name, labels];(* name is the index'th 
						    fn defined in this file *)
		base == basenode [fn_array.index];
		argnode == base + (argnum note)
	    in

(* Unfortunately I can't see inside the graph here so this code won't compile
if (not is_arg [nodes.argnode]) cor (fn [to_arg [nodes.argnode]] <> name) cor 
	(num [to_arg [nodes.argnode]] <> argnum [note]) then
    put "\nSCREWED UP SOMETHING in call/ fn/ node maps\n" fi;
*)
if name [fn_array.index] <> name then 
put "\n**def_fn_index or something WRONG ";put index;put name;put "\n" fi;

		args_edges [argnode, locs note, loctable, graph]

	    ni;
	    notes := tail notes
	od;

ni  (* nbbs, .. , nlocs, .. LOCS, Notes .. *)

	fi (* else no point in considering it further *)


      ni; (* fn_info, bb, tmpblist, alloc_num, .. LocTable  *)

      fn_index += 1

    od; (* loop through all rooted components *)

OPEN
	print [graph];
CLOSE
Null []
