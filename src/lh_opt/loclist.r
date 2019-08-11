# include "defs.h"
(* Implements lists of objects, such as the list of objects that may be *)
(* referenced by a certain virtual register.				*)

(* NB July 14th : I am pretty sure that MEM_OBJ and NONE_OBJ are identical
   in effect, (making ANY and ALCNIL identical). Both represent non-heap
   objects.
   But the nodes MEMNODE and NONENODE are used differently : MEMNODE is
   a sink, while NONE is only a source.
   Perhaps MEM_OBJ and NONE_OBJ should be amalgamated (and ANY and ALCNIL)
   and the distinction between MEMNODE and NONENODE should be made clear *)

(* STACK_OBJ and STATIC_OBJ are effectively equivalent at the moment (July 14)
   because they both force DEAs to be with size 0, or if only they appear then
   there is no point in deallocing. But it might be useful to distinguish them
   later.  			*)

(* Things which point to nothing must have NONE_OBJ on their loclists
	(see "x := loci;if - then y := x else z := x fi;x := nil; y := nil"
	    It looks as though the y := nil kills the last ref to loci but
	y might not kill any references at all; thus oneloc must check for
	NONE on the list; init bb values are set to {NONE}, rather than {}.
*)

(* for objects that are not reference counted use negative nos.;
   keep MEM_OBJ as the lowest numbered  - easier to find 
	MEM_OBJ = -4 : if it appears on a list it means that nothing is
			really known about the objects that could be on the 
			list.
		There is no point having other objects on lists with 
		MEM_OBJ, but no harm either, except efficiency.
	NONE_OBJ = -3 : similar to MEM_OBJ; means variable might point to 
			a non-object (nil, uninit..). Can't deallocate if this
			appears.
	STACK_OBJ = -2 : similar to STATIC_OBJ, usedfor things taken off stack.
	STATIC_OBJ = -1 : if this appears on a list then any deallocation has
			to be with size = 0 because the object might not be a
			heap object. Or the fn which is returning, if it is an
			allocator, has to be flagged as being possibly not a
			heap object *)

(* the nil ref will indicate not_found, empty list --> top;
   bottom is made from num_locs (= list 1,2,3...)	*)

(* this file has full control over representation
	I think the lists are all ordered *)

(* note that because of the way locs.r uses these loclists, only LOCS$find and
   LOCS$look will see notfound values - they then fix it so that anyone else
   only sees top. Thus other functions need not cope with notfound, as things
   stand now *)

func [num_locs : val Short] {

    let
	exit == func [val Short; impure] val Void { extern "_exit" };

	LocTable === (Array [500, Short]);(* gives node # for representative
						of the size of alloc, or the
						result node for MAYBEALC fns *)
	LL == extend {(List [Short])} with S
	    {
		member == func [x : val Short; s : val S] val Boolean {
				if is_nil s then False
				elsif x = (head s) then True
				elsif x < (head s) then False
				else member [x, tail s]
				fi
			};

		any_member == func [s1, s2 : val S] val Boolean {
			if is_nil s1 then False
			else member [head s1, s2]
				cor any_member [tail s1, s2]
			fi
			};

		eq_lists == func [s1, s2 : val S] val Boolean {
				if is_nil s1 then
						is_nil s2
				elsif is_nil s2 then False (* <= s1 not nil *)
				else
					(head s1 = head s2) cand
				        eq_lists [tail s1, tail s2]
				fi
			};

		remove == func [s1, s2 : val S; neednone : var Boolean] val S {
			if is_nil s2  then s2
			elsif (head s2 > 0) (* real loc *) cand 
				member [head s2, s1] then
				(* remove head s2 from s2, and make sure that
					there is a none_obj on the final s2 *)
				neednone := True;
				remove [s1, tail s2, neednone]
			else cons [head s2, remove [s1, tail s2, neednone]]
			fi			
			};

		add_none == func [s : val S] val S {
			if (is_nil s) cor (head s) > NONE_OBJ then
					cons [NONE_OBJ, s]
			elsif (head s) = NONE_OBJ then s
			else cons [head s, add_none [tail s]]
			fi
			};

		mrg == func [s1, s2 : val S] val S {

			    if is_nil s1  ==> s2
			     # is_nil s2  ==> s1
			     # else   ==>
				 if  (head s1) < (head s2) ==>
					cons [head s1, mrg [tail s1, s2]]
				  #  (head s1) = (head s2) ==>
					cons [head s1, mrg [tail s1, tail s2]]
				  #  else   ==>
					cons [head s2, mrg [s1, tail s2]]
				fi
			    fi
			}
	     } (* with *)
    in
	(Ref [LL])
    with L {
	create == func [x : val List [Short]] val L {
		let v == LL$New []
		in 
		    v := LL$In [x];
		    L$In [v]
		ni
	    };

	(* creates a loclist from a single loc *)
	create == func [i : val Short] val L {
		  let c == LL$New []
		  in
		    (* if i > num_locs then exit [1] fi; *)
		    c := LL$cons [i, LL$''];
		    L$In [c]
		  ni
	    };

	(* to get a copy of the list, not just the ref cell - because 
		replace messes things up -- not used on june 29th !! *)
	copy == func [l : val L; impure] val L {
			let n == L$New [];
			    ll == LL$New[]
			in if l = L$notfound [] then n := L$notfound []
			   else
				ll := l^;
				n := L$In[ll]
			   fi;
			   n	(* returned *)
			ni
		    };

	(* this value is used for the hash table - if lookup produces notfound
		then find inserts a top value instead		*)
	notfound == func [] val L {L$Nil []};

	(* put this in-line *)
	isnotfound == func [l : val L; impure] val Boolean
			{ l = L$notfound };

	top == func [] val L (* ref to empty list *)
			{ L$create [(List [Short])$''] };

	is_top == func [l : val L;impure] val Boolean
			{LL$is_nil [l^]};

	(* this is used when putting things into memory to have a list of
		locs that mem_var could have - 
	     MEM_OBJ is used when taking things out of memory *)
	bottom == func [] val L { 
			let b == LL$New [];
			    i == Short$New []
			in
			    i := num_locs; (* to produce an ordered list *)
			    b := LL$'';
			    do i > 0 ==>
				b := cons [i, b];
				i -= 1
			    od;
			    L$In [b]
			ni
		 };

	(* used to make l1 refer to the list l2 refers to *)
	replace == func [l1, l2 : val L; impure] val Void
			{ l1^ := l2^ };

	member == func [x : val Short; l : val L;impure] val Boolean
			{ LL$member [x, l^] };

	any_member == func [l1, l2 : val L; impure] val Boolean
	(* is any element of l1 a member of l2 *)
			{ LL$any_member [l1^, l2^] };

	eq == func [l1, l2 : val L; impure ] val Boolean
			{ LL$eq_lists [l1^, l2^] };
	(* I depend on order - check that creates do it right *)

	removelocs == func [l1, l2 : val L;impure] val L {
		(* remove all real locs that are in l1 from l2, and if
			any are removed make sure to put in NONE_OBJ *)
		let need_none == Boolean$New [False];
		    removedlist == LL$remove [l1^, l2^, need_none];
		    finallist == if need_none then LL$add_none [removedlist]
				  else removedlist fi
		in
		    L$create [LL$Out [finallist]]
		ni
		};

	merge == func [l1, l2 : val L; impure] val L
			{ L$create [LL$Out [LL$mrg [l1^, l2^]]] };

	(* true if MEM or NONE appears *)
	unknown == func [l : val L; impure] val Boolean
			{ not is_nil [l^] cand ((head [l^]) = MEM_OBJ 
						cor (head [l^]) = NONE_OBJ)};

	(* it is feasible to look into deallocing a variable if its loclist
		l, represents some real locs (> 0) 
		but does not represent MEM_OBJ or NONE (i.e. not unknown) *)
	feasible == func [l : val L; impure] val Boolean
			{ not unknown [l]
			  cand 
			    (* there is an element > 0 *)
			    let t == LL$New []
			    in t := l^;
			       do not (is_nil [t]) cand [head t] <= 0 ==>
					t := tail t
				od;
				(* now if t is not nil, there is an elt > 0 *)
				not is_nil [t]
			    ni
			};

	(* if l is a one (real) location list return that  element,
					   		else return 0 *)
	oneloc == func [l : val L; impure] val Short {
		    (* if l is a one element list return that element *)
			let
			    lst == l^
			in
			    if (not is_nil [lst]) cand (is_nil [tail [lst]])
				cand (head [lst] > 0)
				(* i.e. neither MEM nor NON nor STATIC 
							    nor STACK there *)
			    then head [lst]
			    else 0
			    fi
			ni
		    };

	rc_map == func [l : val L; 
		     f : func [val Short;impure] val Void; 
 		    					impure] val Void {
	(* maps over all the real locs on the list - that is all the ones with 
		which there is an associated ref ct *)

			    let list == LL$New []
			    in
				list := l^;
				do not is_nil [list] ==>
				    if (head list) > 0 then f [head list] fi;
				    list := tail list
				od
			    ni
		};

	loc_map == func [l : val L; loctable : val LocTable;
		     f : func [val Short;impure] val Void; 
 		    					impure] val Void {
	(* maps over all locs for which there is a graph node,
	     mapping locs to graph nodes, and then applying
		the function f, -- for deciding about graph edges *)
			let list == LL$New []
			in
			    list := l^;
			    if not is_nil [list] then
				if (head list) = MEM_OBJ then
					f MEMNODE;
					list := tail list
				fi
			    fi;
			    if not is_nil [list] then
				if (head list) = NONE_OBJ then
					f NONENODE;
					list := tail list
				fi
			    fi;
			    if not is_nil [list] then
				if (head list) = STACK_OBJ then
					f STACKNODE;
					list := tail list
				fi
			    fi;
			    if not is_nil [list] then
				if (head list) = STATIC_OBJ then
					f STATICNODE;
					list := tail list
				fi
			    fi;
				(* now everything in list is something whose
					node can be found in loctable *)
			    do not is_nil [list] ==>
				    let node == loctable.(head list)
				    in
					f node
				    ni;
				    list := tail list
			    od
			ni
		};

	and_map == func [l : val L;
			 f : func [val Short; impure] val Boolean;
							 impure] val Boolean {
	(* this function is used to test if all locs on the list have rc = 1
		When it is called we already know that MEM or NONE are not on
		the list - these would preclude DEAllocation or functions from
		being ALLOCators (in is_alloc) *)

			    let list == LL$New [];
				value == Boolean$New [True]
			    in
				list := l^;
				do (value = True) cand (not is_nil [list]) ==>
				    if (head list) > 0
				    then value := f [head list]
				    fi;
				    list := tail list
				od;
				value
			    ni

		};

	print == func [l : val L; impure] val Void {
			if is_nil [l^] then put ["top"]
			else
			    let list == LL$New []
			    in
				list := l^;
				do not is_nil [list] ==>
					put [" "];
					put [head list];
					list := tail list
				od
			    ni
			fi;
			put ["\n"]
		};
 
	} (* with *)
      hide { In; Out; ^ (* which refer to LL *)}
    ni
   }
