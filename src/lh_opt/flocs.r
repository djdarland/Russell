(* analogous to locs.r in that it maps virtual registers to allocation
   sites.  This is a variant designed for the first pass, which gathers
   interprocedural summary info. *)
# include "defs.h"
# define ABS(I) (if I < 0 then - I else I fi)

func [nlocs : val Short] {
	let 
	    LocList === (extern {"loclist"} [nlocs]);

	    Note === prod {
			callnum : val Short;
			argnum : val Short;
			locs : val LocList (* the set of locs that could be
					refed by the arg passed as argnum-th
					arg in the callnum-th call *)
	    };


	    tablesize == 64;
	    H == (extern {"hash"} 
			[tablesize, Short, LocList, LocList$notfound[]])
	in
	    (Ref [H])

	      with L {

	    (* Look just looks up the var. It returns top (empty loclist) if
		it does not find an entry *)
	    look == func [key : val Short; table : val L;impure]
							 val LocList {
			let hshkey == ABS(key);
			    a == H$lookup [hshkey, key, table^]
			in
			    if a = LocList$notfound [] then
				LocList$top []
			    else 
				a
			    fi
			ni
		    };

	    (* find will insert a 'top' value if it doesn't find the var and
		thus always returns a valid entry - which is a ref whose value
		can be changed 		*)
	    find == func [key : val Short; table : val L;impure] val LocList {
			let hshkey == ABS(key);
			    a == H$lookup [hshkey, key, table^]
			in
			    if a = LocList$notfound [] then
if key >= 0 then 
	DEBUG("\n HASH find unsuccessful - screwed up var lists. var ");
	put [key];put "\n"
fi;
				H$insert [hshkey, key, LocList$top [], table^];
				H$lookup [hshkey, key, table^]
			    else 
				a
			    fi
			ni
		 };

	    top == func [ vlist : val List [Short]] val L {
		(* Insert a top loclist for each var *)
			let h == H$Init [];
			    vs == (List [Short])$New []
			in
			   vs := vlist;
			   do not is_nil [vs] ==>
			     let v == head vs
			     in
				H$insert [v, v, LocList$top [], h]
			     ni;
			     vs := tail vs
			   od;
			   L$In [h]
			ni
		    };

	    none == func [ vlist : val List [Short]] val L {
		(* Insert a none loclist for each var *)
			let h == H$Init [];
			    vs == (List [Short])$New []
			in
			   vs := vlist;
			   do not is_nil [vs] ==>
			     let v == head vs
			     in
				H$insert [v, v, LocList$create [NONE_OBJ], h]
			     ni;
			     vs := tail vs
			   od;
			   L$In [h]
			ni
		    };

	    pair_meet == func [l1, l2 : val L; vlist : val List [Short];
							impure] val Void {
			let vs == (List [Short])$New [];
			    v == Short$New[]
			in
			    (* for each variable in vlist, put the merging of
				its entry in l1 and l2 into l2	*)
			    vs := vlist;
			    do not is_nil [vs] ==>
				v := head vs;
				vs := tail vs;
				let ref1 == L$look [v, l1];
				    ref2 == L$find [v, l2]
				in
				(* replace uses ref2's ref cell *)
				   replace [ref2, merge [ref1, ref2]]
				ni
			    od
			ni
		    };

	    equal == func [l1, l2 : val L; vlist : val List [Short]
						      ;impure] val Boolean {
			let vs == (List [Short])$New [];
			    eq_sofar == Boolean$New [True]
			in  vs := vlist;
			    do eq_sofar cand (not is_nil [vs]) ==>
				if not LocList$eq [L$look [head vs, l1],
						     L$look [head vs, l2]]
				then eq_sofar := False
				fi;
				vs := tail vs
			    od;
			    eq_sofar
			ni
		    };

	    print == func [l : val L; vlist : val List [Short]; impure] {
			let vs == (List [Short])$New [];
			    v == Short$New []
			in
			    vs := vlist;
			    do not is_nil [vs]  ==>

				v := head vs;
				vs := tail vs;

				let entry == L$look [v, l]
				in
				  if not [is_top entry] then
					put[v];DEBUG (" loclist is ");
					LocList$print [entry]
				  fi
				ni
			    od
			ni
		    };

	    apply == func [category, first, second, callnum : val Short;
			   a : val L; vlist : val List [Short];
			   notes : var List [Note];   impure] val Void {
OPEN
	DEBUG ("LOCS apply; category "); put [category];DEBUG ("\n");
CLOSE
		   let refx == L$find [first, a]
		   in
		      if (first >= C0) cand (first <= C4) then Null []
			   (* to avoid setting C0..C4 to anything but nil *)
		      else
		     if

		       (category = ALCNIL)      ==>
			(* x gets nil (none location) *)
				replace [refx, LocList$create [NONE_OBJ]]
		     # (category = ALCSTAT) ==>
				replace [refx, LocList$create [STATIC_OBJ]]
		     # (category = ALCSTACK) ==>
			(* note that I will never actually deallocate an
			    object on the stack (DEA for a stack object
			    has no effect), so it's safe to ignore the
			    fact that I don't know what set of objects it
			    could actually be *)
				replace [refx, LocList$create [STACK_OBJ]]
  		     # (category = ALLOC) cor (category = MAYBEALC) ==>
			(* x := alloc where alloc number is second *)
				replace [refx, LocList$create [second]]
OPEN
	;DEBUG("ALLOC : first loclist is now")
	;LocList$print [L$look [first, a]]
CLOSE

		     # (category = ASSIGN) ==>
			(* x := y *)
OPEN
	DEBUG ("ASSIGN: second and first loclists \n");
	put[second];DEBUG(":");LocList$print [L$look [second, a]];
CLOSE
			replace [refx, L$look [second, a]]
OPEN
	;put[first];DEBUG(":");LocList$print [L$look [first, a]]
CLOSE

		     # (category = WATCHARG) ==>
				Null []

		     # (category = GARCAT) ==>
			(* x := loc numbered second *)
OPEN
	put "GAR :var ";put first;put " := loc#";put second;put "\n";
CLOSE
			replace [refx, LocList$create [second]]

		     # (category = ANY)	     ==>
			(* x := anything from mem / everything *)
			replace [refx, LocList$create [MEM_OBJ]]

		     # (category = TOMEM)	==>
			(* mem_var gets anything second has as well as what it has*)
			let mem_var_val == L$find [mem_var, a];
			    sources == L$look [second, a]
			in
OPEN
	put "TOMEM :var ";put second;put ";
	locs are ";LocList$print [sources];put "\n";
CLOSE

			    replace [mem_var_val, merge [mem_var_val, sources]];

OPEN
	put "Locs now in memory are ";
	LocList$print [L$look [mem_var, a]];put "\n";
CLOSE

			ni

		     # (category = ERRCAT) ==>
			(* set all loclists to top *)
			(* this is weird because I hadn't anticipated it when
			   I decided on the calling convention for apply *)
			let vs == (List [Short])$New []
			in
			    vs := vlist;
			    do not is_nil [vs] ==>
				let v == head vs
				in
				    replace [L$find [v, a], LocList$top []]
				ni;
				vs := tail vs
			    od
			ni

		     # (category = CATTAR) ==>
			(* mem_var gets all of first and second as well as ..*)
			let mem_var_val == L$find [mem_var, a];
			    m == merge [mem_var_val, L$look [first, a]];
			    m1 == merge [m, L$look [second, a]]
			in
			    replace [mem_var_val, m1]
			ni

		     # (category = UNSAFE) ==>
			let mem_var_val == L$find [mem_var, a];
			in
			    replace [mem_var_val, LocList$bottom []]
			ni

		     # (category = ARGCAT) ==>
			(* cons a note onto notes if the var being passed could
				refer to any (arg) locs *)
			(* first = arg#; second = var being passed *)
			let l == copy [L$look [second, a]]
			in
OPEN
	put "\n var being passed is ";put second; put " entry is ";print l;
CLOSE
			    if not [is_top l] then
				let n == Note$Mk [callnum, first, l]
				in
				    notes := cons [n, notes]
				ni
			    fi
			ni
			
		     # else  ==> Null []
		     fi
		     fi (* C0...C4 *)
		   ni (* refx *)
(*
	;put[first];DEBUG(":");LocList$print [L$look [first, a]]
*)
	    }
	} (* with *)
	hide { Out; ^; In }
	ni (* let tablesize *)
}
