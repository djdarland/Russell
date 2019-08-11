(* Tables mapping locations (allocation sites) to reference counts *)
# include "defs.h"

# define R0 (RefNum $In [0])
# define R1 (RefNum $In [1])
# define Rmaxrc (RefNum $In [maxrc])

func [nlocs, maxrc : val Short;
	 C : type P {New; V; :=; Mk : func [a,b,d :val Short] val P}] {
    let

	LocList === (extern {"loclist"} [nlocs]);
	LOCS	=== (extern {"locs"} [nlocs]);

	RefNum 	=== (extern {"refnum"} [maxrc]);
	RArray 	=== (Array [nlocs + 1, RefNum]);

    in  (Ref [RArray])  with R {

	    print == func [r : val R;impure] val Void {
    			let i == Short$New [0];
			    a == r^
			in 
			   do i <= nlocs   ==>
				put i;put ":";put [a.i];put " ";
				i += 1
			   od
			ni;
			put "\n"
		    };

	    top == func [] val R {
			let t == RArray $New [];
			    i == Short$New [0]
			in
			   do i <= nlocs   ==>
				t.i := RefNum $In [0];
				i += 1
			   od;
			   R$In [t]	(* returned *)
			ni
		    };

	    pair_meet == func [r1, r2 : val R; impure] val Void {
			let i == Short$New [0];
			    a1 == r1^;
			    a2 == r2^
			in 
			   do i <= nlocs   ==>
				a2.i := RefNum $max [a1.i, a2.i];
				i += 1
			   od
			ni
		    };

	    equal == func [r1, r2 : val R;impure] val Boolean {
			let a1 == r1^;
			    a2 == r2^;
			    i == Short$New[0]
			in
			    do (i <= nlocs) cand (a1.i = a2.i) ==>
					i += 1
			    od;

			    i > nlocs (* implies equal; else not equal *)
			ni
		    };

	    apply == func [q, category, first, second : val Short;
			   r : val R; l : val LOCS;
			   vlist : val (List [Short]);
			   finalpass : val Boolean;	impure] val C {
			let i == Short$New [];
			    app == r^;
			    clobberer == C$New []; (* return value - only 
						useful in finalpass *)

			    deref == func [x : val Short; impure]
								 val Void {
(* If all the locs x could point to have rc = 1 (0 should be a bug now) then
	the op clobbers.
   If x could point to only one loc then that loc's rc is decremented.	*)

				    let xlocs == LOCS $look [x, l];
					unoloc == LocList $oneloc [xlocs]
					(* loc = 0 => not just one loc *)
				    in
				     if finalpass then
				        (* if there are some locs - no point
					   deallocing otherwise - and 
						all locs have rc = 1 ... *)
					if feasible [xlocs] cand
				           and_map
				              [xlocs,
			                       func [loc : val Short;impure]
			                            val Boolean
					       { (app.loc = R1) cor
					         (LOCS$count [loc, l, vlist]
					         = 1) }]
					then
					    clobberer :=
					      C$Mk[q, x, unoloc]
					(* unoloc used to get size of object
						if only one object possible *)
					fi;
				     fi; (* if final pass *)

				(* actual deref part *)
					if (unoloc <> 0) then
OPEN
	put "DEREF ";put x;put " which refers only to ";put unoloc;
	put " whose rc=";put [app.unoloc];put "\n";
CLOSE
					    app.unoloc :=
						RefNum $dec [app.unoloc]


(* else make sure that RC values of all elements of l(x) (for which there are
		refcounts) are no bigger than their occurences on all lists 
	(including memvar which count as maxrc occurences, 
	  and excluding x (which explains the  -1))
*)

(* This code is executed if we were unable to decrement the refcount of
	x's objects, because there was more than one of them.*)

else 
 rc_map [xlocs, func [loc : val Short; impure] val Void {
			let occs == LOCS$count [loc, l, vlist] - 1
			in
			    if occs < RefNum$Out [app.loc]
			    then
OPEN
	put "**** #occurences of ";put loc;put " is ";
	put occs;
	put " while ref count would have been ";
	RefNum$put [app.loc];
	put "\n";
CLOSE
				   app.loc := RefNum$In [occs]
			    fi
			ni;
			Null []
		      }]

					fi
				    ni
OPEN
  	;DEBUG("\n");
CLOSE

				} (* deref *)
			in
			    clobberer := C$Mk [-1, 0, 0]; (* null value *)
(*
	DEBUG ("RC apply to local "); put [q];DEBUG ("\n");
*)
			    if (category <> UNSAFE)
			      cand (first >= C0) cand (first <= C4) then
						Null []
				(* avoid setting C0..C4 to anything but nil *)
			    else 
			      if
				(category = ANY) cor (category = GARCAT) cor
				(category = ALCNIL) cor (category = ALCSTAT)
				cor (category = ALCSTACK) ==>
					deref [first]
					(* no new refs worth counting *)

			      #	(category = ALLOC) ==>
				(* var(first) := loc(second) *)
					deref [first];
					app.second := RefNum $inc[app.second]

			      #	(category = ASSIGN) ==>
				   let loclist == LOCS $look [second, l]
				   in
				(* can't deref 1st if 2nd is the same var
				   but there should be no ASSIGNs like that *)

					     deref [first];

				(* potential new ref to any loc in second *)
			LocList $rc_map [loclist, 
		             func [loc : val Short;impure]val Void
				   { app.loc := RefNum $inc [app.loc];Null[]}]
				   ni

			      #	(category = WATCHARG) ==>
					Null []

			      #	(category = TOMEM) ==>
			LocList $rc_map [LOCS $look [second, l], 
			     func [loc : val Short;impure]val Void
				     { app.loc := Rmaxrc; Null[]}]

			      # (category = ERRCAT) ==>
				(* simply set all app entries to 0 (top) *)
				let i == Short$New [0]
				in
				   do i <= nlocs   ==>
					app.i := RefNum $In [0];
					i += 1
				   od
				ni

			      #	(category = CATTAR) ==>
				  (* like stack for first and second *)
LocList $rc_map [LOCS $look [first, l], func [loc : val Short;impure]val Void
				   { app.loc := Rmaxrc; Null[]}];
LocList $rc_map [LOCS $look [second, l],func [loc : val Short;impure] val Void
				   { app.loc := Rmaxrc; Null[]}]
			      #	(category = UNSAFE) ==>
				   (* forall locs, if rc not 0 then -> maxrc*)
LocList $rc_map [LocList $bottom[], func [loc : val Short;impure] val Void
					{ if (app.loc <> R0) then
						app.loc := Rmaxrc
					  fi;
					  Null[]}]
			      
			      # else ==> Null []
			      fi
			    fi;

OPEN
	DEBUG("After APPLY rc's are");
	let j == Short$New [1]
	in
   	    do j <= nlocs ==>
	        DEBUG(" ");put j;DEBUG(":");put[app.j];
	        j += 1
   	    od;
            DEBUG("\n")
	ni;
CLOSE

			    V [clobberer] (* returned *)
			ni
		    } (* apply *)

	} (* with *)
    ni
}
