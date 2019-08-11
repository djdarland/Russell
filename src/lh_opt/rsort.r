(* This should be implemented with a sorted list of entries. But I am being
   lazy for now. *)

func [maxval : val Short] {
	(Array [maxval, Boolean])
	with A {

	    NNew == func [] var A {
		let a == A$New [];
		    i == Short$New [0]
		in
		   do i < maxval ==>
			a.i := False;
			i += 1
		   od;
		   a
		ni
		};

	    add == func [x : val Short; a : var A;impure] val Void {
			a.x := True
		};

	(* next returns the entry following x; returns maxval if no more *)
	    next == func [x : val Short; a : var A(*unchanged*)] val Short {
		     let n == Short$New []
		     in
			n := x + 1;
			do (n < maxval) cand (not a.n) ==>
				n += 1
			od;
			n
		     ni
		}
	} with A {New == A$NNew} export { New; add; next}
}
