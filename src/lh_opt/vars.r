# include "defs.h"
# define NOTCONST (-1)

func [max_var : val Short] {
	let predefvars == T2 ^ TL ^ RL ^ SK ^ UN ^ GF ^ SP ^ AR ^ mem_var;

	    (* assists reverse and concat by providing an accumulating reverse *)
	    rev == func [l, accum : val List [Short]] val List [Short] {
			if is_nil l then accum
			else
			    rev [tail l, cons [head l, accum]]
			fi
		};
	
	    (* concatrev reverses the first list and concats onto second list *)
	    concatrev == func [lst1, lst2 : val List [Short]] 
							    val List [Short] {
			rev [lst1, lst2]
		};

	    CArray == Array [max_var, Short];
	    Slist == extern {"slist"} [Short] (* destructive lists with 
								remove op *)
	in
		prod {
			consts : val Ref CArray;
			vs : val Ref Slist;
			killed_vs : val Ref Slist
		    }

	with V {
	    Init == func [] val V {
		let c == CArray$New [];
		    v == Slist$New [];
		    k == Slist$New []
		in
		    (* C0 thro C4 are constant locations always *)
		    c.C0 := 0; c.C1 := 1; c.C2 := 2; c.C3 := 3; c.C4 := 4;

		    v := Slist$'';
		    k := Slist$'';

		    V$Mk [(Ref CArray)$In [c], (Ref Slist)$In [v],
				(Ref Slist)$In [k]]
		ni
	      };

	    reset == func [vvv : val V; impure] val Void {
		(* vs := vs - killed_vs; killed_vs := '' *)
		let cptr == consts [vvv];
		    vptr == vs [vvv];
		    kptr == killed_vs [vvv]
		in
OPEN
put "to reset ks = ";put [kptr^];put "\n";
CLOSE
		    do not is_nil [kptr^] ==>
			remove [head (kptr^), vptr^];
			kptr^ := next (kptr^)
		    od;
OPEN
put "after reset vs = ";put [vptr^];put "\n";
CLOSE
		    (* clear the consts value for all vs and predefvars *)
		    let tempvs == Slist$New []
		    in
			tempvs := vptr^;
			do not is_nil [tempvs] ==>
			    (cptr^).(head tempvs) := NOTCONST;
			    tempvs := next tempvs
			od
		    ni;
		    let tempvs == (List [Short])$New []
		    in
			tempvs := predefvars;
			do not is_nil [tempvs] ==>
			    (cptr^).(head tempvs) := NOTCONST;
			    tempvs := tail tempvs
			od
		    ni;

		ni
	      };
		   
	    gen == func [v : val Short; vvv : val V; impure] val Void {
			add [v, (vs [vvv])^];
			not_constant [v, vvv]
	      };

	    kill == func [v : val Short; vvv : val V; impure] val Void {
			add [v, (killed_vs [vvv])^];
			not_constant [v, vvv]
	      };

	    vs_out == func [vvv : val V; impure] val (List [Short]) {
		(* return a real list of the vs and the predefvars *)

		concatrev [predefvars, normal_list [(vs [vvv])^]]

		(* cheaper than real concat and order doesn't matter *)
		(* Actually predefvars end up in order, but vs aren't *)
	      };

	    constant == func [v, value : val Short; vvv : val V; impure]
								    val Void
			{ ((consts [vvv])^).v := value };

	    not_constant == func [v : val Short; vvv : val V; impure] val Void
			{ ((consts [vvv])^).v := NOTCONST };

	    is_constant == func [v : val Short; vvv : val V; impure]
								val Boolean
			{ ((consts [vvv])^).v <> NOTCONST };

	    value == func [v : val Short; vvv : val V; impure] val Short
			{ ((consts [vvv])^).v };

	    print == func [vvv : val V; impure] val Void {
			put "vars :"; Slist$put [(vs [vvv])^];put "\n";
			put "killed vars :";Slist$put [(killed_vs [vvv])^];put "\n";
			put "\nCONSTS";
			let xx == Short$New [0];
			    c == (consts [vvv])^
			in
			    do xx < max_var ==>
				if c.xx <> 0 (* NOTCONST *) then
				    put xx; put ":"; put [c.xx]; put " "
				fi;
				xx += 1
			    od
			ni;
		    }


	    } hide { consts; vs; killed_vs; New; Mk; :=; V }
	ni
    }
