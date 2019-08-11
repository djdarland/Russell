# include "defs.h"
func [nodes : val Nodes;
	Nodes : type N { . : func [val N; val Short] val NodeInfo};
	NodeInfo : type NI {is_arg : func [val NI] val Boolean;
			    is_result : func [val NI] val Boolean} ] {

    let 
	ResultCode == extend {Short}

	    with S {

	top == func [] val S { S$In [TOP] };
	is_top == func [s : val S] val Boolean {S$Out [s] = TOP};

	none == func [] val S { S$In [NONENODE] };

	equal == func [s1, s2 : val S] val Boolean { s1 = s2 };

	meet == func [s1, s2 : val S] val S {
		if (s1 = S$In [MEMNODE]) cor (s2 = S$In [MEMNODE]) 
						then S$In [MEMNODE]
		elsif (s1 = S$In [TOP]) then s2
		elsif (s2 = S$In [TOP]) then s1
		elsif (s1 = s2) then s1
		(* neither is MEM nor TOP and they are not the same *)
		elsif (s1 = S$In [NONENODE]) cor (s2 = S$In [NONENODE])
					then S$In [MEMNODE] (* bottom *)
		elsif is_arg [nodes.(S$Out [s1])] cor is_arg [nodes.(S$Out [s2])]
					then S$In [MEMNODE] (* bottom *)
		(* for now STACK meet STATIC is STACK (i.e. don't distinguish)*)
		elsif (s1 = S$In [STACKNODE]) cand (s2 = S$In [STATICNODE]) 
							then s1
		elsif (s1 = S$In [STATICNODE]) cand (s2 = S$In [STACKNODE]) 
							then s2
		(* the meet of any pair of allocs, or an alloc with STATIC or
		   STACK is the generic alloc, which means something that can 
		   be freed with size unknown *)
		else S$In [GENERICNODE]
		fi
	    };

	apply == func [n : val Short (* a graph node *); s : val S] val S {

		(* meet the code for this node with s *)
		(* the code for fn result nodes is TOP - info just flows thro;
		   the code for argnodes and all other nodes is themselves *)

		let code == if is_result [nodes.n] then S$In [TOP]
			    else S$In [n]
			    fi
		in
			S$meet [code, s]
		ni
	    }
	} (* with *)
    in
	ResultCode 
	    export { New <<func [] var ResultCode>>; V; :=; Out;
				top; is_top; none; equal; meet; apply; put }
    ni
}
