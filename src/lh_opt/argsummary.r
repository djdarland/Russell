
func [nodes : val Nodes; Nodes : type N {size : func [] val Short;
					. : func [val N; val Short] 
							val NodeInfo};
		NodeInfo : type N {is_result : func [val N] val Boolean;
				   is_mem : func [val N] val Boolean}] {

    let
	L == extend {(List [Short])} with S {

		top == func [] val S {
			S$In [(List [Short])$'']
		    };

		create == func [i : val Short] val S {
			S$In [cons [i, (List [Short])$'']]
		    };

		member == func [x : val Short; s : val S] val Boolean {
				if is_nil s then False
				elsif x = (head s) then True
				elsif x < (head s) then False
				else member [x, tail s]
				fi
			};

		eq == func [s1, s2 : val S; var Void] val Boolean
				{ extern "_eq" };


		equal == func [s1, s2 : val S; var Void] val Boolean {
				eq [s1, s2] cor
				(if is_nil s1 then
						is_nil s2
				elsif is_nil s2 then False (* <= s1 not nil *)
				else
					(head s1 = head s2) cand
				        equal [tail s1, tail s2]
				fi)
			};

		meet == func [s1, s2 : val S] val S {

			    if is_nil s1  ==> s2
			     # is_nil s2  ==> s1
			     # else   ==>
				 if  (head s1) < (head s2) ==>
					cons [head s1, meet [tail s1, s2]]
				  #  (head s1) = (head s2) ==>
					cons [head s1, meet [tail s1, tail s2]]
				  #  else   ==>
					cons [head s2, meet [s1, tail s2]]
				fi
			    fi
			};

		apply == func [n : val Short; s : val S] val S {
		(* if n is a result or mem node then merge that with s;
			else just return s *)
			if is_result [nodes.n] cor is_mem [nodes.n] then
				S$meet [S$create [n], s]
			else s
			fi
		
		    };

		print == func [s : val S] val Void {

			if not is_nil [s] then 
			  put [head s]; put [" "]; S$print [tail s]
			fi

		    };
	    }
    in
	L export { New; V; :=; top; equal; meet; apply; member; print
; create
							}
    ni
}
