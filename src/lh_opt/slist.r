(* For now these lists are not sorted *)
func[t: type T { put: func[val T]val T; =;
				(*<: func [val t; val t] val Boolean*) }] {

    let Dlist == extern {"dlist"} [t]
    in
	extend {Dlist} with S {

	    (* need is_nil, copy, New, '', add, remove, normal_list *)

	    (* inserts x *)
	    add == func [x: val t; s : val S; impure] val Void {
				addr [x, Out [s]]
			};

	    remove == func [x : val t; s : var S;impure] val Void {
		(* removes one occurrence of x, if there is one, from s *)
			let cur == Dlist$New [];
			    found == New [False]
			in use Dlist in
			    cur := Out [s]; (* assume s points to head of list *)

			    if not is_nil [cur]  then
				if (head cur) = x then
				    s := next s;(* when you remove the first elt of the
						list you need to move the head
						pointer on to the next elt *)
				    delete [cur];
				    found := True
			        else
			            cur := next [cur];
			            do not is_nil [cur] cand not found ==>
					if (head cur) = x then
					      found := True;
					      delete [cur]
					fi;
					if not is_nil [cur] then 
							cur := next [cur] fi
			      	    od
			        fi
			    fi
			    (* if not found then there was a duplicate UDC -
						that's OK *)
			ni ni
		};

	    normal_list == func [s : val S;impure] val (List [t]) {

			let l == (List [t])$New [];
			    s1 == S$New []
			in
			    s1 := s;
			    l := (List [t])$'';
			    do not is_nil [s1] ==>
				l := cons [head s1, l];
				s1 := next s1
			    od;
			    l
			ni
		};

	    } hide {Nil; In; Out; addl; addr; delete}
    ni
}
