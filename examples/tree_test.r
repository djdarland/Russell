(* Compile tree.r first *)

(* Computes 3 * 2**n the hard way, where n is read from stdin *)
let
    tree == extern { "tree" };

    TREE == tree[Short];
	     
    x == Short$New[];
    t == TREE$New[]
in
    t := TREE$make_leaf[3];
    x := get[FS];
    do
	x > 0 ==>
            t := make_tree[t, t];
	    x := x - 1;
    od;
    let
        Add_Leaves ==
            func[ x: val TREE ]
		{
                    if  is_leaf[x] ==>
                            leaf_value[x]    
		    #   else ==>
                            Add_Leaves[left_sub_tree[x]]
                            + Add_Leaves[right_sub_tree[x]]
                    fi
                }
    in
        put[ Add_Leaves[t] ]; put["\n"]
    ni
ni
