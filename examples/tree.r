(* Binary tree data type.  Uses a functional representation for tree *)
(* nodes.                                                            *)
func [L: type {}] {
  let
    lr == enum { left, right };
  in use lr in
    union B { leaf: val L; interior: func [val lr] val B }
    with B {
             left_sub_tree == func [x: val B] {
                                B$to_interior[x][left]
                              };
             right_sub_tree == func [x: val B] {
                                 B$to_interior[x][right]
                               };
             leaf_value == B$to_leaf;
             make_leaf == B$from_leaf;
             make_tree == func [l,r: val B] val B {
                            B$from_interior [
                                func [x: val lr] {
                                    if
                                        x = left ==> l
                                    #   x = right ==> r
                                    fi
                                }
                            ]
                          }
    }
    export { New; :=; V; left_sub_tree; right_sub_tree; leaf_value;
             make_leaf; is_leaf; make_tree }
  ni ni
}
