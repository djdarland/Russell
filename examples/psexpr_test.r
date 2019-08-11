(* Compile psexpr.r first.                                    *)
(* Test of binary tree data type.  Computes 2**m the hard way *)
let
    se == extern { "psexpr" };

    SEXPR == se[Short];
	     
    x == Short$New[];
    t == SEXPR$New[]
in
    t := SEXPR$nil;
    x := get[FS];
    do
	x > 0 ==>
	    t := cons[t, t];
	    x := x - 1;
    od;
    let
	Number_of_Leaves ==
	    func[ x: val SEXPR ]
		{
		    if  null[x] ==>
			    1    
		    #   else ==>
			    Number_of_Leaves[car[x]] + Number_of_Leaves[cdr[x]]
                    fi
                }
    in
	put[ Number_of_Leaves[t] ]; put["\n"]
    ni
ni
