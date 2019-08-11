(* Compute      2**n the hard way.  Similar to tree_test.r, but *)
(* with a more conventional binary tree implementation.         *)
let
    SEXPR == union U { atom : val Short;
                       cn: val prod { hd: val U; tl : val U } }
	     with SE {
		car == func[x: val SE] val SE {
			  hd[to_cn[x]]
		       };
		cdr == func[x: val SE] val SE {
			  tl[to_cn[x]]
		       };
		cons == func[x,y: val SE] val SE {
			  SE$from_cn[
                            prod { hd: val SE; tl: val SE } $ Mk [x, y]
                          ]
		       };
		In == SE$from_atom;
		Out == SE$to_atom;
		atom == SE$is_atom  }
	        export { New; := ; V; car; cdr; cons; In; Out; atom };
	     
    x == Short$New[];
    t == SEXPR$New[]
in
    t := SEXPR$In[17];
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
		    if  atom[x] ==>
			    1    
		    #   else ==>
			    Number_of_Leaves[car[x]] + Number_of_Leaves[cdr[x]]
                    fi
                }
    in
	put[ Number_of_Leaves[t] ]; put["\n"];
    ni
ni
