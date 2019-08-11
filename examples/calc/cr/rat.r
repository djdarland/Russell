(* This constitutes an initial attempt at implementing  *)
(* rational arithmetic in Russell.  Some of the         *)
(* algorithms are no doubt suboptimal.                  *)
(* Fractional representations are mormalized if length  *)
(* of den exceeds CUTOFF and denominators are positive. *)
(*							*)
(* functions defined for rat's				*)
(*	norm == func[val R] val R;			*)
(*      * == func[x,y: val R] val R;                    *)
(*      ** == func[x: val R; y: val Long] val R;        *)
(*	inv == func[val R] val R;			*)
(*	/ == func[x,y: val R] val R;			*)
(*	+ == func[x,y: val R] val R;			*)
(*	- == func[x,y: val R] val R;			*)
(*	put == func[val R] val R;			*)
(*	puts == func[val R] val ChStr			*)
(*	to_Float == func[val R] val Float;		*)
(*	In == func[val Long] val R;			*)
(*	compare == func[x,y: val R] val Boolean;	*)
(*	to_Long == func[val R] val Long;		*)

#define skip Null[]

func [] {
    let
	CUTOFF == Short$10;	(* max number of words allowed in
				   unnormalized rational *)

	xl == extern { "xlong" } [];

        (* modulus operation on nonnegative long integers *)
        % ==  func [x,y: val xl] val xl {
                  x - (x/y)*y
	      };

    in use xl in
        prod {n, d : val xl}
        with R {
            norm == func[x: val R] val R {
	     let
		nw == nwords[d[x]]
	     in
	      	if Long$Out[to_Long[nw]] > CUTOFF ==>
		    let
		    	divisor == gcd[n[x], d[x]]
		    in
		    	R$Mk[ n[x]/divisor, d[x]/divisor ]
		    ni;
		# else ==>
		    x
	        fi
	     ni
            };

            * == func[x,y: val R] {
                     norm[R$Mk[n[x]*n[y], d[x]*d[y]]]
		 };

	    ** == func[x: val R; y: val Long] {
		    let
			Y == xl$In[y]
		    in
			if Y >= 0 then
			    R$Mk[n[x]**Y, d[x]**Y]
			else
			    R$Mk[d[x]**(-Y), n[x]**(-Y)]
			fi
		    ni
		  };

	    inv == func[x: val R] {
		       if
			   n[x] >= 0 ==> R$Mk[d[x], n[x]]
		       #   else      ==> R$Mk[-d[x], -n[x]]
		       fi
		   };

	    / == func[x,y: val R] {
		     if n[y] >= 0 ==>
			norm[R$Mk[n[x]*d[y], d[x]*n[y]]]
		     # else ==>
			norm[R$Mk[-n[x]*d[y], -d[x]*n[y]]]
		     fi
		 };
		 
	    shift == func[x: val R; how_much: val Short] {
	    		if how_much >= Short$0 then
	    		  norm[R$Mk[shift[n[x],xl$In[how_much]], d[x]]]
	    		else
	    		  norm[R$Mk[n[x], shift[d[x],xl$In[-how_much]]]]
	    		fi
	             };

            + == func[x,y: val R] {
                     norm[R$Mk[n[x]*d[y] + n[y]*d[x],
                               d[x] * d[y]]]
		 };

	    - == func[x,y: val R] {
                     norm[R$Mk[n[x]*d[y] - n[y]*d[x],
                               d[x] * d[y]]]
		 };

            - == func[x: val R] {
		     R$Mk[-n[x], d[x]]
		 };

	    abs == func[x: val R] {
		       R$Mk[abs[n[x]],d[x]]
		   };

            put == func[x: val R] val R {
			put[R$puts[x]]; x
		   };

	    puts == func[x: val R] val ChStr {
			let
			    cd == gcd[n[x], d[x]];
			    den == d[x]/cd;
			    num == n[x]/cd;
			in
			    if den = xl$1 then
				puts[num];
			    else
				puts[num] ^* "/" ^* puts[den];
			    fi
			ni
		   };

            to_Float == func[x: val R] val Float {
                            to_Float[n[x]]/to_Float[d[x]]
			};

            In == func[x: val Long] val R {
                      R$Mk[from_Long[x], 1]
		  };

            compare == func[x,y : val R] {
                           let
                               diff == n[x]*d[y] - n[y]*d[x]
                           in
                               if
                                   diff > 0 ==> Short$1
                               #   diff < 0 ==> -Short$1
                               #   else     ==> Short$0
                               fi
                           ni
		       };

	    to_Long == func[x: val R] {
			   to_Long[n[x]/d[x]]
		       };

	    get_num == func[x: val R] {
			   to_Long[n[x] / gcd[n[x], d[x]]];
			};

	    get_den == func[x: val R] {
			   to_Long[d[x] / gcd[n[x], d[x]]];
			};

	    build == func[x,y: val Long] {
			norm[R$Mk[from_Long[x], from_Long[y]]]
		     };

	} hide { Mk; n; d; norm }
    ni ni
}
