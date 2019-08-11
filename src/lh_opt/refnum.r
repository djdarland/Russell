(* RefNum objects - Short integers 0...maxrc. Sticky reference counts *)

func [maxrc : val Short] {

let E == extend { Short }
	    with R {

		inc == func [x : val R] val R {
				if x R$< R$In [maxrc]  then
(* to observe how often counts actually get inced to maxrc, as opposed to being
	set to maxrc directly. Useful for deciding on a suitable maxrc value *)
(*
if (x R$+ R$1) = R$In [maxrc] then put "\n\t\tNOTE : counted to maxrc;";
				   put " Is maxrc big enough ?\n"
fi;
*)

					x R$+ R$1
				else R$In [maxrc] 
				fi
			};

		dec == func [x : val R] val R {
				if x R$>= R$In [maxrc]   ==> R$In [maxrc]	(* 'sticky' *)
				#  x R$<= R$0 	==> put [ 
				  "\nWARNING attempt to decrement 0 ref ct."];
						    R$0		(* error ?? *)
				#  else 	==> x R$- R$1
				fi
			};

		max == func [x, y : val R] val R {
				if x < y then
					y
				else x
				fi
			}

		} (* with *)

in   E  

	hide { - <<func [val E] val E>>;
	       - <<func [val E; val E] val E>>;
		 +; +=; -= }	(* really need to hide := to be effective *)
ni
}

