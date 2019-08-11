(* Primitive implementation of interval arithmetic.  A number is *)
(* represented by a floating point value and a tolerance.        *)
(* This assumes that floating point underflow exceptions are     *)
(* disabled.  Fortunately this normally seems to be the case     *)
(* under UNIX.                                                   *)
(* Some functions catch floating point exceptions.  This         *)
(* effectively prevents the client from also doing so.           *)

# define skip Null[]

func [] {
    let
	(* First a lot of gross stuff to get around primitive floating *)
	(* point constants:                                            *)
	zero == 0.0;
	one == 1.0;

	(* Dumb integer exponentiation *)
	** == func[x: val Float; y: val Short] val Float {
		let
		    i == Short$New[];  (* result = x ** i *)
		    result == Float$New[];
		in
		    i := 0; result := 1.0;
		    do
			i < y ==> i += 1; result := result * x
		    #   i > y ==> i -= 1; result := result / x
		    od;
		    result
		ni
	      };
	max_exp == 127;   (* Maximum exponent of a floating point number *)
	inf_toler == 10.0 ** 36;
			(* This tolerance value indicates the result is   *)
			(* completely bogus. Also upper bound on meaning- *)
			(* ful tolerance.                                 *)
	smallest == 10.0 ** (-36);
			(* Upper bound on smallest representable and *)
			(* invertable number                         *)
	max_exp_arg == 82.0;
			(* Largest valid argument to floating point *)
			(* exponential function                     *)
	m_err == 10.0 ** (-15);
		    (* Maximum floating point representation error for x *)
		    (* is x * m_err                                      *)
	(* Compute the representation error, i.e. the maximum error due  *)
	(* to granularity of representation for the floating point value *)
	rep_error == func[x: val Float] val Float {
			max[m_err * abs[x], smallest]
		     };
	abs == func [x: val Float] { if x < 0.0 ==> -x # else ==> x fi };
	abs == func [x: val Short] { if x < 0 ==> -x # else ==> x fi };
	max == func [x, y: val Float] { if x < y ==> y # else ==> x fi };
    in
	prod { appr, tol: val Float }
	with I {
	    (* Meaningless value *)
	    junk == func[] val I { I$Mk[zero, inf_toler] };

	    is_junk == func[x: val I] { tol[x] >= inf_toler };

	    + == func[x,y: val I] val I {
		    if
			exponent[appr[x]] > max_exp
			cor exponent[appr[y]] > max_exp
			cor tol[x] + tol[y] >= inf_toler ==>
			    I$junk
		    #   else ==>
			    let
				new_appr == appr[x] + appr[y]
			    in
				I$Mk[ new_appr,
				      tol[x] + tol[y] + rep_error[new_appr]
				]
			    ni
		    fi
		 };

	    * == func[x,y: val I] val I {
		    if
		      tol[x] + tol[y] >= inf_toler
		      cor exponent[appr[x]] + exponent[tol[y]] >= max_exp
		      cor exponent[appr[y]] + exponent[tol[x]] >= max_exp
		      cor exponent[appr[x]] + exponent[appr[y]] >= max_exp ==>
			(* New tolerance or result might generate overflow *)
			    I$junk
		    # else ==>
			let
                            new_tol == (abs[appr[x]] + abs[tol[x]]) * tol[y]
                                       + (abs[appr[y]] + abs[tol[y]]) * tol[x];
			    new_appr == appr[x] * appr[y]
			in
			    I$Mk[new_appr,
				 new_tol + rep_error[new_appr]
			    ]
			ni
		    fi
		 };

	    (* Convert a monotone function on floating point numbers *)
	    (* to the corresponding function on intervals            *)
	    (* Assumes f is a total function floating point numbers  *)
	    from_monotone ==
		func [f: func[val Float] val Float] {
		    func [x: val I] val I {
			if tol[x] >= inf_toler ==>
			    I$junk
			# else ==>
			    let
				high_appr == f[appr[x] + tol[x]];
				mid_appr == f[appr[x]];
				low_appr == f[appr[x] - tol[x]];
				new_tol == abs[high_appr - low_appr]
					   + rep_error[mid_appr];
			    in
				I$Mk[
				     mid_appr,
				     if new_tol > inf_toler ==>
					inf_toler
				     #  else ==>
					new_tol
				     fi
				]
			    ni
			fi
		    }
		};

	    (* Convert a monotone function on the nonnegative floating   *)
	    (* point numbers to the corresponding function on intervals  *)
	    from_monotone_pos ==
		func [f: func[val Float] val Float] {
		    I$from_monotone [func [x: val Float] val Float {
					f[if x < zero ==> zero # else ==> x fi]
				   }
		    ]
		};

	    (* Additive inverse *)
	    - == func[x: val I] {
		    I$Mk[-appr[x], tol[x]]
		 };

	    (* Subtraction *)
	    - == func[x,y : val I] {
		    x + (-y)
		 };

	    (* Multiplicative inverse (could be improved) *)
	    inv == func[x: val I] {
			if appr[x] - tol[x] < zero
			   cand appr[x] + tol[x] > zero
			   cor appr[x] <= smallest ==>
			    (* Value could be 0 *)
			    I$junk
			# else ==>
			    I$from_monotone[func[x: val Float] {1.0/x}] [x]
			fi
		    };

	    / == func[x,y: val I] {
		    x * inv[y]
		 };

	    (* Return -1 if definitely x < y, +1 if definitely x > y, *)
	    (* 0 if intervals overlap                                 *)
	    compare == func[x, y: val I] val Short {
			    if
			      tol[x] >= inf_toler cor tol[y] >= inf_toler ==>
				0
			    # else ==>
			      if
				appr[x] + tol[x] < appr[y] - tol[y] ==>
				    -1
			      # appr[x] - tol[x] > appr[y] + tol[y] ==>
				    1
			      # else ==>
				    0
			      fi
			    fi
		       };
    
	    max == func[x,y: val I] {
		     if
			tol[x] >= inf_toler cor tol[y] >= inf_toler ==>
			    I$junk
		     #  else ==>
			    if
			      appr[x] - tol[x] > appr[y] + tol[y] ==>
				x
			    # appr[y] - tol[y] > appr[x] - tol[x] ==>
				y
			    # else ==>
				I$Mk[max[appr[x], appr[y]],
				     max[tol[x], tol[y]]]
			    fi
		     fi
		   };

	    exp == func[x: val I] {
		       if appr[x] + tol[x] > max_exp_arg ==>
			   I$junk
		       # else ==>
			   I$from_monotone[Float$exp][x]
		       fi
		   };

	    shift == func[x: val I; n: val Short] {
			let
			    e1 == exponent[appr[x]];
			    e2 == exponent[tol[x]]
			in
			    if abs[e1 + n] > max_exp
			       cor abs[e2 + n] > max_exp ==>
				I$junk
			    # else ==>
				I$Mk[shift[appr[x],n], shift[tol[x],n]]
			    fi
			ni
		     };

	    ln == func[x: val I] {
		    if appr[x] - tol[x] <= zero ==>
			I$junk
		    # else ==>
			I$from_monotone[Float$ln][x]
		    fi
		  };

	    (* reasonable only over [-1.5 .. 1.5] *)
	    sin == func[x: val I] {
			if max[abs[appr[x]-tol[x]], abs[appr[x]+tol[x]]]
			   > 1.5 then
			    I$Mk[zero,one]
			else
			    I$from_monotone[Float$sin][x]
			fi
		   };

	    (* ditto ... *)
	    cos == func[x: val I] {
			if max[abs[appr[x]-tol[x]], abs[appr[x]+tol[x]]]
			   > 1.5 then
			    I$Mk[zero,one]
			else
			    let
				y == sin[x]
			    in
				sqrt[I$from_Float_const[one] - y * y]
			    ni
			fi
		   };

	    atan == func[x: val I] {
			I$from_monotone[Float$atan][x]
		   };

	    sqrt == func[x: val I] {
			I$from_monotone_pos[Float$sqrt][x]
		    };

	    put == func[x: val I] {
			if tol[x] < inf_toler ==>
			    put[appr[x]];
			    put[" (+- "];
			    put[tol[x]];
			    put[")"];
			# else ==>
			    put["junk"];
			fi;
			x
		   };

	    (* Return true if x is known to be accurate *)
	    (* to 2**n.                                 *)
	    accurate == func[x: val I; n: val Short] {
			    tol[x] < inf_toler cand
			    exponent[tol[x]] <= n
			};

	    (* Convert a floating point constant to an interval *)
	    from_Float_const == func[x: val Float] {
				    I$Mk[x, rep_error[x]]
				}
	}
    ni
}
