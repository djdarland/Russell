#define SHORT_PRECISION         /* Short is big enough for precision */
#define SHORT32                 /* 32 bit Short                      */

(* This should be compiled with "rc -pcL" *)

(* 
 * This claims to be an implementation of constructive real numbers.
 * Each number x is represented primarily as a function which given
 * a precision n produces a (Long) number m such that 
 *
 *       | m*4^n - x |  <  4^n
 *
 * For efficiency reasons, the current approximation to the result
 * is also passed to this function.  (It is esp. useful for sqrt.)
 * Additional fields in the representation are used to keep track
 * of the smallest argument (i.e. greatest precision demand)
 * on which the function has already been evaluated, and the
 * corresponding result.  This is used to avoid
 * duplicate evaluation.  Floating point upper and lower bounds
 * are also maintained.  These further reduce the amount of
 * duplicate evaluation.
 *
 * Functions currently available:  
 *
 *      + ==    func [x,y: val I] val I;
 *      - ==    func [x,y: val I] val I;
 *      - ==    func [val I] val I;
 *      max ==  func [x,y: val I] val I;
 *      abs ==  func [val I] val I;
 *	compare == func [x,y: val I; r,a: val Long; impure] val Short;
 *	to_Long == func [val I; val Long; impure] val Long;
 *	inv == 	func [val I; impure] val I;
 *      * ==    func [x,y: val I] val I;
 *      / ==    func [x,y: val I] val I;
 *      sqrt == func [x,y: val I] val I;
 *	exp ==	func [val I; impure] val I;
 *	ln ==	func [val I; impure] val I;
 *      sin ==  func [val I; impure] val I;
 *      cos ==  func [val I; impure] val I;
 *      atan == func [val I; impure] val I;
 *	atan ==	func [x: val I; pi2: val I; impure] val I;
 *      pi ==   func [] val I;
 *      eager == func [val I] val I;
 *	put ==	func [val I; val Long; impure] val Void;
 *	puts ==	func [val I; val Long; impure] val ChStr;
 *      hex_puts == func [val I; val Long; impure] val ChStr;
 *	In ==	func [val Long] val I;
 *      In ==   func [x: val Long; offset: val Long] val I;
 *      . ==    func [w,f,l: val Long] val I;
 *
 *      The algorithms for transcendentals are largely copied from
 *      Richard P. Brent's work.  Some of those that aren't should have
 *      been.
 *
 *      Authors:  Hans-J. Boehm & Vernon Lee
 *)

# define skip Null[]

func [] {
  let
    xLong == extern { "xlong" }[];  (* extended long integer type.  *)
	  			    (* includes full division, etc  *)
    Interval == extern { "cr/interval" }[];
					(* Type of intervals, with standard *)
					(* arithmetic operations.  Each     *)
					(* interval consists of a floating  *)
					(* point approximation and a        *)
					(* tolerance.                       *)

    interval_zero == Interval$from_Float_const[0.0];

    (* The type of the precision argument to a function representing *)
    (* a constructive real.  Note that on a Vax Short integers are   *)
    (* only 16 bits long.  Thus they may not suffice.  With 32 bit   *)
    (* Short integers, as in the Sun implementation,  they should    *)
    (* easily  suffice, since we expect to run out of memory before  *)
    (* we exhaust the range of the precision argument.               *)
#   ifdef SHORT_PRECISION
	PrecT == extend { Short }
        	     with pt {
        		from_Short == pt$In;
        		to_Short == pt$Out;
        		from_xLong == func [x: val xLong] {
        				pt$from_Short[
        				    to_Short[x]
        			        ]
        			      };
        		
        		to_xLong == func [x: val pt] {
        				xLong$from_Short[
        				    to_Short[x]
        				]
        			    };
        		/ == func [x: val pt; y: val Short] val pt {
        			pt$/[x,pt$from_Short[y]]
        		     };
        		% == func [x: val pt; y: val Short] val Short {
        			pt$to_Short[pt$%[x,pt$from_Short[y]]]
        		     };
        		min == func[x,y: val pt] {
        			 if x <= y ==> x # else ==> y fi
        		       };
        		max == func[x,y: val pt] {
        			 if x >= y ==> x # else ==> y fi
        		       };
        		abs == func[x: val pt] {
        			 if x >= pt$0 ==> x # else ==> -x fi
        		       };
        	     };
#       else
	    PrecT == extend { xLong }
		     with pt {
			from_xLong == pt$In<<func[val xLong]val pt>>;
			to_xLong == pt$Out<<func[val pt]val xLong>>;
			from_Short == func [x:val Short] {
					  pt$from_xLong[
					    xLong$from_Short[x]
					  ]
				      };
			to_Short == func [x: val pt] {
					to_Short[
					    to_xLong[x]
					]
				    };
		     };
#       endif
	
	half == 0.5;

    in use xLong in let

        base == Short$4;  (* Must be >= 4, power of 2.  Known to exp, ln *)
        base_exp == Short$2;     (* 2**base_exp = base *)
	Lbase == xLong$from_Short[base];
	Pbase == PrecT$from_Short[base];
	Lbase_exp == xLong$from_Short[base_exp];
	Pbase_exp == PrecT$from_Short[base_exp];
	half_base == Lbase/Short$2;
#       ifdef SHORT32
	   big == PrecT$10000; (* A big number s.t. base_exp*big is *)
			       (* representable as a Short          *)
	   MaxShort == Short$2147483647;
#       else
	   big == PrecT$100;
	   MaxShort == Short$32767;
#       endif
	max_Float == shift[2,125];

	(* Calculate x*base**n (rounded) efficiently *)
	scale == func[x: val xLong; n: val PrecT] val xLong {
                    let
			resultp1 == shift[x, to_xLong[Pbase_exp*(n+PrecT$1)]]
				    + half_base;
                    in
                        shift[resultp1, -Lbase_exp]
                    ni
                 };

	(* Return a reference to a new Long, Boolean, or PrecT variable *)
	create == func[] { (Ref[xLong])$In[xLong$New[]] };
	create == func[x: val Short] {
		      (Ref[Short])$In[
			let s == Short$New[];
			in s := x; s ni] };
	create == func[x: val xLong] { (Ref[xLong])$In[xLong$New[x]] };
	create == func[x: val Boolean] {
		      (Ref[Boolean])$In[
			  let
			    b == Boolean$New[]
			  in
			    b := x; b
			  ni
		      ]
		  };
	create == func[x: val PrecT] {
		      (Ref[PrecT])$In[
			  let
			    p == PrecT$New[]
			  in
			    p := x; p
			  ni
		      ]
		  };

        (* Compute the number of digits in a Long with respect to base *)
	ndigits == func[x: val xLong] {
		     use PrecT in let
			nb == from_xLong[nbits[abs[x]]];
				  (* includes sign bit *)
                     in
			(nb + Pbase_exp - 2)/base_exp
                            (* = ceiling[(nb - 1)/base_exp] *)
		     ni ni
		   };

	cr ==		(* each cr represents a constructive real number x *)
	  prod I {
	    fn: func [val PrecT; val I; impure] val xLong;
				(* function which returns x * base^(-prec)
				   truncated to an xLong integer *)
	    min_prec: val Ref[PrecT];
				(* smallest (finest) precision to which
				   x has been calculated (for efficiency) *)
	    max_val: val Ref[xLong];
				(* fn[x][min_prec[x]] = max_val[x] *)
	    value_valid: val Ref[Boolean];
				(* value_valid => the stored values for
				   min_prec and max_val are valid *)
	    value_exact: val Boolean;
				(* max_val is exact (for constants) *)
	    fp_appr: val Interval;
				(* approximation of the number *)
	  } with I {

	    (*
	     * Given only the function definining a real number, and
	     * an interval approximation to the real number,
	     * produce a representation of the real number.
	     *)
	    Mk2 ==
		func[f: func [val PrecT; val I; impure] val xLong;
		     fp_appr: val Interval] val I
		{
		    I$Mk[f, create[PrecT$0], create[],
			create[False], False,
			fp_appr]
		};

	    (*
	     * Return a number whose value is a good current
	     * approximation to y.
	     *)
	    current_approx ==
		func [y: val I; impure] val I {
		    let
			calculated_prec ==
			    if value_valid[y]^ ==>	min_prec[y]^
			    # else ==>			big
			    fi;

			fp_prec ==
			    if is_junk[fp_appr[y]] ==>	big
			    # else ==>
				PrecT$from_Short[
				    (exponent[tol[fp_appr[y]]] / base_exp)
					+ Short$1
				]
			    fi;

		        prec == min[calculated_prec, fp_prec]
		    in
			I$Mk[ fn[y],
			      create[prec],
			      create[val_I_to_Long[y, prec]],
			      create[True], True,
			      Interval$junk
			]
		    ni
		};

	   (*
	    * get a long approximation of a constructive real x.
	    *)
	    val_I_to_Long ==
		func[x: val I; prec: val PrecT; impure] {
		    if abs[prec] < big cand
		       accurate[fp_appr[x],to_Short[prec] * base_exp] ==>
			(*
			 * floating point appr is accurate enough,
			 * so round it off to right precision and return
			 *)
			from_Float[
			    shift[appr[fp_appr[x]],to_Short[-prec*Pbase_exp]]
			    + half
			]
		    # else ==>
			(*
			 * have to use old value or calculate new one
			 *)
			if (not value_valid[x]^) cor
                           ((not value_exact[x]) cand prec < min_prec[x]^) ==>
			  (*
			   * calculate new long approximation and
			   * save new value in x for later use
			   *)
			    let
				new_val == fn[x][prec, x];
			    in
			     (*
			      * "critical section" bounded by changing
			      * value_valid to False until done. We hope
			      * that assignment is atomic.
			      *)
				value_valid[x]^ := False; (* P *)
				max_val[x]^ := new_val;
				min_prec[x]^ := prec;
				value_valid[x]^ := True; (* V *)
				new_val
			    ni
			# else ==>
			  (*
			   * shift old value to proper precision
			   *)
			    scale[max_val[x]^, min_prec[x]^ - prec]
			fi
		    fi
		};


	   (*
	    * Given a Long, divide by base, rounding off least
	    * significant digit
	    *)
	    round ==
		func[x: val xLong; impure] val xLong {
		    if
		      x >= xLong$0 ==> (x + half_base)/base
		    # x <= xLong$0 ==> (x - half_base)/base
		    fi
		};

	    + ==
		func[x, y: val I] val I {
		    I$Mk2[
			func[prec: val PrecT; val I; impure] {
			    let
				sum == val_I_to_Long[x, prec - PrecT$1]
				     + val_I_to_Long[y, prec - PrecT$1]
			    in
				cr$round[sum]
			    ni
		    	},
			fp_appr[x] + fp_appr[y]
		    ]
                };

	    - == func[x, y: val I] val I {
		    I$Mk2[func[prec: val PrecT; val I; impure] {
			    let
			      diff == val_I_to_Long[x, prec - PrecT$1]
				      - val_I_to_Long[y, prec - PrecT$1]
			    in
				cr$round[diff]
			    ni
			  },
			  fp_appr[x] - fp_appr[y]
		    ]
                 };

	    - == func[x: val I] val I {
		    I$Mk2[func[prec: val PrecT; val I; impure] {
			    -val_I_to_Long[x, prec]
			  },
			  -fp_appr[x]
		    ]
                 };

	    max == func[x, y: val I] val I {
		    I$Mk2[func[prec: val PrecT; val I; impure] {
			    let
			      xappr == val_I_to_Long[x, prec];
			      yappr == val_I_to_Long[y, prec];
			    in
			      if
				xappr >= yappr ==> xappr
			      # else           ==> yappr
			      fi
			    ni
			  },
			  max[fp_appr[x], fp_appr[y]]
		    ]
		   };

	    abs == func[x: val I] val I {
		       I$max[I$-[x], x]
		   };

            (*
	     * Calculate x*base**n (rounded)
	     *)
	    scale == func[x: val I; how_much: val PrecT] val I {
			I$Mk2[func[prec: val PrecT; val I; impure] {
				val_I_to_Long[x, prec - how_much]
			      },
			      if how_much > big ==>
				  Interval$junk
			      # else ==>
				  shift[fp_appr[x],
					to_Short[how_much] * base_exp]
			      fi
			]
                     };
            
            (* Same thing, but by bits rather than digits *)
            shift == func[x: val I; how_much: val Short] val I {
            		use Short in let
            		    r == if how_much < 0 then
            		            base_exp - (-how_much-1) % base_exp - 1
            		         else
            		            how_much % base_exp
            		         fi;
            		    rest == if r = 0 then 0 else r - base_exp fi;
            		    scale_amount == PrecT$from_Short[
            		    			(how_much - rest)/base_exp
            		    		    ];
            		in
            		    I$Mk2[func[prec: val PrecT; val I; impure] {
				    shift[val_I_to_Long[x, prec - scale_amount],
				          xLong$In[rest]];
			          },
			          if how_much > to_Short[big] ==>
				      Interval$junk
			          # else ==>
				      shift[fp_appr[x], how_much]
			          fi
			    ]
            		ni ni
            	     };
            

            (* Find the position of the most significant digit of x *)
	    (* Not guaranteed to look further than the position     *)
	    (* specified by limit argument.                         *)
	    (* Returns a value less than limit if msd was not found *)
	    msd2 ==
		func[x: val I; limit: val PrecT; impure] val PrecT {
		    let
			overflow_prec == PrecT$50;
			default_guess == -PrecT$10;
			no_fp_appr == is_junk[fp_appr[x]];
			offset_guess1 ==
			    if no_fp_appr ==>
				overflow_prec (* Assume overflow *)
			    # else ==>
				PrecT$from_Short[exponent[appr[fp_appr[x]]]]
				      / Pbase_exp - PrecT$3;
			    fi;
		       (* prec value at which x is unlikely to evaluate *)
		       (* to 0                                          *)
			offset_decr == PrecT$16;
			has_prev_appr == value_valid[x]^;
			offset_guess2 ==
			    if has_prev_appr ==> min_prec[x]^
			    # else ==>
				if offset_guess1 = overflow_prec ==>
				    default_guess
				# else ==>
				    offset_guess1
				fi
			    fi;
			offset == PrecT$New[];
			value == xLong$New[];
		    in
			offset := if
				    no_fp_appr cand has_prev_appr ==>
				      offset_guess2
				  # else ==>
				      offset_guess1
				  fi;
			do abs [value := val_I_to_Long[x, offset]] <= 1
			   cand offset >= limit ==> 
			    if offset > offset_guess2 then
				offset := offset_guess2
			    else
				if offset < -offset_decr then
				    offset := offset * PrecT$2
				else
				    offset -= offset_decr
				fi
                            fi
			od;
			if
			  abs[value] > 1 ==>
			    (* x = value * base^offset and value <> 0 *)
			   offset + ndigits[value] - PrecT$1
			# else ==>
			    limit - PrecT$1
			fi
                      ni
		    };

	    (* Identical to the above, except no limit is specified *)
	    (* Diverges on 0 argument.                              *)
	    msd == func[x: val I; impure] val PrecT {
			let
			    limit == PrecT$New[];
			    result == PrecT$New[];
			in
			    limit := -big;
			    do (result := msd2[x,limit]) < limit ==>
				limit := limit * big;
			    od;
			    result
			ni
		   };

	    (* Return the approximate most significant bit position, 	*)
	    (* but not less than min.					*)
	    most_significant_bit_pos ==
	    	   func[x: val I; min: val Short; impure] {
	    	       let
	    	           limit == PrecT$In[(min - Short$2) / base_exp];
	    	           appr_prec == msd2[x, limit] - PrecT$2;
	    	           appr == val_I_to_Long[x, appr_prec]
	    	       in
	    	           PrecT$Out[appr_prec] * base_exp
	    	           + Out[nbits[appr]] - Short$1;
	    	       ni
	    	   };
	    		
	    (* Return 0 if x = y to within the indicated tolerance, *)
	    (* -1 if x < y, and +1 if x > y.  If x and y are indeed *)
	    (* equal, it is guaranteed that 0 will be returned.  If *)
	    (* they differ by less than the tolerance, anything     *)
	    (* may happen.  The tolerance is specified in terms     *)
	    (* of a relative tolerance r (in bits) and an absolute  *)
	    (* tolerance a (in bits).  The tolerance allowed is     *)
	    (* the maximum of (abs(x)+abs(y))*(2**r) and 2**a       *)
	    compare == func[x,y: val I; r,a: val Long; impure] val Short {
			 let
			    fp_result == compare[fp_appr[x], fp_appr[y]]
			 in
			   if
			     fp_result <> Short$0 ==>
			       fp_result
			   # else ==>
			       let
				 r_dig == PrecT$from_xLong[from_Long[r]]
					  /base_exp;
				 a_dig == PrecT$from_xLong[from_Long[a]]
					  /base_exp;
				 xpy == I$+[I$abs[x], I$abs[y]];
				 xpy_msd == I$msd2[xpy, a_dig];
				 prec == max[xpy_msd + r_dig - PrecT$1, a_dig];
				 xval == val_I_to_Long[x, prec];
				 yval == val_I_to_Long[y, prec];
			       in
				 if
				   xval > yval + 1 ==> Short$1
				 # yval > xval + 1 ==> -Short$1
				 # else            ==> Short$0
				 fi
			       ni
			   fi
			 ni
		       };

	    (* Truncate a constructive real number r to an integer *)
	    (* If r is less than an integer n, but within 2**tol   *)
	    (* of n, the answer may be either n or n-1.            *)
	    to_Long == func[x: val I; tol: val Long; impure] {
			 let
			   init_prec == -PrecT$4;
			   init_prec_bits == init_prec * Pbase_exp;
			   init_appr == val_I_to_Long[x, init_prec];
			   sgn == sign[init_appr];
			   abs_init_appr == abs[init_appr];
			   int_part == shift[abs_init_appr,
					     to_xLong[init_prec_bits]];
			   fraction == abs_init_appr
				       - shift[int_part,
					       to_xLong[-init_prec_bits]];
			 in
			   if
			     fraction > 1
			     cand fraction
				  < shift[1, to_xLong[-init_prec_bits]]-1 ==>
				(* Safe to use this approximation *)
				to_Long[int_part * sgn]
			   # else ==>
			       let
				 dig_tol == PrecT$from_xLong[
					       from_Long[tol]
					    ]/base_exp + PrecT$1;
				 xval == val_I_to_Long[x, dig_tol];
				 int_part == shift[abs[xval]+1,
						   to_xLong[dig_tol
							    *Pbase_exp]];
			       in
				 to_Long[sign[xval]*int_part]
			       ni
			   fi
			 ni
		       };

            (* Compute the multiplicative inverse directly *)
	    inv1 == func[prec: val PrecT; x: val I; msd: val PrecT; impure] {
                        let 
			    (* msd == I$msd[x]; *)
			    inv_msd == -PrecT$1 - msd;
			    digits_needed == inv_msd - prec + PrecT$2;
				(* See inv2 for cryptic explanation *)
                            prec_needed == msd - digits_needed;
                            scale_factor == -prec - prec_needed;
			    dividend == scale[1,scale_factor];
			    scaled_divisor == val_I_to_Long[x, prec_needed];
                        in
                            if
			      digits_needed <= PrecT$0 ==> 0
			    # else ==> 
				       (* (dividend + shift[scaled_divisor,-1])
					  / scaled_divisor *)
				       (* rounded division *)
				let
				    abs_dividend ==
					abs[dividend
					    + shift[scaled_divisor,-1]];
				    abs_divisor == abs[scaled_divisor];
				    quotient == xLong$New[];
				in
				    if digits_needed <= PrecT$500 then
					quotient := abs_dividend/abs_divisor
				    else
					quotient := q[fast_div[abs_dividend,
							       abs_divisor]];
				    fi;
				    if (dividend < 0)
					= (scaled_divisor < 0) then
					quotient
				    else
					- quotient
				    fi
				ni
                            fi
                        ni;
                    };
	(*
            (* This uses Newton's method.  It assumes that one iteration  *)
            (* is sufficient                                              *)
	    inv2 == func[prec: val PrecT; prev_appr, x: val I;
			 msdx:val PrecT; impure] {
                        let
			  current_appr == xLong$New[];  (* scaled by precm2 *)
			  last_appr == xLong$New[];     (* scaled by precm2 *)
                          last_ap_valid == Boolean$New[];
			  precm2 == prec - PrecT$2;
			  (* msdx == msd[x]; *)
			  inv_msd == -PrecT$1 - msdx;
						  (* Msd position in result *)
			  digits_needed == inv_msd - prec + PrecT$2;
				(* Number of SIGNIFICANT digits needed for *)
				(* argument, excl. msd position, which may *)
				(* be fictitious, since msd routine can be *)
				(* off by 1.  Roughly 1 extra digit is     *)
				(* needed since the relative error is the  *)
				(* same in the argument and result, but    *)
				(* this isn't quite the same as the number *)
				(* of significant digits.  Another digit   *)
				(* is needed to compensate for slop in the *)
				(* calculation.                            *)
			  prec_x == msdx - digits_needed;
                            (* precision to which argument must be evaluated *)
                          long_x == val_I_to_Long[x, prec_x];
                          (* constants needed for scaling *)
                            '2' == scale[2, -prec_x - precm2];
                            intermediate_prec == -digits_needed;
                            scale1 == prec_x + precm2 - intermediate_prec;
                        in
                          (* Use previous approximation as starting point *)
                               current_appr := val_I_to_Long[
						  current_approx[prev_appr],
                                                  precm2
                                              ];

                          let
                              old == V[current_appr]
                          in
                              (* set current_appr := old * (2 - x * old)   *)
                              (* appropriately scaled                      *)
                              current_appr := scale['2' - long_x*old, scale1];
                              current_appr := scale[old * current_appr,
                                                    intermediate_prec];
                          ni;
			  scale[current_appr, -PrecT$2]
                        ni
		    };
	*)

	    (* The real inverse function.  Uses inv1 to whenever        *)
	    (* the number of significant digits is < Newton_threshold.  *)
	    (* Otherwise it recursively approximates the inverse to     *)
	    (* sligtly more than half the number of significant digits  *)
	    (* and then uses one iteration of Newton's method (inv2) to *)
	    (* refine the approximation.                                *)
	    inv == func[x: val I] val I {
		      I$Mk2[
		       func[prec: val PrecT; prev_appr: val I; impure]
			    val xLong {
                        let
			  msdx == msd[x];
			(*
			  Newton_threshold == PrecT$30;
			  inv_msd == -PrecT$1 - msdx;
			  digits_needed == inv_msd - prec + PrecT$1;
			  prev_min_digits == shift[digits_needed+PrecT$5,
						   -PrecT$1];
			    (* Ceiling[digits_needed/2]                *)
			    (* + 1 since error is squared, number of   *)
			    (*   digits may not quite be doubled.      *)
			    (* + 1 for rounding errors                 *)

                          half_prec == inv_msd - prev_min_digits;
                          prev_digits == if
					    not value_valid[prev_appr]^ ==> PrecT$0
					 #  else ==> ndigits[max_val[prev_appr]^]
					 fi;
			*)
			in
			(*
			  if
			      digits_needed <= Newton_threshold ==>
			*)
                                (* Do it the obvious way *)
				  inv1[prec,x,msdx]
			(*
                          #   else ==>
                                if
                                  prev_digits < prev_min_digits ==>
                                      (* Force recursive evaluation to *)
                                      (* prev_min_digits               *)
                                         val_I_to_Long[prev_appr, half_prec];
                                # else ==> skip
                                fi;
                                (* Newton's method will converge in 1 *)
                                (* iteration                          *)
				inv2[prec, prev_appr, x, msdx]
			  fi;
			*)
			ni
		       },
		       inv[fp_appr[x]]
		      ]
                   };

	    (* Multiplication:                                   *)
	    (* Note that the absolute precision required from    *)
	    (* an argument depends on the value of the other     *)
	    (* argument.  This requires a trial evaluation of    *)
	    (* at least one argument.  Bishop first computes     *)
	    (* an integral approximation to one of the arguments.*)
	    (* This can lead to bad behaviour if the argument    *)
	    (* is much less than one.  Thus we try computing     *)
	    (* both arguments to about half the required         *)
	    (* precision.  As soon as we find one that's nonzero,*)
	    (* we're OK.  If both are 0, the result must be 0.   *)
	    * == func[x,y: val I] val I {
		    I$Mk2[
		     func[prec: val PrecT; val I; impure] {
                      let
			half_prec == shift[prec,-PrecT$1] - PrecT$1;
                        (* It is sometimes convenient to swap x and y ... *)
                        swap == Boolean$New[];
			both_0 == Boolean$New[];  (* Both arguments are close *)
                                                  (* enough to 0 to return 0  *)
                      in
			if 
			   value_valid[x]^ cand abs[max_val[x]^] > 1
			   cor compare[fp_appr[x], interval_zero] <> Short$0 ==>
                            (* easy to find leading digit of x *)
                            swap := False;
                            both_0 := False;
			#  value_valid[y]^ cand abs[max_val[y]^] > 1
			   cor compare[fp_appr[y], interval_zero] <> Short$0 ==>
                            (* easy to find leading digit of y *)
                            swap := True;
                            both_0 := False;
                        #  else ==>
                            if abs[val_I_to_Long[x, half_prec]] > 1 ==>
                                swap := False;
                                both_0 := False;
                            #  abs[val_I_to_Long[y, half_prec]] > 1 ==>
                                swap := True;
                                both_0 := False;
                            #  else ==>
                                both_0 := True;
                            fi
                        fi;
                        if both_0 ==> 0
                        # else ==>
                          let
			    first_arg == if swap ==> y # else ==> x fi;
			    second_arg == if swap ==> x # else ==> y fi;
			    msd1 == msd[first_arg];
			    prec2 == prec - msd1 - PrecT$2;
			    appr2 == val_I_to_Long[second_arg, prec2];
			    msd2 == prec2 + ndigits[appr2] - PrecT$1;
			    prec1 == prec - msd2 - PrecT$2;
			    appr1 == val_I_to_Long[first_arg, prec1];
                            scale_factor == prec1 + prec2 - prec;
                          in
                            scale[appr1 * appr2, scale_factor]
                          ni
                        fi
		      ni;
		     },
		     fp_appr[x]*fp_appr[y]
		    ]
                 };

	    / == func[x,y: val I] val I {
                    x I$* I$inv[y]
                 };

	    sqrt ==
		func[x: val I] val I {
		  I$Mk2[
		    func[prec: val PrecT; prev_appr: val I; impure] val xLong{
			let
			  rec_threshold == PrecT$6;
			  current_appr == xLong$New[];  (* scaled by precm2 *)
			  last_appr == xLong$New[];
			  precm2 == prec - PrecT$2;
			  prect2 == PrecT$2 * prec;

			(* precision for trial eval to find msd *)
			  trial_prec ==
			    max[prec,
				PrecT$from_Short[
				    exponent[appr[fp_appr[x]]]
				    / base_exp - Short$3
				]
			    ];
			  long_val == val_I_to_Long[x, trial_prec];

			(* not necessary for correctness, but could speed*)
			(* things up exponentially			 *)
                          msd == if long_val < Lbase ==>
                                    ndigits[val_I_to_Long[x,prect2]]
				    + prect2 - PrecT$1
                                 # else ==>
				    ndigits[long_val] + trial_prec - PrecT$1;
				 fi;
			  msd_res == shift[msd,-PrecT$1];
			  digits_needed == msd_res - prec + PrecT$2;
				(* incl. 1 for rounding etc. *)
                          prec_x == msd - digits_needed;
                          long_x == val_I_to_Long[x, prec_x];
			  ild == xLong$New[];  (* initial leading digit *)
                          last_ap_valid == Boolean$New[];
			  scale1 == prec_x - precm2*PrecT$2;
                          scaled_x == scale[long_x, scale1];
                                    (* x scaled by 2*precm2 *)
                        in
                          (* Find a reasonable starting point *)
                            if
                                msd_res - prec > rec_threshold ==>
                                  (* Force evaluation to lower precision *)
                                    val_I_to_Long[prev_appr,
						  msd_res
						  - (msd_res - prec)/PrecT$4];
                            #   else ==> skip;
                            fi;
			    if
			       exponent[appr[fp_appr[x]]]
			       > exponent[tol[fp_appr[x]]] + Short$10
			       cor value_valid[prev_appr]^
			       cand abs[max_val[prev_appr]^] > Lbase ==>
                               current_appr := val_I_to_Long[
						  current_approx[prev_appr],
                                                  precm2
                                              ]
                            # else ==>
                               if
                                  msd % Short$2 = Short$0 ==> ild := 1
                               #  else  ==>  ild := 3
                               fi;
                               current_appr := scale[ild, msd_res-precm2]
                            fi;

                          last_ap_valid := False;
                          
                          do not last_ap_valid cor
                             abs[last_appr - current_appr] >= Lbase ==>
                            (* Assertion: in the case of 0, the above will *)
                            (* be false before we divide by 0              *)
                            last_ap_valid := True;
                            last_appr := current_appr;
                            let
                              old == V[last_appr]
                            in
                              current_appr := (old * old + scaled_x)
					      / shift[old,1];
			    ni;
                          od;
			  scale[current_appr, -PrecT$2]
                        ni
		       },
		       sqrt[fp_appr[x]]
		      ]
                    };

            (* Compute an approximation of e^x to precision prec *)
            (* This assumes x < 1/base.                          *)
            (* It uses a Taylor series expansion.                *)
            (* Unfortunately there appears to be no way to take  *)
            (* advantage of old information.                     *)
            (* Note: this is known to be a bad algorithm for     *)
            (* floating point.  Unfortunately, other alternatives*)
	    (* appear to require precomputed information.        *)
	    exp1 == func[prec: val PrecT; x: val I; impure] val xLong {
                      let
                        (* Determine needed calculation accuracy *)
			iterations_needed == shift[-prec,-PrecT$1] + PrecT$1;
                                             (* conservative estimate *)
                          (* Claim: each intermediate term is accurate *)
                          (* to base^calc_precision.  Total error is   *)
                          (* 2*iterations_needed*base^calc_precision   *)
                          (* exclusive of error in x                   *)
                          calc_precision == prec
					    - ndigits[to_xLong[PrecT$2
						      *iterations_needed]]
					    - PrecT$1
					     (* for inaccuracy in x, trunc *);
#                       ifdef SHORT_PRECISION
			  n == Short$New[];    (* number of current iteration *)
#                       else
			  n == xLong$New[];    (* number of current iteration *)
#                       endif
			precm1 == prec - PrecT$1;
			x_apr == val_I_to_Long[x, precm1];
			c_term == xLong$New[]; (* nth term in Taylor series *)
			c_sum == xLong$New[];  (* Taylor series truncated   *)
                                              (* at nth term.              *)
                                              (* Both of the above are     *)
                                              (* scaled by -calc_precision *)
#			ifdef SHORT_PRECISION
			    ZERO == Short$0;	ONE == Short$1;
#			else
			    ZERO == 0;		ONE == 1;
#			endif
	               max_trunc_error == scale[1, (precm1) - calc_precision];
		      in
			n := ZERO;
                        c_term := c_sum := scale[1, -calc_precision];
			do abs[c_term] > max_trunc_error ==>
			    n := n + ONE;
                            c_term := scale[c_term * x_apr, precm1];
			    c_term := c_term/n;
			    c_sum += c_term;
			od;
                        scale[c_sum, calc_precision - prec];
                      ni
                    };

	    exp  == func[x: val I; impure] val I {
		    (* This assumes base = 4 *)
                      let
			rough_appr == val_I_to_Long[x, -PrecT$10];
			comp == 2; (* 2 * 4 ** (-10) *)
                      in
                        if
                          rough_appr < 0 ==> inv[exp[I$-[x]]];
                        # rough_appr >= comp ==>
			  (* scale it *)
                            let
			      fourth_root == exp[I$scale[x,-PrecT$1]];
                              square_root == fourth_root*fourth_root;
                            in
                              square_root * square_root
                            ni;
                        # else ==>
			    I$Mk2[func[prec: val PrecT; prev_appr: val I; impure]
				      val xLong {
				    exp1[prec, x]
				  },
				  exp[fp_appr[x]]
				 ]
                        fi
                      ni
                    };

            (* Compute an approximation of ln(1+x) to precision  *)
            (* prec. This assumes x < 1/base.                    *)
            (* It uses a Taylor series expansion.                *)
            (* Unfortunately there appears to be no way to take  *)
            (* advantage of old information.                     *)
            (* Note: this is known to be a bad algorithm for     *)
            (* floating point.  Unfortunately, other alternatives*)
            (* appear to require tabular information.            *)
	    ln1 == func[prec: val PrecT; x: val I; impure] val xLong {
                      let
                        (* Determine needed calculation accuracy *)
			precm1 == prec - PrecT$1;
                        iterations_needed == -precm1;
                                             (* conservative estimate *)
                          (* Claim: each intermediate term is accurate *)
                          (* to base^calc_precision.  Total error is   *)
                          (* 2*iterations_needed*base^calc_precision   *)
                          (* exclusive of error in x                   *)
                          calc_precision == prec
					    - ndigits[to_xLong[PrecT$2
						       *iterations_needed]]
					    - PrecT$1
					     (* for inaccuracy in x, trunc *);
#                       ifdef SHORT_PRECISION
			  n == Short$New[];    (* number of current iteration *)
			  c_sign == Short$New[]; (* (-1)^(n-1)                *)
#                       else
			  n == xLong$New[];    (* number of current iteration *)
			  c_sign == xLong$New[]; (* (-1)^(n-1)                *)
#                       endif
                        x_apr == val_I_to_Long[x, precm1];
			x_nth == xLong$New[];  (* nth power of x            *)
			c_term == xLong$New[]; (* nth term in Taylor series *)
			c_sum == xLong$New[];  (* Taylor series truncated   *)
					       (* at nth term.              *)
					       (* Both of the above are     *)
					       (* scaled by -calc_precision *)
                        max_trunc_error == scale[1, precm1 - calc_precision];
		      in
#                       ifdef SHORT_PRECISION
			  n := Short$1;
			  c_sign := Short$1;
#                       else
			  n := 1;
			  c_sign := 1;
#                       endif
                        x_nth := 
                          c_term :=
                          c_sum := scale[x_apr, precm1-calc_precision];
			do abs[c_term] > max_trunc_error ==>
#                           ifdef SHORT_PRECISION
				n := n + Short$1;
#                           else
				n := n + 1;
#                           endif
                            c_sign := -c_sign;
			    x_nth := scale[x_nth * x_apr, precm1];
			    c_term := x_nth/(c_sign*n);
			    c_sum += c_term;
                        od;
                        scale[c_sum, calc_precision - prec];
                      ni
		    };

	    (* A version of ln without prescaling  *)
	    (* Converges only in the vicinity of 1 *)
	    simple_ln == func[x: val I] val I {
			    I$Mk2[func[prec: val PrecT; prev_appr: val I; impure]
				       val xLong {
				    ln1[prec, x - I$In[Long$1]]
				  },
				  ln[fp_appr[x]]
			    ]
			 };

	    (* Natural log function.  Uses ln2 = ln[2], computed below *)
	    ln  == func[x: val I; impure] val I {
                      let
			rough_appr == val_I_to_Long[x, -PrecT$2];
                        Lbase2 == Lbase*Lbase;
                      in
                        if
			  rough_appr < -1 ==> (* diverge peacefully *)
					      do True ==> Null[] od;
					      I$In[Long$0]
                        # rough_appr >= -1 cand rough_appr < Lbase2 - 1 ==>
                            I$-[I$ln[I$inv[x]]]
                        # rough_appr >= Lbase2 - 1 cand
			  rough_appr < Lbase2 + Lbase ==>
			    simple_ln[x]
			# rough_appr >= Lbase2 + Lbase cand
			  rough_appr < Lbase2*2 ==>
                            let
                              quarter == I$ln[I$sqrt[I$sqrt[x]]];
                            in
			      I$scale[quarter,PrecT$1]
                            ni;
                        # rough_appr >= Lbase2*2 cand
                          rough_appr < Lbase2*Lbase ==>
                            let
			      log_half == I$ln[x/I$In[Long$2]];
                            in
			      log_half + I$ln2
                            ni;
                        # else ==>
                            let
				nexcdigs == ndigits[rough_appr] - PrecT$3;
                            in
                                I$ln[I$scale[x, -nexcdigs]]
				+ I$In[to_Long[to_xLong[nexcdigs
							* Pbase_exp]]] * I$ln2;
                            ni
                        fi
                      ni
                    };

	(* calculates Taylor series for sin or cos of a constructive    *)
	(* and returns an xLong approximation.  If flag == COS, the cos	*)
	(* is returned, otherwise the sin is returned.			*)

#define COS Short$0		(* flags passed to transxl to indicate  *)
#define SIN Short$1		(* which function to compute.		*)
#define ATAN Short$2		(* (cosine, sine, arctan, arcsin)	*)
#define ASIN Short$3		(* SIN is currently not used.		*)

	    transxl == func[flg: val Short; prec: val PrecT; x: val I; impure]
				val xLong {
	      use PrecT in let

	(* make a good conservative guess at number of iterations	*)
	(* we expect loop to go through.				*)

		its_needed ==	if (flg = ATAN) or (flg=ASIN) ==>
				    shift[-prec,2];	(* quite conservative *)
				# else ==>
				    shift[-prec,-2] + 10;
				fi;
			(* this number depends on the value of comp	*)
			(* used in cos and sin (i.e. the maximum value	*)
			(* that scaling passes to transxl)		*)
			(* the value was empirically derived		*)

		count ==	Short$New[];
		pow ==		Short$New[];

	(* we expect to lose about log[its_needed] digits of accuracy	*)

		calc_precn ==	prec		(* final precision required *)
				- ndigits[to_xLong[2*its_needed]]
				- 1;

		precm1 ==	prec - 1; (* precision minus 1 *)
		scaler ==	scale[xLong$1, -calc_precn];
		count2 ==       xLong$New[];

	    (* these two are scaled by calc_precision *)
		sum ==		xLong$New[];	(* sum of terms in series*)
		term ==		xLong$New[];	(* current term in series *)
		x_term ==	xLong$New[];

#		ifdef SHORT_PRECISION
			twoi == Short$New[];
			'2' == Short$2;
			'1' == Short$1;
			adder == if flg = COS ==> - Short$1
				 # else ==> Short$1
				 fi;
#		else
			twoi == xLong$New[];    (* 2*(current term #) *)
			'2' == xLong$2;
			'1' == xLong$1;
			adder == if flg = COS ==> - xLong$1
				 # else ==> xLong$1
				 fi;
#		endif
		max_error ==	scale[xLong$1, precm1 - calc_precn];

		x_apr == 	val_I_to_Long[x,calc_precn]; (* approx to x *)
		x2 == 		x_apr * x_apr;	   	   (* approx to x^2 *)

		oldterm ==	xLong$New[];
	    in use PrecT in
		count := Short$0;

		twoi := '2';

		if flg = COS ==>	sum := term := scaler;
		# flg = ATAN ==>	x_term := term := sum := x_apr;
		# flg = ASIN ==>	term := sum := x_apr;
		# else ==>		sum := term := x_apr;
		fi;

		(* calculate until term goes to zero due to integer	*)
		(* division truncation or because x_apr was zero.	*)
		if flg = ATAN ==>
		    do abs[term] > max_error ==>
			count := count + Short$1;
			x_term := scale[-x_term * x2,2*calc_precn];
			pow := Short$2*count+Short$1;
			term := x_term/pow;
			sum := sum + term;
		    od;
		# flg = ASIN ==>
		    (* we know arcsin's arg is between 0 and ~.707 *)
		    do abs[term] > max_error ==>
			count += Short$1;

			term := scale[term * x2,2*calc_precn];
			use xLong in let
			    mult == (2*from_Short[count]-1);
			in
			term := term * mult * mult;
			ni ni;

			use Short in
			    term := term/(2*count*(2*count+1));
			ni;
			sum += term;
		    od;
		# else ==> (* SIN, COS *)
		    do abs[term] > max_error ==>
			count := count + Short$1;
		        term := - term * x2;
		        term:= scale[term/(twoi*(twoi + adder)),2*calc_precn];
		        sum := sum + term;
		        twoi := twoi + '2';
		    od;
		fi;

		scale[sum,calc_precn - prec];
	      ni ni ni
	    }; (* end transxl *)



	(* take the sin of a constructive real *)
	    sin == func[x: val I; impure] val I {
		cos[I$pi2 - x]
	    };

	(* cosine of contructive real *)
	    cos == func[x: val I; impure] val I {
		let
		    two ==	I$In[Long$2];
		    one ==	I$In[Long$1];
		    rough_appr == val_I_to_Long[x,-PrecT$4];
		    scaled1 ==     scale[1,PrecT$4]; (* compare to 1 *)
		in
		    if abs[rough_appr] > scaled1 then
			let 
			    scaled8 == scale[8, PrecT$4];
			in
			    if abs[rough_appr] > scaled8 then
			      (* First subtract of multiples of 2 pi *)
				let
				  base2 == base * base;
				  base4 == base2 * base2;
				  long_2pi == from_Long[to_Long[
						use Short in
						  Float$In[2 * base4] * 3.1416
						ni
					      ]];
				  mult_2pi == rough_appr/long_2pi;
				in
				  cos[x - I$In[to_Long[4 * mult_2pi]] * I$pi2];
				ni
			    else
			      (* Use double angle formula *)
				let
				  cshalf == cos[x/two];
				in
				  cshalf * cshalf * two - one;
				ni;
			    fi
			ni
		    else
	            	I$Mk2[
		            func[prec: val PrecT; prev_appr: val I; impure]
			        val xLong {
				    transxl[COS, prec, x];
			        },
			    cos[fp_appr[x]]
		    	];
		    fi
		ni
	    };


	    (* arctan function *)
	    atan == func[x: val I; impure] val I {
	      let
		comp == scale[1,PrecT$2] - 1; (* slightly less than 1 *)
		rough_appr == abs[val_I_to_Long[x,-PrecT$2]];
	      in
		if rough_appr < comp ==>
		    I$Mk2[func[prec: val PrecT; pa: val I; impure]
			val xLong {
			    transxl[ATAN, prec, x];
			},
			atan[fp_appr[x]]
		    ];
		# else ==>
		(* atan x = asin (x / sqrt(x^2 + 1)) *)
		    let
			'1' == I$In[Long$1];
			asinarg == '1' / sqrt[ (x*x) + '1' ];
			ansfn == 
			 func[prec: val PrecT; pa:val I; impure] val xLong
			    { let
				cp == prec-PrecT$1;
				chk == val_I_to_Long[x,cp];
				p2v == val_I_to_Long[I$pi2,cp];
			      in
				if chk = 0 ==> xLong$0
				# else ==>
				  let
				    res == transxl[ASIN,cp,asinarg];
				    fin == scale[p2v-res,-PrecT$1];
				  in
				    if chk<0 ==> -fin;
				    # else ==>   fin;
				    fi
				  ni
				fi
			      ni
			  };
		    in
			I$Mk2[ansfn,atan[fp_appr[x]]];
		    ni
		fi;
	      ni;
	    };


	    (* Compute the arctangent of 1/N the desired precision. *)
	    (* A direct Taylor series expansion is used.            *)
	    (* 4 = base <= N <= sqrt(MaxShort) is assumed.          *)
	    (* Note that this is much faster than using the general *)
	    (* atan function.  It is used primarily to compute pi.  *)
	    atan1 == func[prec: val PrecT; N: val Short] val xLong {
                      let
                        (* Determine needed calculation accuracy *)
			iterations_needed == shift[-prec,-PrecT$1] + PrecT$1;
                                             (* conservative estimate *)
                          (* Claim: each intermediate term is accurate *)
                          (* to base^calc_precision.  Total error is   *)
                          (* 2*iterations_needed*base^calc_precision   *)
			  calc_precision == prec
					    - ndigits[to_xLong[PrecT$2
						      *iterations_needed]]
					    - PrecT$1
					     (* for inaccuracy in x, trunc *);
#                       ifdef SHORT_PRECISION
			  n == Short$New[];    (* number of current iteration *)
#                       else
			  n == xLong$New[];    (* number of current iteration *)
#                       endif
			c_power == xLong$New[]; (* nth power of 1/N         *)
			c_term == xLong$New[];  (* current term in series   *)
			c_sum == xLong$New[];  (* Taylor series truncated   *)
					       (* at nth term.              *)
					       (* Both of the above are     *)
					       (* scaled by -calc_precision *)
			sign == New[Short$1];  (* Sign of current term *)
			Nsquared == N * N;
#                       ifdef SHORT_PRECISION
			    ONE == Short$1;  TWO == Short$2;  FOUR == Short$4;
#			else
			    ONE == 1;        TWO == 2;        FOUR == 4;
#			endif
			max_trunc_error == scale[1, prec - calc_precision
						    - PrecT$1];
			'1' == scale[1, -calc_precision];
		      in
			n := ONE;
			c_sum := c_term := c_power := '1'/N;
			do c_term > max_trunc_error ==>
			    n := n + TWO;
			    sign := -sign;
			    c_power := c_power/Nsquared;
			    c_term := c_power/n;
			    if sign = ONE then
				c_sum += c_term
			    else
				c_sum -= c_term
			    fi;
			od;
                        scale[c_sum, calc_precision - prec];
                      ni
                    };


	   (* pi-valued function.                     *)
    (*
	   (* Uses pi/4 = 4 * atan(1/5) - atan(1/239) *)
	    local_pi == func[] {
		I$Mk2[func[prec: val PrecT; val I; impure] {
		      },
		      use Short, Interval in let
			'1' == from_Float_const[1.0];
			'4' == from_Float_const[4.0];
			'5' == from_Float_const[5.0];
			'239' == from_Float_const[239.0];
		      in
			'4' * ('4' * atan['1'/'5'] - atan['1'/'239'])
		      ni ni
		]
	    };
    *)

	   (* Uses alternating arithmetic/geometric mean algorithm *)
	   (* from Gaston Gonnet's book or Brent's paper.          *)
	   (* The error analysis is sloppy, but should be very     *)
	   (* conservative.  Since there are no arguments          *)
	   (* involved, this doesn't hurt much.                    *)
	    local_pi == func[] {
		I$Mk2[func[prec: val PrecT; val I; impure] {
		       if prec > -PrecT$3000 then
			(* 4 * (4 * atan(1/5) - atan(1/239)) *)
			scale[I$atan1[prec-PrecT$3,Short$5]
			      - I$atan1[prec-PrecT$2,Short$239], -PrecT$1]
		       else
			let
			  bit_prec == to_xLong[- prec * Pbase_exp];
			  slop == 10 + 4 * nbits[bit_prec];
			  calc_prec == bit_prec + slop;
			  a == New[shift[1, calc_prec]];
			  b == New[sqrt[shift[1, 2*calc_prec - 1]]];
			  t == New[shift[1, calc_prec - 2]];
			  x == New[0];  (* Log[2] x in Gonnet's book *)
			  epsilon == 10;
			  sqr == func[x: val xLong] { x * x };
			in
			  do a - b > epsilon ==>
			    let
			      tmpa == V[a]
			    in
			      a := shift[a + b, -1];
			      b := sqrt[tmpa * b];
			      t := t - shift[sqr(a - tmpa), x - calc_prec];
			      x += 1;
			    ni
			  od;
			  scale[shift[(sqr[a+b])/(shift[t,2]),
				      2 * Lbase_exp - slop],
				-PrecT$2]
			ni
		       fi
		      },
		      use Short, Interval in let
			'1' == from_Float_const[1.0];
			'4' == from_Float_const[4.0];
			'5' == from_Float_const[5.0];
			'239' == from_Float_const[239.0];
		      in
			'4' * ('4' * atan['1'/'5'] - atan['1'/'239'])
		      ni ni
		]
	    };
			
	   (* mark a value as being special and returned the marked value *)
	   (* Such special values are evaluated in chunks, rather than a  *)
	   (* digit at a time.                                            *)
	    eager ==
	      func[x: val I] val I {
		let
		  (* round up precision to next highest multiple of 200 *)
		    fn ==
		      func[prec: val PrecT; prev: val I; impure] {
			  if prec < min_prec[x]^ then
			      let
				  Incr == PrecT $200;
				  next == Incr*((prec-Incr)/Incr);
				in
				    val_I_to_Long[x,next]
				ni
			  fi;
			  val_I_to_Long[x,prec]
		      };
		in
		    I$Mk2[fn,fp_appr[x]]
		ni
	      };


	    (* pi/2 value used by other functions *)
	    pi2 == eager[I$local_pi[]/I$In[Long$2]];

	    (* ln[2] value used above *)
	    ln2 == let
			'10/9' == I$In[Long$10]/I$In[Long$9];
			'25/24' == I$In[Long$25]/I$In[Long$24];
			'81/80' == I$In[Long$81]/I$In[Long$80];
			'7' == I$In[Long$7];
			'2' == I$In[Long$2];
			'3' == I$In[Long$3];
		    in
			eager['7'*I$simple_ln['10/9']
			      - '2'*I$simple_ln['25/24']
			      + '3'*I$simple_ln['81/80']];
		    ni;


	    pi == func [impure] { I$pi2 * I$In[Long$2] };

	    put == func[x: val I; ndigits: val Long; impure] {
                   (* print an approximation of x to ndigits *)
                   (* decimal digits to the right of the     *)
                   (* decimal point.                         *)
                   (* The rounding procedure favors the      *)
		   (* value.                                 *)
		     let
                        pos_ndigits == if
					 ndigits <= Long$0 ==> PrecT$0
				       # else ==> PrecT$from_xLong[
						      from_Long[ndigits]
						  ]
                                       fi;
                        from_dec_prec ==
                            (* Conservative conversion from decimal precision *)
                            (* spec                                           *)
				func[n: val PrecT] {
				    (n * PrecT$10)/(PrecT$3 * Pbase_exp)
				    - PrecT$2
                                };
                        prec == from_dec_prec[-pos_ndigits];
                        value == val_I_to_Long[x, prec];
                        abs_value == abs[value] + 1;
                                (* get an upper bound and then truncate *)
			bin_prec == prec * Pbase_exp;
			int_part == shift[abs_value, to_xLong[bin_prec]];
			exp == 10**(to_xLong[pos_ndigits]);
			fract_part == abs_value - shift[int_part,
							to_xLong[-bin_prec]];
                               (* Fraction * base**(-prec) *)
			dec_fract_part == shift[fract_part*exp,
						to_xLong[bin_prec]];
                               (* Fraction * 10**(pos_ndigits) *)
			nzeroes == PrecT$New[];
			       (* Number of leading 0s in fraction *)
			Ptmp == PrecT$New[];
			tmp == xLong$New[];
                     in
                        (* find nzeroes *)
			  nzeroes := PrecT$0;
                          tmp := dec_fract_part;
                          do
                            (tmp := 10 * tmp) < exp
			    cand nzeroes < pos_ndigits - PrecT$1
				 ==> nzeroes += PrecT$1
                          od;

                        if
                            value < 0 ==> put["-"];
                        #   else ==> put[" "];
                        fi;
                        put[int_part];
                        put["."];
                        (* Add zeroes *)
			  Ptmp := nzeroes;
                          do
			    Ptmp > PrecT$0 ==> put["0"]; Ptmp -= PrecT$1
                          od;
			put[dec_fract_part];
			x
                     ni
                   };


	    puts == func[x: val I; ndigits: val Long; impure] val ChStr {
                   (* return a string containing	     *)
		   (* an approximation of x to ndigits	     *)
                   (* decimal digits to the right of the     *)
                   (* decimal point.                         *)
                   (* The rounding procedure favors the      *)
		   (* truncated value.                       *)
		     let
			result == ChStr$New[];
                        pos_ndigits == if
					 ndigits <= Long$0 ==> PrecT$0
				       # else ==> PrecT$from_xLong[
						      from_Long[ndigits]
						  ]
                                       fi;
                        from_dec_prec ==
                            (* Conservative conversion from decimal precision *)
                            (* spec                                           *)
				func[n: val PrecT] {
				    (n * PrecT$10)/(PrecT$3 * Pbase_exp)
				    - PrecT$2
                                };
                        prec == from_dec_prec[-pos_ndigits];
                        value == val_I_to_Long[x, prec];
                        abs_value == abs[value] + 1;
                                (* get an upper bound and then truncate *)
			bin_prec == prec * Pbase_exp;
			int_part == shift[abs_value, to_xLong[bin_prec]];
			exp == 10**(to_xLong[pos_ndigits]);
			fract_part == abs_value - shift[int_part,
							to_xLong[-bin_prec]];
                               (* Fraction * base**(-prec) *)
			dec_fract_part == shift[fract_part*exp,
						to_xLong[bin_prec]];
                               (* Fraction * 10**(pos_ndigits) *)
			nzeroes == PrecT$New[];
			       (* Number of leading 0s in fraction *)
			Ptmp == PrecT$New[];
			tmp == xLong$New[];
                     in
                        (* find nzeroes *)
			  nzeroes := PrecT$0;
                          tmp := dec_fract_part;
                          do
                            (tmp := 10 * tmp) < exp
			    cand nzeroes < pos_ndigits - PrecT$1
				 ==> nzeroes += PrecT$1
                          od;
                        if
                            value < 0 ==> result := "-";
                        #   else ==> result := " ";
                        fi;
			result := result ^* fast_puts[int_part];
                        result := result ^* ".";
                        (* Add zeroes *)
			  Ptmp := nzeroes;
                          do
			    Ptmp > PrecT$0 ==> result := result ^* "0"; 
					       Ptmp -= PrecT$1
                          od;
			result := result ^* fast_puts[dec_fract_part];
			result
                     ni
                   };


	  (*
	   * same as puts, but returns a hex string.  ndigits is number
	   * of hex digits rather than decimal.
	   *)
	    hex_puts ==
		func[x: val I; ndigits: val Long; impure] val ChStr {
		    let
		      (* result string *)
			result == ChStr$New[];

		      (* # digits to return *)
                        pos_ndigits ==
			    if ndigits <= Long$0 ==> PrecT$0
			    # else ==> PrecT$from_xLong[from_Long[ndigits]]
			    fi;

		      (* conversion from hex precision *)
                        from_hex_prec == func[n: val PrecT] { n * PrecT$2; };

		      (* precision in base *)
                        prec == from_hex_prec[-pos_ndigits];

		      (* Long value of mantissa, shifted *)
                        value == val_I_to_Long[x, prec];

		      (* truncate to get int_part *)
                        abs_value == abs[value];
			bin_prec == to_xLong[prec * Pbase_exp];
			int_part == shift[abs_value, bin_prec];	(* truncs *)

		      (*
		       * Fraction * base^(-prec), which, since base = 4,
		       * 	= Fraction * 16^(pos_ndigits)
		       *)
			fract_part == abs_value - shift[int_part,-bin_prec];
			exp == 16**(to_xLong[pos_ndigits]);

		      (* Number of leading 0s in fraction *)
			nzeroes == PrecT$New[];

			Ptmp == PrecT$New[];	(* loop variable *)
			tmp == xLong$New[];	(* another loop variable *)
		    in
		      (* find nzeroes *)
			nzeroes := PrecT$0;
			tmp := fract_part;
			do
			    (tmp := 16 * tmp) < exp
			    cand nzeroes < pos_ndigits - PrecT$1
				 ==> nzeroes += PrecT$1
			od;

                        if  value < 0 ==> result := "-";
                        #   else ==> 	  result := " ";
                        fi;

                        result := result ^* hex_puts[int_part];
                        result := result ^* ".";

		      (* Add zeroes *)
			Ptmp := nzeroes;
			do  Ptmp > PrecT$0 ==>
				result := result ^* "0"; 
				Ptmp -= PrecT$1
			od;

			result := result ^* hex_puts[fract_part];
			result
		    ni
		};


	    In == func[x: val Long] val I {
		      I$Mk[func [prec: val PrecT; val I; impure] {
			       scale[from_Long[x],-prec];
			   },
			   create[PrecT$0],
			   create[from_Long[x]],
			   create[True], True,
			   if abs[from_Long[x]] > max_Float ==>
				Interval$junk
			   # else ==>
				Interval$from_Float_const[to_Float[from_Long[x]]]
			   fi
		      ];
		  };

            (* convert x*base^offset *)
	    In == func[x: val Long; offset: val Long] val I {
		    let
		      Poffset == PrecT$from_xLong[from_Long[offset]]
		    in
		      I$Mk[func [prec: val PrecT; val I; impure] {
			      scale[from_Long[x],Poffset-prec]
			   },
			   create[Poffset],
			   create[from_Long[x]],
			   create[True], True,
			   if abs[from_Long[x]] > max_Float
			      cor abs[Poffset] > big ==>
			     Interval$junk
			   # else ==>
			     shift[Interval$
				    from_Float_const[to_Float[from_Long[x]]],
				   Long$Out[offset]*base_exp]
			   fi
		      ];
		    ni
		  };

	    . == func[whole, fraction, length: val Long] val I {
		    let
			(* Convert everything from Long to xLong. Gross... *)
			Xwhole == from_Long[whole];
			Xlength == from_Long[length];
			Xfraction == from_Long[fraction];
                    in
                      if
			Xfraction = 0 ==>
			   I$In[whole]
		      # else ==>
			   let
			      multiplier == 10**Xlength;
			      Xint_val == Xwhole * multiplier + Xfraction;
			   in
			      I$In[to_Long[Xint_val]]
			      / I$In[to_Long[multiplier]]
			   ni
                      fi;
		    ni
                 };
        }
    in
	cr
	hide {Mk; Mk2; fn; scale; min_prec; max_val; value_valid; value_exact;
	      fp_appr; val_I_to_Long; msd; msd2; inv1; (* inv2; *) exp1; ln1;
	      transxl; round; atan1; simple_ln; ln2; local_pi; pi2}
    ni ni ni
}
