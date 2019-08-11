(*
 * This implements a numeric type equivalent to the constructive reals.
 * Rational arithmetic is used whenever possible to save time.
 * Everything is rational unless one of several things happens - these things
 * are currently ill-defined.  Most functions act the way other languages
 * handle integers and floats for rationals and reals, respectively.  That
 * is, operations on rats use rat ops, but operations which have any real
 * operands use the reals operations and convert any rationals to real.
 *)

#define skip Null[]

func [] {
    let
	xlong == extern { "xlong" } [];
	cr == (extern { "cr/cr" } []) with r { '1' == func[] {r$In[Long$1]} };
	Rat == extern { "cr/rat" } [];

	to_cr ==
	    func[x: val Rat] val cr {
		cr$In[get_num[x]] / cr$In[get_den[x]];
	    };
		
	to_cr == func[x: val cr] val cr { x };

	fusion == union {
		    creal: val cr;
		    rat: val Rat
		  }
	with F {
	    conv_to_cr ==
		func [x: val F] val cr {
		    if is_rat[x] ==>
			to_cr[to_rat[x]];
		    # else ==>
			to_creal[x];
		    fi
		};

	    force_cr_rep ==
		(* Return x represented as a constructive real *)
		func [x: val F] val F {
		    if is_rat[x] ==>
			F$from_creal[to_cr[to_rat[x]]]
		    # else ==>
			x
		    fi
		};

	    new_bifunc ==
		func [f: func[x,y: val Rat] val Rat;
		      g: func[x,y: val cr] val cr]
		{
		    func[x,y: val F] val F {
			if (is_rat[x] cand is_rat[y]) ==>
			    F$from_rat[f[to_rat[x],to_rat[y]]];
			# else ==>
			    F$from_creal[g[conv_to_cr[x],conv_to_cr[y]]];
			fi
		    }
		};

	    new_unfunc ==
		func [f: func[val Rat] val Rat;
		      g: func[val cr] val cr]
	        {
		    func[x: val F] val F {
		        if (is_rat[x]) ==>	F$from_rat[f[to_rat[x]]];
		        # else ==>		F$from_creal[g[to_creal[x]]];
		        fi
		    }
		};

	    new_unfunc ==
		func [f: func[val Rat] val Rat;
		      g: func[val cr; impure] val cr]
	        {
		    func[x: val F; impure] val F {
		        if (is_rat[x]) ==>	F$from_rat[f[to_rat[x]]];
		        # else ==>		F$from_creal[g[to_creal[x]]];
		        fi
		    }
		};

	    (* Exponentiation.  Assumes positive base. *)
	    power == func[x,y: val F; impure] val F {
		      let
			den == Long$New[];  (* denominator of exponent *)
			num == Long$New[];
			dir_cutoff == Long$500;
			    (* For bigger exponents use logs *)
		      in use Long in
			if is_rat[y]
			   cand ((den := get_den[to_rat[y]]) = 1
				 cor (den = 2))
			   cand (((num := get_num[to_rat[y]]) <= dir_cutoff)
				 cand (num >= -dir_cutoff)
				 cor (is_rat[x]
				      cand get_den[to_rat[x]] = 1)) then
			   if is_rat[x] then
			      let
				int_power == to_rat[x] ** num;
			      in
				if den = 1 then
				    F$from_rat[int_power]
				else
				    sqrt[F$from_rat[int_power]]
				fi
			      ni
			   else (* irrational, nice exponent *)
			      let
				pos_num == if num >= 0 then num else -num fi;
				pos_power == extern{"cr/exp"}
						    [to_creal[x], pos_num];
				int_power == if num >= 0 then
						 pos_power
					     else
						 cr$inv[pos_power]
					     fi;
			      in
				if den = 1 then
				    F$from_creal[int_power]
				else
				    F$from_creal[sqrt[int_power]]
				fi
			      ni
			   fi
			else  (* Need to use logs *)
			   F$from_creal[cr$exp[conv_to_cr[y]
					       * ln[conv_to_cr[x]]]]
			fi
		      ni ni
		     };

	    conv_crfunc ==
		func[f: func[val cr; impure] val cr] {
		    func[x: val F; impure] {
			F$from_creal[f[conv_to_cr[x]]];
		    }
		};

	    conv_crfunc ==
		func[f: func[val cr] val cr] {
		    func[x: val F] {
			F$from_creal[f[conv_to_cr[x]]];
		    }
		};

	    + == F$new_bifunc[Rat$+, cr$+];
	    * == F$new_bifunc[Rat$*, cr$*];
	    - == F$new_bifunc[Rat$- <<func[x,y: val Rat]val Rat>>,
			      cr$- <<func[x,y: val cr]val cr>>];
	    / == F$new_bifunc[Rat$/, cr$/];
	    - == F$new_unfunc[Rat$- <<func[val Rat]val Rat>>,
			      cr$- <<func[val cr]val cr>>];
	    inv == F$new_unfunc[Rat$inv, cr$inv];
	    abs == F$new_unfunc[Rat$abs, cr$abs];

	    In ==
		func[x: val Long] val F {
		    F$from_rat[Rat$In[x]];
		};

	    (* if a rational's numerator or denominator is "too big" it's
		printed as a real *)
	    put ==
		func[x: val F; prec: val Long; impure] val Void {
		    if is_creal[x] ==>
			cr$put[to_creal[x],prec]; skip;
		    # is_rat[x] ==>
			let px == to_cr[to_rat[x]]
			in
			    cr$put[px,prec]; skip;
			ni;
		    # else ==>	(* should never happen *)
			Rat$put[to_rat[x]]; skip;	(* throw away prec *)
		    fi
		};

	    puts ==
		func[x: val F; prec: val Long; impure] val ChStr {
		    let s ==
		    	    if is_creal[x] then
				cr$puts[to_creal[x],prec];
		    	    else
				let px == to_cr[to_rat[x]]
				in
				    cr$puts[px,prec];
				ni;
			    fi;
		    in
			s
		    ni
		};
		
	    most_significant_bit_pos == 
	        func[x: val F; min: val Short; impure] {
	            if is_creal[x] then
	                cr$most_significant_bit_pos[to_creal[x], min]
	            else
	                let px == to_cr[to_rat[x]]
			in
			    cr$most_significant_bit_pos[px,min];
			ni
		    fi
	        };
	        
	    shift == 
	        func[x: val F; how_much: val Short; impure] {
	            if is_creal[x] then
	                F$from_creal[cr$shift[to_creal[x], how_much]]
	            else
	                F$from_rat[Rat$shift[to_rat[x], how_much]]
		    fi
	        };

	    hex_puts ==
		func[x: val F; prec: val Long; impure] val ChStr {
		    let s ==
		    	    if is_creal[x] ==>
				cr$hex_puts[to_creal[x],prec];
		    	    # is_rat[x] ==>
				let px == to_cr[to_rat[x]]
				in
				    cr$hex_puts[px,prec];
				ni;
			    # else ==>	(* should never happen *)
				"BLEAGH!";
			    fi;
		    in
			s
		    ni
		};

	    sqrt == F$conv_crfunc[cr$sqrt];
	    exp == F$conv_crfunc[cr$exp];
	    ln == F$conv_crfunc[cr$ln];
	    sin == F$conv_crfunc[cr$sin];
	    cos == F$conv_crfunc[cr$cos];
	    atan == F$conv_crfunc[cr$atan];

	    pi == func[impure] val F {
		F$from_creal[cr$pi[]];
	    };

	    . == func[w,f,l: val Long; impure] val F {
		let
		    scale_factor == to_Long[(xlong$10)**(xlong$from_Long[l])];
		in
		    F$from_rat[Rat$build[w * scale_factor + f, scale_factor]]
		ni
	    };

	    to_Long == func[x: val F; tol: val Long; impure] {
		if is_rat[x] ==>
		    Rat$to_Long[to_rat[x]];
		# else ==>
		    cr$to_Long[to_creal[x], tol];
		fi
	    };

	    compare == func[x,y: val F; r,a: val Long; impure] val Short {
		if is_rat[x] cand is_rat[y] ==>
		    Rat$compare[to_rat[x],to_rat[y]];
		# else ==>
		    cr$compare[conv_to_cr[x],conv_to_cr[y],r,a];
		fi
	    };
	    eager == func[x: val F] val F {
		if is_rat[x] ==>
		    x
		# else ==>
		    F$from_creal[eager[to_creal[x]]]
		fi
	    };
	}
    in
	fusion
	export { New; :=; V; +; *; /; .;
		 - <<func[x,y: val fusion]val fusion>>;
		 - <<func[val fusion]val fusion>>;
		inv; abs; In; put; puts; cos; sqrt; exp;
		sin; atan; 
		ln;
		pi;
		power; eager;
		to_Long; compare; is_rat;
		hex_puts; force_cr_rep;
		most_significant_bit_pos; shift;
	      }
    ni
}
