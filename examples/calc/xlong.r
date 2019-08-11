#define Short32 (* Short integers are 32 bits long     *)
		(* if not defined, 16 bits are assumed *)
#define skip Null[]

func [] {
  let
#   ifdef Short32
	 SMaxShort == 2147483647;
	 Sdig_size == 31; (* # of 1 bits in MaxShort *)
#   else
	 SMaxShort == 32767;
	 Sdig_size == 15; (* # of 1 bits in MaxShort *)
#   endif
    MaxShort == Long$In[SMaxShort];
    dig_size == Long$In[Sdig_size];
    (* Build the n-fold concatenation of a string c *)
    repeat   ==  func[c: val ChStr; n: val Short] val ChStr {
                   let
		     curr_exp == Short$New[];
		     curr_power == ChStr$New[];
		     curr_result == ChStr$New[];
		     rem_n == Short$New[];
		   in
                     (* curr_exp := 1; *)
                     curr_power := c;
                     curr_result := "";
                     rem_n := n;
                     do rem_n > 0 ==>
                       (* curr_result * c^(rem_x*curr_exp)  =  y^x *)
                       (* curr_power = c^curr_exp, * = concat,     *)
		       (* ^ = repeated concatenation		   *)
                       if rem_n % 2 = 1 then
                           (* rem_n -= 1; *)
                           curr_result := curr_result ^* curr_power
                       fi;
                       (* curr_exp *= 2; *)
		       rem_n := rem_n/2;
                       curr_power := curr_power ^* curr_power;
                     od;
                     curr_result
		   ni
                 };
  in
    extend {Long} with L {
	from_Float == func[x: val Float] val L {
			(L$In<<func[val Long]val L>>)[to_Long[x]]
		      };

	from_Short == L$In<<func[val Short]val L>>;
	to_Short == L$Out<<func[val L]val Short>>;

	from_Long == L$In<<func[val Long]val L>>;
	to_Long == L$Out<<func[val L]val Long>>;

        (* Compute y^x, x >= 0 *)
	**   ==  func[y,x: val L] val L {
                   let
		     curr_exp == L$New[];
		     curr_power == L$New[];
		     curr_result == L$New[];
		     rem_x == L$New[];
		   in use L in
                     (* curr_exp := 1; *)
                     curr_power := y;
                     curr_result := 1;
                     rem_x := x;
                     do rem_x > 0 ==>
                       (* curr_result * y^(rem_x*curr_exp)  =  y^x *)
                       (* curr_power = y^curr_exp *)
                       if
			   odd[rem_x]  ==>
                               (* rem_x -= 1; *)
                               curr_result *= curr_power
                           # else ==>
                               skip
                       fi;
                       (* curr_exp *= 2; *)
		       rem_x := shift[rem_x, -1];
                       curr_power *= curr_power;
                     od;
                     curr_result
		   ni ni
                 };

	(* A fast version of factorial:  *)
	! == func[n: val L] val L {
		use L in let
		    (* Compute n * (n-m) * (n-2m) * ...  *)
		    f == func[n, m: val L] {
			    let
				k == 2 * m
			    in
				if n > m then
				    f[n, k] * f[n-m, k]
				else
				    n
				fi
			    ni
			 };
		in
		    if n = 0 then
			1
		    else
			f[n,1]
		    fi
		ni ni
	     };
    
	(* Convert a Long to a Float. Caller is responsible for checking *)
	(* that an overflow will not occur.                              *)
	to_Float == func [x: val L] val Float {
			use L in let
			    nbits == 15;  (* Number of bits that can *)
					  (* be converted at once    *)
			    Snbits == L$to_Short[nbits];
			    result == Float$New[];
			    rem == L$New[];
			    log_multiplier == Short$New[];
			    tmp == L$New[];
			    c_chunk == Short$New[];
			    sgn == use Short in
				     if x < L$0 ==> -1.0 # else ==> 1.0 fi
				   ni
			in
			    result := use Short in 0.0 ni;
			    log_multiplier := Short$0;
			    rem := L$abs[x];
			    do rem <> 0 ==>
				tmp := shift[rem, -nbits];
				c_chunk := L$to_Short[rem - shift[tmp, nbits]];
				rem := tmp;
				result := result + shift[Float$In[c_chunk],
							 log_multiplier];
				log_multiplier += Snbits;
			    od;
			    sgn * result
			ni ni
		    };

	abs == func [x: val L] { if x <= L$0 ==> -x # else ==> x fi };
	sign == func [x: val L] val L {
		    use L in
		      if x < 0 ==> -1
		      #  x > 0 ==> 1
		      #  else  ==> 0
		      fi
		    ni
		};
	min == func [x,y: val L] { if x < y ==> x # else ==> y fi };
	max == func [x,y: val L] { if x > y ==> x # else ==> y fi };

	-= == func [x: var L; y: val L] val L { x := x - y };
	+= == func [x: var L; y: val L] val L { x := x + y };
	*= == func [x: var L; y: val L] val L { x := x * y };
	/= == func [x: var L; y: val L] val L { x := x / y };

	(* Compute 2^n/x using Newton iteration: *)
        (* new = old * (2^(n+1) - x*old) * 2^-n  *)
	(* Assumes x > 0			 *)
	(* The result is only approximate	 *)
	inv == func [x: val L; n: val L]  val L {
		   use L in let
			cutoff == 1024;  (* < cutoff bits ==> use division *)
			xlen == nbits[x];
			rlen == n - xlen + 2; (* upper bound on  *)
					      (*  result length. *)
			half_prec == rlen/2;
		   in
			if rlen < cutoff cor xlen < half_prec then
			    shift[1,n]/x
			else
			    let
				xscale_amount == xlen - half_prec - 1;
				appr_inv == inv[shift[x, - xscale_amount],
                                                rlen];
				inv_scale_amount ==  n - rlen
						     - xscale_amount;
			    in
				shift[shift[appr_inv, n+1-inv_scale_amount]
                                      - x * (appr_inv * appr_inv),
				      2*inv_scale_amount - n];
			    ni
			fi
		   ni ni
	       };

	(* Compute an approximation (accurate to +- 3) to sqrt(x) *)
	sqrt == func [x: val L]  val L {
		   use L in let
			cutoff == 96;  (* < cutoff bits ==> use floating pt *)
			fast_div_threshold == 4096;
			xlen == nbits[x];
		   in
			if xlen < cutoff then
			    L$from_Long[to_Long[sqrt[to_Float[x]]]]
			else
			    let
				half_len == shift[xlen, -1];
				scale == if odd[half_len] then
					     half_len - 3
					 else
					     half_len - 2
					 fi;
				appr_sqrt == shift[
                                                sqrt[shift[x, -scale]],
						shift[scale, -1]];
				quotient == if xlen < fast_div_threshold then
						x/appr_sqrt
					    else
						appr_div[x, appr_sqrt]
					    fi;	
			    in
				shift[appr_sqrt + quotient, -1]
			    ni
			fi
		   ni ni
	       };

	(* Fast division using above inverse routine, assumes args >= 0 *)
	    fast_div ==
		func[x,y: val L] val prod {q: val L; r: val L} {
		    use L in let
			ly == nbits[y];
			lx == nbits[x];
			lr == lx - ly;
			inv_bits == lr + 2;
			scaled_y == shift[y, inv_bits - ly];
			scaled_x == shift[x, inv_bits - lx];
			appr_inv == inv[scaled_y, 2 * inv_bits];
			quotient == L$New[];
			remainder == L$New[];
			nq == L$New[];
		    in
			quotient := shift[scaled_x * appr_inv, 
					  lx - ly - 2 * inv_bits];
			remainder := x - quotient * y;
			if remainder >= shift[y,1] then
			    nq := remainder/y;
			    quotient += nq;
			    remainder -= nq * y;
			elsif remainder >= y then
			    quotient += 1;
			    remainder -= y;
			    nq := 1;
			elsif remainder < 0 then
			    nq := (-remainder)/y + 1;
			    quotient -= nq;
			    remainder += nq * y;
			else
			    nq := 0;
			fi;     
			if nwords[nq] > 1 then
			    put "Fast_div blew it\n"
			fi;
			prod {q: val L; r: val L}$Mk[quotient, remainder]
		    ni ni
		};

	(* Approximate division *)
	    appr_div ==
		func[x,y: val L] val L {
		    use L in let
			ly == nbits[y];
			lx == nbits[x];
			lr == lx - ly;
			inv_bits == lr + 2;
			scaled_y == shift[y, inv_bits - ly];
			scaled_x == shift[x, inv_bits - lx];
			appr_inv == inv[scaled_y, 2 * inv_bits];
		    in
			shift[scaled_x * appr_inv, 
			      lx - ly - 2 * inv_bits]
		    ni ni
		};

	(* Divide and conquer base conversion: *)
	    fast_puts ==
		func[x: val L] val ChStr {
		    if x < L$0 then
			"-" ^* fast_puts1[x]
		    else
			fast_puts1[x]
		    fi
		};

	    fast_puts1 ==
                func[x: val L] val ChStr {
		    let 
			cutoff == 256;
                             (* < cutoff words ==> use naive algorithm *)
			l == to_Short[nwords[x]];
		    in
			if l < cutoff then
			    puts[x]
			else
		            let
				dec_power == 5 * l;
				divisor == L$In[10] ** L$In[dec_power];
				split == fast_div[x, divisor];
				quotient == q[split];
				remainder == r[split];
				head == fast_puts1[quotient];
				tail == fast_puts1[remainder];
				length == len[tail];
				zeroes == repeat["0", dec_power-length];
			    in
				head ^* zeroes ^* tail
			    ni
			fi
		    ni
		};

	(* Binary gcd algorithm *)
	gcd == func[x,y: val L] val L {
		flush[File$stdout];
		use L in let
		    u == New[abs[x]];
		    v == New[abs[y]];
		    t == New[1];
		    k == New[Short$0];
		    '-1' == -1;
		    used_u == Boolean$New[];
		in
		    if x = 0 then
			y
		    elsif y = 0 then
			x
		    elsif y = 1 then (* common case that can be sped up *)
			1
		    else
			do not[odd[u]] cand not[odd[v]] ==>
			    k := k + Short$1;
			    u := shift[u,'-1'];
			    v := shift[v,'-1'];
			od;
			if odd[u] then t := - v
			else t := u
			fi;
			do t <> 0 ==>
			    do not[odd[t]] ==> t := shift[t,'-1'] od;

			    if t < 0 then
				v := - t
			    else
				u := t
			    fi;

			    t := u - v;
			od;
			shift[u,L$In[k]];
		    fi
		ni ni
	       };

	(* Convert to hex string representation *)
	hex_puts ==
	    func[x: val L; impure] val ChStr {
	      let
		 base == Short$16;
		 to_ChStr == func[x: val Short] {
				if x < 10 then
				    ChStr$In[x + Out["0"]]
				else
				    ChStr$In[x - 10 + Out["a"]]
				fi;
			     };
		 hputs1 == func[x: val L; impure] val ChStr {
			     use L in
			       if 
#                                ifdef UNDEFINED
				   (* The Russell code: *)
				     x > 0 ==> (hputs1[x/base])
					       ^* to_ChStr[x%base]
				   # x = 0 ==> ""
#                                else
				   (* The dirty (but much faster) hack: *)
				   x >= 0 ==>
				     let
				       s == func[val L; impure] val ChStr
						{ extern "_to_hex" } [x];
				       i == New[Short$0];
				       length == len[s];
				     in
				       (* Delete all leading 0s *)
					 do i < length cand s.i = Out["0"] ==>
					   i += Short$1;
					 od;
				       substr[s,i,length-i];
				     ni
#                                endif
			       # x < 0 ==> "-" ^* hputs1[-x]
			       fi
			     ni
			   };
	      in use L in
		  if x = 0 then "0" else hputs1[x] fi
	      ni ni
	    };
    }
  ni
}
