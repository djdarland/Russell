(* Returns a function that performs a single Solovay-Strassen test *)
(* on the number n                                                 *)
(* The correctness argument appears to require that n is not a     *)
(* perfect square.                                                 *)
func[] {
    let
	xl == extern { "xlong" } [];
    in use xl in let
	factors == 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37;

	(* Compute the value of a Jacobi symbol returns a value *)
	(* different from 1 and -1 if gcd(a,b) != 1             *)
	J == func[a,b: val xl] {
		if a = 0 then
		    b
		elsif a = 1 then
		    1
		elsif a > b then
		    J[a%b,b]
		elsif odd[a] then
		    J[b,a]
		    * if use Short in a % 4 = 1 cor b % 4 = 1 ni then
			    1
			else
			    -1
		      fi
		else
		    J[shift[a,-1], b]
		    * let
			mod8 == b % Short$8
		      in
			if use Short in mod8 = 1 cor mod8 = 7 ni then
			    1
			else
			    -1
			fi
		      ni
		fi
	     };

	random == func[impure] val Short { extern "_random" };
	Zmod == extern { "primes/Zmod" };
	** == extern { "cr/exp" };
    in
	func[N: val Long; impure] {
	  if N < Long$2 then
	    False[]
	  elsif N = Long$2 then
	    True[]
	  elsif N % Short$2 = Short$0 then
	    False[]
	  else
	    let
		n == from_Long[N];
		a == abs[xl$In[random[]]] % n;
		g == gcd[n, if n > a*factors then a*factors else a fi];
		Zmodn == Zmod N;
		'-1' == Zmodn$from_Long[-Long$1];
		jacobi == Zmodn$New[];
		power == Zmodn$New[];
	    in
		if a = 0 then
		    True
		elsif g <> 1 then
		    False
		else
		    power := (Zmodn$from_Long[to_Long[a]]) ** to_Long[(n - 1)/2];
		    if power <> Zmodn$'1' cand power <> '-1' then
			False
		    else
			jacobi := Zmodn$from_Long[to_Long[J[a,n]]];
			power = jacobi
		    fi
		fi
	    ni
	  fi
	}
    ni ni ni
}
