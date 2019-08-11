(* Build a lazy list of primes, and then print the first 900 elements *)
(* Note that the implementation of streams is reasonably good;        *)
(* however the algorithm is so bad that no compiler can possibly bail *)
(* it out.  Compile with -p                                           *)

# define scons(x,y) cons[x, func[]{y}]

let
    S == LList[Short];

    (* Produce list of integers > n *)
    ints == func[n: val Short] val S {
		scons(n, ints[n+1])
	    };

    (* Produce the sublist of all nonmultiples of m *)
    nonm == func[m: val Short; s: val S] val S{
		if head[s] % m = 0 then
		    nonm[m, tail[s]]
		else
		    scons(head[s], (nonm[m, tail[s]]))
		fi
	    };

    (* Produce sublist of s of numbers n that are not multiples of *)
    (* numbers preceding n in the list.                            *)
    p == func[s: val S] val S{
		scons(head[s], (p[nonm[head[s], tail[s]]]))
	 };

    primes == p[ints[2]];

    s == S$New[];
    i == New[0];
in
    s := primes;
    do i < 900 ==>
	put i; put ": "; put[head[s]]; put "\n";
	s := tail[s];
	i += 1;
    od
ni

