(* A faster version of factorial.  If you don't believe it, try 5000! *)

use Long in let
    (* Compute n * (n-m) * (n-2m) * ...  *)
    f == func[n, m: val Long] {
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
    
    ! == func[n: val Long] { f[n, 1] };
    x == Long$New[];
in
    do
	(put["Factorial of?"]; x := In[get[FS]]) >= 0  ==>  put[x!]; put "\n";
    od
ni ni
