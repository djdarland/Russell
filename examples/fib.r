let
        f1 == Short$New[];
        f2 == Short$New[];
        fib == Short$New[];
        i == Short$New[];
        n == 22;
in
        i := 1;
        f1 := 1;     (* fib[i-1] *)
        f2 := 0;    (* fib[i-2] *)
	fib := 1;    (* fib[n]   *)
        do
	    (* fib = fib[i] = f1 + f2 *)
	    i < n ==>
		i := i + 1;
		f2 := f1;
		f1 := fib;
		fib := f1 + f2
	od;
        put[fib]; put["\n"]
ni
			
