
# define fn  func[ x: val Short ] val Short

let
    Y == func[ g func[ fn] fn ] {
	     func [ x val Short ] val Short {
		 (g[Y[g]]) [x]
	     }   
	 };

    G == func[ f fn  ] {
	     func[ n val Short ] val Short {
		 if
		    n > 0 ==> n * f [n - 1]
		 #  n = 0 ==> 1
		 fi
	     }
	};

    fact == Y[G]

in

    let 
	x == Short$New[];

	getarg == func[FS: var Void] {
		    put["Factorial of? "];
		    get[FS]
		  };

	nl == func[] {put["\n"]};
    in

	x := getarg[FS];

	do

	    x >= 0  ==>  put[fact[x]];  nl[]; x := getarg[FS]

	od

    ni

ni
