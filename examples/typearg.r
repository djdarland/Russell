let
    f == func[x: val T; T: type t{+ :  func[x,y:val t] val t;
				  New; New: func[val t]var t; V; := ;
				  put: func[val t] val t}] {
	    let
		y == New[x];
		z == T$New[];
	    in
		z := x;
		put[y (T$+) z]; put "\n"
	    ni
	 }
in
    f[13, Short];
    f[Long$13, Long];
    f[13.0, Float]
ni
