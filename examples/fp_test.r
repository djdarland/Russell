let
    epsilon == 0.0001;
    deriv == func[f: func[val Float]val Float] {
		 func[x: val Float] val Float {
	 	    (f[x] - f[x - epsilon])/epsilon
		 }
	     };
    square == func[x: val Float] {x * x};
    double == deriv[square]
in
    put[double[13.0]]; put["\n"];
ni
