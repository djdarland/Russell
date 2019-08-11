let
    p == func[ x: val Short ]
         {
            if
		x < 2 ==> 1
	    #   else ==> p[ x-1 ] + p[ x-2 ]
            fi
	 }
in
    put[ p[ 22 ] ]; put["\n"];
ni
