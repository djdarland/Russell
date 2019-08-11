(* This is a simple illustration of a parametrized data type. *)
(* The problem was suggested by Scott Danforth.               *)
(* It turned out to be a surprisingly useful test of the      *)
(* compiler.                                                  *)
let
    (* Point data type, with addition operation *)
    point == prod {
		xpos, ypos: val Float
	     } with p {
		+ == func [x, y: val p] {
			p$Mk[xpos[x] + xpos[y], ypos[x] + ypos[y]]
		     }
	     } export {Mk; +; xpos; ypos};

    (* Poor square root routine.  Actually it's now built in, but this *)
    (* is more interesting.                                            *)
    sqrt == func [x: val Float] {
		let
		    xn == Float$New[];
		    xnp1 == Float$New[];
		    eps == 0.0001;
		    abs == func[x:val Float] {
				if x > 0.0 ==> x # else ==> -x fi
			   }
		in
		    xn := x;
		    do abs((xnp1 := xn/2.0 + x/(2.0*xn)) - xn) > eps ==>
			xn := xnp1;
		    od;
		    xn
		ni
	    };

    (* Line data type, parametrized with respect to the type of points *)
    line == func [p: type P {xpos, ypos: func[val P] val Float}] {
		prod {
		    x, y: val p; (* end points *)
		} with L {
		    length == func[z: val L] {
				let
				    xdiff == xpos[x[z]] - xpos[y[z]];
				    ydiff == ypos[x[z]] - ypos[y[z]]
				in
				    sqrt[xdiff * xdiff + ydiff * ydiff]
				ni
			      }
		} export {Mk; length}
	    };

in use point, line[point] in
    (* length of line from (1,1) to (3,3) *)
    put[length[Mk[Mk[1.0, 1.0], Mk[3.0, 3.0]]]]; put "\n";
ni ni
