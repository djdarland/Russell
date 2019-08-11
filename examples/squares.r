(* compile lazy_sexpr.r first *)
(* compile with rc -pL        *)

# define impure var Void
# define delay(se) SEdelay[func[impure] val SE {se}]

let
    SE == extern{"lazy_sexpr"} [Long]
in use SE, Long in
    let
        Squares == func[n: val Long; impure] {
                       cons[In[n*n],delay(Squares[n+1])]
                   };
        squares == Squares[1];
        ith == func[x: val SE; i: val Long; impure] val SE {
                    let
                        y == SE$New[];
                        current == Long$New[];
                    in
                        current := 1;
                        y := x;
                        do current < i ==>
                            y := cdr[y]; current := current + 1;
                        od;
                        car[y];
                    ni
                };
        n == Long$New[]
    in
        do (n := In[Short$get[]]) > 0 ==>
            put [Out[ith[squares, n]]];
            put "\n";
        od
    ni
ni ni
