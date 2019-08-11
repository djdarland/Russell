(* compile memo.r first *)
(* Must be compiled with rc -pL *)

# define impure var Void

let
    tabsize == 30;

    (* Convert a recursive function from Short to Long to a memo-function *)
    memo == func [f: func[val Short; impure] val Long; impure] {
                extern {"memo"} [f, func[x: val Short] {x % tabsize}, tabsize]
            };

    fib == memo[func[ x: val Short; impure ] {
                    if
                        x < 2 ==> Long$1
                    #   else ==> fib[ x-1 ] + fib[ x-2 ]
                    fi
                }];

in
    put[ fib[ 35 ] ]; put["\n"];
ni
