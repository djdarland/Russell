# define impure var Void
# define delay(l) func[] val ll {l}

let
    ll == LList[Short]
in use ll in
    let
        Ints == func[n: val Short] {
                       cons[n,delay(Ints[n+1])]
                };
        ints == Ints[1];
        ith == func[x: val ll; i: val Short] val Short {
                    let
                        y == ll$New[];
                        current == Short$New[];
                    in
                        current := 1;
                        y := x;
                        do current < i ==>
                            y := tail[y]; current += 1;
                        od;
                        head[y];
                    ni
                };
        n == Short$New[]
    in
        (* Grab .5 Meg *)
        Expand_Hp[128];
        do (n := Short$get[]) > 0 ==>
            put [ith[ints, n]];
            put "\n";
        od
    ni
ni ni
