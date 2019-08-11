let
  compose ==
    func[f: func[val t2]val t3; g: func[val t1]val t2; t1,t2,t3: type{}]
        {func[x: val t1]val t3 { f[g[x]] }};
  incr == func[x: val Short] { x + 1 };
  nput == compose[Short$put, incr];
in
  nput[3]
ni

