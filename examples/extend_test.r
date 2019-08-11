let
  Strange == extend { Short } with S { a == S$+ };
  f == func[T:type t {a: func[x,y: val t]val t; '1'; '2'; ^+}] val T
        {use T in a[21,2] ni}
in
  put["This should print 13, 17, and 23\n"];
  put[Strange$13]; put['\n'];
  put[Out[Strange$17]]; put['\n'];
  put[f[Strange]]; put['\n'];
ni
