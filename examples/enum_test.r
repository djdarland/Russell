let
    fruits == enum {apple, orange, banana, cherry}
	      with F {
		put == func[x: val F] {
		       use F in
			if
			  x = apple ==> put("apple");
			# x = orange ==> put("orange");
			# x = banana ==> put("banana");
			# x = cherry ==> put("cherry");
			fi
		       ni; x }
	      };
    f == fruits$New[];
in
    f := fruits$First;
    do
	(put[f]; put["\n"]; f <> fruits$Last) ==> f := Succ[f]
    od;
    put["Finished printing names of "];
    put[fruits$Card];
    put[" fruits\n"]
ni
