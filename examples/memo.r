(* This should be compiled with -cpL *)
#define impure var Void

(*
 * Turn a function into one that remembers some previous arguments
 * and the corresponding results.
 * The function hash takes an argument of type T1 and produces
 * a Short integer >= 0 and < limit.
 *)
func[fn: func[val T1; impure]val T2; hash: func[val T1] val Short;
     limit: val Short; T1: type t {=; put: func[val t] val t};
     T2: type{}] func[val T1; impure] val T2 {
  let
    arg_res == prod {arg: val T1; result: val T2};
    table_entry == union {
                       present: val arg_res;
                       not_present: val Void;
		   }
                   with TE {
		       NNew == func[] {
				   let
				       x == TE$New[]
                                   in
                                       x := TE$from_not_present[Null[]];
				       x
				   ni
			       }
                   }
		   with TE {
		       New == TE$NNew;
		   };
    table_tp == Array[limit, table_entry];
    table == table_tp$New[];
  in
    func[x: val T1; impure] {
      let
	te == table.hash[x]
      in
        if
          is_present[te] cand arg[to_present[te]] = x  ==>
              result[to_present[te]]
        # else ==>
              let
		res == fn[x]
	      in
                te := table_entry$from_present[arg_res$Mk[x, res]];
		res
	      ni
	fi
      ni
    }
  ni
}
     
