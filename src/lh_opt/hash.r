# define DEBUG(S) write [File$stdout, S]
(* Implementation of a hash table with indicees of type Short *)
(* Deletions are currently not supported.                     *)
(* New will return an uninitialised array; Init will return an*)
(* initialised array, ready for insertions		      *)

func[size: val Short; key_type: type{=}; ET: type{}; not_found: val ET] {
    let
      (* Hash table chains and associated operations *)
	Entry == prod { key_val: val key_type; value: val ET };
	L == List[Entry];  (* Hash table chains *)
	list_add == func[key: val key_type; value: val ET; list: val L] {
			use L, Entry in
			    cons[Mk[key,value],list]
			ni
		    };
	list_find == func[key: val key_type; list: val L] val ET {
			if
			    is_nil[list] ==> not_found
			#   else ==>
				if
				    key_val[head[list]] = key ==>
					value[head[list]]
				#   else ==>
					list_find[key, tail[list]]
				fi
			fi
		     };

      (* Function mapping any positive short into the range 0..(size - 1) *)
	hash == func[x:val Short] val Short { x % size };

	HTable == (Array[size, L])
		with HT {
	    insert == func[hash_val: val Short;	(* hashed by caller *)
			   key: val key_type;
			   value: val ET; table: var HT] val Void {
			let
			    h == hash[hash_val]
			in
			    table.h := list_add[key, value, table.h]
			ni
		      };
	    lookup == func[hash_val: val Short;
			   key: val key_type; table: var HT] val ET {

			list_find[key, table.(hash[hash_val])]
		      };

	    Init == func[] var HT {HT$New []}
(*
		(* Since all new lists are inited. to '', no need to do it
								explicitly *)
		 	let
			    t == HT$New[];
			    i == Short$New[0];
			in
			    do i < size ==>
				t.i := L$'' (* empty *);
				i += 1;
			    od;
			    t
			ni
		     };
*);
(* unused so far - both :='s

	    := == func[x: var HT; y: val HT] val HT {
		    let
			i == Short$New[0];
		    in
			do i < size ==>
			    x.i := y.i;
			    i += 1;
			od;
			y
		    ni
		  };
	(* overloaded :=  -- this one saves on array copies *)
	    := == func[x: var HT; y: var HT] var HT {
		    let
			i == Short$New[];
		    in
			i := 0;
			do i < size ==>
			    x.i := y.i;
			    i += 1;
			od;
			y
		    ni
		  };
*)
	}
    in
	HTable export {(*:= <<func[var HTable; val HTable] val HTable>>;
		       := <<func[var HTable; var HTable] var HTable>>;*)
		       Init; (*V;*) insert; lookup}
    ni
}
