# include "defs.h"
# define QuadList (List [Short])

func [in_file : val File; max_quads : val Short;    ] {

(* is there a cleverer way to do this patching stuff ? *)

    let
      tablesize == Short$1024;
      MAXLABELSZ == 512;

      QorPL == union Q {
		quad : val Short;
		fn : val Short; (* only for strings that are names of fns
					being defined (BSF/BFN); the value is
					the index into fn_array, later *)
					
		patchlist : val QuadList;
	    };

      QorPLRef == extend {Ref [QorPL]} with Q {
	    create == func[x: val QorPL] {
		let
		    v == QorPL$New[];
		in
		    v := x;
		    Q$In[v]
		ni
	    }};

      Dtable == extern {"hash"} [tablesize, Short, Short, -1];

      Htable == extern {"hash"}[tablesize, ChStr, QorPLRef, QorPLRef$Nil []];

      (* hash function returns a POSITIVE number; the htable implementation
	 takes care of getting it in range *)
      hash == func [sym : val ChStr] val Short {
		let length == len [sym]
		in
			(31 * ChStr$Out[sym] + 5 * sym.(length - 1) + length)
		ni
		};

    in
	prod P {
	    dests : val Ref Dtable;
	    htable : val Ref Htable
		}

    with L {

	fn_def == func [opc : val Short] val Boolean {
			(opc = BSF) cor (opc = BFN)};

	branch == func [opc : val Short] val Boolean {
			(opc >= 1) cand (opc <= 3)};

	getstr == func [s : val ChStr; i : val Short; impure] val ChStr
		{
		    let c == readc [in_file] in
			if (c <> ChStr$In[0]) and (not [eof[in_file]])
			then
			    if (i + 1) >= MAXLABELSZ then
				write [File$stderr, "Label too long\n"];
				L$getstr [ChStr$'', 0]
			    else L$getstr [s ^* c, i + 1]
			    fi
			else s
			fi
		   ni
		};


	Init == func [] val L {(***********************************)

		let ht == Htable$Init [];
		    ds == Dtable$Init [];
		in
			L$Mk [(Ref Dtable)$In [ds], 
			      (Ref Htable)$In [ht]]
		ni
	    };

	(* should only be used for q a qd that is a branch *)
	q_for_lbl_at == func [q : val Short; labels : val L;impure] val Short
			 { lookup [q, q, (dests [labels])^] };

	exists == func [name : val ChStr; labels : val L; impure]
								val Boolean {
			(name <> ChStr$'') cand
				not (Htable$lookup [hash name, name, 
							(htable [labels])^]
					= QorPLRef$Nil [])
		    };

	is_defined_fn == func [name : val ChStr; labels : val L;impure]
								val Boolean {
		(* return True if there is an entry for this string and it is
			a fn variant; null strings result from CLI calls
			which I can't deal with (can't hash the string) *)
		    (name <> ChStr$'') cand
		    let	l == Htable$lookup [hash name, name, 
							(htable [labels])^]
		    in
		      not (l = QorPLRef$Nil []) cand is_fn [l^]
		    ni
		};

	def_fn_index == func [name : val ChStr; labels : val L; impure] 
								val Short {
		    let l == Htable$lookup [hash name, name, 
							(htable [labels])^]
		    in
		      to_fn [l^]	(* the value stored with the name *)
		    ni
		};


	process_string == func [curr_quad, opc, fn_def_num : val Short;
					labels : val L;	impure]	val ChStr {

	  let 		str == L$getstr [ChStr$'', 0]
	  in
(*
DEBUG( "\n");put curr_quad;put " "; DEBUG( str);
*)

	    if L$fn_def [opc] then
		(* enter the string with a fn variant *)
		let new_entry == QorPL$New []
		in
		    new_entry := QorPL$from_fn [fn_def_num];
		    Htable$insert [hash str, str, QorPLRef$create [new_entry],
							   (htable [labels])^]
		ni		
	    elsif L$branch [opc] cor (opc = LBL) then
	    (* do whatever needs to be done with hash table and patch lists *)
		let
		    h == hash [str];
		    l == QorPLRef$New[];
		in

		    l := Htable$lookup [h, str, (htable [labels])^];
		    if  l = QorPLRef$Nil []	(* i.e. not found *)
		    then

			let new_entry == QorPL$New [] in
			    if (opc = LBL) then
(*
DEBUG( "\ndef of new :"); DEBUG( str);
*)
				new_entry := QorPL$from_quad [curr_quad + 1]
				(* the defined lbl refers to the next quad *)
			    else 
(*
DEBUG( "\nuse of new :"); DEBUG( str);
*)
				new_entry :=
				  QorPL$from_patchlist [cons [curr_quad, QuadList $'']]
			    fi;

			    Htable$insert [h, str, QorPLRef$create [new_entry],
							   (htable [labels])^]
			ni (* new_entry *)

		    else (* str was in the table *)

			if (opc = LBL) then
(*
DEBUG( "\ndef of old :"); DEBUG( str);
*)
			(* map patch on QorPL$to_patchlist [l^]    i.e.
			for each j on pl set dests [j] to curr_quad + 1 *)
			    let patch == func [pl : val QuadList; impure]
							val Void {
				if is_nil [pl] then Null[]
				else
				    let j == head pl in
				        insert [j, j, curr_quad + 1, 
							(dests [labels])^];
				        patch [tail pl]
				    ni;
			    	fi;
			    } in patch [QorPL$to_patchlist [l^]] ni;
			    (* now the l patchlist can be replaced by the correct
			      quadnum for the label being defined	*)
			    l^ := QorPL$from_quad [curr_quad]

			elsif QorPL$is_quad [l^] then
(*
DEBUG( "\nuse,already defined :"); DEBUG( str);
*)
			    (* label already defined - it's quad is known *)
			    insert [curr_quad, curr_quad, QorPL$to_quad [l^],
							    (dests [labels])^]
			else
(*
DEBUG( "\nother use of old :"); DEBUG( str);
*)
			   l^ := QorPL$from_patchlist [cons [curr_quad, 
							  to_patchlist [l^]]];

			fi
		    fi (* l is not found *)

		ni (* h, l *)
	    fi; (* branch or LBL *)

	    str (* returned value *)

	  ni (* str *)

	} (* process string *)
      } (* with *) export { Init; q_for_lbl_at; exists; 
				is_defined_fn; def_fn_index; process_string}
ni
} (* func *)

