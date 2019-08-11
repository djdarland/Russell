(* read.i reads the next quad, decides if dull, and stores opc, arg1, arg2,
	arg3, dull *)

  let
    (* these locals hold info that will eventually go into the quad *)
    dull == Boolean$New [];
    readw == func [val File; impure] val Short { extern "_getw" };
    opc == readw [in_file]
  in
    if not eof [in_file] then

	if (opc < 0) or (opc >= N_OP_CODES) then 
		write [File$stderr, "Invalid opcode\n"];
		exit [1]
	fi;

	if (opc = CLL) cor (opc = CLC) then numcalls += 1 fi;

	dull := uninteresting [opc];

	if (opc < MAX_LABEL_OP) then
	    (********  reading and processing string args  ******)

	    let str == LBLS$process_string [q, opc, fn_count, labels];
	        arg1 == Arg1type$from_str[str];
	    in  
		if (opc = BFN) cor (opc = BSF) then
		(* record names of defined functions *)
			(* record the fn_name and the quad number, whether BSF,
				and then count *)
			fn_list := cons [FNprod$Mk [str, q, (opc = BSF)], fn_list];
			fn_count += 1
		elsif (opc = CLL) cor (opc = ERR) then
		(* record names of called functions *)
			fn_names.next_call_num := str;
			fn_callquads.next_call_num := q;
			next_call_num += 1

		elsif (opc = LBA) then
			precedingLBA := str;

		fi;

		basics.q := Basics$Mk [opc, arg1, 0, 0, dull]

	    ni (* str *)
	else
	  (* read the three 'ordinary' args *)
	  let
	    arg1 == readw [in_file];
	    arg2 == readw [in_file];
	    arg3 == readw [in_file]
	  in 
	    if (opc = CLC) then
		      fn_names.next_call_num := precedingLBA;
		      fn_callquads.next_call_num := q;
		      next_call_num += 1
	    elsif (opc = CLI) then 
		      fn_names.next_call_num := ChStr$'';
		      fn_callquads.next_call_num := q;
		      next_call_num += 1
	    fi;

	    (* make the quad *)
	     basics.q := Basics$Mk [opc, Arg1type$from_arg[arg1], arg2,
						arg3, dull]

	  ni (* arg1, arg2, arg3 *)
	fi;

	q += 1

    (* else EOF unexpected *)
    fi
  ni (* dull, readw, opc *);
