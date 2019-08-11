(* Input/output of summary information for compilation units *)
func [] {
    let 
	TwoBits === prod { mem, thro : val Boolean };
	FnSumm === prod {
			code : val Short;
			size_or_argnum : val Short;
			numargs : val Short;
			args_summ : val (List [TwoBits])
		    };

	readw == func [val File; impure] val Short { extern "_getw" };
	writew == func [val Short; val File; impure] val Void {extern "_putw"};
	exit == func[n: val Short; impure] val Void { extern "_exit" };

	reverse == func [l : val (List [TwoBits])] val (List [TwoBits]) {
		    let l1 == (List [TwoBits])$New [];
			lres == (List [TwoBits])$New []
		    in
			l1 := l;
			lres := (List [TwoBits])$'';
			do not is_nil [l1] ==>
				lres := cons [head l1, lres];
				l1 := tail l1
			od;
			lres
		    ni
		}
    in
	extend {File}
	with SF {
	Open == func [name : val ChStr; read : val Boolean; impure] val SF {
		let f == File$open [name, (if read then "r" else "w" fi)]
		in
		    if f = File$Null [] then 
			write [File$stdout, "Failed to open "];
			write [File$stdout, name];write [File$stdout, "\n"];
			exit[1]
		    fi;
		    SF$In [f]
		ni
		};

	read_ct == func [sf : val SF; impure] val Short {
		readw [SF$Out [sf]]
		};

	read_name == func [sf : val SF; impure] val ChStr {
		let c == ChStr$New [];
		    res == ChStr$New [ChStr$'']
		in
		    c := readc [SF$Out [sf]];
		    do (c <> ChStr$In [0]) cand not [eof [SF$Out [sf]]] ==>
			res := res ^* c;
			c := readc [SF$Out [sf]]
		    od;
		    res
		ni
		};

	read_summ == func [sf : val SF; impure] val FnSumm {
		let f == SF$Out [sf];
		    code == readw [f];
		    soa == readw [f];
		    numargs == readw [f];
		    n == Short$New [numargs];
		    tb_list == (List [TwoBits])$New [];
		    readbool == func [f : val File; impure] val Boolean
				{readb [f] <> 0}
		in
		    tb_list := (List [TwoBits])$'';
		    do n > 0 ==>
			let m == readbool [f];
			    t == readbool [f]
			in
			    tb_list := cons [TwoBits$Mk [m,t], tb_list]
			ni;
			n -= 1
		    od;

		    FnSumm$Mk [code, soa, numargs, reverse [tb_list]]
		ni
		};

	discard_summ == func [sf : val SF; impure] val Void {
		let f == SF$Out [sf]
		in
		readw [f](*code*); readw [f](*size or argnum*);
		let n == Short$New [readw f]
		in
		    do n > 0 ==> (* read the boolean bytes *)
			readb [f]; readb [f];
			n -= 1
		    od
		ni
		ni
		};

	(* Read all summary information contained in the named file *)
	gather_summaries ==
		func [name : val ChStr;
		      labels : val LB; summary : val SU;
		      LB : type L {exists : func [val ChStr; val L; impure]
		      				 val Boolean};
		      SU : type S {append : func [val ChStr; val FnSumm; val S;
		         			  impure] val Void};
		      impure] val Void {
	    let sf == SF$Open [name, True]
	    in
		do not eof [sf] ==>
		    let fnname == SF$read_name [sf]
		    in
		      if not [eof [sf]] then
		      if exists [fnname, labels] then (* it's probably called
							in the current file, 
							and worth reading info*)
			append [fnname, SF$read_summ [sf], summary]
		      else SF$discard_summ [sf]
		      fi
		      fi
		    ni;
	        od;
	    SF$close [sf]
	    ni
	  };

	write_summ == func [name : val ChStr; fs : val FnSumm; sf : val SF;
							impure] {
		let f == SF$Out [sf]
		in
		write [f, name];
		writeb [f, 0]; (* terminating null *)
		writew [code fs, f];
		writew [size_or_argnum fs, f];
		writew [numargs fs, f];
		let tb_list == (List [TwoBits])$New [];
		    writebool == func [f : val File; b : val Boolean]
				{writeb [f, if b then 1 else 0 fi]}
		in  tb_list := args_summ fs;
		    do not is_nil [tb_list] ==>
			writebool [f, mem [head tb_list]];
			writebool [f, thro [head tb_list]];
			tb_list := tail tb_list
		    od
		ni
		ni
		}
   } (* with *)
  ni
}
