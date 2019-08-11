(*
 * destructive doubly-linked lists
 *)
# define NIL from_nil[0]
# define NEWNIL D$Out[D$create[from_nil[0]]]

# define DEBUG
# ifdef DEBUG
#  define debug(x) x
# else
#  define debug(x)
# endif

func[t: type T { put: func[val T]val T }] {
    let
	Elt == prod {
	    vl: val t;
	    next, last: val Ref[Relt];
	};

	Relt == union {
	    e: val Elt;
	    nil: val Short;
	};

    in use Ref[Relt], Relt in let
	dlist == extend { Ref[Relt] } with D {
	    create == func[x: val Relt] {
		let
		    v == Relt$New[];
		in
		    v := x;
		    D$In[v]
		ni
	    };
	    '' == func[] val D { D$create[NIL] };
	    is_nil == func[d: val D; impure] { is_nil[d^] };


	  (* copy a dlist *)
	    copy == func[x: val D; impure] val D {
		let
		    copyr == func[x: val Ref[Relt]; ls: val Relt; impure] val Relt {
			if is_nil[x^] then
			    NIL
			else
			    let
				newnode ==
				    Relt$from_e[Elt$Mk[
					vl[to_e[x^]],
					(Ref[Relt])$In[Relt$New[]],
					(Ref[Relt])$In[Relt$New[]]
				    ]];
			    in
				last[to_e[newnode]]^ := ls;
				next[to_e[newnode]]^ :=
				    copyr[next[to_e[x^]],newnode];
				newnode
			    ni
			fi
		    };
		    copyl == func[x: val Ref[Relt]; nx: val Relt; impure] val Relt {
			if is_nil[x^] then
			    NIL
			else
			    let
				newnode ==
				    Relt$from_e[Elt$Mk[
					vl[to_e[x^]],
					(Ref[Relt])$In[Relt$New[]],
					(Ref[Relt])$In[Relt$New[]]
				    ]];
			    in
				next[to_e[newnode]]^ := nx;
				last[to_e[newnode]]^ :=
				    copyl[last[to_e[x^]],newnode];
				newnode
			    ni
			fi
		    };
		in
		    if is_nil[x^] then D$create[from_nil[0]]
		    else
			let
			    newnode ==
				Relt$from_e[Elt$Mk[
				    vl[to_e[x^]],
				    (Ref[Relt])$In[Relt$New[]],
				    (Ref[Relt])$In[Relt$New[]]
				]];
			in
			    last[to_e[newnode]]^ :=
				copyl[last[to_e[x^]],newnode];
			    next[to_e[newnode]]^ :=
				copyr[next[to_e[x^]],newnode];
			    D$create[newnode]
			ni
		    fi
		ni
	    };

	  (* add x to right of cell d points to *)
	    addr == func[x: val t; d: val D; impure] val Void {
		if is_nil[d] then
		    d^ := from_e[Elt$Mk[x,NEWNIL,NEWNIL]];
		else
		    if is_nil[next[to_e[d^]]^] then
			let
			    nlst == D$Out[D$create[d^]]; (* new ptr to d^ *)
			in
			    next[to_e[d^]]^ :=
				from_e[Elt$Mk[x,NEWNIL,nlst]]
			ni
		    else
			let
			    nw == from_e[Elt$Mk[
					x,
					D$Out[D$create[next[to_e[d^]]^]],
					D$Out[D$create[d^]]
				  ]]
			in
			    last[to_e[next[to_e[d^]]^]]^ := nw;
			    next[to_e[d^]]^ := nw;
			ni
		    fi
		fi
	    };

	  (* add x to left of cell d points to *)
	    addl == func[x: val t; d: val D; impure] val Void {
		if is_nil[d] then
		    d^ := from_e[Elt$Mk[x,NEWNIL,NEWNIL]];
		else
		    if is_nil[last[to_e[d^]]^] then
			let
			    nnxt == D$Out[D$create[d^]]; (* new ptr to d^ *)
			in
			    last[to_e[d^]]^ :=
				from_e[Elt$Mk[x,nnxt,NEWNIL]]
			ni
		    else
			let
			    nw == from_e[Elt$Mk[
					x,
					D$Out[D$create[d^]],
					D$Out[D$create[last[to_e[d^]]^]]
				  ]]
			in
			    next[to_e[last[to_e[d^]]^]]^ := nw;
			    last[to_e[d^]]^ := nw;
			ni
		    fi
		fi
	    };

	    put_one == func[d: val D; impure] val Void {
		if is_nil[d^] then put "nil"
		else
		    put "("; put[vl[to_e[d^]]]; put")";
		fi
	    };

	    put_one == func[d: var D; impure] val Void {
		if is_nil[d^] then put "nil"
		else
		    put "("; put[vl[to_e[d^]]]; put")";
		fi
	    };

	    put == func[d: val D; impure] val Void {
		let
		    cur == D$New[];
		in
		    cur := d;
		    do ( if is_nil[cur^] then False else True fi) ==>
			let
			    v == vl[to_e[cur^]];
			in
			    put[v]; put ","
			ni;
			cur := D$In[next[to_e[cur^]]];
		    od
		ni
	    };
	    putl == func[d: val D; impure] val Void {
		let
		    cur == D$New[];
		in
		    cur := d;
		    do ( if is_nil[cur^] then False else True fi) ==>
			let
			    v == vl[to_e[cur^]];
			in
			    put[v]; put ","
			ni;
			cur := D$In[last[to_e[cur^]]];
		    od
		ni
	    };

	  (* delete the object d^ from the list it's in *)
	    delete == func[d: var D; impure] val Void {
		if is_nil[d^] then put "cannot delete nil object\n"
		else
		    let
			dobj == to_e[d^];
			ls == last[dobj];
			nx == next[dobj];
		    in
			if is_nil[ls^] then
			    if is_nil[nx^] then
			      (* nothing to delete - not connected *)
				0
			    else
			      (* is on left end of a list *)
				last[to_e[nx^]]^ := NIL;
			    fi
			else
			    if is_nil[nx^] then
			      (* is on right end of a list *)
				next[to_e[ls^]]^ := NIL;
			    else
			      (* is in the middle of a list *)
				last[to_e[nx^]]^ := ls^;
				next[to_e[ls^]]^ := nx^;
			    fi
			fi
		    ni
		fi
	    };


	    last == func[d: var D; impure] val D { last[V[d]] };
	    last == func[d: val D; impure] val D {
		put "last["; put_one[d]; put "]\n";
		if is_nil[d^] then
		    put "dlist: tried to do last on nil list\n";
		    D$''
		else
		    let res == D$In[last[to_e[d^]]]
		    in
			put "last returning "; put_one[res]; put "\n"; res
		    ni
		fi
	    };
	    next == func[d: var D; impure] val D { next[V[d]] };
	    next == func[d: val D; impure] val D {
		if is_nil[d^] then
		    put "dlist: tried to do next on nil list\n";
		    D$''
		else
		    D$In[next[to_e[d^]]]
		fi
	    };
	    head == func[d: var D; impure] val t {
		if is_nil[d^] then
		    put "dlist: tried to take head of empty dlist\n";
		fi;
		vl[to_e[d^]]
	    };
	};
    in
	dlist
	hide {
	    In<<func[val Ref[Relt]]val dlist>>;
	    Out<<func[val dlist]val Ref[Relt]>>;
	    In<<func[var Relt]val dlist>>;
	    Out<<func[val dlist; impure]var Relt>>;
	    ^; create;
	}
    ni ni ni
}


