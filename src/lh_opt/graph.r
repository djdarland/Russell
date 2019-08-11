# include "defs.h"
# define ABS(I) (if I < 0 then - I else I fi)
# define NULLNODE (-1)

(* The MEMNODE is both source and sink of edges, unlike the other standard
	nodes which are only sources; when an edge goes TO MEM it indicates
	an argument gets squirrelled away *)

func [graph_size, num_fns : val Short; Vflag : val Boolean] {
let

	put_list == func [l : val List [Short]] val Void {
		if not is_nil [l] then 
		  put [head l]; put [" "]; put_list [tail l]
		fi
	    };

    ResultNode == extend {ChStr}; (* Fn name *)
    ArgNode == prod {
		    fn : val ChStr;
		    fn_node : val Short; (* the node representing the result *)
		    num : val Short
		};
    AllocNode == extend {Short}; (* size/atomicity *)  
			
    NodeInfo == union {
		    result : val ResultNode;
		    arg : val ArgNode;
		    alloc : val AllocNode;
		    mem : val Boolean;
		    none : val Boolean;
		    static : val Boolean;
		    stack : val Boolean
		    };
			
    Nodes == Array [graph_size, NodeInfo];

    Edges == Array [graph_size, (List [Short])];

    DForder == func [edges : val Edges; size : val Short; impure]
							val (List [Short]) {
		let mark == (Array [size, Boolean])$New [];
		    list == (List [Short])$New [];
		    root == Short$New [];
		    n == Short$New [];
		    search == func [n : val Short; impure] val Void {
			    let es == (List [Short])$New []
			    in
				mark.n := True;
				es := edges.n;
				do not is_nil [es] ==>
					if not [mark.(head es)] then
						search [head es]
					fi;
					es := tail es
				od
			    ni;
			    list := cons [n, list]
			}
		in
		    n := 0;
		    do n < size ==>
			mark.n := False;
			n += 1
		    od;

		    list := (List [Short])$'';
		    root := 0;
		    (* while there are unmarked nodes search the next one *)
		    do (* put next unmarked node in root *)
			((do root < size cand mark.root ==> root += 1 od);
			root < size)

		    ==>
				search [root]
		    od;

		    list (* returned *)
		ni
	};

    LocTable === (Array [500, Short]);(* gives node # for representative
						of the size of alloc, or the
						result node for MAYBEALC fns *)

    Summary === extern {"summary"} [num_fns, Vflag];

    NodeTable == extern {"hash"} [50, Short, Short, NULLNODE];
    		(* keys are loc sizes/atomicity - i.e. +ve or -ve *)
		(* values are node numbers *)

	(* these next two are really only needed inside flow function, 
	    except for type arg to flow, which references FnSumm *)
	TwoBits === prod { mem, thro : val Boolean };
	FnSumm === prod {
		code : val Short;
		size_or_argnum : val Short;
		numargs : val Short;
		args_summ : val (List [TwoBits])
	    };

    (* the NodeTable and nextnode are only needed during the building stages *)
    Graph == prod {
		nodes : val Ref Nodes;
		preds : val Ref Edges;
		succs : val Ref Edges;
		sizednodes : val Ref NodeTable;
		nextnode : val Ref Short;
		firstalloc : val Ref Short
	      }

	with G {
	Init == func [] val G {
		let n == Nodes$New [];
		    p == Edges$New [];
		    s == Edges$New [];
		    sa == NodeTable$Init [];
		    next == Short$New [AVAIL_NODE];
		    firstalloc == Short$New [next]
		in
		    n.GENERICNODE := NodeInfo$from_alloc [AllocNode$In 0];(* 0 => unknown size/ atomicity *)
		    n.MEMNODE := NodeInfo$from_mem [True];
		    n.NONENODE := NodeInfo$from_mem [True];
		    n.STATICNODE := NodeInfo$from_static [True];
		    n.STACKNODE := NodeInfo$from_stack [True];

		    G$Mk [(Ref Nodes)$In [n], (Ref Edges)$In [p], 
			  (Ref Edges)$In [s],
			  (Ref NodeTable)$In [sa], (Ref Short)$In [next],
			  (Ref Short)$In [firstalloc] ]

		ni
	    };

	function_nodes == func [name : val ChStr;
				(*index,*) num_args : val Short;
					graph : val G; 
			base : var Short;
							impure] val Short {

		let n == (nodes [graph])^;
		    next == V[(nextnode [graph])^];
		    a == Short$New [1]
		in
		    n.next := NodeInfo$from_result [ResultNode$In [name]];
		    do a <= num_args ==>
			n.(next + a) := NodeInfo$from_arg [
						ArgNode$Mk [name, next, a]];
OPEN
put "\n";put (next + a);put " th node is argnode";
CLOSE
			a += 1
		    od;
		    (nextnode [graph])^ := next + num_args + 1;
		    (firstalloc [graph])^ := (nextnode [graph])^;
			(* the value left here after all calls to this
					function is the desired value *)

OPEN
put "\n";put next;put "th node is the result node for ";put name;put "\n";
CLOSE

base :=		    next (* return the first position used here *)
		ni

	    };

	alloc_node == func [size : val Short; graph : val G; impure] 
							     val Short {
				(* includes check for already existing *)
			    if size = 0 then
				GENERICNODE (* return node for 0-sized objs *)
			    else 
				let abssize == ABS(size);
				    table == (sizednodes [graph])^;
				    node == lookup [abssize, size, table]
				in
				   if (node <> NULLNODE) then
					node (* return node for allocs of this size*)
				   else
				     (* create a node for this size of alloc *)
				     let next == V[(nextnode [graph])^];
					 n == (nodes [graph])^
				     in
					insert [abssize, size, next, table];
					n.next := NodeInfo$from_alloc [
						AllocNode$In [size]];
OPEN
put next;put "th node is allocnode for size ";put size;put "\n";
CLOSE

					(nextnode [graph])^ := next + 1;
					next (* return *)
				     ni
				   fi (* there already ?*)
				ni
			    fi (* 0-sized ? *)
		};

	link == func [n, m : val Short; graph : val G; impure] val Void {

			let ss_of_n == ((succs [graph])^).n;
			    ps_of_m == ((preds [graph])^).m
			in
				ss_of_n := cons [m, ss_of_n];
				ps_of_m := cons [n, ps_of_m]
			ni
	    };

	args_edges == func [reached_node : val Short;
			    locs : val LocList;
			    loctable : val LocTable;
			    graph : val G; 
			    LocList : type L
				{loc_map : func [val L;
					         val LocTable; 
				     		 func [val Short;impure] 
								val Void;
						 impure]
					    			val Void;
				 print : func [val L; impure] val Void };
			    impure] val Void {

			let n == (nodes [graph])^
			in
OPEN
put "Locs that reach the node ";
if reached_node = MEMNODE then put "MEM : "
else put reached_node; put " : "  fi;
LocList$print [locs];
put "Of those the ARGS are ";
CLOSE
				LocList$loc_map [locs, loctable,
					func [l : val Short; impure] val Void {
					    if is_arg [n.l] then
OPEN
put [l];put " ";
CLOSE
						link [l, reached_node, graph];
					    fi
					}];
OPEN
put "\n"
CLOSE
			ni
		};

	result_edges == func [resultnode : val Short;
			      locs : val LocList;
			      loctable : val LocTable;
			      graph : val G;
			      LocList : type L
				{loc_map : func [val L;
					         val LocTable; 
				     		 func [val Short;impure] 
								val Void;
						 impure]
							       val Void };
			      impure] val Void {

OPEN
put "\nNodes that reach the resultnode ";
put resultnode;put ": ";
CLOSE

(* I am not allowed to claim that a fn is an allocator unless any object it
   might return is not on memory's list. Once I put an edge from a loc's node
   to the result node I am assuming that if that is the only node that reaches
   then the fn is an allocator !!!!  I really have to replace any locs that
   are in mem by nonenode *)
			    LocList$loc_map [locs, loctable,
				(* function on nodes *)
				func [l : val Short; impure] val Void {

				    link [l, resultnode, graph];
OPEN
put l; (*  **************** *)
put " "
CLOSE
				}];
OPEN
put " (linked to the resultnode)\n"
CLOSE
		};

	print == func [graph : val G; impure] val Void {

		put "\n**** The Nodes of the Graph ****\n";
		let n == Short$New [0];
		    numnodes == (nextnode [graph])^;
		    ns == (nodes [graph])^;
		    ss == (succs [graph])^
		in
		    do n < numnodes ==>
			put n;
			let node == ns.n;
			    edges == (List [Short])$New []
			in
			    if is_result [node] ==>
				put " RESULT for ";
				put [to_result node]

			    # is_arg [node] ==>
				put " ARG # ";
				put [num [to_arg node]];
				put " for ";
				put [fn [to_arg node]]

			    # is_alloc [node] ==>
				put " ALLOC with size ";
				put [to_alloc node]

			    # else ==>
				put " MEM/NONE/STATIC/STACK"

			    fi;	

			    edges := ss.n;
			    put " Succs :";
			    do not is_nil [edges] ==>
				put [head edges]; put " ";
				edges := tail edges
			    od;

			    put "\n"
			ni;
			n += 1
		    od
		ni
	    };

	flow == func [graph : val G; summary_outfile : val SI;
			SI : type S {
				   write_summ : func [val ChStr;
					val FnSumm;val S;impure] val Void};
							impure] val Summary {

	    let numnodes ==  V[(nextnode [graph])^];
		fa == V [(firstalloc [graph])^];

		nnns == V [(nodes [graph])^];
		RR == extern {"resultsummary"} [nnns, Nodes, NodeInfo];
		RRArray == Array [numnodes, RR];

		TwoBitsArray == Array [fa, TwoBits];

	    whither_args == func [graph : val G; impure] val TwoBitsArray {

		(* arg info - needs a backward pass from MEM and result
		   nodes; need to carry lists of result nodes (and MEM) node
		   encountered on the path back. Apply involves adding the
		   current node to the list, if it is a result or mem node.
		   If the list at an argnode changes on a pass, have to repeat.
		   Start with empty lists at all nodes. Meet is a union op.

		   Decide whether to store values before or after apply ??
			Once the flow is over, look at argnodes and see if
		   either MEM or the corresponding result node is on the list;
		   Set the bits in the summary accordingly. *)

		let ns == V[(nodes [graph])^];
		    AA == extern {"argsummary"} [ns, Nodes, NodeInfo];
		    Args == (Array [numnodes, AA]);
		    args == Args$New [];
		    i == Short$New [0]
		in

		   (* init the nodes to top (empty list) *)
		   do i < numnodes ==>
		   (*this should put MEM and resultnodes onto their own lists *)
			args.i := AA$apply [i, AA$top []]; 
			(* if is_result or is_mem i then AA$create [i]
				else AA$top [] fi*)
			i += 1
		   od;
		let change == Boolean$New [True];
		    nodelist == (List [Short])$New [];
		    DFlist == DForder [(preds [graph])^, numnodes]
		in
		    (* Iterate until no change *)
		    do change ==>
			change := False;
			nodelist := DFlist;
			do not is_nil [nodelist] ==>

			    let node == head nodelist;
				tmp == AA$New [];
				ss == (List [Short])$New []

			    in
				tmp := AA$top;
				(* meet over all succs of node *)
				ss := ((succs [graph])^).node;
				do not is_nil [ss]  ==>
				    tmp := AA$meet [args.(head ss), tmp];
				    ss := tail ss
				od;
 				(* apply *)
				tmp := AA$apply [node, tmp];
				(* if is_result or is_mem node then 
					AA$meet [AA$create [node], tmp]
				    else tmp fi *)

				(* check for change *)
				if (not change) then
				  if (not AA$equal [tmp, args.node])
				  then change := True
				  fi
				fi;
				(* update the value *)
				args.node := tmp

			    ni; (* node, tmp *)
			    nodelist := tail nodelist
			od (* nodelist *)
		    od; (* change *)
		ni; (* change *)
		(* Now the lists in the arg positions need to be searched
			for MEM or the corresponding result node *)

		let summ == TwoBitsArray$New []
		in
		i := 0;
		do i < fa ==>
		    if is_arg [ns.i] then
			let argnode == to_arg [ns.i];
			    mem == AA$member [MEMNODE, args.i];
			    thro == AA$member [fn_node [argnode], args.i]
			in
			(* in fact if it reaches mem then I don't care
						if it reaches its result *)
				summ.i := TwoBits$Mk [mem, thro];
OPEN
if mem or thro then
	put i;put ": ";
	put "Arg #";put [num [argnode]];
	put " of ";
	put [fn [argnode]]; 
	put " reaches";
	if mem then put " MEM, " fi;
	if thro then put " the result" fi;
	put "\n"
fi (* mem or thro *)
CLOSE
			ni;
		    fi; (* is_arg *)
		    i += 1
		od;

			summ (* returned *)

		ni (* summ *)
		
		ni (* AA, args *)

	      };

	    whence_results == func [graph : val G; impure] val RRArray {

		(* result info - forward pass from the standard nodes, and
		   allocnodes. Start with top at all nodes. 
		   The values are just single things - code and size. The meet
		   operation is complicated. The Apply is only interesting at 
		   the standard and allocnodes.
			Once the flow is over the values I want are in the
		   result nodes.		*)

		let results == RRArray$New []
		in

		(* init the nodes to top *)
		let i == Short$New [0]
		in
		   do i < numnodes ==>
		   (* this should set all but resultnodes to themselves,
				resultnodes to top *)
			results.i := RR$apply [i, RR$top []]; 
			i += 1
		   od
		ni;
		
		let change == Boolean$New [True];
		    nodelist == (List [Short])$New [];
		    DFlist == DForder [(succs [graph])^, numnodes]
		in
		    (* Iterate until no change *)
		    do change ==>
			change := False;
			nodelist := DFlist;
			do not is_nil [nodelist] ==>

			    let node == head nodelist;
				tmp == RR$New [];
				ps == (List [Short])$New []
			    in
				tmp := RR$top [];
				(* meet over all preds of node *)
				ps := ((preds [graph])^).node;
				do not is_nil [ps]  ==>
				    tmp := RR$meet [results.(head ps), tmp];
				    ps := tail ps
				od;
				(* apply *)
				tmp := RR$apply [node, tmp];
				(* check for change *)
				if (not change) then
				  if (not RR$equal [tmp, results.node])
				  then change := True
				  fi
				fi;
				(* update the value *)
				results.node := tmp

			    ni; (* node, tmp *)
			    nodelist := tail nodelist
			od (* nodelist *)
		    od; (* change *)

		ni; (* change *)

(* the values now in the resultnode positions are the 
	desired values *)
OPEN
let i == Short$New [AVAIL_NODE];
in
    do i < fa ==>
	let node == ((nodes [graph])^).i
	in
	    if is_result [node] then
		put [Out [to_result [node]]];
		put "'s result summary node is : ";
		RR$put [results.i]; put "\n"
	    fi
	ni;
	i += 1
    od
ni;
CLOSE

			results (* returned *)
		ni (* results *)
	      }; (* whence_results *)


	    (**********************************)
	    argsumm == whither_args [graph];
	    resultsumm == whence_results [graph];
	    (***********************************)

	    ns == (nodes [graph])^;

	    summary == Summary$Init [];

	    i == Short$New [AVAIL_NODE]

	    in
		(* now I can go through the two arrays function by function
			and produce a full summary for the function *)
		do i < fa ==> (* or count by num_fns *)
		    let name == Out [to_result [ns.i]];
			n == Out [resultsumm.i];
				 (* n is THE node that reaches the result *)
			code == if n = NONENODE   ==> ALCNIL
				#  n = STATICNODE  ==> ALCSTAT
				#  n = STACKNODE   ==> ALCSTACK
				#  n = TOP         ==> ALCNIL
				(* no path from ALLOC or STD node to the 
					result; seems equiv to NONE *)
				# is_alloc [ns.n] ==> ALLOC
				# is_arg [ns.n] cand
					(fn_node [to_arg [ns.n]] = i) 
					(* it is an arg of this function *)
					cand (num [to_arg [ns.n]] 
							< MAXWATCHARGS)
						  ==> ASSIGN
				# else 		  ==> ANY
		(* should I produce UNSAFE in some cases ?? *)
				fi;

			sizeorargnum == if code = ALLOC then 
						Out [to_alloc [ns.n]]
					elsif code = ASSIGN then
						num [to_arg [ns.n]]
					else 0
					fi;
			arglist == (List [TwoBits])$New [];
			num_args == Short$New [0]
		    in
			i += 1; (* for the result node *)
			arglist := (List [TwoBits])$'';
			do (i < fa) cand (is_arg [ns.i]) ==>
			    num_args += 1;
			    arglist := cons [argsumm.i, arglist];
			    (* note that the arglist is in reverse order -
				the head is the num_args'th one *)
			    i += 1
			od;

			let fs == FnSumm$Mk [code, sizeorargnum, 
						 num_args,arglist]
			in
(*at the moment append also prints the info *)
			    append [name, fs, summary];
			    write_summ [name, fs, summary_outfile]
			ni
		    ni
		od;

		summary (* returned *)
	    ni
	    } (* flow *)

	} (* with *)

in 
		Graph 

export {Init; function_nodes; alloc_node;
			args_edges; result_edges; print; flow }
ni}
