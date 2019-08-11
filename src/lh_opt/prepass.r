(*
Main routine.  Invoked with args:

flags input_code output_code [input_summary [output_summary]]

The flags argument is always present.  It is either - or -verbose.
The two code files use RIC (Russell Intermediate Code).  The input summary file
is the concatenation of summary information for all referenced compilation
units.
*)
# include "defs.h"
# include "../root_path.h"
# define ONLYINTRA Boolean$False
(* if ONLYINTRA then ----- this was never completed ------
		classify does not produce any MAYBEALCs or ARGCATs
		firstpass is left out completely
		lastpass doesn't bother trying to refine all ARGCATs
		and MAYBEALCs
		graph flow is not called
		(any amount of graph stuff can be left out)
*)
# define MAX(x, y) (if (x >= y) then x else y fi)
# define QuadList (List [Short])
# define BBList (List [Short])

let
	put_list == func [l : val List [Short]] val Void {
		if not is_nil [l] then 
		  put [head l]; put [" "]; put_list [tail l]
		fi
	    };

	(* assists reverse and concat by providing an accumulating reverse *)
	rev == func [l, accum : val List [t]; t : type {}] val List [t] {
			if is_nil l then accum
			else
			    rev [tail l, cons [head l, accum], t]
			fi
		};
	
	reverse == func [l : val List [t]; t : type {}] val List [t] {
			rev [l, (List [t])$'']
		};

	(* concatrev reverses the first list and concats onto second list *)
	concatrev == func [lst1, lst2 : val List [t]; t : type {}] 
							      val List [t] {
			rev [lst1, lst2]
		};

	(* assumes non-empty list argument *)
	last == func [lst : val List [Short]] val Short {
			if is_nil [tail lst] then [head lst]
			else last [tail lst]
			fi
		};

	remove == func [x : val Short; lst : val QuadList] val QuadList {
			if is_nil [lst] then lst
			elsif (head lst) = x then (tail lst)
			else cons [head lst, remove [x, tail lst]]
			fi
		};

  Arg1type == union A1 {
		arg : val Short;
		str : val ChStr;
		};

  (* one of these records is associated with every quad in the input *)
  Basics == prod B {
	    opc : val Short;
	    arg1 : val Arg1type;
	    arg2, arg3 : val Short;
	    dull : val Boolean;
	};

  MAX_QUAD == Short$20000;
  BA == Array [MAX_QUAD, Basics];
  basics == BA$New [];

  q == Short$New[0];

  exit == func[n: val Short; impure] val Void { extern "_exit" };
  popen == func [cmd, mode : val ChStr; impure] val File { extern "_popen"};
  pclose == func [val File; impure] val Void { extern "_pclose"};
  atoi == func [val ChStr;impure] val Short { extern "_atoi"};
  uninteresting == func [opc : val Short;impure] val Boolean {
	  (* things I don't ever need to see after reading in the quads	*)
	  (* NULLCAT is for things I don't need to see after all 
				the bbs have been set up *)
	    (opc = BSF) (* I think ?? *) cor
	    (opc = LBL) cor (opc = EXT) cor (opc = LBR) cor
	    (opc = DDT) cor (opc = FDT) cor
	    ((opc >= TFB) cand (opc <= ADT)) cor
	    ((opc >= ADI) cand (opc <= ABI)) cor (* integer ops *)
	    ((opc >= AND) cand (opc <= NOT)) cor (* boolean ops *)
	    (opc = ALS) cor (opc = LBA)

	};

  get_file == func [a : val Short; c : val ChStr; impure] val File {
		let file_name == ChStr$New [] in
			file_name := ChStr$"";
			if argc > a 
			then file_name := argv [a]
			fi;
			let file == File$open [file_name, c]
			in
			    if file = File$Null then
				write[File$stderr, "Can't open "];
				write[File$stderr, file_name];
				write[File$stderr, "\n"];
				write[File$stderr,
				      "Usage: src RIC_file out_RIC_file "
				       ^* "info_in info_out\n"];   
				exit[1];
			    fi;
			    file (* returned *)
			ni (* file *)
		 ni};

  in_file == let rawfile == get_file [2, "r"];
		 delab_cmd == ROOT_PATH ^* "/src/delab/delab " ^* argv [2]
	     in
		close [rawfile];
		let f == popen [delab_cmd, "r"]
		in if f = File$Null [] then write [File$stderr,
					"Problem opening 'delab' pipe\n"];
					exit [1]
		   fi;
		   f
		ni
	     ni;
	     
  Vflag == argv[1] = "-verbose";

  LBLS == extern {"labels"} [in_file, MAX_QUAD];
  labels == LBLS$Init [];

  precedingLBA == ChStr$New [];
  
  max_calls === 1000; (* ideally this will become a per function const *)
  Names === Array [max_calls, ChStr];
  fn_names == Names$New []; (* map for each function called in this file;
				for ARGs to find their function *)
  fn_callquads == (Array [max_calls, Short])$New [];
		    (* map of quadnums for calls corresponding to call nums *)
  next_call_num == Short$New [0];   (* for the maps of calls to fn names and
					to call quads *)

  FNprod == prod { name : val ChStr;
		   quad : val Short;
		   bsf : val Boolean;
		};
  fn_list == (List [FNprod])$New []; (* list of fns defined in the file;
		has to be made into textual order at a certain point for
		next_fst_qd arg to rsort to be correct *)
			(* this data is later incorporated into an array *)
  fn_count == Short$New [0]; (* essentially the length of the fn_list *)

  numcalls == Short$New [0] (* used only to choose SummaryIn table size -
				counts all CLCs and CLLs *)

(* all the above is needed on the first read of the quads *)

in

fn_list := (List [FNprod])$'';


do (not [ eof [in_file]]) cand (q < MAX_QUAD) ==>
	(* read the next quad, decide if dull, store 
	   also set up list of fns defined and array of fns called - these are
	   needed before classify can do it's work 
	  read.i also increments q if it successfully read and stored a quad *)

(*****************)
# include "read.i"
(*****************)

od;

pclose [in_file];

if q = MAX_QUAD then put ["\nInput file too big\n"]; exit [1] fi;

fn_list := reverse fn_list; (* to give textual order *)


(* now all the quads are in the array, and I know the dest of all branches
   and I have a list of the functions defined, in textual order *)

(* now I have to classify and sort out args *)
let (* declare stuff that covers all functions *)


    num_quads == V[q];	(* i.e. 0..(num_quads-1) array entries *)

    num_fns == V[fn_count];

  (* this record is used for info about "interesting" quads *)

    More == prod M {
	    cat : val Short;		(* category of the operation *)
	    x_var : val Short;		(* use depends on 'cat'. Usually it's
					   the var on the left of assignmt *)
	    other : val Ref Short;	(* depends on 'cat' *)

			(* other holds allocator size till DFsearch, after which
			   it holds alloc_num, and size goes in the component's
								    loctable *)

	    size : val Ref Short; (* only for allocators - shouldn't really
					use a whole field, but the previous 
					scheme, which used other for size, until
					the size went into the loctable, didn't
					work once I had interproc 
							(two big phases) *)
	    extra : val Short	(* last_var for hinted STIs !! *)
				(* call_index for ARGs and CLL and CLC *)
	};

    MA == Array [num_quads, More];
    more == MA$New [];

    extra == Short$New [];
    last_var == Short$New [FIRST_AVAIL_LOC - 1];
				(* records the highest var# seen so far *)

    hinted == Boolean$New [False]; (* true when hints apply to the next quad *)
    hint_type == Short$New [];
    hint_info == Short$New [];
    PTregister == Short$New [0] (* holds the special register number of 
					an arg which is hinted PT, so that
					it is seen when the call is to be
					classified *)

in   (* classify *)


(* process the summary files specified in the command line, entering in a 
   summary table
   any summary that refers to a function called in this file - look for it
   in the labels table.
	I think that these only need to be seen by classify *)
let ncalls == V [numcalls];
    SummaryIn == extern {"summary"} [ncalls / 10 + 5, Vflag];
    inputsummary == SummaryIn$Init [];
    SI == extern {"summaryio"}[];
in
    (* argv[4] is <dataflow input> arg *)
    if argc > 4 then
		SI$gather_summaries [argv 4, labels, inputsummary]
    fi;


    q := 0;
    next_call_num := 0;

    do q < num_quads ==>

    (* classify all non-dull quads - produce more entries for them *)
    (* use list of defined functions *)
    (* use inputsummary to help classify ARGs and CALLs to interfile fns *)
# include "classify.i"

	q += 1

    od;

ni; (* ncalls, SummaryIn *)

(* I now want to define an array for the functions (size = fn_count), with
   fields for name, DF_order of basic blocks, a map from basic blocks of this
   function to the quad number or however I want to represent bbs, and all 
   that other stuff.
	Then I need to gather all this information *)

let
    BB == prod B {
		quads : val QuadList;
		last : val Short;
		vlist : val List [Short]
	};

    max_bbs_per_fn == (num_quads / 6) + 100;
    BBArray == (Array [max_bbs_per_fn, BB]);
    bb == BBArray$New [];

    FNRec == prod {
		short : val Boolean; (* True for BSF functions *)
		quad : val Short; (* the quad that is the BFN/BSF *)
		name : val ChStr;
		numargs : val Short;
		basenode : val Short; (* Result node #; arg nodes follow *)
		bbs : val BBArray; (* in textual order with extra info *)
		bbsuccs : val (Array [max_bbs_per_fn, BBList]);
		bbpreds : val (Array [max_bbs_per_fn, BBList]);
		blist : val BBList; (* DF ordered list of the bbs *)
		rtn_bb : val Short; (* the bb that contains the RTN *)
		length : val Short; (* #bbs in the function *)
		};

    fn_array == (Array [num_fns, FNRec])$New [];

    Graph === extern {"graph"} [num_fns * 20, num_fns, Vflag];
    graph == Graph$Init [];

    next_interesting == func [q : val Short; impure] val Short {
	(* returns the number of the next interesting quad starting at q *)
		let r == Short$New [q]
		in
		    do  (r < num_quads) cand (dull [basics.r])
			==>
				r += 1
		    od;

		    (* now either r is interesting or r is num_quads *)
		    r
		ni
		};

    fn_index == Short$New [] (* for counting out the functions *)

in


OPEN (* debugging stuff *)
	let num_quads == V [q];
	    qq == Short$New [0]
	in
	    do qq < num_quads   ==>
		if (opc [basics.qq] = ARG)  ==>
			put ["ARG	"];
			if not dull [basics.qq] then
			   let m == more.qq in
				   if (cat [m] = ARGCAT) then
					put ["ARGCAT	"];
					put [x_var [m]];put "	";
					put [fn_names.(extra [m])];
					put [fn_callquads.(extra [m])]
				   else put [cat m];put " ????"
				   fi
			   ni
			else put ["NPed => dull"]
			fi;
			put "\n"

		#  (opc [basics.qq] = CLL) cor (opc [basics.qq] = CLC) cor
			(opc [basics.qq] = CLI) 		==>
		    let b == basics.qq;m == more.qq
		    in
			if (opc [b] = CLL) then put "CLL"
			elsif  (opc [b] = CLC) then put "CLC"
       			else   put "CLI"
			fi;
			if (cat [m] = MAYBEALC) then
				put "	MAYBEALC	";
				put [fn_names.(extra [m])]
			else put [cat [m]]
			fi;
			put "\n"
		    ni
		# else  ==> Null []
		fi;
		qq += 1
	    od
	ni; (* num_quads, qq *)

	let l == (List [FNprod])$New []
	in l := fn_list;
	   do not is_nil [l] ==>
		put [quad [head l]];
		put [name [head l]];
		put "first opc is ";put [opc [basics.(quad [head l])]];
		put " Short? ";put [bsf [head l]];
		put "\n";
		l := tail l
	   od
	ni;
CLOSE

(*****************************************************)


q := next_interesting [0];


fn_index := 0; (* fn_list is in textual 0,1,... order now *)
do fn_index < num_fns      ==>

    let fn_name == name [head fn_list];
	fst_qd == quad [head fn_list];
	short == bsf [head fn_list];

	next_fst_qd == if not is_nil [tail fn_list]
			then quad [head [tail fn_list]]
			else num_quads
		       fi;
	max_arg_num == Short$New [0];

	Rsort == (extern {"rsort"} [next_fst_qd]);
	leaders == Rsort$New [];
		(* uses an array of size next_fst_qd; next will return that
			value if it doesn't find another leader *)

	b == Short$New [];		(* indexes basic blocks *)

	BHash == (extern {"hash"}[(max_bbs_per_fn / 4) + 2, Short, Short, -1]);
	bbforldrs == BHash$Init [];
	(* used to find the bb# corresponding to the dest. of branches
	       when trying to link up the control flow;
	   computed when basic blocks are formed *)

    in
	(* find the leaders in this function *)
	(* since you are looking at all opcs, also find the highest # arg *)
	q := next_interesting [fst_qd (*+ 1*)];
	add [q, leaders];
	do (opc [basics.q]) <> RTN     ==>
	  let op == opc [basics.q]
	  in
	    if op = BR   ==> 
		    let dest ==	next_interesting[q_for_lbl_at [q, labels]]
		    in
			    add [dest, leaders]; (* dest of a branch *)
			    q := next_interesting [q + 1];

			    (* have to make following quad be a leader so it
				doesn't become part of this basic block;
				if it is dead code it will be discarded after
				the links have been made *)
			    add [q, leaders]
		    ni
	    #  (op = BRT) or
	       (op = BRF) ==>
		    let dest == next_interesting [q_for_lbl_at [q, labels]];
			follow == next_interesting [q + 1]
		    in
			add [follow, leaders]; (* follows a branch *)
			add [dest, leaders]; (* dest of branch *)
			q := follow
		    ni
	    #  (op = GAR) ==>
		    max_arg_num := MAX (max_arg_num, to_arg [arg1 [basics.q]]);
		    q := next_interesting [q + 1]
	    #  else     ==>	q := next_interesting [q + 1]
	    fi
	  ni
	
	od; (* loop through to end of function, finding leaders *)

(* end of finding leaders for this function *)
OPEN
	DEBUG("\nfinished finding leaders for fn #");put fn_index;
CLOSE

(****** Now build basic blocks *************)

(******* This is where I can build approx active vars lists - all vars active
	in each basic block. The resulting list becomes vlist for the bb *)
(*******  This is where I can work out consts within basic blocks - for each
	leader, set all live vars to not-const. Then for LDNs set value, and
	for MOVs copy values, and for ALH/ALA put the value, if const, into
	the other field *)

	let  (* declare things used only in the building process *)
	  last_in_fn == V[q]; (* RTN quad *)
	  l == Short$New [];
	  n == Short$New [];

	  qlist == QuadList $New [];
	  last_in_bb == Short$New [];

	  (* using max_var for all functions is a bit wasteful of space !! *)
	  max_var == last_var + 1;
	  VI == extern {"vars"} [max_var];
	      (* the following code does depend a little on the impl. of VI *)
	      (* e.g. for MOV if arg1 value is not a constant the operation
		makes arg2 not constant *)
	      (* because -1 represents NOTCONST, I CAN'T store -ve consts *)

	  (* VI$Init is not very expensive *)

	  vars == VI$Init [];

	in
	  l := next [fst_qd - 1, leaders]; (* ???????????????????????? *)
	  b := 0;		(* first basic block in this function *)

	  do l <= last_in_fn ==>

OPEN
	DEBUG("\nBEGIN bb whose leader is ");put [l];
	put "; opc is ";put [opc [basics.l]];
	put "; cat is ";put [cat [more.l]];
	DEBUG("\n");
CLOSE


	    insert [l, l, b, bbforldrs];	(* b is l's basic block *)

	    reset [vars];

	    qlist := QuadList $'';

	    (* for interesting quads l to before next leader (i.e. this bb) *)
	    n := next [l, leaders];
	    q := l;

	    (* last_in_bb doesn't need to be inited since this loop always 
							    iterates once ?? *)
	    do q < n ==>

		(* maintain active vars info and constants info *)

# include "varstuff.i"

		if cat [more.q] <> NULLCAT then 
				qlist := cons [q, qlist]
		fi; (* else no need to do all the apply stuff to them *)
		last_in_bb := q; 
		      (* the value remaining after this loop is the one used *)
		q := next_interesting [q+1]
	    od;

	    bb.b := BB$Mk [reverse qlist, last_in_bb, vs_out [vars]];
OPEN
	DEBUG("\nbb ");put [b];
	if is_nil [quads [bb.b]] then
		put " empty"
	else
		put " first quad :";put [head [quads [bb.b]]];
		put " first opcode : ";put [opc [basics.(head [quads [bb.b]])]];
	fi;
	put "\n"; 
	print [vars];
CLOSE

	    b += 1;
	    l := n	    
	  od; (* go to next leader/ basic block *)

	ni;(* last_in_fn, l, n, qlist, ... VI, vars *)

(* end of building basic blocks for this function -- bb is ready to store *)

OPEN
	DEBUG("\nFinished building basic blocks");
CLOSE

(* Now start linking preds and succs *)
(* bb 0 is the root for each function - bbs are numbered from 0 for each fn *)


      let  (* declare things needed when this function's info is stored *)
	    n_bbs == V[b];		(* 0 .. n_bbs - 1 *)
	    rtn_bb == Short$New [];
	    bbsuccs == (Array [max_bbs_per_fn, BBList])$New [];
	    bbpreds == (Array [max_bbs_per_fn, BBList])$New [];
	    blist == BBList $New [];

      in
	    (* initialise the bbsuccs and bbpreds *)
	    let nil == QuadList $''
	    in
		b := 0;
		do b < n_bbs ==>
		    bbsuccs.b := nil;
		    bbpreds.b := nil;
		    b += 1
		od
	    ni;

	    let link == func [p, r : val Short; impure] val Void {
		(* links p to r by adding p to r's predecessors and r to p's successors *)
		(* p and r are basic block numbers *)
			let ss == bbsuccs.p;
			    ps == bbpreds.r
			in
				ss := cons [r, ss];
				ps := cons [p, ps]
			ni
		}
	    in
		(* compute the ctrl edges by looking at opc's of 
					the last quad in each bb *)
		b := 0;
		do b < n_bbs ==>
		    let lq == last [bb.b];	(* last quad in bb *)
		  	op == opc [basics.lq];
		    in
		      if
			 op = BR   ==>
				let dest ==
				  next_interesting[q_for_lbl_at [lq, labels]];
				  b1 == lookup [dest, dest, bbforldrs]
				in
				    if b1 <> -1 then
					link [b, b1];
				    fi
				ni

		     #   (op = BRT) or
			 (op = BRF) ==>	
				(* both arms may end up at the same place since unint-
				   eresting quads are ignored. Only make one link. *)
				let b1 == b + 1; (* one branch *)
				    p2 == next_interesting[q_for_lbl_at 
								[lq, labels]];
				    b2 == lookup [p2, p2, bbforldrs] 
							(* other branch*)
				in
					if b1 <> b2  cand (b2 <> -1)
					then link [b, b2];
					fi;
					if b1 <> -1 then link [b, b1];
					fi
				ni

		     #	(op = RTN)    ==> rtn_bb := b    (* no successors *)

		     # 	else        ==>	(*for other quads preceding leaders*)
					if (b + 1) < n_bbs then
						link [b, b + 1]
					 fi

		      fi
		  ni; (* lq, op *)

	  b += 1
	od; (* go to next bb while b < n_bbs *)
	ni; (* link *)

	(* Remove dead basic blocks *)
	let nil == QuadList $''
	in
	    b := 1;(* bb 0 is not dead even though it has no preds *)
	    do b < n_bbs ==>
		if is_nil [bbpreds.b] then
		    let ss == QuadList $New []
		    in
			ss := bbsuccs.b;
			do not is_nil [ss] ==>
			    bbpreds.(head ss) := remove [b, bbpreds.(head ss)];
			    ss := tail ss
			od
		    ni
		fi;
		b += 1
	    od
	ni;

(* now all the flow info has been computed *)
OPEN
(***************** DEBUGGING OUTPUT ***************************)
		let b == Short$New [0];
		    block == BB$New []
		in
			do b < n_bbs ==>
				block := bb.b;
				put ["\nBasic Block "];put b;
(*				put ["\nvs "]; put_list [vlist [block]];
*)
				put [":\nquads "];put_list [quads [block]];
				put ["\npreds "];put_list [bbpreds.b];
				put [" succs "]; put_list [bbsuccs.b];

				b += 1
			od
		ni;
(*************************************************************)
CLOSE

(* end of computing control flow - info stored in bbsuccs, bbpreds *)
(* order them, record vars, locs ... *)

(* Now compute DF_order on the bbs, and compute other info at the same time *)

	let mark == (Array [n_bbs, Boolean])$New[];

	    (* recursive DFS *)
	    DFsearch == func [b : val Short; impure] val Void {

			let s == BBList $New[];
			in
			    mark.b := True;
			    s := bbsuccs.b;
		   	    do
		        	not is_nil [s] ==>
				    if not mark.(head s) then
						DFsearch [head s]
				    fi;
				    s := tail s
			    od
			ni;

			blist := cons [b, blist]

		} (* DFsearch func *)
	in
		(* initialise the mark array *)
		b := 0;
		do
		    b < n_bbs ==> mark.b := False;
		    b += 1
		od;
    
		blist := BBList $'';
		DFsearch [0]; (* computes blist *)
OPEN
DEBUG("\nOrdered bbs :");put_list blist;
CLOSE

	ni; (* mark, DFsearch *)

	if not short then max_arg_num := 1 fi; (* just the ar *)

(* This piece of code, and function_nodes were reworked to get around a bug
- base was a value returned by the fn, but it was getting ridiculous values *)
	let base == Short$New []
	in
	    base := if ONLYINTRA then 0 (* irrelevant *)
		    else

		function_nodes [fn_name, (*fn_index,*) max_arg_num, graph
	,base
									];
		(* creates nodes for the result and args, numbered from base *)
(*
put "The base (returned by function_nodes) is ";put base;put "\n";
*)
			base


		    fi;

	fn_array.fn_index := FNRec$Mk [short, fst_qd, fn_name, max_arg_num, 
					base, bb, bbsuccs, bbpreds,
						blist, rtn_bb, n_bbs]
	ni;

	(* end of doing stuff with this function *)
      ni; (* n_bbs, bbsuccs, bbpreds, blist *)


    ni; (* fn_name, fst_qd, leaders, b, bbforldrs *)

    fn_list := tail fn_list; (* the list of names and first quads *)
    fn_index += 1
od; (* go on to next function *)

(* all functions done - fn_array completely computed *)

OPEN
(* At this point I could check all the info for all the functions -
	name, firstqd, ordered bbs, other bb info, succs and preds .. *)
	fn_index := 0;
	put "************ CHECK all functions near end of prepass **********";
	do fn_index < num_fns ==>
	    let ff == fn_array.fn_index
	    in
		put "\n";
		put [name ff];put " length=";put [length ff];
		put " base=";put [basenode ff];
		put " blist is ";put_list [blist ff]
	    ni;
	    fn_index += 1
	od;
(*********************************************************************)
CLOSE

if not ONLYINTRA then

# include "firstpass.i" 
(* that processes all functions and builds the entire interprocedural graph *)

fi;

let
    SI == extern {"summaryio"} [];
    info_out_name == if argc > 5 then argv [5] else ChStr$"/dev/null" fi;
    summary_outfile == SI$Open [info_out_name, False];
    Summary === extern {"summary"} [num_fns, Vflag];
    (**********************************)
    file_summary == if ONLYINTRA then Summary$Init []
		    else Graph$flow [graph, summary_outfile]; (* returns an obj of type Summary *)
		    fi;
    (**********************************)

    UA == (extern {"out"} [num_quads, MAX_QUAD]);
    updates == UA$New []
in


    (****************************************************************)
    (**************   process each function *************************)
    (****************************************************************)
    OPEN
        DEBUG( "\nStart lastpass, function by function\n");
    CLOSE

    fn_index := 0;
    do fn_index < num_fns ==> 


(**********************)
# include "lastpass.i"

(* For this function, 
      quad by quad -
	use the file_summary to refine the classification of MAYBEALCs and ARGCATS
	record any real allocators and hinted STIs

      if there are any locs, do the RC and LOCS data flow,

      interpret the STI info

      interpret the DEA info

*)
(**********************)

      fn_index += 1

    od; (* loop through all functions *)

    (* output the program again, including any updates *)
    let out_file == get_file [3, "w"]
    in
	UA$write_file [Arg1type, Basics, BA, basics, updates, out_file]
;
Null []

    ni

ni (* Summary, file_summary, UA, updates *)
(* that was all the last pass stuff *)


ni (* B, ..., fn_array .. *)

ni (* let num_quads, num_fns *)

ni (* first let which declared everything down to uninteresting *)
