# include "defs.h"
# define STATIC_GC_TMP (FIRST_AVAIL_LOC-1)
	/* A temporary that may be safely used by compile time GC */

(* This file maintains the update array stuff, and then outputs the program
   when asked to, incorporating any updates	*)

func [num_quads, MAX_QUAD : val Short] {

    let updateinfo == prod U {kind, x, y, z, w : val Short};
	UpdateList == List [updateinfo]
    in
	(Array [num_quads, UpdateList])
     with UA {

	NNew == func [] var UA {
		    let q == Short$New [0];
		        u == UA$New []
		    in
			do q < num_quads ==>
			    u.q := UpdateList$'';
			    q += 1
		       od;
		       u
		    ni
		};

	add == func [u : var UA; q : val Short;
			kind, x, y, z, w : val Short] val Void {
		    u.q := cons [updateinfo$Mk [kind, x, y, z, w], u.q]
		};

	write_file == func [Arg1type : type T {
				is_str : func [val T] val Boolean;
				to_str : func [val T] val ChStr;
				to_arg : func [val T] val Short};
			    Basics : type B {V;
				opc, arg2, arg3 : func [val B] val Short;
				arg1 : func [val B] val Arg1type};
			    BA : type A {
				. : func [var A;val Short] var Basics};
			    quads : var BA;
			    updates : var UA;
			    out_file : val File; impure] val Void {

	    let
		writew == func [val Short; val File; impure] val Void 
						{extern "_putw"};
		write_quad == func [opc, arg1, arg2, arg3 : val Short;impure]
								val Void {
			writew [opc, out_file];
			writew [arg1, out_file];
			writew [arg2, out_file];
			writew [arg3, out_file]
		      };

		write_str_quad == func [opc : val Short; str : val ChStr;
							   impure] val Void {
			writew [opc, out_file];
			write [out_file, str];
			writeb [out_file, 0]	(* terminating Null *)
		      };

		q == Short$New [0]
	    in
	      do q < num_quads ==>

		(* handle any updates intended for before the qth quad *)
		let ulist == updates.q;
		    fixed == Boolean$New [False]
			(* KILL and STI updates set this to avoid iterating
				over other updates, after having changed q;
			      there should never be any other updates anyway*)
		in
		do not fixed cand not [is_nil ulist] ==>
		let uinfo == head ulist;
		    upd_kind == kind uinfo
		in
		    ulist := tail ulist;

		if upd_kind = DEA_update ==>
OPEN
put "HINT DEA"; put [x uinfo]; put " "; put [y uinfo]; put "\n";
CLOSE
			(* Need to check that the deallocated object *)
			(* is not referenced in the instruction      *)
			(* that kills it.  This can happen with	     *)
			(* LDI AR,0,AR, which we do regularly.       *)
			(*   Partially (?) fixed 2/8/91 - HB	     *)
			let
			    qinfo == quads.q;
			in
			    if opc[qinfo] = LDI cand
			       to_arg[arg1[qinfo]] = x[uinfo] then
			       if arg3[qinfo] <> x[uinfo] then
			           put "-OO messed up on quad ";
			           put q; put "\n";
			           abort[]
			       fi;
			       write_quad [DCL, STATIC_GC_TMP, DCL_ADDR, SK];
			       write_quad [LDI, arg3[qinfo], arg2[qinfo],
			       	   	   STATIC_GC_TMP];
			       write_quad [HINT, DEA, x uinfo, y uinfo];
			       write_quad [MOV, STATIC_GC_TMP, arg3[qinfo], SK];
			       write_quad [UDC, STATIC_GC_TMP, SK, SK];
			       q += 1; fixed:= True;
			    else
			       write_quad [HINT, DEA, x uinfo, y uinfo]
			       (* x is the var (loc) and y is the size 	*)
			       (* q is unchanged - still need to output	*)
			       (* the qth quad 				*)
			    fi;
			ni

		 # upd_kind = KILL_update ==>
OPEN
put "KILL quad ";put q;put " whose opc=";put [opc [quads.q]]; put "\n";
CLOSE
			q := q + 1; fixed := True

		 # (upd_kind = postponed_UDC)
		   cor (upd_kind = postponed_DEAD) ==>
			(* HINT DEA x  y *)
			(* UDC  x   SK SK or HINT DEAD x SK *)
OPEN
put "after quad ";put [q-1];
DEBUG("\nHINT DEA");put [x uinfo];DEBUG(" ");put [y uinfo];
if upd_kind = postponed_UDC then DEBUG("\nUDC")
else DEBUG("\nHINT DEAD") fi;
put [x uinfo]; put "\n";
CLOSE
			write_quad [HINT, DEA, x uinfo, y uinfo];
			if upd_kind = postponed_UDC then
				write_quad [UDC, x uinfo, SK, SK]
			else write_quad [HINT, DEAD, x uinfo, SK]
			fi
			(* q unchanged *)

		 # upd_kind = RL_before ==>
			(* should be just before the LBA *)
		    let v == x uinfo;
		    in
OPEN
put q;put "\nMOV RL to var ";put v;
CLOSE
			write_quad [DCL, v, DCL_ADDR, SK];
			write_quad [MOV, RL, v, SK];
			(* DEAD of RL seems unnecessary right before a CLC *)
			(*write_quad [HINT, DEAD, RL, SK];*)
		    ni

		# upd_kind = RL_after (* equivalent to postponed_UDC *) ==>
			(* should be just after CLC *)
		    let v == x uinfo;
			sz == y uinfo
		    in
			write_quad [HINT, DEA, v, sz];
			write_quad [UDC, v, SK, SK]
		    ni

		 # upd_kind = STI_update ==>
		    let v == x uinfo;
		    	offset == y uinfo;
			next_var == z uinfo;
			size == w uinfo		    
		    in if size = 3 then (* STI loc offset C3 *)
OPEN
DEBUG("\nSTI ");put v;DEBUG(" ");put offset;DEBUG(" C3 ");
CLOSE
		   		write_quad [STI, v, offset, C3]
			else (* need to load size into next_var first *)
OPEN
DEBUG("\nbig STI ");put [v];DEBUG(" ");put offset;DEBUG(" ");put next_var;
DEBUG(" ");put size;DEBUG(" ");
CLOSE
				write_quad [DCL, next_var, DCL_INT, SK];
				write_quad [LDN, size, next_var, SK];
				write_quad [STI, v, offset, next_var];
				write_quad [UDC, next_var, SK, SK]
		   	fi
		    ni;
		    q += 2;  (* to skip both the HINT STSZ and the STI instr *)
		    fixed := True

		fi (* case on upd_kind *)
		ni (* uinfo, upd_kind *)
		od	(* iterate over ulist; abort if KILL or STI updates *)
		ni (* ulist *);

		(* write out the next quad *)
		let qinfo == quads.q
		in
OPEN put q;DEBUG(" "); CLOSE
		    if is_str [arg1 [qinfo]]
		    then write_str_quad [opc [qinfo], to_str [arg1 [qinfo]]]
		    	else write_quad [opc [qinfo], to_arg [arg1 [qinfo]], 
						 arg2 [qinfo], arg3 [qinfo]]
		    fi
		ni;
		q += 1
		
	  od
	ni
	}
      } with UA {New == UA$NNew}
	export {New; add; write_file}
     ni
}

