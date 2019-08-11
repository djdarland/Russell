(* infinite precision calculator for any "curses" terminal *)
(* Must be compiled with -pL                 *)
# define KEYPAD
# undef KEYPAD

# define skip Null[]
# define info_loc(y,x) wmove[info_win,y,x]
# define info_put(s) wput[info_win,s]

let
  Win == extern { "window" } [];
  xLong == extern { "xlong" } [];
  pt == extern {"primes/prime_test" } [];
  sleep == func[val Short; impure] val Void { extern "_sleep" };
  system == func[val ChStr; impure] val Void { extern "_system" };
in use Win in
  let
    RIGHT == 0;		(* some procedure flags *)
    LEFT == 1;
    modes == enum {DEC, HEX, FLOAT};    (* Calculator display modes *)

    SIGINT == 2;                (* interrupt signal *)
    SIGWINCH == 28;             (* signal indicating window changed *)

    ScrnWid == Short$New[];     (* width of screen (columns) *)
    ScrnHgt == Short$New[];     (* height of screen (lines) *)
    Help_avail == Boolean$New[];  (* Enough space for help *)
    windows_exist == New[False];  (* They've been created *)
    Xwindows == New[False];
    MainWid == Short$New[];
    HistWid == Short$New[];
    MainHgt == Short$New[];
    StartPos == Short$New[];
    InfoHgt == 23;
    get_lines == func[impure] val Short { extern "_get_lines" };
    get_columns == func[impure] val Short { extern "_get_columns" };
    forced_resize == func[impure] val Boolean { extern "_forced_resize" };
			(* Returns true iff preceding call to get_lines *)
			(* or get_columns itself adjusted window size   *)

    (* Reset window dimensions to the given screen size.  Remove any *)
    (* existing windows.                                             *)
    reset_size == func[ncols, nlines: val Short; impure] {
		      ScrnWid := ncols;
		      ScrnHgt := nlines;
		      if ScrnWid < 17 (* not enough for Float *)
			 cor ScrnHgt < 6 then
			 if windows_exist then
			     echo[]; nocbreak[]; nl[]; endwin[];
			 fi;
			 write[File$stderr,
			       "Window too small\n"];
			 exit[1];
		      fi;
		      if windows_exist then
			if Help_avail then
			    delwin[info_win];
			fi;
			delwin[bord_win];
			delwin[main_win];
		      fi;
		      if ScrnHgt < 23 cor ScrnWid <50 then
			MainWid := ScrnWid-2;
			HistWid := MainWid+2;
			Help_avail := False;
			write[File$stderr,
			      "Not enough space for help window\n"];
			sleep[1];
		      else
			Help_avail := True;
			MainWid := ScrnWid-22;
			HistWid := MainWid+2;
		      fi;
		      StartPos := -MainWid/2;
		      old_pos := main_pos := StartPos;
		      MainHgt := ScrnHgt-3;
		  };

    make_windows == func[impure] val Void {
		      bord_win := Win$Mk[ScrnHgt-1,MainWid+2,0,0];
		      main_win := Win$Mk[MainHgt,MainWid,1,1];
		      hist_win := Win$Mk[1,HistWid,ScrnHgt-1,0];
		      if Help_avail then
			info_win :=
			     Win$Mk[InfoHgt,ScrnWid-MainWid-3,0,MainWid+3];
		      fi;
		      windows_exist := True;
		    };
		    
    history == New[""];
		    
    add_hist == func[s: val ChStr; impure] val Void {
    		    history := history ^* s;
    		    if len[history] >= HistWid then
    		        history := substr[history,
    		        	          len[history] - (HistWid-1),
    		        	          HistWid-1]
    		    fi;
    		    wmove[hist_win,0,0];
    		    wput[hist_win,history];
    		    wrefresh[hist_win];
    		    move_to_top_of_stack[];
    		};

    bord_win == Win$New[];
    main_win == Win$New[];
    info_win == Win$New[];
    hist_win == Win$New[];

    main_pos == Short$New[];
    old_pos ==	Short$New[];
    mode ==     modes$New[];
    working == 	Boolean$New[];

    update_info ==
	func[impure] val Void {
	  if Help_avail then
	    wmove[info_win,1,1];
	    use modes in let
		lc == -main_pos + if main_pos<0 ==> -1 # else ==> 0 fi;
		rc == -main_pos - MainWid +
			if (-main_pos-MainWid<0) ==> 1 # else ==> 0 fi;
		lst == puts[lc];
		rst == puts[rc];
		ll == New[len[lst]];
		lr == New[len[rst]];
		lpr == if ll <= 6 ==> lst # else ==> ll := 5; "*****" fi;
		rpr == if lr <= 6 ==> rst # else ==> lr := 5; "*****" fi;
		x == getx[main_win];
		y == gety[main_win];
		i == New[0];
	    in
		wput[info_win," ["]; wput[info_win,lpr];
		wput[info_win,","]; wput[info_win,rpr];
		wput[info_win,"]"];
		i := ll + lr;
		do i < 12 ==> wput[info_win, " "]; i += 1 od;
		info_loc(2,1);
		if (mode = HEX) ==>
		    wput[info_win," Hex mode    "];
		    info_loc(15,1); info_put("m float/dec mode");
		    info_loc(16,1); info_put("<,> scroll view");
		    info_loc(17,1); info_put("$ center");
		# (mode = DEC) ==>
		    wput[info_win," Decimal mode"];
		    info_loc(15,1); info_put("m hex/float mode");
		    info_loc(16,1); info_put("<,> scroll view");
		    info_loc(17,1); info_put("$ center");
		# (mode = FLOAT) ==>
		    wput[info_win," Float mode  "];
		    info_loc(15,1); info_put("m dec/hex mode  ");
		    info_loc(16,1); info_put("               ");
		    info_loc(17,1); info_put("        ");
		fi;
		wrefresh[info_win];
		wmove[main_win,y,x];	(* move cursor back to main_win *)
		wrefresh[main_win];
	    ni ni
	  fi
	};

    werror ==
	func[s: val ChStr; impure] val Void {
	  if Help_avail then
	    let
	      x == getx[main_win]; y == gety[main_win];
	    in
	      wmove[info_win,3,1];
	      wput[info_win,"                 "];
	      wmove[info_win,3,1];
	      wput[info_win,ChStr$In[7]];         (*beep*)
	      wput[info_win,s];
	      wrefresh[info_win];
	      wmove[info_win,3,1];
	      sleep[1];
	      wput[info_win,"                 "];
	      wrefresh[info_win];
	      update_info[];
	      wmove[main_win,y,x];
	      move_to_top_of_stack[];
	    ni
	  else
	    wput[main_win,ChStr$In[7]];         (*beep*)
	    wrefresh[main_win];
	  fi
	};

    move_to_top_of_stack ==
	func[impure] val Void {
	    wmove[main_win,trunc_sp[stack_ptr+1],0];
	    wrefresh[main_win];
	};

    get_prec ==
	func[impure] val Short {
	    MainWid + main_pos + 1;
	};

    sidestep ==
	func[amt, x: val Short; impure] val Void {
	  if amt = 0 ==>
	    if x = LEFT ==>
		main_pos -= MainWid-1;
	    # else ==>
		main_pos += MainWid-1;
	    fi;
	  # else ==>
	    if x = LEFT ==>
		main_pos -= amt;
	    # else ==>
		main_pos += amt;
	    fi
	  fi;
	  werase[main_win]; wrefresh[main_win];
	  update_info[];
	};

    working_flag ==
	func[impure] val Void {
	  if Help_avail then
	    wmove[info_win,3,1];
	    wput[info_win," working... "];
	    wrefresh[info_win];
	  fi;
	  working := True;
	};

    done_working ==
	func[impure] val Void {
	  if Help_avail then
	    wmove[info_win,3,1];
	    wput[info_win,"            "];
	    wrefresh[info_win];
	  fi;
	  working := False;
	  move_to_top_of_stack[];
	};

    (* Print a message in a position where it will be overwritten by *)
    (* the "working" flag.                                           *)
    print_msg ==
	func[msg: val ChStr; impure] val Void {
	    if Help_avail then
		wmove[info_win,3,1];
		wput[info_win, msg];
		wrefresh[info_win];
	    else
		wput[main_win, msg];
		wrefresh[main_win];
		sleep[1];
		display[];
	    fi;
	    move_to_top_of_stack[];
	};

    print_help ==
	func[impure] val Void {
	  if Help_avail then
	    info_loc(4,1); info_put("~ change sign");
	    info_loc(5,1); info_put("R square root");
	    info_loc(6,1); info_put("X exponential");
	    info_loc(7,1); info_put("P pi");
	    info_loc(8,1); info_put("S sine");
	    info_loc(9,1); info_put("K cosine");
	    info_loc(10,1); info_put("T arctan");
	    info_loc(11,1); info_put("I interchange");
	    info_loc(12,1); info_put("^ power (base>0)");
	    info_loc(13,1); info_put("L logarithm");
	    info_loc(14,1); info_put("Q copy bottom");
	    (* Leave room for M, <, >, $ *)
	    info_loc(18,1); info_put("[sp] enter");
	    info_loc(19,1); info_put("[bs] backup");
	    info_loc(20,1); info_put("? more help");
	    info_loc(21,1); info_put("^D quit");
	    wrefresh[info_win];
	  fi;
	};


    print_more_help ==
	func[impure] val Void {
	  if Help_avail then
	    wclear[info_win];
	    box[info_win,ChStr$Out["|"],ChStr$Out["-"]];
	    info_loc(1,1); info_put("=c save in loc");
	    info_loc(2,1); info_put("Gc get from loc");
	    info_loc(3,1); info_put("   where c is");
	    info_loc(4,1); info_put("   any character");
	    info_loc(5,1); info_put("^L redraw the");
	    info_loc(6,1); info_put("   screen");
	    info_loc(7,1); info_put("n<,n>, scroll n");
	    info_loc(8,1); info_put("       digits");
	    info_loc(9,1); info_put ("Z  truncate to");
	    info_loc(10,1); info_put("   integer    ");
	    info_loc(11,1); info_put("! factorial");
	    info_loc(12,1); info_put("# is it prime?");
	    info_loc(15,1); info_put("^C abort last");
	    info_loc(16,1); info_put("   calculation");
	    info_loc(18,1); info_put("hit any key to");
	    info_loc(19,1); info_put("return to");
	    info_loc(20,1); info_put("calculator");
	    wrefresh[info_win];
	    let c == wgetc[info_win]
	    in
		wclear[info_win];
		box[info_win,ChStr$Out["|"],ChStr$Out["-"]];
		print_help[];
		update_info[]
	    ni
	  fi
	};

    erase_one ==
	func[impure] val Void {
	    let x == getx[main_win]; y == gety[main_win];
		px == if x>0 ==> x-1 # else ==> 0 fi;
	    in
		wmove[main_win,y,px];
		wput[main_win," "];
		wmove[main_win,y,px];
		wrefresh[main_win];
	    ni
	};

    (* returns position of first occurence of character of c in s *)
    index ==
	func[s: val ChStr; c: val Short] val Short {
	    let i == Short$New[];
		l == len[s];
	    in
		i := 0;
		do (s.i <> c) cand i < l ==>
		    i += 1;
		od;
		if i = l ==> -1 # else ==> i fi;
	    ni
	};

    (* Display the the currently appropriate section of string s *)
    (* s is presumed to be a prefix of a real number containing  *)
    (* enough precision for the display request.                 *)
    putnum ==
	func[s: val ChStr; impure] {
	    let
		p == index[s, Out["."]];
	    in
		if p >= -main_pos ==> (* main_pos <= 0 && int part wraps off
					 left side of current view *)
		    wput[main_win,substr[s,p+main_pos,MainWid]];
		# p = -1 ==>
		    wput[main_win,"BLEAGH"];
		# else ==>
		    let x == getx[main_win]; y == gety[main_win];
			inc == -main_pos - p;	(* spaces between left side of
						   scrn and decimal pt *)
		    in
			if inc >= MainWid then
			    wput[main_win, "--->"];
			else
			    let
			      d == substr[s,0,MainWid-inc]
			    in
			      if d = " " then
				wput[main_win, "--->"];
			      else
				wmove[main_win,y,x+inc];
				wput[main_win,d];
			      fi
			    ni
			fi
		    ni
		fi;
		wrefresh[main_win];
	    ni
	};

   (*
    * Ascii codes
    *)
    esc_ch ==	ChStr$In[27];	(* ESC *)
    backsp ==   ChStr$In[8];    (* Backspace *)
    del_ch ==   ChStr$In[127];  (* Delete *)
    eof_ch ==	ChStr$In[4];	(* EOF *)
    redraw ==   ChStr$In[12];   (* ^L (redraw screen) *)
    cond_redraw == ChStr$In[1]; (* ^A, redraw if size is different   *)
				(* generated in response to SIGWINCH *)

   (*
    * keypad characters - keypad sends a sequence starting with esc_ch
    * and terminating in one of these.
    *)
    KEYPAD9 ==	"y";		(* 9 key on wyse and vi200		*)
    KEYPAD0 ==	"p";		(* 0 key on wyse and vi200		*)
    KEYPADDOT == "n";		(* . key on wyse and vi200		*)
    KEYPADPF1 == "P";		(* PF1 key on wyse *)
    KEYPADPF2 == "Q";		(* PF2 key on wyse *)
    KEYPADPF3 == "R";		(* PF3 key on wyse *)
    KEYPADPF4 == "S";		(* PF4 key on wyse *)
    KEYPADDASH == "m";		(* dash key on wyse, vi200 keypads *)
    KEYPADCOM == "l";		(* comma key on wyse keypad	*)
    KEYPADENT == "M";           (* enter key on wyse keypad     *)

    newline ==       func [impure] { wput[main_win,"\n"] };
#   define NL   newline[]

  (*
   * UNIX interface
   *)
    exit == func [val Short; impure] val Void { extern "_exit" };

    is_eof_char == func [c: val ChStr] val Boolean {Out[c] = 255};

    clear == func [impure] val Void { extern "_clearM" };

    refresh == func [impure] val Void { extern "_refreshM" };

  (*
   * Calculator stuff
   *)
    inf_p == extern { "cr/fusion" } [];    (* constructive reals           *)
    stack_lim ==        100;             (* max num of entries in stack  *)
    stack_elem == 	inf_p;		(* type of elements on stack	*)

   (*
    * define the stack
    *)
    stack_type == (Array[stack_lim, stack_elem])
	with I {
            (*
	     * subscription, with array 0 filled for <0 indices
	     *)
		sub == func[a: var I; i: val Short; impure]
			   val stack_elem {
			    if
				i >= 0 ==> I$.[a,i]
			    #   else  ==>  stack_elem$In[Long$0]
			    fi
		       };

		push == func[a: var I; sp: var Short;
			     new: val stack_elem] val Void {
			    sp += 1;
			    a.sp := new;
			};

		pop == func[a: var I; sp: var Short] val Void {
			 if
			     sp >= 0 ==> sp -= 1;
			 #  else    ==> skip;
			 fi
		       };
	} hide {V};	(* end of stack type definition *)

    (*
     * The cache of string representations of values
     *)

    ce_empty_flag == -Long$1000000;

    cache_entry == prod {
			ce_base: val modes; (* mode used to do base conversion *)
			ce_prec: val Long;  (* precision used in conversion    *)
			ce_string: val ChStr; (* output of base conversion     *)
		    } with CE {
			empty == func [] {
				    CE$Mk[modes$DEC, ce_empty_flag, ""]
				 };
			NNew == func[] var CE {
				    let
					r == CE$New[]
				    in
					r := CE$empty[];
					r
				    ni
				}
		    } with CE { New == CE$NNew };

    cache_type == Array[stack_lim, cache_entry];

    (* raise x to the power y *)
    power == func[x,y: val stack_elem; impure] {
		    stack_elem$power[x,y];
		};

   (*
    * misc variables and constants
    *)
    stack ==            stack_type$New[]; (* the stack *)

    cache ==            cache_type$New[]; (* A cache of print representations *)
					  (* for numbers, parallel to stack.  *)

    regs ==             (Array[256, stack_elem])$New[];
					   (* Memory locations *)

    stack_ptr == 	Short$New[];	(* pointer to a stack element	*)
    trunc_sp == 	func[sp: val Short; impure]
    			  { if sp >= MainHgt then MainHgt - 1 else sp fi };
    i == 		Short$New[];

    entering == 	Boolean$New[];  (* Currently entering a number	*)
    seen_period == 	Boolean$New[];  (* Saw dec point in current number *)
    esc_seq ==		Boolean$New[];	(* saw a keypad prefix		*)
    saw_e == 		Boolean$New[];  (* Interpret a + or - as part of *)
    					(* the number being entered.	 *)

    upper_case ==	func[c: val ChStr] val ChStr {
			    if (ChStr$Out[c] >= ChStr$Out['a']) cand
			       (ChStr$Out[c] <= ChStr$Out['z']) ==>
				ChStr$In[ChStr$Out[c]-Short$32];
			    # else ==> c
			    fi
			};
    lower_case ==	func[c: val ChStr] val ChStr {
			    if (ChStr$Out[c] >= ChStr$Out['A']) cand
			       (ChStr$Out[c] <= ChStr$Out['Z']) ==>
				ChStr$In[ChStr$Out[c]+Short$32];
			    # else ==> c
			    fi
			};
    huge == Long$10000000;  (* A value greater than any reasonable *)
			    (* precision request.                  *)
#   define esc_seq_match(c,m)	(esc_seq cand upper_case(c) = m)
#   define normal_match(c,m)    ((not esc_seq) cand upper_case(c) = m)

    entry ==		ChStr$New[];	(* current incomplete entry	    *)
    prec ==             Long$New[];     (* current operating precision      *)

    reprint_input == func [impure] val Void {
    			wput[main_win, entry];
		     };

    c == 		ChStr$New[];	(* Current input character	*)

   (*
    * Return the i the entry in the display cache, updating it if necessary
    * Assumes that mode is either HEX or DEC
    *)
    get_cache == func[i: val Short; impure] val ChStr {
		    let
			ce == cache.i (*ce = the variable, not the value *);
		    in
			if mode <> ce_base[ce]
			   cor ce_prec[ce] = ce_empty_flag
			   cor ce_prec[ce] < prec then
			    if mode = modes$HEX then
				ce := cache_entry$Mk[mode, prec,
						     hex_puts[stack.i, prec]];
			    else
				ce := cache_entry$Mk[mode, prec,
						     puts[stack.i, prec]];
			    fi
			fi;
			ce_string[ce]
		    ni
		 };

    ln10 == ln[stack_elem$In[Long$10]];

   (*
    * Display n in scientific notation
    *)
    putfloat ==
	func[n: val stack_elem; impure] {
	    use stack_elem, Long in let
		(* Precisions used in comparisons *)
		abs_prec == -1000;
		rel_prec == -200;
		small_prec == -20;
		exp_field_wid == 10;
		abs_n == abs[n];
		exponent == xLong$New[];
		printed_val == stack_elem$New[];
		scale_factor == stack_elem$New[];
		'0.1' == 0.1;
		'1' == xLong$1;
	    in
		if compare[n, 0.0, rel_prec, abs_prec] = Short$0 then
		    wput[main_win, " 0.0"];
		else
		    exponent := xLong$from_Long[to_Long[ln[abs_n]/ln10,
							small_prec]];
		    scale_factor := In[to_Long[xLong$10**abs[exponent]]];
		    if abs[exponent] > xLong$500 then
			scale_factor := force_cr_rep[scale_factor];
			(* Constructive real arithmetic is probably faster *)
			(* than rational arithmetic.                       *)
		    fi;
		    if exponent >= xLong$0 then
			printed_val := n / scale_factor;
		    else
			printed_val := n * scale_factor;
		    fi;
		    (* Compensate for truncation toward 0 in to_Long *)
		      if compare[abs[printed_val], 1.0,
				 small_prec, small_prec] < Short$0 then
			printed_val := printed_val * stack_elem$In[10];
			exponent -= xLong$1;
		      fi;
		    (* mantissa: *)
		      wput[main_win, puts[printed_val,
					  In[MainWid] - exp_field_wid - 3]];
		    (* exponent *)
		      wput[main_win, " E"];
		      wput[main_win, puts[exponent]];
		fi
	    ni ni
	};

   (*
    * (re-)display the stack
    *)
    display ==
	func[impure] {
	    use modes in let
		i == Short$New[]
	    in
		wmove[main_win,0,0];
		werase[main_win];
		i := 0;
		do i <= stack_ptr cand i < MainHgt-1 ==>
		    wmove[main_win,i,0];
		    working_flag[];
		    if mode = FLOAT then
			putfloat[stack.i];
		    else
			putnum[get_cache[i]];
		    fi;
		    done_working[];
		    i += 1;
		od;
		wmove[main_win,trunc_sp[stack_ptr+1],0]
	    ni ni
	};

   (*
    * (re-)display the top element on the stack.  Currently does not take
    * into account numbers wrapping around (i.e. one stack elt = one line)
    *)
    display_top ==
	func[impure] {
	    if stack_ptr >= Short$0 cand stack_ptr < MainHgt-1 then
		erase_top[];
		working_flag[];
		if mode = modes$FLOAT then
		    putfloat[stack.stack_ptr];
		else
		    putnum[get_cache[stack_ptr]];
		fi;
		done_working[];
		wmove[main_win,trunc_sp[stack_ptr+1],0]
	    fi;
	};

   (*
    * erase the top entry on the stack, and clear the associated cache
    *)
    erase_top ==
	func[impure] {
	    if (stack_ptr >= Short$0 cand stack_ptr < MainHgt) ==>
		cache.stack_ptr := cache_entry$empty[];
		wmove[main_win, trunc_sp[stack_ptr], 0];
		wclrtoeol[main_win];
		wrefresh[main_win];
	    # else ==> skip
	    fi
	};

   (*
    * Build a constructive real from hex representation
    *)
    hex_point == func[whole, fraction, length: val Long; impure] val stack_elem {
		    let
			(* Convert everything from Long to xLong. Gross... *)
			Xwhole == xLong$from_Long[whole];
			Xlength == xLong$from_Long[length];
			Xfraction == xLong$from_Long[fraction];
                    in
                      if
			Xfraction = xLong$0 ==>
			   stack_elem$In[whole]
		      # else ==>
			   let
			      multiplier == xLong$16**Xlength;
			      Xint_val == Xwhole * multiplier + Xfraction;
			   in
			      stack_elem$In[to_Long[Xint_val]]
			      / stack_elem$In[to_Long[multiplier]]
			   ni
                      fi;
		    ni
                 };
                 
   (*
    * Convert a character string to a stack_elem value.
    *)
    convert_entry ==
    	func [impure] val stack_elem {
    	    let
    	        int_part == New[Long$0];
    	        fract_part == New[Long$0];
    	        exp_part == New[Long$0];
    	        exp_negative == New[False];
    	        scale == New[0];	(* No. of digits right of dec. pt. *)
    	        i == New[0];
    	        current == Short$New[];
    	        zero == ChStr$Out["0"];
    	        nine == ChStr$Out["9"];
    	        letter_a == ChStr$Out["a"];
    	        letter_f == ChStr$Out["f"];
    	        letter_A == ChStr$Out["A"];
    	        letter_F == ChStr$Out["F"];
    	        period == ChStr$Out["."];
    	        is_digit == func[c: val Short; impure] {
    	            	       (c >= zero cand c <= nine)
    	            	       cor (mode = modes$HEX
    	            	            cand (c >= letter_a cand c <= letter_f)
    	            	                 cor (c >= letter_A cand c <= letter_F))
    	            	   };
    	        digit_val == func[c: val Short] {
    	        	       Long$In[
    	        		if c >= zero cand c <= nine then
    	        		    c - zero
    	        		elsif c >= letter_a cand c <= letter_f then
    	        		    c - letter_a + 10
    	        		else
    	        		    c - letter_A + 10
    	        		fi
    	        	       ]
    	        	     };
    	        base == use Long,modes in if mode = HEX then 16 else 10 fi ni;
    	    in
    	        do (current := entry.i; is_digit[current]) ==>
    	            int_part := int_part * base;
    	            int_part := int_part + digit_val[current];
    	            i += 1;
    	        od;
    	        if current = period then
    	            i += 1;
    	            do (current := entry.i; is_digit[current]) ==>
    	                fract_part := fract_part * base;
    	                fract_part := fract_part + digit_val[current];
    	                scale += 1;
    	                i += 1;
    	            od;
    	        fi;
    	        if upper_case[ChStr$In[current]] = "E" then
    	            i += 1;
    	            current := entry.i;
    	            if current = ChStr$Out["+"] then
    	                i += 1;
    	            elsif current = ChStr$Out["-"] then
    	                i += 1;
    	                exp_negative := True;
    	            fi;
    	            do (current := entry.i; is_digit[current]) ==>
    	                exp_part := exp_part * base;
    	                exp_part := exp_part + digit_val[current];
    	                i += 1;
    	            od;
    	            if exp_negative then exp_part := - exp_part fi;
    	        fi;
    	        use stack_elem, Long in
    	          if current <> Short$0 then
    	            werror[" syntax err. "]; stack_elem$In[0]
    	          else
    	            if mode = modes$HEX then
			hex_point[int_part,fract_part,Long$In[scale]]
		    else
		        if exp_part = 0 then
		            In[1]
		        else
		            power[In[base],In[exp_part]]
		        fi
		        * stack_elem$.[int_part,
				       fract_part,Long$In[scale]]
		    fi
		  fi
		ni
    	    ni
    	};

   (*
    * put current entry on the stack
    *)
    fix_entry ==
	func [impure] val Void {
	    if entering then
		if stack_ptr = stack_lim-1 then
		    wput[main_win,"\n"];
		    werror[" stack too deep"];
		    wrefresh[main_win];
		    stack_ptr -= 1;
		fi;
		add_hist[entry];
		let
		    new_val == convert_entry[];
		in
		    push[stack,stack_ptr,new_val];
		ni;
		entry := ""; saw_e := False;
		entering := False;  seen_period := False;
	    fi
	};

   (*
    * execute a binary operation
    * assume one stack entry = one line
    *)
    binop ==
	func [f: func[x,y: val stack_elem] val stack_elem; impure]
		val Void {
	    fix_entry[];	(* put current number (if any) on stack *)
	    if stack_ptr <= 0 ==>
		werror[" too few args"];
	    # else ==>
		let
		    x == sub[stack,(stack_ptr-1)];
		    y == sub[stack,stack_ptr];
		in
		    erase_top[]; pop[stack, stack_ptr];
		    erase_top[]; pop[stack, stack_ptr];
		    (* We do the push piecemeal, so that stack_ptr     *)
		    (* has a predictable value if we diverge and abort *)
		    stack_ptr += 1;
		    working_flag[];
		    stack.stack_ptr :=  f[x, y];
		ni;
		display_top[];
	    fi
	};

    (* An impure version *)
    binop ==
	func [f: func[x,y: val stack_elem; impure] val stack_elem; impure]
		val Void {
	    fix_entry[];	(* put current number (if any) on stack *)
	    if stack_ptr <= 0 ==>
		werror[" too few args"];
	    # else ==>
		let
		    x == sub[stack,(stack_ptr-1)];
		    y == sub[stack,stack_ptr];
		in
		    erase_top[]; pop[stack, stack_ptr];
		    erase_top[]; pop[stack, stack_ptr];
		    stack_ptr += 1;
		    working_flag[];
		    stack.stack_ptr :=  f[x, y];
		ni;
		display_top[];
	    fi
	};

   (*
    * execute a unary operation
    *)
    unop ==
	func [f: func[y: val stack_elem; impure] val stack_elem; impure]
		val Void {
	    fix_entry[];
	    if stack_ptr < 0 ==>
		werror[" need an arg!"];
	    # else ==>
		let
		    y == sub[stack,stack_ptr];
		in
		    erase_top[]; pop[stack, stack_ptr];
		    stack_ptr += 1;
		    working_flag[];
		    stack.stack_ptr :=  f[y];
		ni;
		display_top[];
	    fi
	};

    (* Again, for pure operations *)
    unop ==
	func [f: func[y: val stack_elem] val stack_elem; impure]
		val Void {
	    fix_entry[];
	    if stack_ptr < 0 ==>
		werror[" need an arg!"];
	    # else ==>
		let
		    y == sub[stack,stack_ptr];
		in
		    erase_top[]; pop[stack, stack_ptr];
		    stack_ptr += 1;
		    working_flag[];
		    stack.stack_ptr :=  f[y];
		ni;
		display_top[];
	    fi
	};

   (*
    * Check whether an argument is definitely negative.
    * May fail in the direction of identifying a number as nonnegative.
    * Has the side effect of displaying the "working" sign,
    * WHICH MUST BE CLEARED by caller.  Normally this happens
    * implicitly as the result of a redisplay operation.
    *)
     is_neg == func[x:val stack_elem; impure] val Boolean {
		    working_flag[];
		    use Long in let
			pos_prec == if prec < 0 then 0 else prec fi;
		    in
			if is_rat[x] then
			    compare[x, stack_elem$In[0],-huge,-huge] < Short$0
			else
			    compare[x, stack_elem$In[0],
				    -huge,
				    -pos_prec*4-100 (* abs prec in bits *)]
			    < Short$0
			fi
		    ni ni
	       };

   (*
    * Check whether the two argument are definitely not equal.
    * Has the side effect of displaying the "working" sign,
    * WHICH MUST BE CLEARED by caller.  Normally this happens
    * implicitly as the result of a redisplay operation.
    *)
     are_unequal ==
		  func[x,y: val stack_elem; impure] val Boolean {
		    working_flag[];
		    use Long in let
			pos_prec == if prec < 0 then 0 else prec fi;
		    in
			if is_rat[x] then
			    compare[x, y, -huge, -huge] <> Short$0
			else
			    compare[x, y,
				    -huge,
				    -pos_prec*4-100 (* abs prec in bits *)]
			    <> Short$0
			fi
		    ni ni
		  };

   (*
    * Check whether an argument is definitely zero.
    * May fail in the direction of identifying a number as nonzero.
    *)
     is_zero == func[x:val stack_elem; impure] val Boolean {
		    use Long in
			if is_rat[x] then
			    compare[x, stack_elem$In[0],-huge,-huge] = Short$0
			else
			    False  (* Can't possibly tell *)
			fi
		    ni
	       };

in
   (* initialization *)
    Expand_Hp[64];
    stack_ptr := -1;                    (* empty stack          *)

    (* Start up display *)
      reset_size[get_columns[], get_lines[]];
      initscr[];
      nonl[]; cbreak[]; noecho[];
      make_windows[];

    esc_seq := entering := seen_period := False;
    entry := ""; saw_e := False;
    
    c := " ";
    prec := Long$In[get_prec[]];	(* precision depends on window	   *)

    let

	add_digit ==
	    func[dig: val ChStr; impure] val Void {
	        entry := entry ^* dig;
	        saw_e := mode <> modes$HEX cand (dig = 'e' cor dig = 'E');
	    };
	    

	(* remove a digit from number being entered *)
	rem_digit ==
	    func[impure] val Void {
	        let
	            length == len[entry];
	        in if length <> 0 then
		    erase_one[];		(* erase digit *)
		    if entry.(length-1) = ChStr$Out["."] then
		        seen_period := False;
		    fi;
		    if length > 1 then
		        saw_e := upper_case[ChStr$In[entry.(length-2)]] = "E"
		    else
		    	saw_e := False
		    fi;
		    entry := substr[entry, 0, length-1];
		fi ni
	    };
    in
       Xwindows := func[impure] val Boolean { extern "_is_x_win" } [];
       (* initialize windows *)
	working := False;

	mode := modes$DEC;    /* default is decimal mode */
	print_help[];
	box[bord_win,ChStr$Out["|"],ChStr$Out["-"]];
	if Help_avail then
	    box[info_win,ChStr$Out["|"],ChStr$Out["-"]];
	fi;
	wrefresh[bord_win];
	update_info[];
	wmove[main_win,0,0];
	wrefresh[main_win];

       (*
	* Initialize register array
	*)
	let
	    i == New[0]
	in
	    do i < 256 ==>
		regs.i := stack_elem$In[Long$0];
		i += 1;
	    od
	ni;

	if Signal[SIGWINCH] then
	    c := cond_redraw;
	fi;

       (*
	* loop on input character until a ^D is entered
	*)
	do not (is_eof_char(c) cor c = ChStr$In[-1] /* X windows */) ==>
	 let
	  stopped == Signal[SIGINT];
	 in
	  if stopped ==>	(* check for interrupt *)
	    if not(working) ==>
		werror[" what?       "];
	    # else ==>
		werror[" aborted     "];
		if normal_match(c,"<") cor normal_match(c,">") ==>
		    if main_pos > old_pos ==>
			main_pos := old_pos
		    # else ==>
			main_pos -= MainWid-1
		    fi;
		    entry := ""; saw_e := False;
		    entering := False;  seen_period := False;
		    prec := Long$In[get_prec[]];
		    update_info[];
		    display[];
		# else ==>
		    if not(entering) ==>
			erase_top[]; pop[stack, stack_ptr];
			done_working[];
		    # else ==>
			fix_entry[];
			done_working[]
		    fi
		fi
	    fi
	  # else ==>	    
	    if

	  (* digit from typewriter keys *)
	    (not esc_seq) cand
		((c >= "0" cand c <= "9") cor
		 (c >= "A" cand c <= "F") cor
		 (c >= "a" cand c <= "f")) ==>
		entering := True;
		c := lower_case[c];
		wput[main_win,c];
		wrefresh[main_win];
		add_digit[c];

	    # not esc_seq cand c = "m" ==>
		mode := if mode = modes$Last then
			    modes$First
			else
			    Succ[mode]
			fi;
		update_info[];
		display[];
		add_hist["m"];

	    # not esc_seq cand c = "M" ==>
	    	mode := if mode = modes$First then
			    modes$Last
			else
			    Pred[mode]
			fi;
		update_info[];
		display[];
		add_hist["M"];

	    # normal_match(c,"?") ==>
		print_more_help[];

	    # normal_match(c,"$") ==>
		main_pos := StartPos;
		prec := Long$In[get_prec[]];
		update_info[];
		display[];

	    # normal_match(c,"<") ==>
		let amt == Long$Out[to_Long[convert_entry[], -Long$2]]
		in
		    old_pos := main_pos;
		    sidestep[amt,LEFT];
		    prec := Long$In[get_prec[]];
		    entry := ""; saw_e := False;
		    entering := seen_period := False;
		    display[];
		ni;

	    # normal_match(c,">") ==>
		let amt == Long$Out[to_Long[convert_entry[], -Long$2]]
		in
		    old_pos := main_pos;
		    sidestep[amt,RIGHT];
		    prec := Long$In[get_prec[]];
		    entry := ""; saw_e := False;;
		    entering := seen_period := False;
		    display[];
		ni;

# ifdef KEYPAD
	  (* keypad key start flag *)
	    # c = esc_ch ==>
		esc_seq := True;
# endif

	  (* keypad digit *)
	    # esc_seq cand c >= KEYPAD0 cand c <= KEYPAD9 ==>
		entering := True;
		let
		    dig == Long$In[ChStr$Out[c] - ChStr$Out[KEYPAD0]];
		    char == puts[dig];
		in
		    wput[main_win,char];
		    wrefresh[main_win];
		    add_digit[char];
		ni;
		esc_seq := False;
	  (* decimal point *)
	    # esc_seq_match(c,KEYPADDOT) cor normal_match(c,".") ==>
	        esc_seq := False;
		if (not seen_period) ==>
		    entering := True;
		    wput [main_win,"."];
		    add_digit["."];
		    wrefresh[main_win];
		    seen_period := True;
		# else ==> 
		    skip
		fi;

	  (* backspace - delete top entry from stack *)
	    # c = backsp cor c = del_ch  cor normal_match(c,"\\") ==>
		if entering cand entry <> "" ==>
		    rem_digit[];
		# else ==>
		    entering := False; saw_e := False;
		    if (stack_ptr >= 0) ==> erase_top[];
					    stack_ptr -= 1;
		    # else ==>		    skip
		    fi;
		    add_hist["\\"];
		fi;
		esc_seq := False;

	   (* add top two entries *)
	    # esc_seq_match(c,KEYPADPF1) cor normal_match(c,"+") ==>
	        esc_seq := False;
	        if saw_e then
	            (* Start of an exponent *)
	            wput[main_win,"+"];
		    wrefresh[main_win];
		    add_digit["+"];
	        else
		    binop[stack_elem$+];
		    add_hist["+"];
		fi;

	   (* subtract top two entries *)
	    # esc_seq_match(c,KEYPADPF2) cor normal_match(c,"-") ==>
	        esc_seq := False;
	        if saw_e then
	            (* Start of an exponent *)
	            wput[main_win,"-"];
		    wrefresh[main_win];
		    add_digit["-"];
	        else
		    binop[stack_elem$-
			  <<func[x,y: val stack_elem] val stack_elem>>];
		    add_hist["-"];
		fi;

	   (* multiply top two entries *)
	    # esc_seq_match(c,KEYPADPF3) cor normal_match(c,"*") ==>
	        add_hist["*"];
		binop[stack_elem$*];
		esc_seq := False;

	   (* interchange top values *)
	    # normal_match(c,"I") ==>
		fix_entry[];
		if stack_ptr >= 1 then
		    let
			top == V[stack.stack_ptr];
			second == V[stack.(stack_ptr-1)];
		    in
			erase_top[];
			pop[stack, stack_ptr];
			erase_top[];
			pop[stack, stack_ptr];
			push[stack, stack_ptr, top];
			display_top[];
			push[stack, stack_ptr, second];
			display_top[];
		    ni
		fi;   
		add_hist["I"];

	   (* Save a value in a register *)
	    # normal_match(c, "=") ==>
		fix_entry[];
		display_top[];
		add_hist["="];
		let
		    rid == wgetc[main_win]
		in 
		    add_hist[ChStr$In[rid]];
		    regs.(rid) := sub[stack,stack_ptr];
		ni;

	   (* Get a value from a register *)
	    # normal_match(c, "G") ==>
		fix_entry[];
		display_top[];
		add_hist["G"];
		let
		    rid == wgetc[main_win]
		in 
		    add_hist[ChStr$In[rid]];
		    push[stack, stack_ptr, regs.(rid)];
		ni;
		display_top[];

	   (* divide top entry into second from top *)
	    # esc_seq_match(c,KEYPADPF4) cor normal_match(c,"/") ==>
		fix_entry[];
		use Long in let
		    arg2 == sub[stack,stack_ptr];
		in
		    if is_zero[arg2] then
			display_top[];
			werror["0 divisor"];
		    else
			binop[stack_elem$/];
		    fi
		ni ni;
		add_hist["/"];

	   (* change sign *)
	    # esc_seq_match(c,KEYPADDASH) cor normal_match(c,"~") ==>
		unop[stack_elem$-
			<<func[val stack_elem] val stack_elem>>];
		esc_seq := False;
		add_hist["~"];

	   (* power *)
	    # normal_match(c,"^") ==>
		fix_entry[];
		let
		    arg1 == sub[stack,stack_ptr - 1];
		in
		    if is_neg[arg1] then
			display_top[];
			werror["negative base"];
		    elsif is_zero[arg1] then
			display_top[];
			werror["zero base"];
		    else
			binop[power];
		    fi
		ni;
		add_hist["^"];

	   (* square root *)
	    # esc_seq_match(c,KEYPADCOM) cor normal_match(c,"R") ==>
		fix_entry[];
		let
		    arg == sub[stack,stack_ptr];
		in
		    if is_neg[arg] then
			display_top[];
			werror["sqrt(negative)"];
		    else
			unop[stack_elem$sqrt];
		    fi
		ni;
		add_hist["R"];

	   (* pi *)
	    # normal_match(c,"P") ==>
		fix_entry[];
		display_top[];
		push[stack, stack_ptr, stack_elem$pi[]];
		display_top[];
		add_hist["P"];

	   (* sin *)
	    # normal_match(c,"S") ==>
		unop[stack_elem$sin];
		add_hist["S"];

	   (* cosine *)
	    # normal_match(c,"K") ==>
		unop[stack_elem$cos];
		add_hist["K"];

	   (* arctangent *)
	    # normal_match(c,"T") ==>
		unop[stack_elem$atan];
		add_hist["T"];

	   (* enter number onto stack *)
	    # (esc_seq cand c = KEYPADENT)
	      cor c = " "
	      cor ((c = "\n" cor c = "\r") cand not Xwindows) ==>
		let
		    y == gety[main_win];
		in
		    wmove[main_win,y,0];
		    wclrtobot[main_win];
		    fix_entry[];
		    display_top[];
		ni;
		add_hist[" "];

	   (* quit program *)
	    # c = eof_ch ==>
		wmove[hist_win,0,0];
		wput[hist_win,"\n"];
		wrefresh[hist_win];
		endwin[];
		exit[0];

	   (* redraw the screen *)
	    # c = redraw cor c = cond_redraw ==>
	      (* Suspend SIGWINCH in the middle of this.  This appears *)
	      (* to be essential under under SunOS 4.0, since SIGWINCH *)
	      (* appears to be stuttered in an rlogin session.         *)
		func [impure] val Void { extern "_block_sigwinch" } [];
	      let
		nlines == get_lines[];
		ncols == get_columns[];
		size_changed == nlines <> ScrnHgt cor ncols <> ScrnWid
				cor forced_resize[];
	      in
		if size_changed then
		  reset_size[ncols, nlines];
		  nl[]; nocbreak[]; echo[]; endwin[];
		  initscr[];
		  nonl[]; cbreak[]; noecho[];
		  make_windows[];
		fi;
		if c = redraw cor size_changed then
		  clear[];
		  wclear[hist_win];
		  wclear[bord_win];
		  if Help_avail then
		    wclear[info_win];
		  fi;
		  wclear[main_win];
		  refresh[];
		  box[bord_win,ChStr$Out[" "],ChStr$Out[" "]];
		  box[bord_win,ChStr$Out["|"],ChStr$Out["-"]];
		  if Help_avail then
		    box[info_win,ChStr$Out[" "],ChStr$Out[" "]];
		    box[info_win,ChStr$Out["|"],ChStr$Out["-"]];
		  fi;
		  print_help[];
		  update_info[];
		  if Help_avail then
		    wrefresh[info_win];
		  fi;
		  wrefresh[bord_win];
		  prec := Long$In[get_prec[]];
    		  add_hist[""];
		fi;
		func [impure] val Void { extern "_unblock_sigwinch" } [];
		(* Redisplay in any case, since a SIGWINCH interrupts *)
		(* computation.                                       *)
		display[];
		reprint_input[];
		wrefresh[hist_win];
		wrefresh[main_win];
	      ni;

	   (* duplicate top entry on stack *)
	    # normal_match(c,"Q") ==>
	        add_hist["Q"];
		fix_entry[];
		display_top[];
		let
		    top == sub[stack,stack_ptr]
		in
		    if (stack_ptr < stack_lim-1) ==>
			push[stack, stack_ptr, top];
			display_top[];
		    # else ==>
			wput[main_win,"\n"];
			werror[" stack too deep"];
			wrefresh[main_win];
		    fi
		ni;

	   (* exponential function *)
	    # normal_match(c,"X") ==>
		unop[stack_elem$exp];
		add_hist["X"];

	   (* natural logarithm *)
	    # normal_match(c,"L") ==>
		fix_entry[];
		let
		    arg == sub[stack,stack_ptr];
		in
		    if is_neg[arg] then
			display_top[];
			werror["log(negative)"];
		    elsif is_zero[arg] then
			display_top[];
			werror["log(0)"];
		    else
			unop[stack_elem$ln];
		    fi
		ni;
		add_hist["L"];

	    (* Truncate to integer *)
	    # normal_match(c,"Z") ==>
		let
		    abs_prec == -Long$1000;
		in
		    fix_entry[];
		    working_flag[];  (* Normally used only during *)
				     (* redisplay.  But here      *)
				     (* function evaluation       *)
				     (* takes time.               *)
		    unop[func[x: val stack_elem; impure]
			 { stack_elem$In[to_Long[x, abs_prec]] }
		    ];
		ni;
		add_hist["Z"];

	    # c = "!" ==>
		fix_entry[];
		let
		    arg == to_Long[sub[stack,stack_ptr], -Long$20];
		in
		    if stack_ptr < 0 cor
		       arg < Long$0 cor
		       are_unequal[stack_elem$In[arg],
				   sub[stack, stack_ptr]] then
		      display_top[];
		      werror("not pos int");
		    else
		      erase_top[]; pop[stack, stack_ptr];
		      stack_ptr += 1;
		      stack.stack_ptr :=
			    stack_elem$In[to_Long[(xLong$In[arg])!]];
		      display_top[];
		    fi
		ni;
		add_hist["!"];

	    # c = "#" ==>
		fix_entry[];
		add_hist["#"];
		let
		    arg == to_Long[sub[stack,stack_ptr], -Long$20];
		    sqrt_arg == to_Long[sqrt[abs[sub[stack,stack_ptr]]],
					-Long$20];
		    abs == func[x: val Long]
				{ if x < Long$0 then -x else x fi };
		    abs_arg == abs[arg];
		    answer == New[True];
		    factor == New[0];
		    i == New[2];
		    limit == if abs_arg > Long$100 then
				100
			     else
				Out[abs_arg] - 1
			     fi;
		in
		    if stack_ptr < 0 cor
		       are_unequal[stack_elem$In[arg],
				   sub[stack, stack_ptr]] then
		      display_top[];
		      werror("not integer");
		    elsif sqrt_arg*sqrt_arg = abs_arg then
		      display_top[];
		      print_msg "square";
		    else
		      (* working flag is still set from are_unequal call *)
		      do i < limit cand answer ==>
			if abs_arg % i = 0 then
			    factor := i;
			    answer := False;
			fi;
			i += 1;
		      od;
		      if answer then
			  answer := pt[abs_arg]
		      fi;
		      display_top[];
		      if answer then
			if abs_arg < Long$In[limit*limit]
			   cor abs_arg = Long$2 then
			    print_msg "prime      ";
			else
			    print_msg "prob. prime";
			fi
		      else
			if factor <> 0 then
			    print_msg ["factor: " ^* puts[factor]];
			else
			    print_msg "composite";
			fi
		      fi
		    fi
		ni;

	   (* ignore all other input *) 
	    # else  ==>
		skip;
	    fi
	  fi;
	  c := ChStr$In[wgetc[main_win]];
	 ni
	od;
    wmove[hist_win,0,0];
    wput[hist_win,"\n"];
    wrefresh[hist_win];
    endwin[];
    exit[0];
ni ni ni ni



