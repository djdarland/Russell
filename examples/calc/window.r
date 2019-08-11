(* curses interface with Russell *)

#define skip Null[]

func [] {
    let
	window == extend { Short } with W {

	    Mk == func[w,x,y,z: val Short; impure] val W
		     { extern "_get_newwin" };

	    delwin == func[x: val W; impure] val Void
			{ extern "_delwin" };

	  (* curses functions *)

	    wstandout == func[w: val W; impure] val Void
		{ extern "_wstandout" };

	    wstandend == func[w: val W; impure] val Void
		{ extern "_wstandend" };

	    wmove == func[w: val W; y,x: val Short; impure] val Void
		{ extern "_wmove" };

	    touchwin == func[w: val W; impure] val Void
		{ extern "_touchwin" };

	    wrefresh == func[w: val W; impure] val Void
		{ extern "_wrefresh" };

	    initscr == func[impure] val Void { extern "_initscr" };

	    box == func[w: val W; vert,hor: val Short; impure] val Void
		{ extern "_box" };

	    wclear == func[w: val W; impure] val Void { extern "_wclear" };
	    wclrtoeol == func[w: val W; impure] val Void
		{ extern "_wclrtoeol" };
	    wclrtobot == func[w: val W; impure] val Void
		{ extern "_wclrtobot" };

	    endwin == func[impure] val Void { extern "_endwin" };
	    werase == func[w: val W; impure] val Void { extern "_werase" };

	  (* curses macros converted *)

	    nonl == func[impure] val Void { extern "_nonlM" };
	    nl == func[impure] val Void { extern "_nlM" };
	    nocbreak == func[impure] val Void { extern "_nocbreakM" };
	    cbreak == func[impure] val Void { extern "_cbreakM" };
	    noecho == func[impure] val Void { extern "_noechoM" };
	    echo == func[impure] val Void { extern "_echoM" };


	  (* other things that an interface was necessary for *)

	    wput == func[w: val W; s: val ChStr; impure] val Void
		{ extern "_putwin" };
	    gety == func[w: val W; impure] val Short { extern "_gety" };
	    getx == func[w: val W; impure] val Short { extern "_getx" };
	    wgetc == func[w: val W; impure] val Short
		{ extern "_win_getch" };
	};
    in
	window
	export { Mk; delwin; New <<func[] var window>>; :=; V;
		 wmove; wrefresh; initscr; box; wclear; wclrtoeol; 
		 wclrtobot; nonl; nl; nocbreak; cbreak; noecho; echo;
		 wput; gety; getx; wgetc;
		 endwin; werase; wstandout; wstandend; touchwin;}
    ni
}


	    
