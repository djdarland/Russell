(*
 * Russell Compiler Help Program
 * Original author: Hans-J. Boehm
 * Revised 1/13/87 by Glenn Skinner to look at env variables
 *)
 
# include "root_path.h"

let
    DefaultBaseDirectory        == ROOT_PATH ^* "/src";
    DefaultPager		== "/usr/ucb/more";
    usage_line			== "Usage: rhelp [-k] [construct]\n";

    (*
     * Functions to interface to the host system.
     *)
    open == func[val ChStr; flags, mode: val Short; impure] val Short
                { extern "_open" };
    O_RDONLY == 0;  (* read flag for open *)
    system == func [val ChStr; impure] val Short { extern "_system" };
    execl == func [name, arg0, arg1: val ChStr; val Short; impure] val Short
                  { extern "_execl" };
    exec2 == func [name, arg: val ChStr; FS: impure]
                  { execl[name, name, arg, 0] };
    chdir == func [val ChStr; impure] val Short { extern "_chdir" };
    getenv == func[val ChStr; impure] val ChStr { extern "_getenv" };
    (* We use Short below to accomodate C semantics for boolean values. *)
    inenv == func[val ChStr; impure] val Short { extern "_inenv" };

    to_lower == func [x: val ChStr] val ChStr {
	let
	    first == substr[x,0,1];
	    infinity == 100;
	in
	    if
		len[x] = 0 ==> x
	    #   else ==>
		     if
			Out[first] <= Out['Z'] cand
			Out[first] >= Out['A'] ==>
			   ChStr$In[Out[first] + 32]
		     #   else ==>
			   first
		     fi  ^* to_lower[substr[x,1,infinity]]
	    fi
	ni
    };
    (*
     * Return the value of the environment variable env,
     * or default if env isn't set in the environment.
     *)
    GetenvDefaulted == func[env, default: val ChStr; impure] val ChStr {
	if
	    inenv[env] <> 0 ==> getenv[env]
	#   else ==> default
	fi
    };
    (* N.B.: wired in subdirectory structure. *)
    help_dir == GetenvDefaulted["RUSSELL_BASE", DefaultBaseDirectory]
		^* "/help/";
    MORE == GetenvDefaulted["PAGER", DefaultPager]
in
    if
        argc = 1 ==>
            system[MORE ^* " " ^* help_dir ^* "general"];
            put["Available help files are:\n"];
            exec2["/bin/ls", help_dir]
    #   argc = 2 ==>
            let
                fn == help_dir ^* to_lower[argv[1]]
            in
                if
                    open[fn, O_RDONLY, 0] >= 0  ==>
			system[MORE ^* " " ^* help_dir ^* to_lower[argv[1]]];
                #   else  ==>
                        put["No help available for "];
                        put[argv[1]];
                        put["\nAvailable help files are:\n"];
                        exec2["/bin/ls", help_dir];
                fi
            ni
    #   argc = 3 ==>
            chdir[help_dir];
            if
                argv[1] = "-k"  ==>
                    system["fgrep -i " ^* argv[2] ^* " *"
                           ^* " | " ^* MORE]
            #   argv[2] = "-k"  ==>
                    system["fgrep -i " ^* argv[1] ^* " *"
                           ^* " | " ^* MORE]
            #   else ==>
                    put[usage_line]
            fi
    #   else ==>
            put[usage_line]
    fi
ni
