/*
 *               Russell compiler driver
 *
 *  Syntax: rc [-flags] file [file.o ...]
 *
 *  File names ending in ".o" are assumed to be (non-Russell) object
 * files to be linked in. 
 *
 *  Flags:
 *      B   - Debugging flag: see rc
 *      D   - Debugging flag: see rc
 *      i   - initialize compiler (implies -U, unless -I is present)
 *      d   - identical to i except generates demand loadable initialized
 *            compiler on the VAX.
 *      I   - invoke the initialized compiler (default)
 *      U   - invoke the uninitialized compiler
 *      a   - generate abstract syntax instead of VAX code (defunct)
 *      A   - generate alternate machine code - VAX on a SUN, or SUN
 *            on a VAX
 *      m   - generate ML abstract machine code (defunct)
 *      G   - produce general, machine independent, intermediate code
 *      O   - invoke the "optimizer".  Repeated to invoke the
 *            intermediate code optimizer.  Tripled to unroll loops once.
 *      c   - generate a .o file with signature information, which can
 *            be linked into another program.
 *      S   - do not assemble the object program.  Leave it around
 *            as a .s file
 *      r   - leave the result relocatable (passed on to loader)
 *      p   - run the standard C preprocessor on the source
 *      L   - interpret import rule Liberally.  Allow functions
 *            with trailing var Void argument to import variables
 *      P   - enable execution profiling
 *      T   - enable tracing
 *      V   - print known call graph and opt. info to stdout
 *      N   - no use of Callcc
 *      f   - generate intermediate code that does not explicitly
 *            update the stack pointer.
 *            However, a deallocation instruction preceded by HINT
 *            ONS is assumed to turn into a noop, at least if the
 *            size is blatantly constant.
 *            (Used for RIC to C translator.)
 *      F   - same as f, but also arrange that all GAR instructions
 *            precede any real code, and that BSF functions
 *            don't access the global frame pointer.  No assumptions
 *            are made about HINT ONS.
 *            (Used for RIC to ILOC translator.)
 *      R   - generate intermediate code that maintains identifier
 *            bindings in virtual registers whenever possible.  Probably
 *            useful only in the presence of a good global register
 *            allocator for the target machine.
 *	l<name>
 *	    - each instance in the command line of this option is passed
 *	      on to ld (this is a library link)
 *	o <name>
 *	    - put the result in file named <name> instead of a.out
 *      C   - (cleanup) remove any .s or .G files created
 *      h   - heap allocate all variables and allocation records
 *            (only useful to measure the impact of these optimizations)
 *      u   - Use a user supplied version of rc.x or Grc.x, the main
 *            compiler executable.
 *   	X   - Generate C callable functions for components of the type
 *            denoted by the expression in the source file.
 *	x   - generate executable compatible with Xerox Portable Common
 *    	      Runtime.  All heap allocation is done with explicit calls.
 *            
 *
 *  If the intermediate code generator is used, in the most general case,
 * the sequence of activated processes is:
 *
 *      1) C preprocessor
 *      2) Grc.x, the body of the compiler
 *      3) RICfilter, a local optimizer for Russell Intermediate Code (with -O)
 *      4) RICopt, a static garbage collector (with -OO)
 *      5) cg, a final code generator
 *      6) as
 *      7) ld
 *
 */

# include <stdio.h>
# include <signal.h>
# include <a.out.h>
# include <sys/file.h>
# include <sys/types.h>
# include <sys/stat.h>
# include "../parm.h"
# include "mesg.h"

# ifdef malloc
#   undef malloc    /* We want the real one */
#   undef free
# endif

# define DEBUG DEBUG


char *ppname; 
char *rcname;
char *optname;
char *RICoptname;
char *RICfiltername;
char *cgname;

/* Unique names for temporary files */
char *rctmp0name;   /* Preprocessor output */
char *rctmp1name;   /* Optimizer input */
char *rctmp2name;   /* Signature and Rc optimization info out */
char *rctmp3name;   /* Intermediate code optimizer data flow info in  */
char *rctmp4name;   /* intermediate code optimizer data flow info out */
		    /* also used as input file to RICfilter           */

/* Flag variables.
 */
boolean aflag;              /* generate abstract syntax */
boolean oflag;		    /* rename a.out as given file name */
boolean Oflag;              /* optimize for execution speed */
boolean OOflag;             /* invoke RIC optimizer */
boolean OOOflag;            /* unroll loops once */
boolean Sflag;              /* do not assemble */
boolean cflag;              /* incomplete program */
boolean rflag;              /* relocatable object file */
boolean pflag;              /* Run C preprocessor */
boolean Pflag;              /* Execution profiling */
boolean Gflag;              /* Intermediate code */
boolean CGflag;             /* Run separate code generator */
boolean Cflag;              /* clean up afterwards */
boolean fflag;              /* No stack pointer updates */
boolean Fflag;              /* RT style stack frames */
boolean Rflag;              /* Locals in registers */
boolean Xflag;              /* Externally callable code */
boolean xflag;		    /* PCR code */
boolean uflag;              /* Use rc.x from current directory */
boolean Vflag;		    /* Print optimization info */

boolean take_next= FALSE;   /* next argument is final_name */
char *lppflags = "";        /* Legal preprocessor flags. */
char *lpmnflags  = "BDidaMpLPTVNfFRhx"; /* Legal Rc flags.            */
				        /* G,c are handled separately */
char *lprtflags  = "IUOGcSArlCXOu";  /* legal flags, used only by   */
 				     /* the root process.   	    */
char appflags[100];         /* Actual preprocessor flags. */
char apmnflags[100];        /* Actual rc flags. */

char *objfiles[MAXNOBJFILES];     /* object files that need to be linked */
				  /* with this program                   */
int nobjfiles = 0;       /* number of valid entries in objfiles */

char *libraries[MAXNLIBRARIES];   /* libraries needed for linking */
int nlibraries = 0;      /* number of valid entries on libraries */

char * malloc();

int intr_handler();

/* Add a copy of the file name ofile to the list of object files to be loaded */
void add_objfile(ofile)
char * ofile;
{
    char * nfn = malloc(strlen(ofile)+1);
    int i;
    boolean duplicate = FALSE;

    if (nobjfiles >= MAXNOBJFILES) {
        fprintf(stderr, "Too many object files\n");
        exit(1);
    }
    strcpy(nfn,ofile);
    for (i = 0; i < nobjfiles; i++) {
	if (strcmp(nfn, objfiles[i]) == 0) {
/*
            fprintf(stderr,
                    "Warning: External file %s mentioned twice\n",
		    nfn);
*/
            duplicate = TRUE;
        }
    }
    if (!duplicate) {
      objfiles[nobjfiles++] = nfn;
    }
}

/* Name of Rc to execute. */
char Rcname[15] = "";

/* Name of code file produced by main pass or RICfilter, if invoked */
char * codefilename;

/* Name of code file produced by main compiler pass */
char * main_out_fname;

char * outfilename();
char * mktemp();

boolean dloadflag = FALSE;  /* generate a demand loadable initialized */
                            /* compiler.                              */

main(argc,argv)
int argc;
char **argv;
{   int argf;   /* After all flags are                              */
                /* are read argv[argf] is the file name.            */
    int status;
    extern exit();
    boolean compile_only = FALSE;
    int argn;

    /* If quit is sent, just exit (w/o core dump). */
        signal(SIGQUIT, exit);

    /* Clean up after SIGINT */
        signal(SIGINT, intr_handler);

    ppname = PPNAME;
    rcname = RCNAME;
    optname = OPTNAME;
    cgname = CGNAME;
    RICoptname = RICOPTNAME;
    RICfiltername = RICFILTERNAME;

    argf = -1;

#   ifndef VAX
      /* Initialize to use separate code generator */
	Gflag = TRUE;
	CGflag = TRUE;
#   endif
    /* Process any flag options. */
        strcpy(appflags, "-");
        strcpy(apmnflags, "-");
        for (argn = 1; argn < argc; argn++) {
	 if (take_next) {
	  int al = strlen(argv[argn]);
	  if (strcmp(argv[argn]+al-2,".o")==0 || 
	      strcmp(argv[argn]+al-2,".r")==0) {
	   fprintf(stderr,"Questionable -o usage: Might overwrite %s.\n",
								argv[argn]);
	   goto usage;
	  } else {
	   add_objfile(argv[argn]);
	   take_next = FALSE;
	  }
         } else if ( argv[argn][0] == '-' ) {
          char *flagp;
          char *farg = "0";
	  switch( argv[argn][1] ) {	/* may be special or just flag */

	   case 'l' :           /* library */
	    if (nlibraries >= MAXNLIBRARIES) {
		fprintf(stderr, "Too many libraries\n");
		exit(1);
	    }
	    libraries[nlibraries++] = argv[argn];
	    break;

	   case 'o' :		/* put output in file argv[argn+1] */
	    oflag = TRUE;
	    add_objfile(argv[argn]);	/* make -o option an obj file */
	    take_next = TRUE;
	    break;

	   default:

            for (flagp = &(argv[argn][1]); *flagp != 0; flagp++) {
                farg[0] = *flagp;
                if ( index(lppflags, *flagp) != 0 ) {
                    strcat(appflags, farg);
                } else if ( index(lpmnflags, *flagp) != 0 ) {
                    strcat(apmnflags, farg);
		} else if ( index(lprtflags, *flagp) == 0 ) {
                    fprintf(stderr, "Bad flag: %c\n", *flagp);
                }
                switch ( *flagp ) {
                    case 'd' :
                        dloadflag = TRUE;
                        /* Otherwise identical to i */
                    case 'i' :
			/* Unless otherwise specified, use Rc. */
			    if ( strcmp(Rcname, "") == 0 )
                                strcpy(Rcname, "Rc");
                        compile_only = TRUE;
                        break;
		    case 'I':
			if (Gflag) {
			  strcpy(Rcname, "rc.x");
			} else {
			  strcpy(Rcname, "Grc.x");
			}
			break;
		    case 'C':
			Cflag = TRUE;
			break;
                    case 'U':
			strcpy(Rcname, "Rc");
			break;
		    case 'a':
			aflag = TRUE;
			break;
		    case 'A':
			Sflag = TRUE;
#                       ifdef VAX
			  CGflag = TRUE;
			  Gflag = TRUE;
#                       else
			  CGflag = FALSE;
			  Gflag = FALSE;
#                       endif
			break;
		    case 'O':
			if (OOflag) {
			  OOOflag = TRUE;
			} else if (Oflag) {
			  OOflag = TRUE;
			} else {
			  Oflag = TRUE;
			}
                        break;
                    case 'c':
                        if (nobjfiles != 0) {
                          fprintf(stderr, "Warning: .o files ignored with -c\n");
                        }
			cflag = TRUE;
			break;
		    case 'X':
			Xflag = TRUE;
			break;
                    case 'S':
			Sflag = TRUE;
			break;
                    case 'r':
                        rflag = TRUE;
                        break;
                    case 'p':
                        pflag = TRUE;
                        break;
                    case 'P':
                        Pflag = TRUE;
			break;
		    case 'G':
			Gflag = TRUE;
			CGflag = FALSE;
			compile_only = TRUE;
			break;
		    case 'f':
			fflag = TRUE;
			break;
		    case 'F':
			fflag = TRUE;
			Fflag = TRUE;
			break;
		    case 'R':
			Rflag = TRUE;
			break;
		    case 'u':
			uflag = TRUE;
			break;
		    case 'x':
		        xflag = TRUE;
		        break;
		    case 'V':
		    	Vflag = TRUE;
		    	break;
                } /* end switch (*flagp) */
            } /* end for (flagp...) */
	  } /* end switch( argv...) */
	 } else {
	    /* argv[argn] not flag argument */
            int arg_len = strlen(argv[argn]);
            if (argv[argn][arg_len-1] == 'o'
                && argv[argn][arg_len-2] == '.') {
                if (cflag) {
                    fprintf(stderr, "Warning: .o files ignored with -c\n");
                }
                add_objfile(argv[argn]);
            } else { /* Russell source file */
                if (argf == -1) { /* first one */
                    argf = argn;
                } else {
                    fprintf(stderr, "One source file at a time, please!\n");
                    goto usage;
                }
            } /* end if (argv...) */
         } /* end if (argv[argn][0] == '-')... */
        } /* end for (argn...) */

    if (argf == -1) {
        fprintf(stderr, "No Russell source file\n");
        goto usage;
    }

    if (cflag && Gflag && !CGflag) {
	fprintf(stderr, "Warning: -G suppresses -c\n");
	cflag = FALSE;
    }


    if (Xflag && cflag) {
	fprintf(stderr, "Warning: -X suppresses -c\n");
	cflag = FALSE;
    }

#   ifdef RT
#     ifndef GEN_C
       /* -f is mandatory for RT code generator  */
	if (CGflag) {
	    if (!Fflag) {
		strcat(apmnflags, "F");
	    }
	    Fflag = TRUE;
	}
#     endif
#   endif

#   ifdef GEN_C
      /* RIC-to-C translator expects at least -f */
	if (CGflag) {
	    if (!Fflag) {
		strcat(apmnflags, "f");
	    }
	}
#   endif

    if (Gflag) {
	strcat(apmnflags, "G");
	if (Rcname[0] != 'R') {
	    /* Haven't seen -U */
	    strcpy(Rcname, "Grc.x");
	}
    }


    if (Xflag) {
	strcat(apmnflags, "X");
    }

    if (cflag) {
	strcat(apmnflags, "c");
    }

    if (Oflag) {
	strcat(apmnflags, "O");
    }

    if (OOflag) {
	if (!Gflag) {
	    fprintf(stderr, "-OO was ignored - works only in conjunction with intermediate code generator\n");
	} else if (OOOflag) {
	    strcat(apmnflags, "OO");
	} else {
	    strcat(apmnflags, "O");
	}
    }

    /* Generate temp file names */
        rctmp0name = mktemp("/tmp/rctmp0XXXXXX");
        rctmp1name = mktemp("/tmp/rctmp1XXXXXX");
	rctmp2name = mktemp("/tmp/rctmp2XXXXXX");
	if (Gflag & Oflag) {
	  rctmp4name = mktemp("/tmp/rctmp4XXXXXX");
	}
	if (OOflag) {
	  rctmp3name = mktemp("/tmp/rctmp3XXXXXX");
	}

    /* Remove old object file list */
        unlink(OBJFILELIST);

    /* Generate file name for code output from main compiler or RICfilter */
	if (OOflag || Oflag && ! Gflag) {
	    codefilename = rctmp1name;
	} else {
	    codefilename = outfilename(argv[argf], (Gflag? 'G' : 's'));
	}

    /* Generate file name for code output from main compiler */
	if (Oflag & Gflag) {
	    main_out_fname = rctmp4name;
	} else {
	    main_out_fname = codefilename;
	}

    if (pflag) {
      /* Fork and exec preprocessor with stdout going to rctmp0 */
        if( rfork() == 0 ) {
            fclose(stdout);
            if( fopen( rctmp0name, "w" ) == NULL ) {
                fprintf(stderr, "Cannot create %s\n", rctmp0name);
                exit(1);
            }
            execl(ppname, "cpp", appflags, argv[argf], 0 );
            /* exit */

            fprintf(stderr, "Can't exec preprocessor\n");
            exit(1);
        }

      /* Wait for preprocessor to finish, then report its status. */
        wait( &status );
        if( status != 0 ) {
            fprintf(stderr, "Compilation terminated after preprocessing.\n");
            printstatus(status);
            rmfiles();
            exit(1);
        }
    }

    /* Fork and exec rc */
        if( rfork() == 0 ) {
	    char fullrcname[150];

	    if (uflag) {
		fullrcname[0] = '\0';
	    } else {
		strcpy(fullrcname, rcname);
	    }
	    if (Rcname[0] == 0)
		strcat(fullrcname, "rc.x");
            else
		strcat(fullrcname, Rcname);
#           if vax
              if(dloadflag) {
		/* Check that current version of Rc is demand loadable */
                    int rcdescr;
		    long magic_n;   /* magic number of Rc file */

                    if ((rcdescr = open(fullrcname,0)) == -1 ||
                         read(rcdescr, &magic_n, sizeof (long)) <= 0) {
                            fprintf(stderr,"Can't read %s\n", fullrcname);
                            rmfiles();
                            exit(1);
                    }
                    if (magic_n != ZMAGIC && magic_n != NMAGIC) {
                        fprintf (stderr,"Can't generate demand loadable file from old format a.out file\n");
                    }
                    close(rcdescr);
              }
#           endif
	    if (cflag && OOflag) {
	      execl(fullrcname, "Rc", argv[argf],
                    apmnflags, pflag? rctmp0name : argv[argf],
		    main_out_fname,
		    rctmp2name, rctmp3name, 0 );
	    } else if (cflag) {
	      execl(fullrcname, "Rc", argv[argf],
                    apmnflags, pflag? rctmp0name : argv[argf],
		    main_out_fname,
                    rctmp2name, 0 );
	    } else if (OOflag) {
	      execl(fullrcname, "Rc", argv[argf],
                    apmnflags, pflag? rctmp0name : argv[argf],
		    main_out_fname,
		    rctmp3name, 0 );
	    } else {
	      execl(fullrcname, "Rc", argv[argf],
                    apmnflags, pflag? rctmp0name : argv[argf],
		    main_out_fname, 0 );
	    }
            /* exit */

            fprintf(stderr, "Can't exec %s\n", fullrcname);
            rmfiles();
            exit(1);
        }

    /* Wait for rc to finish then report its status. */
        wait( &status );
        if( status != 0 ) {
	    fprintf(stderr, "Compilation terminated prematurely.\n");
            printstatus(status);
            rmfiles();
            exit(1);
        }

    /* Add object files to list */
      {
#       define MAXFILELEN 100
        FILE * objfilelist  = fopen(OBJFILELIST, "r");
        char namebuf[MAXFILELEN+1];
        int len;
        int retcode;

        if (objfilelist != NULL) {
            while (!feof(objfilelist)) {
                retcode = (int) fgets(namebuf, MAXFILELEN, objfilelist);
                /* delete trailing newline */
                  {
                    len = strlen(namebuf);
                    if (namebuf[len-1] == '\n') {
                      namebuf[--len] = 0;
                    }
                  }
                if (retcode != NULL && len != 0) {
                    add_objfile(namebuf);
                }
            }
        }
      }

    if (Oflag && !Gflag) {
      /* Fork and exec Vax "optimizer" */
	if( rfork() == 0 ) {
	    char * optfilename = outfilename(argv[argf], 's');

	    if( freopen( optfilename, "w", stdout ) == NULL ) {
		fprintf(stderr, "Cannot create %s\n", optfilename);
                exit(1);
            }
	    execl("/bin/awk", "awk", "-f", OPTNAME,
		  codefilename, 0);
            /* exit */

	    fprintf(stderr, "Can't exec /bin/awk\n");
	    exit(1);
        }

	/* Wait for optimizer to finish, then report its status. */
	  wait( &status );
	  if( status != 0 ) {
	    fprintf(stderr, "Optimization failed\n");
	    printstatus(status);
	    rmfiles();
	    exit(1);
	  }
    } /* if (Oflag && !Gflag) ... */

    if (Oflag && Gflag) {
      /* Fork and exec RICfilter */
	if( rfork() == 0 ) {
	    if( freopen( codefilename, "w", stdout ) == NULL ) {
		fprintf(stderr, "Cannot create %s\n", codefilename);
                exit(1);
            }
	    if( freopen( main_out_fname, "r", stdin ) == NULL ) {
		fprintf(stderr, "Cannot create %s\n", main_out_fname);
                exit(1);
            }
	    execl(RICfiltername, "RICfilter", 0);
	    /* exit */

	    fprintf(stderr, "Can't exec %s\n", RICfiltername);
	    exit(1);
        }

	/* Wait for RICfilter to finish, then report its status. */
	  wait( &status );
	  if( status != 0 ) {
	    fprintf(stderr, "Local RIC optimization failed\n");
	    printstatus(status);
	    rmfiles();
	    exit(1);
	  }
    } /* if (Oflag && Gflag) ... */

    if (OOflag) {
      /* Fork and exec RIC optimizer */
	if( rfork() == 0 ) {
	    char * optfilename = outfilename(argv[argf], 'G');

	    execl(RICoptname, "RICopt", (Vflag? "-verbose" : "-"),
	          codefilename, optfilename, rctmp3name,
		  cflag? rctmp4name : "/dev/null", 0);
            /* exit */

	    fprintf(stderr, "Can't exec %s\n", RICoptname);
	    exit(1);
        }

	/* Wait for optimizer to finish, then report its status. */
	  wait( &status );
	  if( status != 0 ) {
	    fprintf(stderr, "Optimization failed\n");
	    printstatus(status);
	    rmfiles();
	    exit(1);
	  }
    } /* if (OOflag) ... */

    if (compile_only) {
        rmfiles();
        exit(0);
    }

    if (CGflag) {
      char * gf_name;
      char * global_ar_name();
      
      if (xflag && !cflag) {
          /* This is a top level file destined to be run in a */
          /* multi-threaded world.  Storing the global frame  */
          /* pointer in a single global variable wont do.     */
          /* instead we set things up so that each function   */
          /* will know to get GF from the right place.  This  */
          /* isn't necessary in subsidiary compilations,      */
          /* they only see a part of the global frame that    */
          /* should be identical for everybody.		      */
          gf_name = global_ar_name(argv[argf]);
      } else {
          gf_name = "global_ar";
      }
      /* Fork and exec final code generator */
	if( rfork() == 0 ) {
	    if (Oflag) {
	      execl(cgname, cgname,
		    (xflag? "-Ox" : "-O"),
		    outfilename(argv[argf], 'G'),
#               ifndef RT
		    outfilename(argv[argf], 's'),
#               endif
#		ifdef GEN_C
		    gf_name,
#		endif
		    0);
	    } else if (xflag) {
	      execl(cgname, cgname,
		    "-x",
		    outfilename(argv[argf], 'G'),
#               ifndef RT
		    outfilename(argv[argf], 's'),
#               endif
#		ifdef GEN_C
		    gf_name,
#		endif
		    0);
	    } else {
	      execl(cgname, cgname,
		    outfilename(argv[argf], 'G'),
#               ifndef RT
		    outfilename(argv[argf], 's'),
#               endif
#		ifdef GEN_C
		    gf_name,
#		endif
	      0);
	    }
	    /* exit */

	    fprintf(stderr, "Can't exec %s\n", cgname);
	    rmfiles();
            exit(1);
        }

	/* Wait for code generator to finish, then report its status. */
	  wait( &status );
	  if( status != 0 ) {
	    fprintf(stderr, "Final code generation failed\n");
	    printstatus(status);
	    rmfiles();
	    exit(1);
	  }
    } /* if (CGflag) ... */

    if (!Sflag) {
      /* assemble the result */
        if (rfork() == 0) {
	    execl("/bin/as", "as", "-o", outfilename(argv[argf], 'o'),
					 outfilename(argv[argf], 's'), 0);
            fprintf(stderr, "Can't exec /bin/as\n");
            rmfiles();
            exit(1);
        }
        wait(&status);
        if (status != 0) {
            fprintf(stderr, "Assembly failed\n");
            printstatus(status);
            rmfiles();
            exit(1);
        }

	if (!cflag && !Xflag) {
          char * ld_argv[MAXNOBJFILES + 10];
          int ld_argc = 0;
          int i;

          /* Add current file and standard files to list to be loaded */
	    add_objfile(outfilename(argv[argf], 'o'));
	    if (!rflag) {
#               ifdef RT
                  add_objfile(strcat(RROOT, "/rt_runtime/rrt0.o"));
#               else
                  add_objfile(strcat(RROOT, "/runtime/rrt0.o"));
#               endif
	      /* Add C library to list of libraries */
		if (nlibraries >= MAXNLIBRARIES) {
		  fprintf(stderr, "Too many libraries\n");
		  exit(1);
		}
		if (Pflag) {
		  libraries[nlibraries++] = "/usr/lib/libc_p.a";
		} else {
		  libraries[nlibraries++] = "/lib/libc.a";
		}
	    }

          /* build argument vector for ld command */
            ld_argv[ld_argc++] = "ld";
            if (rflag) {
              /* Make object file relocatable */
                ld_argv[ld_argc++] = "-r";
	    } else {
		ld_argv[ld_argc++] = "-e";
		ld_argv[ld_argc++] = "start";
		if (Pflag) {
		    ld_argv[ld_argc++] = "/lib/gcrt0.o";
		} else {
		    ld_argv[ld_argc++] = "/lib/crt0.o";
		}
	    }
            for (i = 0; i < nobjfiles; i++) {
                ld_argv[ld_argc++] = objfiles[i];
            }
	    for (i = 0; i < nlibraries; i++) {
		ld_argv[ld_argc++] = libraries[i];
            }
            ld_argv[ld_argc++] = 0;
          /* Fork and exec ld command */
            if (rfork() == 0) {
	      execv("/bin/ld", ld_argv);
              fprintf(stderr, "Can't exec /bin/ld\n");
              rmfiles();
              exit(1);
            }
            wait(&status);
            if (status != 0 && (!rflag /* needed due to ld misfeature */)) {
              fprintf(stderr, "Linking failed\n");
              printstatus(status);
            }
	} else if (cflag) {
	  /* append signature and optimization information to end of .o file */
            FILE * rctmp2 = fopen(rctmp2name,"r");
	    FILE * ofile = fopen(outfilename(argv[argf], 'o'), "r+");
            long strsize;
            struct exec header;
            extern char * etext;
            char c;
#           if defined(SUN) && defined(EXTRA_MAGIC)
	      struct extra_sections es;
	      unsigned long es_start;	/* Start position of extra section */
	      				/* header.			   */
	      unsigned long es_length;
#	    endif

            if (ofile == NULL) {
		fprintf(stderr, "Can't open %s\n",
				outfilename(argv[argf], 'o'));
                rmfiles();
                exit(1);
            }
            if (rctmp2 == NULL) {
                fprintf(stderr, "Can't open %s\n", rctmp2);
                rmfiles();
                exit(1);
            }
            fread(&header, sizeof (struct exec), 1, ofile);
            fseek(ofile, N_STROFF(header), 0);
            fread(&strsize, sizeof (long), 1, ofile);
	    fseek(ofile, N_STROFF(header) + strsize, 0);
	    
#           if defined(SUN) && defined(EXTRA_MAGIC)
	      /* Leave space for extra section header and length */
	        es_start = ftell(ofile);
	        fseek(ofile, sizeof (struct extra_sections) + sizeof(int), 1);
#	    endif

	    if (OOflag) {
		/* Copy file of data flow info, preceded by its size */
		struct stat buf;
		FILE * rctmp4 = fopen(rctmp4name,"r");
		int count;

		if (rctmp4 == NULL) {
		    fprintf(stderr, "Optimizer failed to write flow info\n");
		    putw(0, ofile);
		} else {
		    fstat(fileno(rctmp4), &buf);
		    putw(buf.st_size, ofile);
		    count = 0;
		    while (c = getc(rctmp4), !feof(rctmp4)) {
			putc(c, ofile); count ++;
		    }
		    if (count != buf.st_size) {
			fprintf(stderr, "Stat returned incorrect size for %s\n",
				rctmp4name);
			abort(count, buf.st_size);
		    }
		}
	    } else {
		putw(0, ofile);
	    }
	    /* Copy file consisting of &etext, objfilelist, and signature */
	      while (c = getc(rctmp2), !feof(rctmp2)) {
		putc(c, ofile);
              }
#           if defined(SUN) && defined(EXTRA_MAGIC)
	      /* Write the extra section header and length */
	        es_length = ftell(ofile) - es_start
	                    - sizeof (struct extra_sections)
	                    - sizeof(int);
	        fseek(ofile, es_start, 0);
	        es.extra_magic = EXTRA_MAGIC;
	        es.extra_nsects = 1;
	        if (!fwrite(&es, sizeof (struct extra_sections), 1, ofile)) {
	            fprintf(stderr, "Couldnt write extra sections hdr\n");
	        }
	        putw(es_length, ofile);
#	    endif

            fclose(ofile);
            fclose(rctmp2);
        }
    }

    if (Cflag) {
      if (!Gflag || CGflag) {
	unlink(outfilename(argv[argf],'G'));
      }
      if (!Sflag) {
	unlink(outfilename(argv[argf],'s'));
      }
#     ifdef GEN_C
	unlink(outfilename(argv[argf],'c')); 
#     endif  
      unlink(OBJFILELIST);
    }
    rmfiles();
    exit(0);

usage:
        fprintf(stderr, "Usage: rc [-%s%s%s] filename\n",
                lppflags, lpmnflags, lprtflags);
        exit(1);
}


/*
 *  Convert a ___.r filename into a ___.x filename
 */
char *outfilename(str, x)
char *str;
char x;
{
register char *p, *q;
char * result;

    result = (char *)malloc(strlen(str) + 4);
    p = str; q = result;
    while( *p )
        *q++ = *p++;
    q--;
    if( (q[-1] == '.') && (q[0] == 'r') ) {
	*q++ = x;
        *q++ = 0;
    } else {
	sprintf(result, "a.%c", x);
    }
    return(result);
}


/*
 *  Given a file name, return the corresponding name for the global
 * activation record pointer.
 */
char * global_ar_name(s)
char *s;
{
register char * result = (char *)malloc(strlen(s) + 20);
register char * buf = (char *)malloc(strlen(s) + 1);
int len = strlen(s);

    strcpy(buf,s);
    if (buf[len-1] == 'r' && buf[len-2] == '.') {
        buf[len-2] = '\0';
    }
    sprintf(result, "%s_ar_save_loc", buf);
    free(buf);
    return(result);
}


/*
 * printstatus(s)
 *
 *  Give the reason the child died, if not from natural causes.
 */
printstatus(s)
unsigned s;
{   unsigned e = s & 0177;

    if ( mesg[e] != 0 ) {
        fprintf(stderr, "Reason: %s", mesg[e]);
        if ( s & 0200 )
            fprintf(stderr, " -- Core was dumped\n");
        else
            fprintf(stderr, "\n");
    }
}


/*
 *  rfork()
 *
 *  fork() and check that it worked. Return fork() code.
 */
rfork()
{   register int forkcode;
    forkcode = fork();
    if (forkcode == -1) {
        fprintf(stderr, "Can't fork!!!\n");
        exit(1);
    }
    return(forkcode);
}

/*
 * remove temporary files
 */
rmfiles()
{
    unlink(rctmp0name);
    unlink(rctmp1name);
    unlink(rctmp2name);
    if (OOflag) {
      unlink(rctmp3name);
    }
    if (Oflag & Gflag) {
      unlink(rctmp4name);
    }
}

/*
 * handler for interrupt signal
 */
intr_handler()
{
    rmfiles();
    exit(1);
}
