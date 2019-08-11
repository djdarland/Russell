
#
# define DEBUG DEBUG
# define LDBUG          /* etext set incorrectly */
char copyright1[] = "Copyright 1986-1989 Hans-Juergen Boehm, Alan J. Demers, Kumar Srikantan, Vernon Lee, and Lucy Hederman.\n";
char copyright2[] = "Copyright (c) 1990-1993 by Xerox Corporation.  All rights reserved.\n";
char copyright3[] = "This material is provided as is, with no warranty expressed or implied.  Any use is at your own risk.\n";

/*
 * Russell compiler -- Passes 1 through 5
 * 
 *  Pass 1 :
 *     initial syntax analysis
 *  
 *  Pass 2 :
 *     symbol table generation
 *
 *  Pass 3 :
 *     minor tree massaging
 *
 *  Pass 4:
 *     signature deduction and checking
 *
 *  Pass 5c:
 *     Vax code generation
 *
 *  Pass5d:
 *     Intermediate code generation (run instead of pass5c)
 *
 * Input: 
 *  ifile: preprocessor output
 *      stdin is reopened as ifile
 *
 * Output :
 *  ofile: VAX assembly code or intermediate code.
 *  sigfile: contains signature of program (-c flag only)
 *  optfile: contains the concatenation of optimization info from
 *           files mentioned in "extern" expressions
 *
 * Diagnostics :
 *  written to stderr
 *
 * Compiler control messages :
 *  written to stderr
 *
 *
 * Calling Sequence:
 * Rc [-BDSidaMcLPTGfVxXhOOOfF] actual_ifile ifile ofile [sigfile] [optfile]
 *          sigfile should appear only with -c
 *          optfile should appear only with -OO
 *          actual_ifile is the name of the original input file.
 *                  It is never opened, but is used to build function names
 *
 * Flags:
 *      B   - Debugging flag. Turn on bad allocation debug.
 *      D   - Debugging flag. Turn on yydebug.
 *      S   - Debugging. Catch signals and flush buffers before aborting.
 *      i   - Just build the syntax tree, and write out the final core image
 *            on rc.x .  rc.x is in a.out format.  The parse tree produced by
 *            rc.x for any RUSSELL program will then be inserted
 *            in place of the denotation represented by <cntrl b> in the
 *            first program.  Compilation then proceeds as normal. This allows 
 *            building initialized RUSSELL compilers which know about a 
 *            certain standard prologue.
 *              Boolean and Integer should be defined in the standard Prologue.
 *            Boolean must be defined as a parameter.  The initial occurrence
 *            of the identifier Boolean must be as the first identifier in a
 *            list of parameters having the same signature. (Generally it will
 *            be the only element of such a list. )
 *              This is of course both machine and operating system dependent.
 *            It is intended to work only with
 *            Berkeley VAX UNIX. (Which form is used depends on macros de-
 *            fined in parm.h.) An old style a.out file is produced on the VAX.
 *      d -   Same as i except produces a demand loadable file on the VAX.
 *            This requires that the old version of Rc also be demand 
 *            loadable.
 *      a -   Generate abstract syntax rather than VAX code.
 *            (obsolete, no longer implemented.)
 *      M -   allocate all variables in memory. (Opposite of R)
 *      c -   Generate code only for the user supplied (not initial env.)
 *            code.  Set things up so that it can be called from another
 *            Russell program.  Write signature information onto sigfile.
 *      p -   infile is preprocessor output.
 *      L -   Allow functions with trailing var Void argument to violate
 *            import rule.
 *      P -   Generate code to count function calls
 *      T -   Generate trace code
 *      G -   Generate general intermediate code
 *      V -   Verbose - print call graph info on stdout
 *      N -   No use of Callcc
 *      f -   It is not possible to update the stack pointer SP
 *            HINT ONS is however assumed to be understood by the code
 *            generator.
 *      F -   Same as f, but in addition BSF functions can't access GF
 *            and GAR instructions must precede first nested procedure call.
 *            HINT ONS is not assumed to be understood.
 *      R -   use registers for identifier bindings
 *      O -   optimize at the expense of compilation time and possibly
 *            execution space
 *      OO -  also prepare to run intermediate code optimizer
 *      OOO - unroll loops once
 *      X -   compile an externally callable type.
 * 	x -   Generate code for Xerox Portable Common Runtime.
 *      h -   heap allocate variables and activation records, unless the
 *            variables would otherwise be in registers.
 */

# include <stdio.h>
# include <a.out.h>
# include "parm.h"
# include "arith.h"

typedef long NODE;          /* Not really, but dont want to include */
			    /* streedefs here.                      */

# define NIL ((NODE *)0)

/*  Flag args. */
boolean BADflag;
boolean initflag;
boolean dloadflag;
boolean aflag;
boolean Mflag;
boolean cflag;
boolean pflag;
boolean Lflag;
boolean Pflag;
boolean Tflag;
boolean Gflag;
boolean Vflag;
boolean Nflag;
boolean fflag;
boolean Fflag;
boolean Rflag;
boolean Oflag;
boolean OOflag;
boolean OOOflag;
boolean Xflag;
boolean xflag;
boolean hflag;
long max_int_regs;
long max_addr_regs;

char * entry_name;  /* "" for regular compilations.  Used for -c */

extern int GC_dont_gc;

/*
 *  Input and output file names and declarations.
 */
char *ifname;
char *ofname;
char *sigfname;
char *optfname;
char *actual_ifname;
FILE *ofile;
FILE *sigfile;
FILE *optfile;

/*
 *  In-line code production for type constructions
 */

char * Vspcl_to_inline();
char * Gspcl_to_inline();
char * (* spcl_to_inline)();

/*
 *  Input translation for in-line specifications
 */
char * Vinline_cnvt(s)
char *s;
{ return(s); }

char * Ginline_cnvt();

char * (* inline_cnvt)();

/*
 *  Yacc flags 
 */
extern int yynerrs;
extern int yydebug;

/*
 *  Current file name, line number
 */
extern unsigned yyinfnm;  /* string table pointer */
extern int yyline;

/*
 * end of text symbol used in writing out initialized compiler
 */

extern etext;

/*
 * Syntax tree produced by pass 0.
 */
extern NODE * stxtree;
extern NODE * insrtptr;

main(argc,argv)
int argc;
char **argv;
{   int argn;   /* arg number currently being processed. */

#   ifdef VERBOSE
	fprintf(stderr, "Starting up\n");
#   endif
    if (insrtptr == NIL) {
	/* Not a pre-initialized compiler */
/*        GC_dont_gc = TRUE; */
	GC_init();
    }
    /* Initialize flags */
	dloadflag = initflag = BADflag = aflag = yydebug = FALSE;
    /* Initialize file name pointers. */
	ifname = ofname = sigfname = optfname = actual_ifname = (char *)0;
    for (argn = 1; argn < argc; argn++) {
        switch ( argv[argn][0] ) {
            case '-' :
                {   char *flagp;
                    for (flagp = &(argv[argn][1]); *flagp != '\0'; flagp++) {
                        switch (*flagp) {
                            case 'B':
                                BADflag++;
                                break;
                            case 'D':
                                yydebug++;
                                break;
			    case 'S':
				catchsigs();
                                break;
                            case 'i':
                                initflag = TRUE;
                                break;
                            case 'd':
                                initflag = TRUE;
                                dloadflag = TRUE;
				break;
			    case 'a':
				aflag = TRUE;
				break;
			    case 'M':
				Mflag = TRUE;
                                break;
                            case 'c':
                                cflag = TRUE;
                                break;
                            case 'p':
                                pflag = TRUE;
                                break;
                            case 'L':
                                Lflag = TRUE;
                                break;
                            case 'P':
                                Pflag = TRUE;
                                break;
                            case 'T':
                                Tflag = TRUE;
				break;
			    case 'G':
				Gflag = TRUE;
				break;
			    case 'V':
				Vflag = TRUE;
				break;
			    case 'N':
				Nflag = TRUE;
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
			    case 'O':
				if (OOflag) {
				    OOOflag = TRUE;
				} else if (Oflag) {
				    OOflag = TRUE;
				} else {
				    Oflag = TRUE;
				}
				break;
			    case 'X':
				Xflag = TRUE;
				break;
			    case 'x':
			        xflag = TRUE;
			        break;
			    case 'h':
				hflag = TRUE;
				break;
                            default:
				dbgmsg("Rc: illegal flag option: %c\n", *flagp);
                                abort();
                        }
                    }
                }

            case '\0' :
                break;

	    default:
		if ( actual_ifname == (char *)0 ) {
		    actual_ifname = argv[argn];
		} else if ( ifname == (char *)0 ) {
                    ifname = argv[argn];
		} else if (ofname == (char *)0) {
                    ofname = argv[argn];
		} else if (sigfname == ((char *)0) && cflag) {
		    sigfname = argv[argn];
		} else if (optfname == ((char *)0) && OOflag) {
		    optfname = argv[argn];
		} else {
		    fprintf(stderr, "Rc: Too many args\n");
		    exit(1);
		}
                break;
        }
    }

    if (ofname == ((char *)0)) {
	fprintf(stderr, "Rc: Too few arguments\n");
	exit(1);
    }
    if (cflag && sigfname == ((char *)0)) {
	fprintf(stderr, "Rc: Missing file name for signature info\n");
	exit(1);
    }
    if (OOflag && optfname == ((char *)0)) {
	fprintf(stderr, "Rc: Missing file name for optimization info\n");
	exit(1);
    }

    if (Xflag && !Gflag) {
	fprintf(stderr, "Vax code generator cannot be used with -X\n");
	exit(1);
    }

    /* Set up max_..._regs appropriately */
	max_int_regs = 0;
	max_addr_regs = 0;
	if (Mflag && Rflag) {
	    fprintf (stderr, "-M contradicts -R, ignoring both\n");
	    Mflag = Rflag = FALSE;
	}
#       ifdef SUN
	    if (!Mflag) {
#             ifdef GEN_C
		/* Regs ~ C variables.  There is no point in not using */
		/* all we can.                                         */
		  Rflag = TRUE;
#             else
		/* Try to allocate 3 int variable and 1 ptr variable */
		/* to regs. This leaves a few for temporaries.       */
		max_int_regs = 3;
		max_addr_regs = 1;
#             endif
	    }
#       endif
#       ifdef RT
	    if (!Mflag) {
		/* Leave the decisions to the register allocator.  It does */
		/* MUCH better than we can here.                           */
		Rflag = TRUE;
	    }
#       endif

    /* Initialize */
	if (insrtptr == NIL /* no previous initialization has taken place */) {
	    initids();
	    if (Gflag) {
		init_RIC_table();
	    }
	}

    /* open input file */
        if ( freopen( ifname, "r", stdin ) == NULL ) {
            fprintf(stderr, "Can't open %s\n", ifname);
            exit(1);
        }

    if (OOflag) {
      /* Open file for optimization info */
	optfile = fopen(optfname, "w");
	if (optfile == NULL) {
	    fprintf(stderr, "Can't open %s\n", optfname);
	    exit(1);
	}
    }
    /* Set up entry_name */
      if (cflag || Xflag) {
	int ifname_len = strlen(actual_ifname);
	char * p;

	entry_name = (char *) malloc(ifname_len+1);
	strcpy(entry_name, actual_ifname);
	if (entry_name[ifname_len-2] == '.') {
	    entry_name[ifname_len - 2] = '\0';
	}
	/* Replace all slashes by periods. */
	  for (p = entry_name; *p != '\0'; p++) {
	    if (*p == '/') {
		*p = '.';
	    }
	  }
      } else {
        entry_name = "";
      }
    
    /* Set up correct line number, file name */
      if (!pflag) {
        yyinfnm = stt_enter(ifname, strlen(ifname)+1);
        yyline = 0;
        addposition(yyinfnm, yyline);
      }

    if (!Gflag) { Rflag = FALSE; }

    /* Set up correct input translation for in-line code */
      if (Gflag) {
	inline_cnvt = Ginline_cnvt;
      } else {
	inline_cnvt = Vinline_cnvt;
      }

    /*
     * Build syntax tree (pass 1).
     */
#       ifdef VERBOSE
	    fprintf(stderr, "Calling yyparse\n");
#	endif
	yyparse();


    if( yynerrs != 0 )
        exit(1);

    if( initflag ) {
        /* write out current core image (except for stack) as a loadable file */
                /* and quit */
            int rcxdescr;  /* output file descriptor */
            struct exec hdr;  /* header for file */
            unsigned sbrkval; /* least unused location */
	    long symtsize = 4; /* size of empty string table */
	    long end_text;
	    long page_size;
#           ifdef SUN
		long begin_data = roundup(((long) (&etext)), SEGSIZ);
#           endif

            /* make sure stdout buffer is empty in core image */
                fflush(stdout);

	    if ( (rcxdescr = creat((Gflag? "Grc.x" : "rc.x"),0755)) == -1 ) {
		fprintf(stderr, "Can't create %s\n",
			(Gflag? "Grc.x" : "rc.x"));
                exit(1);
            }
	    /* initialize hdr */
#               ifdef SUN
		    page_size = PAGSIZ;
#               else
#                   ifdef PYRAMID
			page_size = 2048;
#                   else
#                       ifdef RT
			    page_size = 2048;
#                       else
                            page_size = 1024;
#                       endif
#                   endif
#               endif
		sbrkval = (long) sbrk(0);
		if (dloadflag) {
#                   ifdef SUN
#                     ifdef LDBUG
			end_text = roundup((((long) &etext)-32), page_size);
			/* This is a kludge to avoid what looks like a    */
			/* SUN 3.0 ld bug.  It appears that etext is off  */
			/* by 32.                                         */
#                     else
			end_text = roundup(((long) &etext), page_size);
#                     endif
#                   else
			end_text = roundup(((long) &etext), page_size);
#                   endif
		} else {
		    end_text = (long) &etext;
		}
#               ifdef SUN
		    if (!dloadflag) {
			fprintf(stderr, "Use -d instead of -i\n");
			exit(1);
		    }
		    hdr.a_text = end_text - page_size;
#               else
		    hdr.a_text = end_text;
#               endif
		if (dloadflag) {
#                   ifdef VERBOSE
			fprintf(stderr,
				"Generating demand paged core image\n");
#                   endif
		    /* generate demand paged output */
		    hdr.a_magic = ZMAGIC;
		} else {
#                   ifdef VERBOSE
			fprintf(stderr,
				"Generating core image\n");
#                   endif
		    hdr.a_magic = OMAGIC;
		}
#               ifdef SUN
		    hdr.a_machtype = MACH_TYPE;
		    hdr.a_data = sbrkval - begin_data;
#               else
#                 ifdef RT
		    hdr.a_data = sbrkval - 0x10000000;
#                 else
		    /* Contiguous text and data segments */
		    hdr.a_data = sbrkval - end_text;
#                 endif
#               endif
		if (dloadflag) hdr.a_data = roundup(hdr.a_data,page_size);
		hdr.a_bss = 0;
		hdr.a_syms = 0;
#               ifdef SUN
		    hdr.a_entry = page_size + sizeof(struct exec);
#               else
		    hdr.a_entry = 0;
#               endif
		hdr.a_trsize = hdr.a_drsize = 0; /* no relocation */
	    /* Write the file */
		write(rcxdescr, &hdr, sizeof (struct exec));
#               ifdef SUN
		    /* Write text segment */
		      if (write(rcxdescr, hdr.a_entry,
				(end_text - hdr.a_entry)) == -1) {
			fprintf(stderr, "Couldn't write text segment\n");
			perror("Rc");
#                       ifdef UNDEFINED
			    fprintf(stderr,
			            "Descriptor:%X, start:%X, size:%X\n",
				    rcxdescr, hdr.a_entry,
				    end_text - hdr.a_entry);
#                       endif
			exit(* ((char *) 0x80000000));
		      }
		    /* Write new data segment */
		      if (write(rcxdescr, begin_data,
				sbrkval - begin_data) == -1) {
			fprintf(stderr, "Couldn't write data segment\n");
			perror("Rc");
			exit(1);
		      }
#               else
		    if (dloadflag)
			lseek(rcxdescr, page_size, 0);
#                   ifdef RT
		      if (write(rcxdescr, (char *) 0, end_text) == -1) {
			fprintf(stderr, "Couldn't write text segment\n");
			perror("Rc");
			exit(1);
		      }
		      if (write(rcxdescr, (char *)0x10000000,
					  sbrkval - 0x10000000)
			  == -1) {
			fprintf(stderr, "Couldn't write data segment\n");
			perror("Rc");
			exit(1);
		      }
#                   else
		      if (write(rcxdescr, (char *) 0, sbrkval) == -1) {
			fprintf(stderr, "Couldn't write initialized file\n");
			perror("Rc");
			exit(1);
		      }
#                   endif
#               endif
		/* Write an empty string table */
#                 ifdef SUN
		    lseek(rcxdescr,
			  hdr.a_data + hdr.a_text,
			  0);
#                 else
		    lseek(rcxdescr,
			  (dloadflag? page_size : sizeof (struct exec))
			  + hdr.a_data + hdr.a_text,
			  0);
#                 endif
		  write(rcxdescr, &symtsize, sizeof (long));
	     exit(0);
        }


    /*  printf("*** End of Russell parse       \n"); */
    build_Idtable();

    if( yynerrs != 0 )
	exit(1);

    /*
     *  Pass2 - build symbol table
     */ 
#	ifdef VERBOSE
	    fprintf(stderr, "Building symbol table\n");
#	endif
	build_symtab( stxtree );
	if (yynerrs != 0) exit(2);

    /*
     *  Pass3 - number tree nodes and fix up tree
     */

#	ifdef VERBOSE
	    fprintf(stderr, "Fixing up syntax tree\n");
#	endif
	fixup (stxtree);
	if (yynerrs != 0) exit(3);


    if (yydebug)
	prtree(stxtree);

    GC_dont_gc = FALSE;		/* Garbage collection may be useful during */
				/* signature checking.                     */


    /*
     *  Pass4 - signature deduction and checking
     */
	/* Set up the correct routine for in_line code */
	    if (Gflag) {
		spcl_to_inline = Gspcl_to_inline;
	    } else {
		spcl_to_inline = Vspcl_to_inline;
	    }

#	ifdef VERBOSE
	    fprintf(stderr, "Checking signatures\n");
#	endif
	tl_checksigs(stxtree);
        if (yynerrs != 0) exit(1);

    if (yydebug)
	prtree(stxtree);


    /*
     *  Pass 5 - abstract syntax tree or code generation
     */

#	ifdef VERBOSE
	    fprintf(stderr, "Generating code\n");
#	endif
	ofile = fopen(ofname, "w");
        if ( ofile == NULL ) {
            fprintf(stderr, "Can't create output file (%s)\n", ofname);
            exit(8);
	}
	if (aflag)
	    /* generate abstract syntax only */
		fprintf(stderr,
                        "Abstract syntax generation no longer implemented\n");
	else {
	    extern NODE * bld_den_seq_f();
	    extern NODE * signature_f();
	    extern NODE * first_elmnt();

	    /* generate vax or intermediate code */
                if (cflag) {
		    /* Partial compilation - need to write out signature */
		    /* and external file info                            */
		    if (Gflag) {
		      Ggeneratepcode( ofile, stxtree,
				      first_elmnt(bld_den_seq_f(insrtptr)) );
		    } else {
		      Vgeneratepcode( ofile, stxtree,
				      first_elmnt(bld_den_seq_f(insrtptr)) );
		    }
                    if (yynerrs > 0) exit(1);
                    sigfile = fopen(sigfname, "w");
                    if (sigfile == NULL) {
                        fprintf(stderr, "Can't open %s\n", sigfname);
                        exit(1);
		    }
		    /* Write &etext to allow checking of compiler version */
	                putw(&etext, sigfile);

		    /* Copy object file list to signature file */
		    /* Terminate with empty line               */
			{
			    extern FILE * objfilelist;
			    int c;

			    /* Probably still open for writing. */
			    /* Close and reopen                 */
				if (objfilelist != NULL) {
				    fclose(objfilelist);
				}
				objfilelist = fopen(OBJFILELIST, "r");
			    if (objfilelist != NULL) {
				while ((c = getc(objfilelist)) != EOF) {
				    putc(c, sigfile);
				}
			    }
			    putc('\n', sigfile);
			}
		    sig_out(sigfile, signature_f(insrtptr));
		    fclose(sigfile);
		} else if (Xflag) {
		    /* Generate externally callable versions of all   */
		    /* functions in a type expression.                */
		      GgenerateXcode( ofile, stxtree,
				      first_elmnt(bld_den_seq_f(insrtptr)) );
		} else {
		    if (Gflag) {
		      Ggeneratecode( ofile, stxtree );
		    } else {
		      Vgeneratecode( ofile, stxtree );
		    }
                    if (yynerrs > 0) exit(1);
                }
	}
	fflush( ofile );
    exit(0);
}
