#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG
/* Routines to read in a signature and optimization information */

# ifdef TRACE
#   define IFTRACE(x) x
# else
#   define IFTRACE(x)
# endif

# include "parm.h"
# include <stdio.h>
# include <a.out.h>
# include "stree/ststructs.mh"
# include "pass3/is_local.h"
# define FMTERR 7  /* exit code if file contents dont look like syntax tree */
# define MAXLISTELMTS 100000  /* Maximum plausible list length */

char FMTMSG[] = "Bad signature format for %s (not compiled with -c?)\n";

extern FILE * unparse_file;

char * Sinf_name;  /* name of last Sinfile passed to sig_in */

extern boolean Gflag;

extern boolean OOflag;

/* List of .o files whose signature information has already been read. */
# define fs_nil ((struct File_Sigs *) 0)
struct File_Sigs {
    char * fs_name;
    NODE * fs_signature;
    struct File_Sigs * fs_next;
} *file_sigs = fs_nil;

/* List of local declarations: */
struct decl_entry {
    NODE * de_decl;
    int de_number;
    struct decl_entry * de_next;
} *decl_nums;       /* list of declaration nodes and corresponding numbers */
                    /* This is a silly data structure, but the list is     */
		    /* unlikely to have huge length                        */

static int decl_num = 0;  /* last number assigned to a declaration */

/* Add a new declaration to decl_nums.  Assign it the next available number */
# define add_decl(decl) { \
    struct decl_entry * o = (struct decl_entry *) \
    			malloc(sizeof (struct decl_entry)); \
    o -> de_number = (++decl_num); \
    o -> de_decl = decl; \
    o -> de_next = decl_nums; \
    decl_nums = o; \
}

# define NONE (NODE *)(-1)

/* Get the node associated with declnum.  Return NONE if there isn't any */
static NODE * get_decl(decl_num)
int decl_num;
{
    struct decl_entry *p = decl_nums;

    while (p != NIL ) {
        if (decl_num == p -> de_number) {
            return(p -> de_decl);
        }
        p = p -> de_next;
    }
    return(NONE);
}

/* free decl_nums structure */
static free_decls()
{
    struct decl_entry *p = decl_nums;
    struct decl_entry *q;

    while (p != NIL ) {
        q = p;
        p = p -> de_next;
        free(q);
    }
}

NODE * sig_in1();

extern char tokenbuf[1000]; /* used as string temporary */

/* Read a 0 terminated string from Sinfile.
 * A NIL pointer is represented as a string consisting of a single FF
 * (delete) character.
 * Allocate space for the string and return a pointer to the heap
 * object.
 */
char * get_string(Sinfile)
FILE * Sinfile;
{
    int len = 0;
    char c;
    char * result;

    while((c = getc(Sinfile)) != 0) {
        if (ferror(Sinfile) || feof(Sinfile) || len >= (sizeof tokenbuf)-1) {
	    fprintf(stderr, FMTMSG, Sinf_name);
#           ifdef TRACE
              printf("bad string\n");
              abort();
#           endif
            exit(FMTERR);
        }
        tokenbuf[len++] = c;
    }
    tokenbuf[len] = 0;
    if (len == 1 && tokenbuf[0] == '\377') {
        return(NIL);
    }
    result = (char *)malloc(len+1);
    strcpy(result, tokenbuf);
    return(result);
}

/* Read a word from Sinfile, and check for errors */
int readw(Sinfile)
FILE * Sinfile;
{
    int result;

    if (ferror(Sinfile) || feof(Sinfile)) {
	fprintf(stderr, FMTMSG, Sinf_name);
#       ifdef TRACE
          printf("readw: read error\n");
          abort();
#       endif
        exit(FMTERR);
    }
    result = getw(Sinfile);
    if (ferror(Sinfile)) {
	fprintf(stderr, FMTMSG, Sinf_name);
#       ifdef TRACE
          printf("readw: read error\n");
          abort();
#       endif
        exit(FMTERR);
    }
    return(result);
}

/* Identifiers are written out as
 *              kind
 *              representation kind
 *              selection expression (if any)
 *              declaration number (local) or address (global)
 *                                 (not used for selection)
 *              name (0 terminated string, empty if local type id)
 *
 * The following options exist for the representation kind field:
 */
# define LOCALREP  0
# define GLOBALREP 1
# define SELECTREP 2 

/* Read an identifier.  Return a pointer to the corresponding id node */
/* It is assumed that the kind field has already been read.           */
NODE * get_name(Sinfile,kindno)
FILE *Sinfile;
int kindno;
{
    unsigned string_index;
    int rep;
    NODE * selt = NIL;
    NODE * last_def = NIL;
    NODE * result;
    int decl_number;

#   ifdef DEBUG
	if (kindno != LETTERID && kindno != OPRID) {
            dbgmsg("get_name: bad node kind\n");
            abort();
        }
#   endif
    rep = readw(Sinfile);
    switch(rep) {
        case SELECTREP:
            selt = sig_in1(Sinfile);
            break;
        case GLOBALREP:
            last_def = (NODE *)readw(Sinfile);
            break;
        case LOCALREP:
            decl_number = readw(Sinfile);
            last_def = get_decl(decl_number);
            if (last_def == NONE) {
		fprintf(stderr, FMTMSG, Sinf_name);
#               ifdef TRACE
                  printf("No declaration %d\n", decl_number);
                  abort();
#               endif
                exit(FMTERR);
            }
            break;
    }
#   ifdef DEBUG
	if (last_def != NIL &&
	    last_def -> kind != DECLARATION &&
            last_def -> kind != PARAMETER &&
            last_def -> kind != TYPESIGNATURE &&
            last_def -> kind != PRODCONSTRUCTION &&
            last_def -> kind != UNIONCONSTRUCTION &&
	    last_def -> kind != MODPRIMARY) {
	    dbgmsg("get_name: bad definition %X\n", last_def);
            abort();
        }
#   endif
    /* Read identifier name */
    {
      int len = 0;
      char c;

      while((c = getc(Sinfile)) != 0) {
          if (ferror(Sinfile) || feof(Sinfile) || len >= (sizeof tokenbuf)-1) {
	      fprintf(stderr, FMTMSG, Sinf_name);
#             ifdef TRACE
                printf("error reading id name \n");
                abort();
#             endif
              exit(FMTERR);
          }
          tokenbuf[len++] = c;
      }
      tokenbuf[len] = 0;
      if (len == 0) {
          string_index = -1;
      } else {
#         ifdef TRACE
	      printf("getname: read identifier name: %s\n", tokenbuf);
#         endif
          string_index = stt_enter(tokenbuf, len+1);
      }
    }
    result = mknode(kindno, string_index);
    result -> id_last_definition = last_def;
    initfld(&(result -> sel_type), selt);
    result -> id_def_found = TRUE;
    return(result);
}

/* Read in a list of expression (or signature) trees from Sinfile     */
/* The next number on Sinfile is assumed to be the number of elements */
/* in the list.                                                       */
NODE * list_in(Sinfile)
FILE * Sinfile;
{
    int nelements = readw(Sinfile);
    int i;
    NODE * result = emptylist();

    if (((unsigned) nelements) > MAXLISTELMTS) {
	fprintf(stderr, FMTMSG, Sinf_name);
#       ifdef TRACE
          printf("absurdly long list\n");
          abort();
#       endif
        exit(FMTERR);
    }
    for (i = 0; i < nelements; i++) {
        addright(result, sig_in1(Sinfile));
    }
    return(result);
}

/* Read an expression tree from Sinfile.  Return a pointer to the tree.    */
/* This is the same representation used by sig_out. It is designed to be   */
/* relatively efficient.  Local identifiers are represented by the number  */
/* of their declaration.  Such numbers are assigned in preorder fashion.   */
/* This may theoretically build an incorrectly structured tree, if it      */
/* is asked to read a file not produced by sig_out.                        */
NODE * sig_in(Sinfile, name)
FILE * Sinfile;
char * name;
{
  NODE * result;
  /* Save current state, in case this is a nested call */
  char * old_Sinf_name = Sinf_name;
  struct decl_entry * old_decl_nums = decl_nums;
  long old_decl_num = decl_num;

  decl_nums = NIL;
  decl_num = 0;
  Sinf_name = name;
  result = sig_in1(Sinfile);
# ifdef TRACE
    printf("Read signature\n");
    unparse_file = stdout;
    unparse(result);
    printf("\n");
# endif
  free_decls();
  /* Restore previous state */
    Sinf_name = old_Sinf_name;
    decl_nums = old_decl_nums;
    decl_num = old_decl_num;
  return(result);
}


/* Read and return the signature information from fn.o.  (The ".o" */
/* is appended by read_signature.)                                 */
/* Expects to be called only during parsing phase.                 */
/* The string fn is expected to be heap allocated, and not to      */
/* be deallocated by the caller after the call.                    */
NODE * read_signature(fn)
char *fn;
{
  int name_length = strlen(fn);
  FILE * extern_file;
  char * ext_fname;
  NODE * result;
  struct File_Sigs * p;
  extern char etext;

  /* Check whether signature has already been read.  If so, simply return */
  /* previously read signature.                                           */
    for (p = file_sigs; p != fs_nil; p = p -> fs_next) {
	if (strcmp(fn, p -> fs_name) == 0) {
	    return(p -> fs_signature);
	}
    }

  ext_fname = (char *) malloc(name_length+3);
  /* build file name */
    strcpy(ext_fname, fn);
    ext_fname[name_length] = '.';
    ext_fname[name_length+1] = 'o';
    ext_fname[name_length+2] = 0;
          /* Get and fill in signature info */
    if ((extern_file = fopen(ext_fname, "r")) == NULL) {
      fprintf(stderr, "Couldn't open %s\n", ext_fname);
      yyperror("Cant open external Russell object file");
      result = mknode (REXTERNDEF, NIL, NIL);
    } else {
      unsigned long strsize;
      struct exec header;
      extern FILE * optfile;
      unsigned long optsize;
      int c;

      /* seek past end of string table */
	fread(&header, sizeof (struct exec), 1, extern_file);
	fseek(extern_file, N_STROFF(header), 0);
	fread(&strsize, sizeof (long), 1, extern_file);
	fseek(extern_file, N_STROFF(header) + strsize, 0);

#     if defined(SUN) && defined(EXTRA_MAGIC)
	/* Skip past secondary section header */
	{
	  struct extra_sections es;
	  
	  fread(&es, sizeof (struct extra_sections), 1, extern_file);
	  if (es.extra_magic != EXTRA_MAGIC) {
	    fprintf(stderr, "Bad secondary magic number in %s (%s)\n",
	            ext_fname, "Not compiled with -c?");
	    exit(1);
	  }
	  if (es.extra_nsects != 1) {
	    fprintf(stderr, "Unrecognized extra sections in executable %s\n",
	            ext_fname);
	    exit(1);
	  }
	  /* Discard secondary section size */
	    (void) getw(extern_file);
	}
#     endif
      	  
      /* Copy or discard optimization information */
	optsize = getw(extern_file);
	for (; optsize > 0; optsize--) {
	    if ((c = getc(extern_file)) == EOF) {
		fprintf(stderr, "Bad optimization info in %s (not compiled with -c?)\n",
			ext_fname);
		exit(1);
	    }
	    if (OOflag) {
		putc(c, optfile);
	    }
	}
	
      /* Check that signature info was generated by this version of the */
      /* compiler.							*/
        if ((char *)(getw(extern_file)) != &etext) {
            fprintf(stderr, "Outdated executable %s - must be recompiled\n",
                    ext_fname);
            exit(1);
        }


      /* Copy any nested external files to OBJFILELIST       */
      /* This should really be done when we know that these  */
      /* are really needed, but it's very unlikely they      */
      /* won't be.                                           */
	{
	    extern char str_code_buf[];  /* General purpose buffer */
	    char *p;
	    extern void add_objfile();
	    int c;  /* actually a character */

	    do {
		/* Read a line from extern_file */
		    p = str_code_buf;
		    do {
			c = getc(extern_file);
			if (c == '\n') {
			    *p = '\0';
			} else if (c == EOF) {
			    fprintf(stderr, "%s has bad format\n",
				    ext_fname);
			    yyperror("Bad external object file");
			    exit(1);
			} else {
			    *p++ = c;
			}
		    } while (c != '\n');
		if (p != str_code_buf) {
		    add_objfile(str_code_buf);
		}
	    } while (p != str_code_buf);
	}

      /* Now read signature info */
       result = sig_in(extern_file, ext_fname);

      fclose(extern_file);
    }
  free(ext_fname);
  /* Add result to file_sigs list */
    p = (struct File_Sigs *) malloc(sizeof (struct File_Sigs));
    p -> fs_next = file_sigs;
    p -> fs_name = fn;
    p -> fs_signature = lock(result);
    file_sigs = p;

  return(result);
}

NODE * sig_in1(Sinfile)
FILE * Sinfile;
{
register NODE * result;
int kindno;

#   ifdef TRACE
      printf("sig_in: position = %d\n", ftell(Sinfile));
#   endif
    kindno = readw(Sinfile);
    if (feof(Sinfile)) {
#       ifdef TRACE
	    printf("End of file\n");
	    abort();
#       endif
	fprintf(stderr, FMTMSG, Sinf_name);
	exit(FMTERR);
    }
#   ifdef TRACE
      printf("sig_in: kind = %d(%s)\n", kindno, kindname(kindno));
#   endif

    switch ( kindno ) {

        case -1:
                return(NIL);

        case DECLARATION:
                {
                    NODE * id;
                    NODE * sig;
		    NODE * den;
		    int sig_transp;
		    NODE * result;

		    id = sig_in1(Sinfile);
		    sig_transp = readw(Sinfile);
                    sig = sig_in1(Sinfile);
                    den = sig_in1(Sinfile);
		    result = mknode(DECLARATION, id, den, sig);
		    result -> decl_sig_transp = sig_transp;
		    return(result);
                }

        case BLOCKDENOTATION:
                {
                    NODE * decl_l;
                    NODE * den_s;
                    int len_decl_l = readw(Sinfile);
                    int i;
                    NODE * v;
                          
                    if (((unsigned) len_decl_l) > MAXLISTELMTS) {
			fprintf(stderr, FMTMSG, Sinf_name);
#                       ifdef TRACE
                          printf("declaration list too long\n");
                          abort();
#                       endif
                        exit(FMTERR);
                    }
                    decl_l = emptylist();
                    /* First add dummy declaration nodes to decl_nums */
                      for (i = 0; i < len_decl_l; i++) {
                        v = mknode(DECLARATION, NIL, NIL, NIL);
                        addright(decl_l, v);
			add_decl(v);
#                       ifdef TRACE
			    printf("Added explicit declaration number %d\n",
				   decl_num);
#                       endif
                      }
                    maplist(v, decl_l, {
                      int decl_kind;
                      NODE * id;
                      NODE * sig;
                      NODE * den;

                      decl_kind = readw(Sinfile);
                      if (decl_kind != DECLARATION) {
			fprintf(stderr, FMTMSG, Sinf_name);
			IFTRACE(
                          printf("bad declaration\n");
			  abort();
			)
			exit(FMTERR);
                      }
                      id = sig_in1(Sinfile);
                      sig = sig_in1(Sinfile);
                      den = sig_in1(Sinfile);
                      initfld(&(v -> decl_id), id);
                      initfld(&(v -> decl_denotation), den);
                      initfld(&(v -> decl_signature), sig);
                    });
                    den_s = list_in(Sinfile);
                    return(mknode(BLOCKDENOTATION, decl_l, den_s));
                }

        case APPLICATION:
                {
                    NODE * op;
                    NODE * args;
    
                    op = sig_in1(Sinfile);
                    args = list_in(Sinfile);
                    return(mknode(APPLICATION, op, args));
                }

        case LOOPDENOTATION:
        case GUARDEDLIST:
                return(mknode(kindno, list_in(Sinfile)));

        case GUARDEDELEMENT:
                {
                    NODE * guard;
                    NODE * element;

                    guard = sig_in1(Sinfile);
                    element = sig_in1(Sinfile);
                    return(mknode(GUARDEDELEMENT, guard, element));
                }

        case OPRID:
        case LETTERID:
                return(get_name(Sinfile, kindno));

        case FUNCCONSTR:
                {
                    NODE * sig;
                    NODE * body;

                    sig = sig_in1(Sinfile);
                    body = sig_in1(Sinfile);
                    return(mknode(FUNCCONSTR, sig, body));
                }

        case USELIST:
                {
                    NODE * type_list;
                    NODE * den_seq;

                    type_list = list_in(Sinfile);
                    den_seq = list_in(Sinfile);
                    return(mknode(USELIST, type_list, den_seq));
                }

        case MODPRIMARY:
                {
                    NODE * result = mknode(MODPRIMARY, NIL, NIL);

                    add_decl(result);
#                   ifdef TRACE
			printf("Added mp declaration number %d\n",
			       decl_num);
#                   endif
                    initfld(&(result -> mp_primary),
                            sig_in1(Sinfile));
                    initfld(&(result -> mp_type_modifier),
                            sig_in1(Sinfile));
                    return(result);
                }

        case PRODCONSTRUCTION:
        case UNIONCONSTRUCTION:
                {
                    NODE * result = mknode(kindno, NIL, NIL);

                    add_decl(result);
#                   ifdef TRACE
			printf("Added type c. declaration number %d\n",
			       decl_num);
#                   endif
                    initfld(&(result -> prod_local_type_id),
                            sig_in1(Sinfile));
                    initfld(&(result -> prod_components),
                            list_in(Sinfile));
                    return(result);
                }

	case WORDELSE:
                return(mknode(WORDELSE));
                                         
        case PARAMETER:
                {
                    NODE * id;
                    NODE * sig;

                    id = sig_in1(Sinfile);
                    sig = sig_in1(Sinfile);
                    return(mknode(PARAMETER, id, sig));
                }

        case FUNCSIGNATURE:
                {
                    NODE * param_list = emptylist();
                    int nparams;
                    int i, param_kind;
                    int constr_info;
		    NODE * constr;
		    int spcl;
			     
		    spcl = readw(Sinfile);
		    result = mknode(FUNCSIGNATURE,
				    (Gflag?
				       (char *) get_RIC(Sinfile)
				       	  /* in-line code */
				     : get_string(Sinfile)),
				    NIL, NIL);
		    result -> fsig_special = spcl;

                    /* Read number of parameters and add blank nodes to */
                    /* decl_nums.                                       */
                        nparams = readw(Sinfile);
                        if (((unsigned) nparams) > MAXLISTELMTS) {
			    fprintf(stderr, FMTMSG, Sinf_name);
#                           ifdef TRACE
                              printf("Too many parameters\n");
#                           endif
                            exit(FMTERR);
                        }
                        for (i = 0; i < nparams; i++) {
                            NODE * v;

                            v = mknode(PARAMETER, NIL, NIL);
                            addright(param_list, v);
                            add_decl(v);
#                           ifdef TRACE
			      printf("Added parameter declaration number %d\n",
				     decl_num);
#                           endif
                        }

                    /* Fill in parameter nodes */
                        maplist(v, param_list, {
                            param_kind = readw(Sinfile);
                            if (param_kind != PARAMETER) {
				fprintf(stderr, FMTMSG, Sinf_name);
				IFTRACE(
                                  printf("bad parameter\n");
                                  abort();
				)
                                exit(FMTERR);
                            }
                            initfld(&(v -> par_id), sig_in1(Sinfile));
                            initfld(&(v -> par_signature), sig_in1(Sinfile));
                        });

                    initfld(&(result -> fsig_param_list), param_list);
                    initfld(&(result -> fsig_result_sig), sig_in1(Sinfile));
                    /* Restore info about function construction */
#                     define CONSTR_UNKNOWN 0
#                     define CONSTR_AVAIL 1
#                     define SLINK_AVAIL 2
                      constr_info = readw(Sinfile);
                      switch (constr_info) {
                        case CONSTR_UNKNOWN:
                            break;

			case SLINK_AVAIL:
			    result -> fsig_slink_known = TRUE;
			    /* and continue: */

                        case CONSTR_AVAIL:
                            constr = mknode(FUNCCONSTR,
                                            result,
					    mknode(EXTERNDEF,NIL));
                            lock(constr);
                            result -> fsig_construction = constr;
                            constr -> fc_complexity = readw(Sinfile);
                            constr -> fc_code_label = get_string(Sinfile);
			    constr -> ar_static_level =  readw(Sinfile);
			    constr -> ar_size = readw(Sinfile);
#                           ifdef TRACE
				printf("Construction=%X, label=%s, compl=%d\n",
				       constr, constr -> fc_code_label,
				       constr -> fc_complexity);
#                           endif
                    }
                    return(result);
                }

        case VALSIGNATURE:
        case VARSIGNATURE:
		return(mknode(kindno, sig_in1(Sinfile)));

	case SIGNATURESIG:
		return(mknode(SIGNATURESIG));

        case TYPESIGNATURE:
                result = mknode(TYPESIGNATURE, NIL, NIL, NIL, NIL, NIL);
                add_decl(result);
#               ifdef TRACE
		    printf("Added type sig. declaration number %d\n",
			   decl_num);
#               endif
                initfld(&(result -> ts_local_type_id), sig_in1(Sinfile));
                initfld(&(result -> ts_clist), list_in(Sinfile));
                /* recover optimization information: */
                    result -> ts_const_code = get_string(Sinfile);
                    result -> ts_string_code = get_string(Sinfile);
		    result -> ts_element_code = get_string(Sinfile);
		    result -> ts_string_max = readw(Sinfile);
		    result -> ts_simple_type = readw(Sinfile);
                return(result);

        case TSCOMPONENT:
                {
                    NODE * id;
                    NODE * sig;

                    id = sig_in1(Sinfile);
                    sig = sig_in1(Sinfile);
                    return(mknode(TSCOMPONENT, id, sig));
                }

        case DEFCHARSIGS:
                {
                    int i;
                    unsigned * base;

		    result = mknode(DEFCHARSIGS, 0, 0, 0, 0);
		    base = &(result -> dcs_0);
                    for(i = 0; i < NVECTORS; i++) {
                        base[i] = readw(Sinfile);
                    }
                    return(result);
		}

	case REXTERNDEF:
		{
		    char * name = get_string(Sinfile);
		    NODE * sig = read_signature(name);

		    result = mknode(REXTERNDEF, sig, name);
		    result -> sig_done = SIG_DONE;
		    return(result);
		}

        case RECORDCONSTRUCTION:
        case EXTENSION:
        case ENUMERATION:
        case RECORDELEMENT:
        case WITHLIST:
        case EXPORTLIST:
        case HIDELIST:
        case EXPORTELEMENT:
		dbgmsg("Signature input can't handle %s yet\n",
                       kindname(kindno));
                return(NIL);

        case QSTR:
        case UQSTR:
        case LISTHEADER:
        case FREEVARNODE:
        case WORDCAND:
        case WORDCOR:
        case EXTERNDEF:
        default:
#               ifdef TRACE
                  printf("Bad kind\n");
                  abort();
#               endif
		fprintf(stderr, FMTMSG, Sinf_name);
                exit(FMTERR);

    };
}
