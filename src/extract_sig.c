# include "parm.h"
# include "stree/ststructs.mh"
# include <a.out.h>
# include <stdio.h>

boolean Vflag = FALSE;
boolean Gflag = TRUE;
char * fn;
FILE * obj_file;
NODE * sig;
extern FILE * unparse_file;

/* Miscellaneous declarations to make this beast link correctly */
char tokenbuf[1000];
int avail_loc;

typedef struct VrLine{
    int vr_vline,            /* virtual line number at which file */
			     /* change or line number jump occurred */
	vr_rline,            /* corresponding real line number */
	vr_fname;            /* string table index of filename */
    struct VrLine * vr_next; /* pointer to next record */  } vrline;

vrline * vrtable = NIL,  /* pointers to first and last table entries */
       * vrtend  = NIL;

int yyvline = 0;  /* curent virtual line number */

yyperror(msg)
char *msg;
{  
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

extern NODE * sig_in();

main(argc,argv)
int argc;
char ** argv;
{
    if (argc == 3) {
	if (strcmp(argv[1], "-V") != 0) {
	    goto Usage;
	}
	Vflag = TRUE;
	fn = argv[2];
    } else if (argc != 2) {
	goto Usage;
    } else {
	fn = argv[1];
    }

    /* Fake vrtable, so it appears plausible */
	vrtable = vrtend = (vrline *) malloc(sizeof (vrline));
	vrtable -> vr_vline = vrtable -> vr_vline = 0;
	vrtable -> vr_fname = stt_enter(fn, strlen(fn)+1);
	vrtable -> vr_next = NIL;

    if ((obj_file = fopen(fn, "r")) == NULL) {
	fprintf(stderr, "Couldn't open %s\n", fn);
	exit(1);
    } else {
	long strsize;
	struct exec header;
	unsigned long optsize;

	/* seek past end of string table */
	  fread(&header, sizeof (struct exec), 1, obj_file);
	  fseek(obj_file, N_STROFF(header), 0);
	  fread(&strsize, sizeof (long), 1, obj_file);
	  fseek(obj_file, N_STROFF(header) + strsize, 0);
	printf("String table starts at 0x%X, compiler info starts at 0x%X\n",
	       N_STROFF(header), N_STROFF(header) + strsize);
#       if defined(SUN) && defined(EXTRA_MAGIC)
	  /* Skip past secondary section header */
	  {
	    struct extra_sections es;
	  
	    fread(&es, sizeof (struct extra_sections), 1, obj_file);
	    if (es.extra_magic != EXTRA_MAGIC) {
	      printf("Bad secondary magic number %d\n",
	             es.extra_magic );
	    }
	    if (es.extra_nsects != 1) {
	      printf("%d unrecognized extra section%s\n",
	             es.extra_nsects-1, (es.extra_nsects>2? "s" : ""));
	    }
	    /* Discard secondary section size */
	      printf("Secondary section size = %d\n", getw(obj_file));
	  }
#       endif

	optsize = getw(obj_file);
	printf("Size of optimization info: %d; Object file list at 0x%X\n",
		optsize, N_STROFF(header) + strsize + optsize + sizeof(int));
	fseek(obj_file, optsize, 1);
	
        printf("&etext = %d\n", getw(obj_file));

	/* Print list of embedded object files */
	{
	    char str_buf[1000];  /* General purpose buffer */
	    char *p;
	    int c;  /* actually a character */

	    if (Vflag) {
		printf("Requires object files:\n");
	    }
	    do {
		/* Read a line from obj_file */
		    p = str_buf;
		    do {
			c = getc(obj_file);
			if (c == '\n') {
			    *p = '\0';
			} else if (c == EOF) {
			    fprintf(stderr, "%s has bad format\n",
				    fn);
				exit(1);
			} else {
			    *p++ = c;
			}
		    } while (c != '\n');
		    if (Vflag && p != str_buf) {
			printf("\t%s\n", str_buf);
		    }
	    } while (p != str_buf);
	    if (Vflag) {
		printf("\n");
	    }
	}
	printf("Signature starts at 0x%X\n", ftell(obj_file));
	/* Now read signature info */
	    sig = sig_in(obj_file, fn);
	    unparse_file = stdout;
	    printf("Signature:\n\t");
	    unparse(sig);
	    printf("\n");
	printf("Signature ends at 0x%X\n", ftell(obj_file));
	if (Vflag) {
	    printf("\n\nSignature in long form:\n");
	    prtree(sig);
	}
	fclose(obj_file);
    }
    exit(0);
  Usage:
    fprintf(stderr, "Usage: %s [-V] filename.o\n", argv[0]);
    exit(1);
}
