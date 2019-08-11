# include <stdio.h>

/* Replace "@" by "\n#" and remove any blank lines in the result. */
/* Replace "@@" by a newline.					  */
/* Delete lines starting with "#"                                 */
/* Delete ^ characters.						  */
main()
{
    char cline[50000];      /* Current line */
    register int cpos = 0;  /* Position of next character in line */
    register int c;         /* Last read character */
    int non_blank = -1;     /* Position of last non-blank character in line. */
    int i;

    while ((c = getchar()) != EOF) {
	switch(c) {
	    case '\n':
	    case '@':
	        if (cline[0] == '#' && cpos == 1) {
	            putchar('\n');
	            cpos = 0;
	            non_blank = -1;
	            break;
	        }
		if (non_blank >= 0) {
		    for (i = 0; i <= non_blank; i++) {
			putchar(cline[i]);
		    }
		    putchar('\n');
		}
		if (c != '@') {
		    cpos = 0;
		    non_blank = -1;
		} else {
		    cline[0] = '#';
		    cpos = 1;
		    non_blank = 0;
		}
		break;

	    case ' ':
	    case '\t':
		cline[cpos++] = c;
		break;

	    case '#':
		if (cpos == 0) {
		    /* Discard line */
		      while ((c = getchar()) != '\n');
		    break;
		}
		/* Otherwise treat it like a normal character:  */

	    case '^':
	    	break;
	    	
	    default:
		non_blank = cpos;
		cline[cpos++] = c;
	}
    }
    if (non_blank >= 0) {
	fprintf(stderr, "Dropped partial line at end\n");
	exit(1);
    }
    exit(0);
}
