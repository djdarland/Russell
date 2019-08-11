#
/*
 *             Abstract Syntax Tree Printer
 */

# include "parm.h"
# include <stdio.h>
# include "stformats.h"
# include "ststructs.mh"
# include "is_ptr.h"
# include "../pass4/sigs.h"
# define VERBOSE VERBOSE

/* Number of columns to indent a subordinate structure under the current one. */
# define NCOLS 2

/*
 *  prtree(p)
 *  Print the syntax tree with root p
 */
prtree(p)
NODE *p;
{
    putchar('\n');
    prtr1(p,0);
    putchar('\n');
}

/*
 * Print the syntax tree with root p
 * starting in column col.
 * Assume head is currently in column 0.
 */
prtr1(arg,col)
NODE * arg;
int    col;
{
register unsigned * p = (unsigned *)arg;
register char * fmtpointer;     /* Printing format string. */


    moveto(col);

    if (p == NIL) {
        printf("NIL");
        return;
    }
    if (p == (unsigned *)ERR_SIG) {
	printf("ERR_SIG");
	return;
    }

    /* Check that p is ok. */
        if (!is_good((NODE *)p)) {
	    dbgmsg("\nprtree: bad NODE pointer: p=%x\n",p);
            abort();
	}

    /* Print kind  Note that kindname checks to see	*/
    /* if the kind is in range.               		*/
	printf("0x%x  %s ", p, kindname(arg -> kind));

    /* Try to print selection type */
        if ((arg -> kind == LETTERID || arg -> kind == OPRID
            || arg -> kind == QSTR || arg -> kind == UQSTR)
            &&arg -> sel_type != NIL) {
            if (arg -> sel_type -> kind == LETTERID) {
                printf("(seld %s) ",
                       getname(arg -> sel_type -> id_str_table_index));
            } else {
                printf("(seld) ");
            }
        }

    /* Handle lists specially. */
        if ( is_list(arg) ) {
	    maplist(q, arg, { putchar('\n'); prtr1(q, col + NCOLS); } );
	    return;
        }

    /* Go through every field of the node, printing it as approprate. */
        fmtpointer = typedescr[((arg -> kind) * 2) + 1];
        while ( * fmtpointer ) {
            switch( * fmtpointer ) {
		case 'a': /* Address. Print in hex with message. */
                    putchar('\n');
                    moveto(col+NCOLS);
		    printf("-> 0x%x", *p);
                    break;

                case 'c': /* Pointer to string. Print string. */
                    putchar('\n');
		    moveto(col+NCOLS);
                    if (*p == NIL) {
			printf("EMPTY");
		    } else {
			char *s = *(char **)p;
			int i;
			for (i = 0; i < 15 && *s != '\0'; i++, s++) {
			    if (*s >= 32 && *s < 127) {
				printf("%c", *s);
			    } else {
				printf("?");
			    }
			}
		    }
                    break;

                case 'i': /* Integer. Print it. */
                    putchar('\n');
                    moveto(col+NCOLS);
                    printf("%d", *p);
                    break;

               case 'l': /* Virtual line number. Print if VERBOSE is defined. */
#                   ifdef VERBOSE
                        findvl(*p);
                        printf("file: %s  line: %d", getname(getfn()), getrl());
#                   endif
                    break;

                case 'p': /* Pointer to subtree. Print subtree. */
		    putchar('\n');
		    if(is_ptr(*p)) {
			prtr1(*p,col + NCOLS);
		    } else {
			moveto(col+NCOLS);
			printf("%d", *p);
		    }
                    break;

                case 's': /* String table offset. Print string. */
                    putchar('\n');
                    moveto(col+NCOLS);
                    if ( *p == -1 ) {
                        printf("surrounding local type identifier");
                    } else if (*p < -1) {
			printf("%d", *p);
		    } else {
                        printf("%s", getname(*p));
		    }
                    break;

                case 'u': /* Unsigned. Print it. */
                    putchar('\n');
                    moveto(col+NCOLS);
                    printf("%u", *p);
                    break;

                case 'v': /* Bit vector. Print as octal value */
                    putchar('\n');
                    moveto(col+NCOLS);
		    printf("%x", *p);
                    break;

                case 'x': /* Don't print. */
                    break;


               default:
                    dbgmsg("\nprtree: format error: format= %c\n",
                            *fmtpointer);
        }
        fmtpointer++;  p++;
    }
}




/*
 * moveto(i)
 * 
 * Move print head to column i.
 * Assume head is currently in column 0.
 */
moveto(i)
{   int j;

    for ( j = 0; j != i; j++ )
        putchar(' ');
}
