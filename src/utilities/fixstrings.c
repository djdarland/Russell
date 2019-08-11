# include <stdio.h>

/*  Compress whitespace and comments out of strings in a C program,
 *  turning all underscores in strings into blanks.
 *  Note comments outside of strings with double quotes inside of
 *  them are not handled correctly.
 */
main()
{
register int c;
int instr = 0;         /* true if we are inside a string  */
int incmt = 0;         /* true if we are inside a comment */
                       /* which is itself inside a string */

    
    while( (c = getchar()) != EOF ) {
        if( instr ) {
            switch(c) {

                case '"': 
                    instr = !instr;
                    break;

                case ' ':       /* white space, ignore */
                case '\t':
                case '\n':
                    continue;

                case '_':
                    c = ' ';
                    break;

                case '/':       /* comment, ignore */
                    { register int lastc;
                        c = getchar();
                        if( c != '*' ) {
                            putchar('/');
                            break;
                        }
                        c = getchar();
                        do {
                          lastc = c;
                          c = getchar();
                        } while( (c != EOF) && ((lastc != '*') || (c != '/')) );
                        continue;
                    }

                default:
                    break;
            }
        } else {
            switch(c) {
                case '"':
                    instr = !instr;
                    break;

                default:
                    break;
            }
        }
        putchar(c);
    }
    fflush(stdout);
}
