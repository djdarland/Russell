# include <stdio.h>
# include "parm.h"

/* This routine generates a listing with nesting levels for '{}', '()',     */
/* and '[]'.                                                                */

char levels[1000]; /* string to be printed underneath source line */
int cur_col; 	   /* First character is considered to be in cur_col 0 */
int bracecount 0,
    bracketcount 0,
    parencount 0;


char tochar(i) /* generate character to be printed for nesting level i */
int i;
{
    i = i < 0 ? -i : i;
    if (i <= 9)
        return('0' + i);
    else if (i <= 9 + 26)
        return('a' + i - 10);
    else 
        return('A' + i - (10 + 26));
}

/*
 *	Get and echo a character from the input stream, updating current col count
 *  and initializing the level indicator.
 */
newchar()
{
    register int c;

	c = getchar();
	cur_col++;
	levels[cur_col] = c == '\t' ? '\t' : ' ';
	if ( c != EOF)
        putchar(c);
    return(c);
}

main()
{
    int c;

    cur_col = -1;
    c = newchar();
    while (c != EOF) {
        switch(c) {
            case '{' :
                levels[cur_col] = tochar(bracecount++); break;
            case '[' :
                levels[cur_col] = tochar(bracketcount++); break;
            case '(' :
                levels[cur_col] = tochar(parencount++); break;
            case '}' :
                levels[cur_col] = tochar(--bracecount); break;
            case ']' :
                levels[cur_col] = tochar(--bracketcount); break;
            case ')' :
                levels[cur_col] = tochar(--parencount); break;
            case '\'' :
                for(;;) {
                    c = newchar();
                    if ( c == EOF || c == '\'' )
                        break;
                    if ( c == '\\' ) {
                        c = newchar();
                        if ( c == EOF )
                            break;
                    }
                }
                break;
            case '"' :
                for(;;) {
                    c = newchar();
                    if ( c == EOF || c == '"' )
                        break;
                    if ( c == '\\' ) {
                        c = newchar();
                        if ( c == EOF )
                            break;
                    }
                }
                break;
            case '\n' :
                levels[cur_col] = '\0';
                printf("%s\n",levels);
                cur_col = -1;
                break;
        }
        if ( c != EOF ) c = newchar();
    }
}
