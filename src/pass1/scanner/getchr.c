/* Scanner input routine. Also updates yycolno.  */
# include <stdio.h>
# include "parm.h"
# include "scan.h"

extern int yycolno;

char getchr()
{
   char c;

   yycolno++;
   if ((c = getchar()) < 32) {
       return (charfix(c));
   } else {
       return(c);
   }
}

/* Fix things up for a control character c */
char charfix(c)
char c;
{
        switch(c) {
		case '\n':	yycolno = 0;
                                return(c);

		case '\015': /* carriage return */

		case '\014': /* form feed */
                                yycolno = 0;
                                return('\n');
                                break;

		case '\t':	/* tab */
				yycolno = ((yycolno + TABWIDTH) / TABWIDTH) * TABWIDTH 
                                          + 1;
                                return(c);
                default:        return(c);
	}
}
