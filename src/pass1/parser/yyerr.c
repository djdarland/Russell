# define MAXERRS 30
# include <stdio.h>
extern int yynerrs; /* number of calls to yyerror and yyperror so far */
extern int yyline; /* input line number */
extern int yycolno; /* input column number */
extern unsigned yyinfnm; /* offset of input file name in string table */

yyerror(msg)
char * msg;
{
    /* Ignore yacc generated error messages */
    if (yynerrs == 0) {
      yynerrs++;
    }
}

yyperror(msg)
char *msg;
{  
    yynerrs++;
    fprintf(stderr, "%s", msg);
    fprintf(stderr, " (file: %s, line: %d, column: %d)\n", getname(yyinfnm),
							   yyline, yycolno);
    if (yynerrs > MAXERRS) {
	exit(1);
    }
}

yywarn(msg)
char *msg;
{  
    fprintf(stderr, "Warning - %s", msg);
    fprintf(stderr, " (file %s, line %d, column %d)\n", getname(yyinfnm),
						       yyline, yycolno);
}
