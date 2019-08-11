#
#include <stdio.h>
#include "../parser/y.tab.h"

extern yylval;
int pflag = 1;

main() 
{
register x;

	while( (x = yylex()) != EOF ) {
		printf("%d: ",x); fflush(stdout);
		if( (x == WORDID) || (x == OPID) ) {
			printf("%s ", getname(yylval) ); fflush(stdout);
		}
		if( (x == QSTRING) || (x == UQSTRING) ) {
			printf("%s ", yylval ); fflush(stdout);
		}
		putchar('\n');
	}
	printf("%d: ",x);
}

yyerror(s)
{
    printf("\nYYERROR: %s\n",s);
}
