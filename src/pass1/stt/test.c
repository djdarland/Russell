#
/*
 * string table routines test driver
 */
# include "parm.h"
# include "sttdefs.h"
# include "stdio.h"

char testbuff[100];

main()
{
register char *p;
int lgth;
sttrelptr rp;

    for(;;) {
        p = testbuff;
        while( (*p = getchar()) != '\n' ) {
			if( *p == EOF ) exit();
            p++;
        }
        *p++ = 0;
        lgth = p - testbuff;
        printf("%s: ",testbuff);
        rp = stt_enter( testbuff, lgth );
        printf("(%l) ",rp);
        p = getname( rp );
        printf("%s\n",p);
    }
}

/*
 *	Dummy yyerror to make things fly.
 */
yyerror(msg)
char msg[];
{
	printf("YYERROR: %s",msg);
}
