#
/*
 * string table routines test driver
 */
# include "parm.h"
# include "pass1/stt/sttdefs.h"
# include <stdio.h>
# include "stree/ststructs.mh"
# include "Idtable.h"

char testbuff[100];

main(argc,argv)
char **argv;
{
register char *p;
int lgth;
sttrelptr rp;
int i;
char prtflag;

	prtflag = (argc > 1);

    for(;;) {
        p = testbuff;
        while( (*p = getchar()) != '\n' ) {
			if( *p == EOF ) exit();
            p++;
        }
		*p++ = 0;
		if ( strcmp(testbuff, "#") == 0)
			break;
        lgth = p - testbuff;
		if( prtflag ) printf("%s: ",testbuff);
        rp = stt_enter( testbuff, lgth );
		if( prtflag ) printf("(%l) ",rp);
        p = getname( rp );
		if( prtflag ) printf("%s\n",p);
	}

	open("/dev/null",1);
	stt_write(3);

	printf("Now enter relative pointers\n");
	while ( (i = scanf("%d", &rp)) != EOF ) {
		if ( i != 0) {
			Identry *e;
			e = retrieve(rp);
			if( (e->i_sttindx != rp) || (e->i_value != -1) ) {
				printf("%u: (%u) = %d\n", rp, e->i_sttindx, e->i_value);
			}
		} else {
			printf("what was that?\n");
			while( getchar() != '\n');
		}
	}
	{ extern int nprobes, nkeys;
		printf("nprobes: %u, nkeys: %u, ratio: %\n",
			nprobes,nkeys,
			( ((double) nprobes) / ((double) nkeys) )
		);
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
