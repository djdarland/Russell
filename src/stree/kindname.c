# include "parm.h"
# include "stree/ststructs.mh"

# ifdef MAIN

#   include <stdio.h>
#   include "stree/stformats.h"
#   undef dbgmsg
#   define dbgmsg printf

char * kindname();
/*
 *  Driver for kind program.
 */
main(argc,argv)
int argc;
char **argv;
{
    printf("kind name = %s\n", kindname(atoi(argv[1])));
}
# else

/* typedescr[] is declared elsewhere */
    extern char *typedescr[];

# endif



/*
 *  kindname(kind)
 *
 *	Return a pointer to an English description of the node kind.
 */
char * kindname(kindno)
int kindno;
{
    if ( kindno < 0 || kindno > LASTKINDVALUE ) {
		return ("unknown kind");
    } else
        return ( typedescr[2*kindno] );
}


