#
/*
 * command maker:
 *
 * Prepends and appends a set of default arguments to a command.
 *
 * Compile a different version of this for each of your favorite commands
 * by changing argtbl below.
 */

/*
 * table of default arguments:
 *
 * actual command name
 * prepended default args
 * 0
 * appended default args
 * 0
 */

# include <stdio.h>

char *argtbl[] = {
    "/bin/echo",                /* actual command name                      */
    "---",
    ">>>",
    0,                          /* passed args go here                      */
    "<<<",
    "---",
    0,
};

/*
 */
main( argc, argv )
char **argv;
{
char *nargv[512];       /* array of argument pointers constructed for   */
                        /* execv(II).  This is much bigger than it need */
                        /* be, but ...                                  */

register int iin, nargc, itbl;

  /* copy prepended default arguments */

    itbl = nargc = 0;
    while( argtbl[itbl] )
        nargv[nargc++] = argtbl[itbl++];

  /* copy the arguments passed to me */

    iin = 1;
    while( iin < argc )
        nargv[nargc++] = argv[iin++];

  /* copy default appended arguments */

    itbl++;
    while( argtbl[itbl] )
        nargv[nargc++] = argtbl[itbl++];

  /* add the trailing null and execv */

    nargv[nargc] = 0;
    execv( argtbl[0], nargv );

  /* whoops! */

    fprintf(stderr, "Can't exec %s\n", argtbl[0]);
    exit(1);
}
