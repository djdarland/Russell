# include <stdio.h>
char * rindex();

/* Given an argument d1/d2/.../dn/f, perform a mv f d1/d2/.../dn/f */
main(argc, argv)
int argc;
char ** argv;
{
    char * slashpos;

    if (argc != 2) {
	fputs("Usage: movd file_name\n", stderr);
	exit(1);
    }
    slashpos = rindex(argv[1], '/');
    /* execl("/bin/echo", "echo", "mv", slashpos+1, argv[1], 0); */
    execl("/bin/mv", "mv", slashpos+1, argv[1], 0);
}
