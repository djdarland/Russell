# include <stdio.h>

# define LIM1 55
# define LIM2 70

/* insert newlines into a file */
main()
{   int c;
    int column  = 1;

    c = getchar();
    while ( c != EOF ) {
        if (column <= LIM1) {
            putchar(c);
            column++;
        }
        else if (column <= LIM2) {
            putchar(c);
            if (('a' <= c && c <= 'z') ||
                ('A' <= c && c <= 'Z') ||
                ('0' <= c && c <= '9')) {
                    column++;
            }
            else {
                putchar('\n');
                column = 1;
            }
        }
        else {
            putchar(c);
            putchar('\n');
            column = 1;
        }
        c = getchar();
    }
    putchar('\n');
}


