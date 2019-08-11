#include <stdio.h>

/*
 * fgenf
 *
 * print like fprintf, except %1d refers to first argument
 *       after fmt string, ...
 */

# define PUTCHAR(c) putc((c),stream)
# define PRINTN(n,b) printn(stream,(n),(b))

void
fgenf(stream,fmt)
FILE *stream;
char *fmt;
{
register char *s;
register int *adx;
register int c;
int argno;
char cmd;
long num;
int base;

        while( (c = *fmt++) != 0 ) {
          switch(c) {
            case '%':
                argno = 0;
                while( ((c = *fmt) >= '0') && (c <= '9') ) {
                    argno = (argno * 10) + (c - '0');
                    fmt++;
                }
                adx = ((int *)(&fmt)) + argno;
                switch( cmd = *fmt++ ) {
                    case 'd':
                        PRINTN((long)(*adx),10);
                        break;
                    case 'u':
                        PRINTN((long)((unsigned)(*adx)),10);
                        break;
                    case 'o':
                        PRINTN((long)(*adx),8);
                        break;
                    case 'x':
                        PRINTN((long)(*adx),16);
                        break;
                    case 'D':
                    case 'U':
                        PRINTN(*((long *)adx),10);
                        break;
                    case 'O':
                        PRINTN(*((long *)adx),8);
                        break;
                    case 'X':
                        PRINTN(*((long *)adx),16);
                        break;
                    case 's':
                        s = (char *)(*adx);
                        while((c = *s++) != 0 ) {
                            PUTCHAR(c);
                        }
                        break;
                    case 'c':
                        c = *adx;
                        PUTCHAR( c );
                        break;
                }
                break;
            default:
                PUTCHAR(c);
                break;
            }
        }

}

/*
 * Print a long signed integer in base b.
 */
printn(stream,n,b)
FILE *stream;
long n;
unsigned b;
{
long a;

    if( n < 0 ) {
        PUTCHAR('-');
        n = (long)(-n);
    }
    if( (a = n/b) != 0 )
        printn(a,b);
    PUTCHAR( (unsigned)((n % b) + '0') );
}
