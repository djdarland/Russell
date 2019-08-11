
/*
 *	character classes
 */

#define		EOFCC	-1
#define     BADCC   0
#define     WHTCC   1
#define     LETCC   2
#define     DIGCC   3
#define 	SEPCC	4
#define 	OPRCC	5
#define 	SQUCC	6
#define 	DQUCC	7

/* Fast version of c = getchr() */

char charfix();

#define GETCHR(c)  (yycolno++, ((c = getchar()) < 32? charfix(c) : c))
