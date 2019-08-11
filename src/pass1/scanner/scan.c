#
/*
 *  Russell scanner
 */

#include    <stdio.h>
#include    <ctype.h> 
#include    "parm.h"
#include    "scan.h"
#include    "../parser/y.tab.h"

#define STKSIZE     5       /* depth of scanner error recovery stack        */


/*
 * code for nil pointer
 */
#define     NIL     0


extern boolean pflag;      /* input is preprocessor output */
#define     ESCCHAR '#'    /* signals position in preprocessor output */


/* entry in reserved identifier table */
struct restab {
    char    * rt_txt;
    int     rt_val; };

/*
 * reserved word tables
 */

struct restab residtab[];
int nresids;

struct restab resoptab[];
int nresops;

/*
 * table of character classes
 * and macro to find character class
 */

int cctab[];

# define CCLASS(c) ((c) == EOF ? EOFCC : cctab[c])


/*
 * global variables for communicating with yacc;
 */

int yyline = 0;
int yycolno = 0;
char * yyinfnm;

int yydebug;

int yylval;

/* declarations for table of virtual line numbers versus real line
 * numbers and filenames. The table is created by the scanner and
 * then used by later passes to convert a virtual line number
 * stored in the syntax tree to the real line number printed in
 * an error message.
 */
typedef struct VrLine{
    int vr_vline,            /* virtual line number at which file */
                             /* change or line number jump occurred */
        vr_rline,            /* corresponding real line number */
        vr_fname;            /* string table index of filename */
    struct VrLine * vr_next; /* pointer to next record */  } vrline;

vrline * vrtable = NIL,  /* pointers to first and last table entries */
       * vrtend  = NIL;

int yyvline = 0;  /* curent virtual line number */

static int scansavc = '\n';    /* n.b. preprocessor line number scan       */
                               /* routine only checks for '@' after '\n'   */

static int scanstk[STKSIZE];
static int stktop = -1;

char tokenbuf[1000];  /* also used by some other routines as string buffer */
/* static */ int tokenlgth;


/*
 *      get next token --
 *      put it in tokenbuf.
 *              return token code.
 */

yylex()
{
register int c;
register cc;
register char *p;
int outtok;

    if (stktop >= 0) {
	outtok = scanstk[stktop--];
	goto out;
    }

retry:
    c = scansavc;
    cc = CCLASS(c);
    yylval = 0; 
    p = tokenbuf;

    while( cc == WHTCC ) {

      if( c == '\n' ) {
	yyline++; yyvline++; 
	GETCHR(c);
	if( c == ESCCHAR && pflag) {
	    rdposition();
	    c = '\n';           /* Repeat check for ESCCHAR next time around */
	    yyline--; yyvline--; /* Line number is correct for next line     */
	}
      } else {
	GETCHR(c);
      }
      cc = CCLASS(c);
    }

    switch(cc) {

    case LETCC:
        do {
            *p++ = c;
            GETCHR(c);
            cc = CCLASS(c);
	} while( (cc == LETCC) || (cc == DIGCC) );
	*p++ = 0;
	tokenlgth = p - tokenbuf;
        outtok = reschk(residtab,nresids,WORDID);
	break;

    case SQUCC: /* single quote */
      {
        boolean saw_quote = FALSE;
        for (;;) {
            if ( cc == EOFCC || c == '\n' ) {
		yyperror("Unterminated quoted identifier");
                break;
            }
	    if (c == '\\') {
                GETCHR(c);
		switch(c) {
		    case 't': *p++ = '\t'; break;
		    case 'n': *p++ = '\n'; break;
		    case 'r': *p++ = '\r'; break;
		    default: *p++ = c;
		}
	    } else {
		*p++ = c;
	    }
            GETCHR(c);
            cc = CCLASS(c);
            if (saw_quote) {
                if (cc == SQUCC) {
                    /* ignore this character and keep scanning */
                        GETCHR(c);
                        cc = CCLASS(c);
                } else {
                    /* end of identifier */
                        break;
                }
            }
            saw_quote = (cc == SQUCC);
        }
        *p++ = '\0';
        outtok = WORDID;
        yylval = stt_enter(tokenbuf,p-tokenbuf);
        break;
      }

    case DQUCC: /* double quote */
      {
        boolean saw_quote = FALSE;
        for (;;) {
            if (cc == EOFCC || c == '\n') {
		yyperror("Unterminated string");
                break;
            }
	    if (c == '\\') {
                GETCHR(c);
		switch(c) {
		    case 't': *p++ = '\t'; break;
		    case 'n': *p++ = '\n'; break;
		    case 'r': *p++ = '\r'; break;
		    default: *p++ = c;
		}
	    } else {
		*p++ = c;
	    }
            GETCHR(c);
	    cc = CCLASS(c);
	    if (saw_quote) {
                if (cc == DQUCC) {
                    /* ignore this character and keep scanning */
                    /* Note that the previous double quote was saved */
                        GETCHR(c);
                        cc = CCLASS(c);
                } else {
		    /* end of string */
                        break;
                }
            }
            saw_quote = (cc == DQUCC);
        }
        /* Delete trailing quote. */
            *(p - 1) = '\0'; 
        outtok = QSTRING;
        /* allocate a buffer for the string and return it */
	    yylval = gc_malloc_atomic(p-tokenbuf-1);
            strcpy(yylval,&(tokenbuf[1])); /* skip leading quote */
	break;
      }

    case SEPCC:
	*p = scansavc = c;
        GETCHR(c);
	if( (scansavc == '(') && (c == '*') )
        /* process a comment */
	    { int startline = yyline;
			     /* temporary line counter used in comments   */
			     /* so error message has a useful line number */
			     /* if EOF occurs inside a comment            */
	      char * startfnm = yyinfnm;
	      int cmtnest = 0;

	      do {
		  switch( scansavc ) {
		      case EOF:
			  yyline = startline;
			  yyinfnm = startfnm;
			  goto retry; /* return an end of file */
		      case '\n':
			  yyline++; yyvline++; 
			  if( c == ESCCHAR && pflag ) {
			      rdposition();
			      c = '\n'; 
				/* Repeat check for ESCCHAR next time around */
			      yyline--; yyvline--;
				/* Line number is correct for next line      */
			  }
			  break;
                      case '*':
                          if( c == ')' )
                          cmtnest--;
                          break;
                      case '(':
                          if( c == '*' ) {
                              cmtnest++;
                              GETCHR(c);
                          }
                          break;
		      case '$':
			  if (c == '$') {
			    GETCHR(c);
			    switch( c ) {
			      case '+':
				yydebug++;
				break;
			      case '-':
				if(yydebug) yydebug--;
				break;
			    }
			  }
                          break;
		  }
		  scansavc = c;
                  GETCHR(c);
	      } while( cmtnest > 0 );
	      /* put2w( S_YYLINE, yyline ); */
	      scansavc = c;
	      goto retry;
	    }
	outtok = *p++;
	break;

    case DIGCC:
	do {
            *p++ = c;
            GETCHR(c);
            cc = CCLASS(c);
        } while( cc == DIGCC || cc == LETCC );
        *p++ = '\0';
        outtok = UQSTRING; /* unquoted string */
        /* allocate buffer and return it, as for quoted strings */
	    yylval = gc_malloc_atomic(p - tokenbuf);
            strcpy(yylval,tokenbuf);
        break;

    case OPRCC:
        do {
            *p++ = c;
            GETCHR(c);
            cc = CCLASS(c);
	} while( cc == OPRCC );
	*p++ = 0;
	tokenlgth = p - tokenbuf;
        outtok = reschk(resoptab,nresops,OPID);
	break;

    case EOFCC:
        scansavc = '\n';  /* Set things up for core image to be subsequently */
                          /* restarted */
        return(EOF);

    case BADCC:
        GETCHR(c);
        scansavc = c;
        goto retry;

    }

    scansavc = c;

out: 

    return ( outtok );

}

/*
 * read current position ( line no, file name )
 *   up to and including newline character
 * This routine clobbers tokenbuf & tokenlgth; this shouldn't matter.
 */
rdposition()
{
register c;
register n = 0;
register char *p;

    while( (GETCHR(c)) == ' ' ) ;

    for(;;) {
        if ( !isdigit(c) ) break;
        n = n * 10 + (c - '0');
        GETCHR(c);
    }
    yyline = n;

    for(;;) {
        if( c == EOF ) goto bad;
    if( c != ' ' ) break;
        GETCHR(c);
    }
    if( c != '"' ) goto bad;
    GETCHR(c);

    p = tokenbuf;
    for(;;) {
        if( c == EOF ) goto bad;
	if( c == '"') break;
	*p++ = c;
        GETCHR(c);
    }
    *p++ = 0;
    tokenlgth = p - tokenbuf;
    yyinfnm = (char *)stt_enter( tokenbuf, tokenlgth );
    addposition(yyinfnm, yyline);

    goto out;

  bad: 
    /* There was a syntax error in the line number specification. */
    /* This is either a preprocessor error or a bizarre input pgm */
      yyperror("Error in line number");
  out:
    /* Scan to the end of the line discarding any junk.  */
      while( (c != '\n') && (c != EOF) ) {
        GETCHR(c);
      }
}

/* add new record to vrtable, associating fn and ln with the current */
/* value of yyvline                                                  */
addposition(fn,ln)
unsigned fn;  /* stt pointer */
int ln;
{   register vrline * p;

    p = (vrline *) gc_malloc(sizeof(vrline));
    if (vrtable == NIL) 
        vrtable = p;
    else
        (vrtend -> vr_next) = p;
    vrtend = p;
    (p -> vr_vline) = yyvline;
    (p -> vr_rline) = ln;
    (p -> vr_fname) = fn;
    (p -> vr_next ) = NIL;
}

/*
 * look up contents of token buf in a reserved-word table
 *   of nentries entries.
 *
 * return value if it is reserved,
 *   otherwise return default value,
 *     add token buf to string table,
 *       and set yylval to the string table pointer.
 */

reschk( tbl, nentries, dflt )
register struct restab *tbl;
{
register int m;
register int l, r;
register char * this_entry;

    l = 0; r = nentries-1;
    while( l < r ) {
      m = (l + r) / 2;
      this_entry = tbl[m].rt_txt;
      if( *this_entry < *tokenbuf ||
          (*this_entry == *tokenbuf && strcmp(this_entry, tokenbuf) < 0) )
        l = m + 1;
      else
        r = m;
    }

    if( strcmp(tbl[l].rt_txt,tokenbuf) == 0 )
      return( tbl[l].rt_val );
    else {
      yylval = stt_enter( tokenbuf, tokenlgth );
      return( dflt );
    }
}


/*
 * push a token back onto the input
 */

yyunlex( tok )
int tok;
{
    if( stktop < (STKSIZE-1) )
    scanstk[++stktop] = tok;
    else
    yyperror("Compiler Error: scanner stack overflow");

}

