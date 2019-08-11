/* Auxiliary functions to manipulate parameter names, parameter lists,    */
/* lists of actual arguments, and lists of temporary declarations.        */
/* Some othe miscellaneous string manipulation routines also appear here. */
# include <stdio.h>
# include "../parm.h"
# include "strings.h"
# include "tables.h"

/* Return the name of the ith function argument */
char * par_name(i)
int i;
{
    register char * result;
    if (i < 100) {
	/* First, a fast version: */
	result = (char *) GC_malloc_atomic(6);
	result[0] = 'a';
	result[1] = 'r';
	result[2] = 'g';
	result[3] = i / 10 + '0';
	result[4] = i % 10 + '0';
	result[5] = '\0';
    } else {
	result = (char *) GC_malloc_atomic(12);
	sprintf(result, "arg%d", i);
    }
    return(result);
}

/* Return the string representation of the integer i */
char * itos(i)
int i;
{
    register char * result;
    if (i > -10 && i < 10) {
	/* First, a fast version: */
	result = (char *) GC_malloc_atomic(3);
	if (i >= 0) {
	  result[0] = i + '0';
	  result[1] = '\0';
	} else {
	  result[0] = '-';
	  result[1] = '0' - i;                    
	  result[2] = '\0';
	}
    } else {
	char buf[30];

	sprintf(buf, "%d", i);
	result = (char *) GC_malloc_atomic(strlen(buf) + 1);
	strcpy(result, buf);
    }
    return(result);
}

/* Translate all control characters in a string to C escape sequences. */
/* The argument is assumed to be a (flat) C string.                    */
char * rmcntrl(s)
char * s;
{
    long new_len = 1 /* Trailing null */;
    register char * p;
    register char * q;
    register unsigned c;
    char * result;
#   define is_cntrl(c) ((c) < 32 || (c) > 127)

    /* Compute upper bound on length of new string */
      for (p = s; *p != '\0'; p++) {
	if (is_cntrl(*p)) {
	  new_len += 4;
	} else if (c == '"' || c == '\\') {
	  new_len += 2;
	} else {
	  new_len += 1;
	}
      }
    q = result = (char *) GC_malloc_atomic(new_len);
    for (p = s; *p != '\0'; p++) {
	c = (*p) & 0xff;
	if (is_cntrl(c)) {
	  *q++ = '\\';
	  if (c == '\n') {
	    *q++ = 'n';
	  } else if (c == '\t') {
	    *q++ = 't';
	  } else {
	    *q++ = c/64 + '0'; c = c%64;
	    *q++ = c/8 + '0';
	    *q++ = c%8 + '0';
	  }
	} else if (c == '"' || c == '\\') {
	  *q++ = '\\';
	  *q++ = c;
	} else {
	  *q++ = c;
	}
    }
    *q = '\0';
    return(result);
}

/* Return a list of declarations of the first n temporaries */
char * tmp_decls(n)
int n;
{
    char * result = "";
    int i;

    if (n < 0) return ("");
    for (i = 0; i <= n; i++) {
	result = concat(result, tmp_name(i));
	if (i != n) {
	    result = concat(result, ",");
	}
    }
    result = concat("word ", concat(result, ";\n"));
    return(result);
}

/* Return a comma separated list of the first n parameter names */
char * par_names(n)
int n;
{
    char * result = "";
    int i;

    for (i = 1; i <= n; i++) {
	result = concat(result, par_name(i));
	if (i != n) {
	    result = concat(result, ",");
	}
    }
    return(result);
}

/* Generate a list of the first n values associated with argument virtual */
/* registers, and delete these arguments from the virtual register table. */
char * arg_list(n)
int n;
{
    char * result = "";
    int i;

    for (i = 1; i <= n; i++) {
	result = concat(result, get_expr(ARGLOC(i)));
	rem_vr(ARGLOC(i));
	if (i != n) {
	    result = concat(result, ",");
	}
    }
    return(result);
}
