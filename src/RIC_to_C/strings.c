
# define DEBUG
# undef DEBUG
# include <stdio.h>

# include "strings.h"  /* Contains data structure definition for strings */

# define new_hdr(p, first, last) { \
    p = (struct str_hdr *)GC_malloc(sizeof(struct str_hdr)); \
    p -> hdr_mark = HDR_MARK; \
    p -> first_part = (first); \
    p -> last_part = (last); \
}

/* Concatenate two strings.  Note that an empty string will not occur */
/* as a leaf unless the place_holder function is used.  Even then at  */
/* most one leaf may be empty.                                        */
char * concat(x, y)
char * x, * y;  /* May actually be struct str_hdr * */
{
    if (*y == '\0') {
        return(x);
    } else if (*x == '\0') {
        return(y);
    } else {
        struct str_hdr *p;

        new_hdr(p,x,y);
        return((char *)p);
    }
}

/* Make a placeholder string that can be embedded in other strings, and */
/* subsequently filled in.                                              */
char * placeholder()
{
    register struct str_hdr *p;

    p = (struct str_hdr *)GC_malloc(sizeof(struct str_hdr));
    p -> hdr_mark = HDR_MARK;
    p -> first_part = CS_NIL;
    p -> last_part = "";
    return((char *)p);
}

/* fill in a placeholder p */
void set_ph(p, s)
struct str_hdr * p;
char * s;
{
    p -> first_part = s;
}

# ifdef UNDEFINED
/* Determine the length of a string in characters */
int str_length(x)
char *x;
{
    if (*x == HDR_MARK) {
        return(str_length(((struct str_hdr *) x) -> first_part)
               + str_length(((struct str_hdr *) x) -> last_part));
    } else {
        return(strlen(x));
    }
}
# endif

int str_length(x)
char * x;
{
#   define STACK_SIZE 1024
#   define TOS_MARK ((char *)0x55555555)
    char * len_stack[STACK_SIZE];
    register char ** stack_ptr = len_stack;  /* 1 above top of stack */
    register char *s;                 /* Substring currently being examined */
#   define PUSH(y)  *stack_ptr++ = (y)
#   define POP      s = *(--stack_ptr)
    register unsigned long len = 0;

    len_stack[STACK_SIZE-1] = TOS_MARK; /* So we can report overflow */
    PUSH(x);
    while (stack_ptr > len_stack) {
	POP;
	while (*s == HDR_MARK) {
	    /* It's important to push the first, rather than the last part. */
	    /* Otherwise the stack grows too much for typical situations.   */
	    /* This way a stack overflow is very unlikely, so we do a       */
	    /* sloppy job at checking for it.                               */
	    PUSH(((struct str_hdr *) s) -> first_part);
	    s = ((struct str_hdr *)s) -> last_part;
	}
	while (*s++) { len++; }
    }
    if (len_stack[STACK_SIZE-1] != TOS_MARK) {
	fprintf(stderr, "str_length: stack overflow\n");
	exit(1);
    }
    return(len);
}


/* Concatenate characters from s to the C string in buffer buf until it has */
/* n characters or s is exhausted.                                          */
void str_firstn(buf, n, s)
char *buf;
int n;
char *s;
{
    if (s == CS_NIL) return;
    if (*s == HDR_MARK) {
        str_firstn(buf, n, ((struct str_hdr *) s) -> first_part);
        if (strlen(buf) < n) {
            str_firstn(buf, n, ((struct str_hdr *) s) -> last_part);
        }
    } else {
        strncat(buf, s, n - strlen(buf));
    }
}

/* Return the last character in s              */
/* Assumes non-flat strings are not empty.     */
char str_last(s)
char *s;
{
    char * t = s;

    while (*t == HDR_MARK) {
	if (*(((struct str_hdr *) t) -> last_part) == '\0') {
	    t = ((struct str_hdr *) t) -> first_part;
	} else {
	    t = ((struct str_hdr *) t) -> last_part;
	}
    }
    if (*t == '\0') return(*t);
    while (*(t+1) != '\0') t++;
    return(*t);
}

/* Convert the string s into a contiguous string starting at *buf  */
/* *buf is updated to point one past the trailing NULL.            */
void str_to_contig1();

void str_to_contig(s, buf)
char * s;
char **buf;
{
    str_to_contig1(s,buf);
    **buf = '\0';
    (*buf)++;
}

void str_to_contig1(x, buf)
char * x;
char **buf;
{
    register char *b = *buf;
#   undef STACK_SIZE
#   define STACK_SIZE 10240
#   define TOS_MARK ((char *)0x55555555)
    char * stc_stack[STACK_SIZE];
    register char ** stack_ptr = stc_stack;  /* 1 above top of stack */
    register char *s;                 /* Substring currently being examined */
#   define PUSH(y)  *stack_ptr++ = (y)
#   define POP      s = *(--stack_ptr)

    PUSH(x);
    while (stack_ptr > stc_stack) {
	POP;
	while (*s == HDR_MARK) {
	    /* Stack would be much smaller if we did this backwards.  But */
	    /* that would cost us time.                                   */
	    PUSH(((struct str_hdr *) s) -> last_part);
	    if (stack_ptr >= &stc_stack[STACK_SIZE]) {
		fprintf(stderr, "str_to_contig1: stack overflow (function too big)\n");
		exit(1);
	    }
	    s = ((struct str_hdr *)s) -> first_part;
	}
	while (*s) { *b++ = *s++; }
    }
    *buf = b;
}

/* Convert a string s to the standard sequential representation. */
/* Return its length in *size.                                   */
char * flatten(s, sz)
char *s;
long * sz;
{
    int len = str_length(s);  /* includes trailing NULL */
    char * result = (char *)GC_malloc_atomic(len+1);
    char * bufptr;

    if (sz != (long *)0) {
	*sz = len;
    }
    bufptr = result;
    str_to_contig(s, &bufptr);
    if (bufptr != result+len+1) {
        abort("flatten: inconsistent length", result, len, bufptr);
    }
    return(result);
}

/* Check two strings for equality. (Rather inefficient) */
int str_eq(s,t)
char *s, *t;
{
    int len_s = str_length(s) + 1;  /* includes trailing NULL */
    int len_t = str_length(t) + 1;  /* includes trailing NULL */
    char *sflat, *tflat;
    char *bufptr;
    extern int strcmp();

    if (len_s != len_t) {
        return(0);
    } else {
	sflat = (char *)GC_malloc_atomic(len_s);
        bufptr = sflat;
	str_to_contig(s, &bufptr);
	tflat = (char *)GC_malloc_atomic(len_t);
        bufptr = tflat;
	str_to_contig(t, &bufptr);
    }
    return(strcmp(sflat, tflat) == 0);
}


