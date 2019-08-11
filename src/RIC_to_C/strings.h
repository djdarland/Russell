/* Strings are either represented as in C, except that they may not start */
/* with HDR_MARK or REF_MARK, or they are represented as a pointer to a   */
/* to an str_hdr structure.                                               */
/* In the latter case, the whole string is the                            */
/* concatenation of all the leaves of the binary tree formed by the       */
/* str_hdr structures.                                                    */

# define HDR_MARK '\201'  /* Meta Control-A */

# define CS_NIL ((char *)0)

struct str_hdr {
    char hdr_mark;  /* always equal to HDR_MARK */
    char * first_part;      /* Either or both may be struct str_hdr * */
    char * last_part;       /* At most one may be an empty string.    */
};

/* Macros to quickly get the first few characters of a string        */
/* These set the second argument to NULL for one past the end of the */
/* string.  They set it to garbage if more than 1 character past the */
/* end is demanded.                                                  */
static char charbuf[4];

# define is_empty(s) (*(s) == '\0')

# define GET_FIRST(s,c) \
    if (*s != HDR_MARK) { \
        c = *s; \
    } else { \
        charbuf[0] = '\0'; \
        str_firstn(charbuf, 1, s); \
        c = charbuf[0]; \
    }

# define GET_SECOND(s,c) \
    if (*s != HDR_MARK) { \
        c = s[1]; \
    } else { \
        charbuf[0] = '\0'; \
        str_firstn(charbuf, 2, s); \
        c = charbuf[1]; \
    }

# define GET_THIRD(s,c) \
    if (*s != HDR_MARK) { \
        c = s[2]; \
    } else { \
        charbuf[0] = '\0'; \
        str_firstn(charbuf, 3, s); \
        c = charbuf[2]; \
    }

char * concat();

char * flatten();

char * placeholder();

void set_ph();
