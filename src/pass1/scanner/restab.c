#
/*
 * Russell
 *
 * tables of reserved words
 */

#include      "../parser/y.tab.h"

int residtab[] = {
    (int)"cand",         CAND,
    (int)"characters",   CHARACTERS,
    (int)"constants",    CONSTANTS,
    (int)"cor",          COR,
    (int)"do",           DO,
    (int)"else",         ELSE,
    (int)"elsif",        ELSIF,
    (int)"enum",         ENUM,
    (int)"export",       EXPORT,
    (int)"extend",       EXTEND,
    (int)"extern",       EXTERN,
    (int)"fi",           FI,
    (int)"field",        FIELD,
    (int)"func",         FUNC,
    (int)"hide",         HIDE,
    (int)"if",           IF,
    (int)"in",           IN,
    (int)"let",          LET,
    (int)"ni",           NI,
    (int)"od",           OD,
    (int)"prod",         PROD,
    (int)"readonly",     READONLY,
    (int)"record",       RECORD,
    (int)"signature",    SIGNATURE,
    (int)"then",         THEN,
    (int)"type",         TYPE,
    (int)"union",        UNION,
    (int)"use",          USE,
    (int)"val",          VAL,
    (int)"var",          VAR,
    (int)"with",         WITH,
};

int nresids = (sizeof residtab) / (2 * (sizeof (int)));  /* sizeof( struct restab ) */


int resoptab[] = {
    (int)":",            COLON,
    (int)"<<",           LEFT_ANGLE_BRACKET,
    (int)"==",           EQUALS_EQUALS,
    (int)"===",          EQUALS_EQUALS_EQUALS,
    (int)"==>",          RIGHT_ARROW,
    (int)">>",           RIGHT_ANGLE_BRACKET,
};

int nresops = (sizeof resoptab) / (2 * (sizeof (int)));  /* sizeof( struct restab ) */
