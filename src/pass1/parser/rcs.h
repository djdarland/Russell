/* The following are bit vectors defining which characters may legally */
/* appear inside quoted identifiers. The msb of each word corresponds  */
/* to the character with smallest ascii code.						   */

/* Note the machine and character set dependence of this whole scheme. */

# ifdef UNDEFINED
#   define RCS0 0144          /* include HT, CR, LF */
#   define RCS1 0      
#   define RCS2 0177777
#   define RCS3 0177777
#   define RCS4 0177777
#   define RCS5 0177777
#   define RCS6 0177777
#   define RCS7 0177776
# else
#   define RCS0 031000000
#   define RCS1 037777777777
#   define RCS2 037777777777
#   define RCS3 037777777776
# endif
