/*
 *  ROUTINE TO CONVERT VIRTUAL LINE NUMBERS IN SYNTAX TREE TO REAL
 *             LINE NUMBERS AND FILE NAMES 
 */

/*
 *    To accomplish the conversion the following sequence of calls
 * must be used:
 *      1) findvl(virtual_line_number);
 *      2) getfn()  and/or  getrl()  can then be used to obtain the
 *         corresponding file name and/or real line number.
 */
 
# include "parm.h"
# include <stdio.h>

# define NIL 0
# define DEBUG DEBUG

/*
 * Declarations for table of virtual line numbers versus real line
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

extern vrline * vrtable;  /* pointer to first table entry */

static vrline * vrptr = NIL; /* set by findvl to point to the last record */
                             /* in vrtable with vr_vline < its agrument   */
static int lastvl; /* last argument to findvl */

findvl(vln)   
register int vln;
{
    register vrline *p,   /* pointer to current record  */
                    *q;   /* pointer to previous record */
    lastvl = vln;
    p = vrtable;
    q = NIL;
    while ( p != NIL && ((p -> vr_vline) <= vln) ) {
        q = p;
        p = p -> vr_next;
    }
    vrptr = q;
#   ifdef DEBUG
        if ( q == NIL ) {
            dbgmsg("findvl: no vrline entry for vline: %d\n", vln);
            abort();
        }
#   endif
}

char *
getfn()
{
#   ifdef DEBUG
        if ( vrptr == NIL ) {
            dbgmsg("getfn: ommitted call to findvl\n");
            abort();
        }
#   endif
    return( vrptr -> vr_fname );
}

getrl()
{
#   ifdef DEBUG
        if ( vrptr == NIL ) {
            dbgmsg("getrl: ommitted call to findvl\n");
            abort();
        }
#   endif

    return( (vrptr -> vr_rline) + (lastvl - (vrptr -> vr_vline)) );
}


