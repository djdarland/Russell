/*
 *                Defines for Russell string table
 */


/*
 *  String table entry structure
 *
 *    sttrelptr          - type of the relative pointers in the table.
 *    STTENTRYSZ(strlen) - computes the size of a string entry, given
 *                         the length of the string it contains.
 *    STT_NO_NEXT_ENTRY  - terminates chains of stt entries.
 *    Sttentry           - a single entry in the string table.
 *    sttnstrings        - number of strings in stt.
 */

typedef unsigned sttrelptr;
#define STTENTRYSZ(strlen)  ( sizeof(sttrelptr) + sizeof(char) * strlen )
#define STT_NO_NEXT_ENTRY   -1

typedef struct {
    sttrelptr stte_next;    /* relative position in string table of next    */
                            /*   entry chained from hash header             */
    char stte_str[1];       /* null-terminated character string             */
} Sttentry;

int sttnstrings;



/*
 *  String table block structure
 *
 *    STTBDATASZ    -  size of the data section of a sttblock
 *    sttblock      -  type of a string table block
 *    STTNBLKS      -  max number of stt blocks (defined in parm.h)
 *    STTBLKNO(rpt) -  maps a relative pointer to an entry into
 *                     the block which it's on.
 *    STTOFFSET(rpt) - maps a relative pointer to an entry into
 *                     its offset from the start of whichever block it's in.
 */

#define STTBDATASZ 512

typedef struct {
    byte sttb_data[STTBDATASZ];
} sttblock;
#define STTBLKNO(rpt)   ( ((unsigned) (rpt)) >> 9 )
#define STTOFFSET(rpt)  ( ((unsigned) (rpt)) & (STTBDATASZ - 1) )
