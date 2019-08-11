/*
 *	Idtable is a 1-1 map between identifier names (represented by
 *	their unique string table index) and definition nodes
 *	defining that identifier. It is allocated using salloc and is
 *	a contiguous array of Identries sorted by Identry.i_sttindx.
 *
 *	The initial i_value for an entry is NIL.
 *	While traversing the syntax tree to build the symbol table,
 *	the definition node for an identifier is always
 *	the one that corresponds to the innermost definition (a la the
 *	russell scope rules).
 */
typedef struct {
	sttrelptr i_sttindx;  /* Index of string in stt. */
	NODE * i_value;
} Identry;

Identry * Idtable;
