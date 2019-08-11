/*
 * Print format table generation macros
 *
 * The codes are:
 *	   code   print as
 *		a		address
 *		c		character string
 *		i		integer
 *		l		virtual line number
 *		p		pointer (follow it down)
 *		s		string table offset
 *		u		unsigned integer
 *		v		bit vector
 *		x		don't print
 */
/*
 * Syntax Tree Structure Definitions
 *
 * This file is a collection of macro invocations
 *
 * The calling environment must contain definitions for
 *
 * TOP          called at the beginning
 * START        called to start each structure declaration
 * INT, UNSIGNED, REFCNT, NODEKIND, NODESTAR, VLINENO, STTINDX,
 *      LISTPTR, BACKREF,CNSTAR(= pointer to Cons Node),STRPTR,
 *	BITVECTOR, NBPTR, HNODESTAR (identical to NODESTAR except hidden,
 *      i.e. not passed to mknode ), SIG, and HSIG.
 *      (The latter two are used for pointers from expression
 *      nodes to signature nodes.)
 *              called to define individual fields
 * FINISH       called to finish each structure
 * BOTTOM       called to finish everything
 */
/*
 *	Note that certain fields are not really part of the syntax tree,
 * but rather comprise the symbol table.  In particular each identifier
 * structure contains a field which eventually points to the last
 * declaration in the innermost enclosing scope of that identifier.
 * All constructs which can define a new identifier contain fields
 * used for chaining all definitions of a given identifier together.
 * If several of these constructs can occur in a given scope, i.e.
 * in the case of declarations and parameters, another field is
 * included to point back to some structure which uniquely identifies
 * that scope, so that illegal multiple declarations can subsequently
 * be detected.
 *  Identifier nodes contain an additional field identifying the
 * surrounding use list.  Use lists themselves are chained together
 * like identifier declarations.  Thus the list of types from which
 * selections can be inferred is easily identifiable.
 *  Also the first field of any structure which can possibly represent
 * a denotation is one for the signature of that denotation.  In general
 * this field is hidden and not used until the signature deduction phase.
 *  Any node which can declare an identifier (outside a signature)
 * contains displacement and level fields in the third and fourth positions
 * following the prefix.  The level number refers to function nesting
 * depth before the storage allocation pass of the code generator, and
 * is then changed to be activation record nesting depth.  (The two may
 * differ, since blocks occasionally require activation records.)
 *  Any signature field must be followed by a corresponding sig_done field.
 */
/* used to be 5, before we dropped refcount */
/*
 * Notes:
 *
 */
     char *typedescr[] = {
      "ListHeader","xliixx",
      "Declaration","xliippiixiiaaaiiiii",
      "Parameter","xliippiiaaa",
  /* also used for unions */    "RElement","xliippiii",
      "VarSignature","xliixip",
      "ValSignature","xliixip",
      "FSignature","xliixiaiicpp",
      "TSignature","xliixiippacccic",
      "TSComponent","xliipp",
  /* Gives list of quoted characters with */    "DefCharSigs","xliivvvvp",
  /* The signature of a signature */    "SignatureSig","xliixi",
      "BlDenotation","xliixiiaippii",
      "UseList","xliixippai",
      "Application","xliixippa",
      "Enumeration","xliixip",
      "Extension","xliixipii",
      "Product","xliixippa",
      "Record","xliixipa",
      "Union","xliixippa",
      "WithList","xliippa",
      "MPrimary","xliixiiippciii",
      "Elist","xliippa",
      "Hlist","xliippa",
      "EElement","xliippp",
      "ConstsKeyWord","xlii",
      "ElseKeyWord","xliixi",
      "CandKeyWord","xlii",
      "CorKeyWord","xlii",
      "GList","xliixip",
      "LDenotation","xliixip",
      "GElement","xliipp",
      "OpId","xliixixsaaiiai",
      "LetterId","xliixixsaaiixi",
  /* quoted string */    "QStr","xliixixcax",
  /* unquoted string */    "UQStr","xliixixcax",
      "FConstruction","xliipiiaipicxi",
  /* A list of these is pointed to	*/    "FreeVarNode","xliiaiii",
  /* used as body of externally defined  */    "ExternDef","xliic",
  /* used as body of separately compiled  */    "RExternDef","xliixic",
  /* Single character type component with */    "DCSException","xliiicia",
};
