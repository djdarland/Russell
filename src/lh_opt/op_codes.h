/* RIC (Russell Intermediate Code) Op codes */

/* The first group has a single string argument */
#define BR 1      /* branch unconditionally                         */
#define BRT 2     /* branch if value in TL is true.                 */
#define BRF 3     /* branch if value in TL is false.                */
#define CLL 4     /* call routine with given label                  */
		  /* Needs to do any cleanup (e.g. stack pops)      */
		  /* necessitated by the preceding ARG instr that   */
		  /* passed the activation record pointer.          */
#define LBL 5     /* attach the given label to the next instruction */
#define EXT 6     /* declare the label to be external               */
		  /* All labels that need to be known to the        */
		  /* outside are marked in this way.  All labels    */
		  /* not beginning with an 'L' are intended to be   */
		  /* unique, and thus may be made globally visible. */
#define LBA 7     /* supply a label argument to immediately         */
                  /* following instruction                          */
#define BFN 9     /* begin named function                           */
                  /* function temporaries may be reserved on the    */
		  /* stack at this point                            */
		  /* FP should be set to point to one below return  */
		  /* address.  AR should be set to first (and only) */
		  /* argument.  Other locations are set explicitly. */
		  /* Locations (incl. AR) are implicitly saved      */
		  /* during a procedure call.                       */
#define TFB 10    /* trace function entry                           */
#define TFE 11    /* trace function exit                            */
#define PRO 12    /* record function entry for profiling            */
#define ADT 13    /* character string data.  A label for the        */
                  /* location of the data may be supplied by a      */
		  /* preceding LBA instruction    OBSOLETE          */
#define ERR 14    /* branch to error routine with indicated label   */
		  /* Arguments may have been pushed.                */
#define BSF 15    /* begin short function.  Identical to BFN,       */
		  /* except that AR should neither be set, nor does */
		  /* the old value need to be saved.                */
		  /* Arguments to BSF functions are passed          */
		  /* individually using ARG, rather than passing a  */
		  /* a single argument containing the activation    */
		  /* record.  If the name of the function does not  */
		  /* start with Ffn, the generated function must be */
		  /* C callable.  (Typically it will be, in any     */
		  /* case.)                                         */
#define LBR 16    /* Attach a symbolic name to the virtual register */
		  /* declared by the immediately following DCL      */
#define DDT 17    /* Generate double precision floating point data. */
		  /* May be labelled with an LBA instruction.       */
		  /* The argument is actually a string representing */
		  /* representing a floating point constant.        */
		  /* Such data is read-only.                        */
#define FDT 18    /* Analogous to FDT, but for single precision     */
		  /* constants.                                     */

#define MAX_LABEL_OP 19

/* The remainder has up to 3 integer arguments.  Unless otherwise  */
/* specified, they refer to temporary locations (virtual           */
/* registers).  An offset is a signed integer value.               */
/* An object is considered accessible only if there is an          */
/* accessible pointer to its beginning.                            */
/* Pointers in any active temporary or on the stack are accessible */
                /* args */
#define DCL 20  /* loc type  --  declare temporary location        */
		/* (i.e. virtual register)                         */
		/* The following types are allowed:                */
#               define DCL_ADDR 1
#               define DCL_INT 2  /* 32 bit integer */
#               define DCL_FLOAT 3
#               define DCL_DBL_FLOAT 4
                /* DCL_ADDR and DCL_INT, are assumed to be the */
                /* same size.  The distinction is only a hint  */
		/* to the machine code generator.              */
		/* DCL_FLOAT is unlikely to be used in the     */
		/* near future.                                */

                /* In the following, size is a location:           */
#define UDC 21  /* loc       --  free temporary location           */
		/* If applied to a predefined location, this       */
		/* constitutes a declaration that the value it     */
		/* contains is dead at this point.                 */
		/* Duplicate UDC's for the same location may       */
		/* occasionally appear.  All but the first can be  */
		/* safely ignored.                                 */
#define ALH 22  /* size loc -- loc := ptr to new heap object       */
#define GAR 23  /* i loc  --  Get the ith argument (passed with    */
		/* ARG) into loc                                   */
#define ALS 24  /* size -- allocate new stack object containing    */
                /* pointer data                                    */
                /* Both ALS and ALH are assumed to clear the       */
                /* allocated space.  This is necessary only for    */
                /* garbage collection, and may frequently be       */
                /* eliminated by an optimizer                      */
#define LDI 25  /* index offset loc -- loc := index[offset]        */
#define STI 26  /* index offset loc -- index[offset] := loc        */
#define CLI 27  /* index offset -- jsr index[offset]               */
		/* Needs cleanup similar to CLL                    */
#define LDN 28  /* signed_value loc -- loc := signed_value         */
#define RTN 29  /*     -- return from function w/ value at loc RL  */
#define LDL 30  /* loc   -- load value of label provided           */
		/* by immediately preceding LBA instruction        */
		/* The label must have been previously defined.    */
#define MOV 31  /* loc1 loc2  --  loc2 := loc1                     */
#define TAR 32  /* arg put_fn  --  save passed argument for trace  */
#define PSH 33  /* loc  --  push contents of loc onto stack        */
                /* equivalent to  ALS 1; STI SP, 0, loc            */
/* #define MVI 34  loc1 loc2  --  *loc2++ := *loc1++               */
#define ADP 35  /* loc1 offset loc2  --  loc2 := &loc1[offset]     */
		/* Add a (word) offset to a pointer value          */
		/* Unlike LDI and STI, offset here is a location   */
#define CLC 36  /* nargs -- Call to a non-Russell routine or a BSF */
		/* style Russell routine.               Nargs      */
		/* is an integer.  The routine name is given by    */
		/* a preceding LBA instruction.  The arguments     */
		/* are supplied in reverse order by prior          */
		/* ARG instructions.                               */

#define ALA 37  /* size loc -- loc := ptr to new atomic heap       */
		/* object.  Object should never contain pointer    */
		/* data.                                           */
#define HINT 38 /* kind arg1 arg2  -- Code generator hint.         */
#define    OPT 1     /* n  --  The next n instructions perform     */
		     /* runtime checks, and may be discarded at    */
		     /* suitably high levels of optimization.      */
#define    NP 2      /* The following ARG instruction refers to a  */
		     /* pointer to an object that may be on the    */
		     /* heap.  The called procedure does not       */
		     /* preserve references to this object.  Thus  */
		     /* the ARG instruction may be disregarded for */
		     /* purposes of static ref. counting           */
#define    AL 3      /* size  atom -- (size is a constant).  The   */
		     /* following procedure call (CLC, CLL, CLI)   */
		     /* returns a new object of the indicated      */
		     /* size.  If size is != 0 the object may      */
		     /* be deallocated by attaching it directly    */
		     /* to the free list of the indicated size.    */
		     /* If size is = 0,  the size of the object    */
		     /* is unknown, and the object could be        */
		     /* statically allocated; rfree should be      */
		     /* called to release it.                      */
		     /* Atom is either 0 or 1, and                 */
		     /* specifies whether the allocated object is  */
		     /* atomic, and thus should be returned to the */
		     /* atomic object free list.                   */
#define    DEA 4     /* loc size -- Indicates that                 */
		     /* the object pointed to by loc may be freed. */
		     /* This type of hint is inserted by the       */
		     /* static reference counter for the benefit   */
		     /* of the code generator.                     */
		     /* A negative size indicates the object is    */
		     /* atomic; a positive size means composite,   */
		     /* and a 0 size means unknown.                */
		     /* Size is a constant, not a location.        */
#define    NSC 5     /* The following CLL or CLI instruction       */
		     /* does not result in a saved continuation.   */
		     /* Thus the reference counts of objects       */
		     /* pointed to only by virtual registers       */
		     /* is unchanged across the call.              */
		     /* (This is always the case for CLC calls.)   */
#define    STSZ 6    /* loc  --  The following STI instruction     */
		     /* should store the size of loc if it's known */
		     /* that loc has exactly one reference, and 0  */
		     /* otherwise. Loc contains a pointer to a     */
		     /* composite object.                          */
#define    PT 7      /* Analogous to NP, but a single reference is */
		     /* preserved (passed through) as the function */
		     /* result.                                    */
#define    DEAD 8    /* loc  --  The value in loc will not be used */
		     /* again.  (The location itself may be reused.*/
		     /* Thus UDC would be inappropriate.)          */
#define    GFU 9     /* GF is about to be updated, or AR is about  */
		     /* to be updated inside a BSF function.       */
		     /* Such a hint always precedes the first such */
		     /* update in a routine.                       */
#define    LIVE 10   /* loc --  The value in loc should be viewed  */
		     /* as live up to this point.  Such a hint is  */
		     /* included whenever a location (other than   */
		     /* GF and AR) could appear to be dead, but a  */
		     /* derived pointer is not.  A value in such a */
		     /* location needs to be retained, so as not   */
		     /* to confuse the garbage collector.          */
		     /* We do not generate such HINTs if it is     */
		     /* known that the value in question is also   */
		     /* stored in an accessible memory location.   */
		     /* Similarly, we do not generate such a HINT  */
		     /* if the derived pointer is only implicit    */
		     /* in an LDI or STI instruction.              */
#define    ET 11     /* Defines the type of the following item     */
		     /* declared by an EXT.  All EXTs that do not  */
		     /* refer to functions are preceded by such a  */
		     /* hint. The second argument is a type        */
		     /* specifier, as for the DCL instruction.     */
		     /* Note that the type refers to the object    */
		     /* stored at the location, not the location   */
		     /* itself.					   */
#define    ONS 12    /* The following allocation instruction may   */
		     /* be implemented as a stack allocation.      */
		     /* This is used only if the compiler is not   */
		     /* allowed to generate ALS instructions (-f   */
		     /* or -F flag).  Each allocation instruction  */
		     /* preceded by a HINT ONS is eventually       */
		     /* followed by a corresponding HINT ONS;      */
		     /* HINT DEA sequence.  All such pairs are     */
		     /* properly nested.  There are no branches    */
		     /* into or our of such a pair.                */
#define ARG 39  /* n loc  --  Pass loc as the nth argument to CLC  */
		/* ARG instructions always occur in reverse order, */
		/* with highest numbered argument first. The       */
		/* lowest numbered argument is numbered 1.         */
		/* Also used to pass the activation record pointer */
		/* through CLL and CLI calls.                      */
		/* MAY reserve space on the stack.                 */

/* Integer operations */
#define ADI 40  /* op1 op2 result -- result := (int)op1 + (int)op2   */
#define SBI 41  /* op1 op2 result -- result := (int)op1 - (int)op2   */
#define MLI 42  /* op1 op2 result -- result := (int)op1 * (int)op2   */
#define DVI 43  /* op1 op2 result -- result := (int)op1 / (int)op2   */
#define NGI 44  /* op result -- result := -op                        */
#define IDT 45  /* data  --  generate integer data.  May be labeled  */
		/* with an LBA                                       */
		/* Consecutive IDTs w/o intervening LBAs generate    */
		/* consecutive data.                                 */
		/* Data is read-only.                                */
#define EQI 46  /* op1 op2 result -- result := (int)op1 = (int)op2   */
#define LTI 47  /* op1 op2 result -- result := (int)op1 < (int)op2   */
#define GTI 48  /* op1 op2 result -- result := (int)op1 > (int)op2   */
#define NEI 49  /* op1 op2 result -- result := (int)op1 != (int)op2  */
#define LEI 50  /* op1 op2 result -- result := (int)op1 <= (int)op2  */
#define GEI 51  /* op1 op2 result -- result := (int)op1 >= (int)op2  */
#define SHI 52  /* op1 op2 result -- result := arith_shift(op1, op2) */
#define ABI 53  /* op1 result     -- result := abs(op1)              */

/* Boolean operations */
#define TRU 60  /* loc  --  load the constant true into the          */
                /* indicated location.                               */
#define FLS 61  /* loc  --  load the constant false into the         */
		/* indicated location.                               */
		/* The following instructions are assumed to work on */
		/* both Booleans and bit vectors:                    */
#define AND 62  /* op1 op2 result  --  result := op1 & op2           */
#define OR  63  /* op1 op2 result  --  result := op1 | op2           */
		/* The following applies only to Booleans:           */
#define NOT 64  /* op result  --  result := ~ op                     */

/* String operations */
#define LDS 70  /* loc -- put a pointer to the string given by the  */
		/* preceding LBA instruction into loc               */
#define LDC 71  /* index offset loc -- loc := index[offset]         */
		/* Differs from LDI in that offset is a virtual     */
		/* register (location) and it is a BYTE rather than */
		/* word displacement from index.  Used to access    */
		/* individual characters in a string.               */

/* Operations on single precision floating point numbers */
#define ADF 80  /* op1 op2 result -- result := (float)op1 + (float)op2   */
#define SBF 81  /* op1 op2 result -- result := (float)op1 - (float)op2   */
#define MLF 82  /* op1 op2 result -- result := (float)op1 * (float)op2   */
#define DVF 83  /* op1 op2 result -- result := (float)op1 / (float)op2   */
#define NGF 84  /* op result -- result := - (float) op                   */
#define EXF 85  /* op1 result --  result := (int) x   s.t.               */
		/*   2 **(x-1) <= op1 < 2**x                             */
#define EQF 86  /* op1 op2 result -- result := (float)op1 = (float)op2   */
#define LTF 87  /* op1 op2 result -- result := (float)op1 < (float)op2   */
#define GTF 88  /* op1 op2 result -- result := (float)op1 > (float)op2   */
#define NEF 89  /* op1 op2 result -- result := (float)op1 != (float)op2  */
#define LEF 90  /* op1 op2 result -- result := (float)op1 <= (float)op2  */
#define GEF 91  /* op1 op2 result -- result := (float)op1 >= (float)op2  */
#define SHF 92  /* op1 op2 result -- result := (float)op1 * 2** (int)op2 */

#define N_OP_CODES 100

/* predefined locations */
#define AR 1    /* activation record pointer             */
#define SP 2    /* stack pointer                         */
#define GF 3    /* pointer to global act. rec.           */
		/* explicitly saved and restored by      */
		/* intermediate code if it is updated.   */
		/* Thus it can easily be assigned to a   */
		/* fixed register.                       */
#define UN 4    /* location containing "undefined" value */
#define SK 5    /* value sink, nothing comes out ...     */
		/* An instruction using SK as            */
		/* destination or an STI SK,... is a     */
		/* noop                                  */
#define RL 6    /* location for function result          */
                /* capable of holding max size object    */
#define RS 0x30000000
		/* result of operation. In-line code only  */
		/* Can't be legitimate operand to anything */
		/* but LDN                                 */
#define TL 8    /* Location tested in conditional branches */
		/* Dead after first reference.             */
#define C0 10   /* always 0                              */
#define C1 11   /* always 1                              */
#define C2 12   /* always 2                              */
#define C3 13   /* always 3                              */
#define C4 14   /* always 4                              */
/* T1 and T2 are short term temporaries.  They should be */
/* used only in fixed code sequences, not involving      */
/* arbitrary embedded code.  T1 is mapped to the same    */
/* location as RL, and may thus conflict with RS in      */
/* inline code sequences.  T2 will never be specified as */
/* the result location for an inline code sequence.      */
/* T1 is assumed to be predeclared.  T2 is not treated   */
/* specially by the final code generator.                */
/* Since T1 and RL are the same, and will presumably be  */
/* mapped to the location used for function results, we  */
/* assume that T1 may be clobbered by ALA and ALH.       */
#define T1 RL
#define T2 16

/* Note: All general locations are expected to be saved and */
/* restored on procedure call.                              */

#define FIRST_AVAIL_LOC 20

/*  Activation record layout (all parts optional):
 *
 *   0: static link
 *   1: 1st arg
 *      ...
 *      locals
 */
#define AR_SL 0

/* 
 *  Function object layout:
 *
 *  0: size
 *  1: ep
 *  2: ip
 */
#define FO_SIZE 0
#define FO_EP   1
#define FO_IP   2
