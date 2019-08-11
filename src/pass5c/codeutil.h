
#ifdef  DEBUG
#define putcomment1(a,b) {fprintf(Voutfile,a,(b)); fprintf(Voutfile,"\n");}
#else
#define putcomment1(a,b) fprintf(Voutfile,"\n")
#endif

#ifdef	DEBUG
#define putcomment2(a,b,c) {fprintf(Voutfile,a,(b),(c)); fprintf(Voutfile,"\n");}
#else
#define putcomment2(a,b,c) fprintf(Voutfile,"\n")
#endif

#ifdef	DEBUG
#define putcomment(a) {fprintf(Voutfile,"\t%s\n",a);}
#else
#define putcomment(a) fprintf(Voutfile,"\n")
#endif

/*
 *   Size of an object in storage units (bytes)
 */
#define	ObjSize	4

/* First available slot in activation record for parameters/locals.
 *    This reserves 1 object to be the static link
 */
#define AR_FIRST_PARM	1

/*  
 *  Function Object description
 */
#define FO_SIZE	0
#define FO_EP	FO_SIZE+ObjSize
#define FO_IP	FO_EP+ObjSize
#define FO_OBJ_SIZE	3

#ifdef	DEBUG
#define ASSERT(a,b) { if (!(a)) {dbgmsg(b);abort();} ;}
#else
#define ASSERT(a,b)
#endif

#ifdef	DEBUG
#define ASSERT2(a,b,c)  {if (!(a)) {dbgmsg(b,c);abort();};}
#else
#define	ASSERT2(a,b,c)
#endif


#define	R0	0x0001
#define R1	0x0002
#define R2	0x0004
#define	R3	0x0008
#define R4	0x0010
#define R5	0x0020
#define R6	0x0040
#define R7	0x0080
#define R8	0x0100
#define R9	0x0200
#define R10	0x0400
#define R11	0x0800
#define Rap	0x1000
#define Rfp	0x2000
#define Rsp	0x4000
#define Rpc     0x8000

/* registers changed by movc instructions */
#define movc_regs (R2|R3|R4|R5)   /* We assume r0 and r1 are not preserved */


#define POP(reg,com) {\
	fprintf (Voutfile,"\tmovl\t(sp)+,%s",reg);\
	putcomment (com);}

#define	POP_DISP(reg,disp,com) {\
	fprintf (Voutfile,"\tmovl\t(sp)+,%d(%s)",(disp)*ObjSize,reg);\
	putcomment (com);}

#define PUSH(reg,com) {\
	fprintf (Voutfile,"\tpushl\t%s",reg);\
	putcomment (com);}

#define	PUSH_DISP(reg,disp,com) {\
	fprintf (Voutfile,"\tpushl\t%d(%s)",(disp)*ObjSize,reg);\
	putcomment (com);}

/*
 *      Frame pointer cache
 *          (These never point to otherwise inaccessible frames -
 *           The garbage collector does not need to know about them.)
 */

#define L0fp "r9"  /* frame pointer for globals */
#define L0FP R9

/* more registers may be used later */   

/*
 *      Move activation record pointer for level level, to dest.
 *      If new register is needed use rdest.
 *      Note : Arguments should be parenthesized.
 *             If rdest is used then we will have dest = rdest (as pointers).
 *             
 */

#define DISPLAY(dest,level,rdest,com) {\
        if (level == 0) { \
            dest = L0fp; \
        } else if (level == Vlevel) { \
            dest = "ap"; \
	} else { \
	    int i = Vlevel - level;  /* number of indirections needed */ \
	    char * source = "ap"; \
\
	    while (i >= 2) { \
		fprintf(Voutfile,"\tmovl\t*(%s),%s", source, rdest); \
		i =- 2; source = rdest; \
		if (i > 0) fprintf(Voutfile, "\n"); \
	    } \
	    if (i == 1) { \
		fprintf(Voutfile,"\tmovl\t(%s),%s", source, rdest); \
	    } \
	    dest = rdest; \
	} \
	putcomment (com);}

#define CODE(line)      { fputs(line, Voutfile); putc('\n', Voutfile); }

/* Assumes the size is in r1: */
#define NEWOBJ { \
  CODE("\tmoval\t_objfreelist[r1],r10");\
  CODE("\tmovl\t(r10),r0");\
  CODE("\tjneq\t1f");\
  SET_GC_INFO;\
  CODE("\tpushl\tr1");\
  CODE("\tcalls\t$1,_allocobj");\
  CODE("1:\tmovl\t(r0),(r10)");\
  Ventry_mask |= R10;}

#define FXD_NEWOBJ(s) {\
  fprintf(Voutfile,"\tmoval\t_objfreelist+%d,r10\n", 4*(s));\
  CODE("\tmovl\t(r10),r0");\
  CODE("\tjneq\t1f");\
  SET_GC_INFO;\
  fprintf(Voutfile, "\tpushl\t$%d\n", (s));\
  CODE("\tcalls\t$1,_allocobj");\
  CODE("1:\tmovl\t(r0),(r10)");\
  Ventry_mask |= R10;}

#define	SET_GC_INFO {\
	fprintf(Voutfile,"\tmovl\t$0x%x,r11\n",(Vgc_mask<<16));\
	Ventry_mask |= R11;}

#define	ASM_HEADER {\
	CODE("\t.globl  _russell_entry"                 );\
	CODE("\t.globl  _objfreelist"                   );\
	CODE("\t.globl  _allocobj"                      );\
	CODE("\t.globl  _entry_ar_sz"                   );\
	CODE("\t.text"                                  );\
	CODE("\t.align  1"                              );\
	CODE("_russell_entry:"				);\
	CODE("\t.word   0"                              );\
	CODE("\tcallg   *4(ap),russell_top_level"       );\
	CODE("\tret"                                    );}

void Vretreg();

char * Vnewreg();

char * Vnewlabel();

extern int Ventry_mask;
extern int Vgc_mask;

extern int Vreg_bit;  /* set by Vnewreg */
