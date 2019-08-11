/**/
/* File data type.  This implements a minimal interface to UNIX files
/**/
#include "types.h"

#define FILESZ 1


/* File_New: func[] var File */

MkIP(File_New())
{
register struct obj *op;    /* var File */
    op = ralloc_comp(FILESZ);
    op->obj_component[0] = 0;
    return(op);
}

MkFVAL0(File_New);


/* File_Assign: func[var File; val File] val File */

MkIP(File_Assign(lop,rop))
struct obj *lop;    /* var File */
struct obj *rop;    /* val File */
{
    lop->obj_component[0] = ((word)rop);
    return(rop);
}

MkFVAL2(File_Assign);


/* File_ValueOf: func[var File] val File */

MkIP(File_ValueOf(aop))
struct obj *aop;    /* var File */
{
    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(File_ValueOf);


/* File_Eq: func[x,y: val File] val File */

MkIP(File_Eq(opx,opy))
struct obj *opx;    /* val File */
struct obj *opy;    /* val File */
{
    return( (struct obj *)
	( opx == opy )
    );
}

MkFVAL2(File_Eq);


/*  close: func [val File] val Void  */

MkIP(File_Close(f))
struct obj * f;
{
    if (fclose((FILE *)f) == EOF) {
        russell_error("Close Failed\n");
    }
}

MkFVAL1(File_Close);


/*  flush: func [val File] val Void  */

MkIP(File_Flush(f))
struct obj * f;
{
    if (fflush((FILE *)f) == EOF) {
        russell_error("Flush Failed\n");
    }
}

MkFVAL1(File_Flush);


/*  eof: func [val File; var Void] val Boolean  */

MkIP(File_Eof(f,fs))
struct obj * f;
struct obj * fs;
{
    return((struct obj *)(feof((FILE *)f) != 0));
}

MkFVAL1(File_Eof);


/*  open: func [name, mode: val ChStr] val File  */

MkIP(File_Open(name, mode))
struct obj * name;
struct obj * mode;
{
    return((struct obj *)(fopen((char *)name, (char *)mode)));
}

MkFVAL2(File_Open);


/*  write: func [f: val File; s: val ChStr] val ChStr  */

MkIP(File_Write(f, s))
struct obj * f;
struct obj * s;
{
    fputs((char *)s, (FILE *)f);
    return(s);
}

MkFVAL2(File_Write);


/*  writeb: func [f: val File; s: val Short] val Short  */
 
MkIP(File_Writeb(f, c))
struct obj * f;
struct obj * c;
{
    putc((char) c, (FILE *)f);
    return(c);
}

MkFVAL2(File_Writeb);


/* readc: func [f: val File; var Void] val ChStr  */

MkIP(File_Readc(f, filsys))
struct obj *f;
struct obj *filsys;
{
long c;
struct obj *(Short_to_ChStr());

    c = getc((FILE *)f);
    return(
	Short_to_ChStr( (struct obj *)c )
    );
}

MkFVAL1(File_Readc);


/* readb: func [f: val File; var Void] val Short  */
 
MkIP(File_Readb(f, filsys))
struct obj *f;
struct obj *filsys;
{
long c;

    c = getc((FILE *)f);
    return((struct obj *)c);
}

MkFVAL1(File_Readb);


/* seek: func [f: val File; val Long] val Void  */

MkIP(File_Seek(f,offset))
struct obj * f;
struct obj * offset;
{
    if (offset -> obj_component[0] != 1) {
        russell_error("Absurdly big seek offset\n");
    }
    if (fseek((FILE *)f, offset -> obj_component[1], 0) == -1) {
        russell_error("Seek Failed\n");
    }
}

MkFVAL2(File_Seek);


/*  stdin, stdout, stderr, null: func[] val File */

MkIP(File_Stdin())
{
    return( (struct obj *) stdin );
}

MkFVAL0(File_Stdin);


MkIP(File_Stdout())
{
    return( (struct obj *) stdout );
}

MkFVAL0(File_Stdout);


MkIP(File_Stderr())
{
    return( (struct obj *) stderr );
}

MkFVAL0(File_Stderr);


MkIP(File_Null())
{
    return( (struct obj *) NULL );
}

MkFVAL0(File_Null);


MkTVAL(File) = {
    &FVAL(File_Assign),
    &FVAL(File_Eq),
    &FVAL(File_New),
    &FVAL(File_Null),
    &FVAL(File_ValueOf),
    &FVAL(File_Close),
    &FVAL(File_Eof),
    &FVAL(File_Flush),
    &FVAL(File_Open),
    &FVAL(File_Readb),
    &FVAL(File_Readc),
    &FVAL(File_Seek),
    &FVAL(File_Stderr),
    &FVAL(File_Stdin),
    &FVAL(File_Stdout),
    &FVAL(File_Write),
    &FVAL(File_Writeb),
};
