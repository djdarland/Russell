/**/
/* chstr.c
/*
/* varying-length character string type
/*
/* representation:
/*
/*  - each value is represented as in C, except that an integral number
/*    of words are used.
/*  - each variable is a cell containing an object pointer
/**/

#include "types.h"

#include <stdio.h>

#define CHSTRVARSZ 1

/* ChStr_New: func[] var ChStr */

char empty_ChStr[] = "";

MkIP(ChStr_New())
{
register struct obj *op;    /* var ChStr */

   op = ralloc_comp(CHSTRVARSZ);
    
    op->obj_component[0] = UNINIT;
    return(op);
}

MkFVAL0(ChStr_New);


MkIP(ChStr_init_New(x))
struct obj * x;
{
register struct obj *op;    /* var ChStr */
struct obj *valp;

    op = ralloc_comp(CHSTRVARSZ);
       
    op->obj_component[0] = (word) x;
    return(op);
}

MkFVAL1(ChStr_init_New);


/* ChStr_Assign: func[var ChStr; val ChStr] val ChStr */

MkIP(ChStr_Assign(lop,rop))
struct obj *lop;    /* var ChStr */
struct obj *rop;    /* val ChStr */
{
    lop->obj_component[0] = (word)rop;
    return(rop);
}

MkFVAL2(ChStr_Assign);


/* ChStr_ValueOf: func[var ChStr] val ChStr */

MkIP(ChStr_ValueOf(aop))
struct obj *aop;    /* var ChStr */
{
    return((struct obj *)(aop->obj_component[0]));
}

MkFVAL1(ChStr_ValueOf);


/* ChStr_Length: func[val ChStr] val Short */

MkIP(ChStr_Length(aop))
struct obj *aop;    /* val ChStr */
{
    return((struct obj *) strlen((char *) (aop->obj_component)));
}

MkFVAL1(ChStr_Length);


/* ChStr_eq: func[val ChStr; val ChStr] val Boolean */

MkIP(ChStr_eq(x, y))
struct obj *x, *y;    /* val ChStr */
{
    return((struct obj *) (strcmp((char *) (x -> obj_component),
                                  (char *) (y -> obj_component)) == 0));
}

MkFVAL2(ChStr_eq);


/* ChStr_ne: func[val ChStr; val ChStr] val Boolean */

MkIP(ChStr_ne(x, y))
struct obj *x, *y;    /* val ChStr */
{
    return((struct obj *) (strcmp((char *) (x -> obj_component),
                                  (char *) (y -> obj_component)) != 0));
}

MkFVAL2(ChStr_ne);


/* ChStr_lt: func[val ChStr; val ChStr] val Boolean */

MkIP(ChStr_lt(x, y))
struct obj *x, *y;    /* val ChStr */
{
    return((struct obj *) (strcmp((char *) (x -> obj_component),
                                  (char *) (y -> obj_component)) == -1));
}

MkFVAL2(ChStr_lt);


/* ChStr_le: func[val ChStr; val ChStr] val Boolean */

MkIP(ChStr_le(x, y))
struct obj *x, *y;    /* val ChStr */
{
    return((struct obj *) (strcmp((char *) (x -> obj_component),
                                  (char *) (y -> obj_component)) <= 0));
}

MkFVAL2(ChStr_le);


/* ChStr_gt: func[val ChStr; val ChStr] val Boolean */

MkIP(ChStr_gt(x, y))
struct obj *x, *y;    /* val ChStr */
{
    return((struct obj *) (strcmp((char *) (x -> obj_component),
                                  (char *) (y -> obj_component)) == 1));
}

MkFVAL2(ChStr_gt);


/* ChStr_ge: func[val ChStr; val ChStr] val Boolean */

MkIP(ChStr_ge(x, y))
struct obj *x, *y;    /* val ChStr */
{
    return((struct obj *) (strcmp((char *) (x -> obj_component),
                                  (char *) (y -> obj_component)) >= 0));
}

MkFVAL2(ChStr_ge);


/*  ChStr_Concat: func[x,y: Val ChStr] val ChStr */

MkIP(ChStr_Concat(opx,opy))
struct obj *opx;    /* val ChStr */
struct obj *opy;    /* val ChStr */
{
register struct obj *op;    /* val ChStr */
register int lw;
int lx, ly, lgth;

    lx = strlen((char *)(opx->obj_component));
    ly = strlen((char *)(opy->obj_component));

#   ifdef UNDEFINED
    /* Confuses the static reference counter */
      if( lx == 0 )
        return( opy );
      if( ly == 0 )
	return( opx );
#   endif

    lgth = lx + ly + 1;
    lw = BYTES_TO_WORDS(lgth + BYTES_PER_WORD - 1);  /* round up */

    op = ralloc(lw);

    strcpy((char *)(op -> obj_component), (char *)(opx -> obj_component));
    strcat((char *)(op -> obj_component), (char *)(opy -> obj_component));

    return(op);
}

MkFVAL2(ChStr_Concat);


/*  ChStr_Sub: func[x: val ChStr; start,length: val Short ] val ChStr */
/*    start and limit are zero-origin; limit-start is result length  */

MkIP(ChStr_Sub(opx,start,length))
struct obj *opx;    /* val ChStr */
struct obj *start;  /* val Short */
struct obj *length; /* val Short */
{
register struct obj *op;    /* val ChStr */
int lgth, lw;

    lgth = strlen((char *)(opx -> obj_component)) - (int)start;
    if( lgth > (int)length )  lgth = (int)length;

    if( ((int)start < 0) || lgth < 0 ) {
	/* Return a newly allocated empty string.                         */
	/* (A statically allocated one would confuse the static reference */
	/* counter.)                                                      */
	op = ralloc(1);
	op -> obj_component[0] = 0;
	return( (struct obj *) op );
    }

    lgth++;
    lw = BYTES_TO_WORDS(lgth + BYTES_PER_WORD - 1);  /* round up */
    op = ralloc(lw);
    strncpy((char *)(op -> obj_component),
            ((char *)(opx -> obj_component)) + (int) start, lgth-1);
    *(((char *)(op -> obj_component)) + lgth - 1) = '\0';

    return(op);
}

MkFVAL3(ChStr_Sub);


/* ChStr_Put: func[ val ChStr ] val ChStr */

MkIP(ChStr_Put(s))
register struct obj *s;
{
    fputs((char *) (s -> obj_component), stdout);
	/* Note that this is different from puts(...) */

    return(s);
}

MkFVAL1(ChStr_Put);


/* ChStr_Getchar: func[ var FS ] val ChStr */

MkIP(ChStr_Getchar())
{
long c;
struct obj *(Short_to_ChStr());

    c = getchar();
    return(
	Short_to_ChStr( (struct obj *)c )
    );
}

MkFVAL0(ChStr_Getchar);


/* Conversion functions: ChStr_to_Short and Short_to_ChStr */

MkIP(Short_to_ChStr(x))
register struct obj *x;
{
register struct obj *op;

    op = ralloc(1);

    ((char *) (op->obj_component))[0] = (char) (((int) x) & 0377);
    ((char *) (op->obj_component))[1] = '\0';
    return( op );
}

MkFVAL1(Short_to_ChStr);


MkIP(ChStr_to_Short(s))
register struct obj *s;
{
    return( (struct obj *)(((char *)(s->obj_component))[0]) );
}

MkFVAL1(ChStr_to_Short);

/* ChStr_Dot: func[val ChStr; i: val Short] val Short      */
/* Return code of ith character                            */

struct obj * ChStr_unchecked_Dot(s,i)
register struct obj *s;   
register long i;          
{                         
    return( (struct obj *)(((char *)(s->obj_component))[i]) );
}

void ChStr_check_Dot(s,i)
register struct obj *s;   
register long i;          
{
    int len = strlen((char *)s);
    
    if (i < 0) {
	russell_error("Negative string index\n");
    } else if (i >= len) {
	fprintf(stderr, "Attempt to access %dth character of string \"%s\"\n",
		      i, ((char *)s));
	print_tr_stack();
	ABORT("Bad string subscription\n");
    }
}

MkIP(ChStr_Dot(s,i))
register struct obj *s;
register long i;
{
    ChStr_check_Dot(s,i);
    return( (struct obj *)(((char *)(s->obj_component))[i]) );
}

MkFVAL2(ChStr_Dot);


/*  ChStr constants are func[]: val ChStr */
/*  Value is encoded in ep                */

MkIP(ChStr_Const(A))
struct ar * A;
{
  /*
   * this simply moves the first word of the activation record
   * into the function result return register
   */
    return((struct obj *)(A -> ar_static_link));
}

MkEPFVAL0(ChStr_c_null,ChStr_Const,"");

#define MkCHSTRCONST(nm,val) \
char ChStr_v_/**/nm [2] = { val, 0 }; \
MkEPFVAL0(ChStr_c_/**/nm,ChStr_Const,ChStr_v_/**/nm)

MkCHSTRCONST(9,9);
MkCHSTRCONST(10,10);
MkCHSTRCONST(13,13);
MkCHSTRCONST(32,32);
MkCHSTRCONST(33,33);
MkCHSTRCONST(34,34);
MkCHSTRCONST(35,35);
MkCHSTRCONST(36,36);
MkCHSTRCONST(37,37);
MkCHSTRCONST(38,38);
MkCHSTRCONST(39,39);
MkCHSTRCONST(40,40);
MkCHSTRCONST(41,41);
MkCHSTRCONST(42,42);
MkCHSTRCONST(43,43);
MkCHSTRCONST(44,44);
MkCHSTRCONST(45,45);
MkCHSTRCONST(46,46);
MkCHSTRCONST(47,47);
MkCHSTRCONST(48,48);
MkCHSTRCONST(49,49);
MkCHSTRCONST(50,50);
MkCHSTRCONST(51,51);
MkCHSTRCONST(52,52);
MkCHSTRCONST(53,53);
MkCHSTRCONST(54,54);
MkCHSTRCONST(55,55);
MkCHSTRCONST(56,56);
MkCHSTRCONST(57,57);
MkCHSTRCONST(58,58);
MkCHSTRCONST(59,59);
MkCHSTRCONST(60,60);
MkCHSTRCONST(61,61);
MkCHSTRCONST(62,62);
MkCHSTRCONST(63,63);
MkCHSTRCONST(64,64);
MkCHSTRCONST(65,65);
MkCHSTRCONST(66,66);
MkCHSTRCONST(67,67);
MkCHSTRCONST(68,68);
MkCHSTRCONST(69,69);
MkCHSTRCONST(70,70);
MkCHSTRCONST(71,71);
MkCHSTRCONST(72,72);
MkCHSTRCONST(73,73);
MkCHSTRCONST(74,74);
MkCHSTRCONST(75,75);
MkCHSTRCONST(76,76);
MkCHSTRCONST(77,77);
MkCHSTRCONST(78,78);
MkCHSTRCONST(79,79);
MkCHSTRCONST(80,80);
MkCHSTRCONST(81,81);
MkCHSTRCONST(82,82);
MkCHSTRCONST(83,83);
MkCHSTRCONST(84,84);
MkCHSTRCONST(85,85);
MkCHSTRCONST(86,88);
MkCHSTRCONST(87,87);
MkCHSTRCONST(88,88);
MkCHSTRCONST(89,89);
MkCHSTRCONST(90,90);
MkCHSTRCONST(91,91);
MkCHSTRCONST(92,92);
MkCHSTRCONST(93,93);
MkCHSTRCONST(94,94);
MkCHSTRCONST(95,95);
MkCHSTRCONST(96,96);
MkCHSTRCONST(97,97);
MkCHSTRCONST(98,98);
MkCHSTRCONST(99,99);
MkCHSTRCONST(100,100);
MkCHSTRCONST(101,101);
MkCHSTRCONST(102,102);
MkCHSTRCONST(103,103);
MkCHSTRCONST(104,104);
MkCHSTRCONST(105,105);
MkCHSTRCONST(106,106);
MkCHSTRCONST(107,107);
MkCHSTRCONST(108,108);
MkCHSTRCONST(109,109);
MkCHSTRCONST(110,110);
MkCHSTRCONST(111,111);
MkCHSTRCONST(112,112);
MkCHSTRCONST(113,113);
MkCHSTRCONST(114,114);
MkCHSTRCONST(115,115);
MkCHSTRCONST(116,116);
MkCHSTRCONST(117,117);
MkCHSTRCONST(118,118);
MkCHSTRCONST(119,119);
MkCHSTRCONST(120,120);
MkCHSTRCONST(121,121);
MkCHSTRCONST(122,122);
MkCHSTRCONST(123,123);
MkCHSTRCONST(124,124);
MkCHSTRCONST(125,125);
MkCHSTRCONST(126,126);


/*  ChStr - the type value */

MkTVAL(ChStr) = {

#   define V(x) &FVAL(ChStr_c_/**/x)
    V(9),     V(10),    V(13),
    V(32),    V(33),    V(34),    V(35),
    V(36),    V(37),    V(38),    V(39),
    V(40),    V(41),    V(42),    V(43),
    V(44),    V(45),    V(46),    V(47),
    V(48),    V(49),    V(50),    V(51),
    V(52),    V(53),    V(54),    V(55),
    V(56),    V(57),    V(58),    V(59),
    V(60),    V(61),    V(62),    V(63),
    V(64),    V(65),    V(66),    V(67),
    V(68),    V(69),    V(70),    V(71),
    V(72),    V(73),    V(74),    V(75),
    V(76),    V(77),    V(78),    V(79),
    V(80),    V(81),    V(82),    V(83),
    V(84),    V(85),    V(86),    V(87),
    V(88),    V(89),    V(90),    V(91),
    V(92),    V(93),    V(94),    V(95),
    V(96),    V(97),    V(98),    V(99),
    V(100),   V(101),   V(102),   V(103),
    V(104),   V(105),   V(106),   V(107),
    V(108),   V(109),   V(110),   V(111),
    V(112),   V(113),   V(114),   V(115),
    V(116),   V(117),   V(118),   V(119),
    V(120),   V(121),   V(122),   V(123),
    V(124),   V(125),   V(126),
#   undef V

    &FVAL(ChStr_c_null),
    &FVAL(ChStr_Dot),
    &FVAL(ChStr_Assign),
    &FVAL(ChStr_lt),
    &FVAL(ChStr_le),
    &FVAL(ChStr_ne),
    &FVAL(ChStr_eq),
    &FVAL(ChStr_gt),
    &FVAL(ChStr_ge),
    &FVAL(Short_to_ChStr),
    &FVAL(ChStr_New),
    &FVAL(ChStr_init_New),
    &FVAL(ChStr_to_Short),
    &FVAL(ChStr_ValueOf),
    &FVAL(ChStr_Concat),
    &FVAL(ChStr_Getchar),
    &FVAL(ChStr_Length),
    &FVAL(ChStr_Put),
    &FVAL(ChStr_Sub),
};
