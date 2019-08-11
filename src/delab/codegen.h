#define MAXLABELSZ 512
#define MISSING -2      /* nonexistent entry in table */

/* code generation macros */
#define genl(op,l)  genl_RIC(Goutfile,op,l)
#define gen0(op)  gen_RIC(Goutfile,op,SK,SK,SK)
#define gen1(op,a1)  gen_RIC(Goutfile,op,a1,SK,SK)
#define gen2(op,a1,a2)  gen_RIC(Goutfile,op,a1,a2,SK)
#define gen3(op,a1,a2,a3)  gen_RIC(Goutfile,op,a1,a2,a3)
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

#define Gallocate Vallocate  /* For the moment they can be the same */
#define Gnewlabel Vnewlabel

/* allocate function object */
#define ALLOC_FO(loc) {\
         gen2(ALH, C3, loc); \
}

#define DISPLAY(dest,level) {\
        if (level == 0) { \
            dest = GF; \
        } else if (level == Glevel) { \
            dest = AR; \
	} else { \
	    int i = Glevel - level;  /* number of indirections needed */ \
            int source = AR; \
\
            dest = avail_loc++; \
            gen2(DCL, dest, DCL_ADDR); \
            while (i != 0) { \
                gen3(LDI, source, 0, dest); \
		source = dest; \
		i--; \
            } \
        }\
}

#define UNDISPLAY(dest) {\
        if (dest != GF && dest != AR) {\
	    gen2(UDC, dest, 1);\
        }\
}

