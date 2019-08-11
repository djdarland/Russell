/*****************************************/
/*                                       */
/* Russell                                */
/*                                       */
/* Includes for Builtin Type Definitions */
/*                                       */
/*****************************************/

#define _TYPES_
#ifndef _RUNTIME_
#include "runtime.h"
#endif

#define MkIP(hdr) /* static */ struct obj * hdr

#ifdef _STDC_
#  define FVAL(nm) FV_##nm
#else
#  define FVAL(nm) FV_/**/nm
#endif

/* Name of conversion function corresponding to C function nm */
#ifdef _STDC_
#  define CF(nm) CF_##nm
#else
#  define CF(nm) CF_/**/nm
#endif

/* Conversion functions for functions with varying numbers of arguments */
struct ar {
    struct ar * ar_static_link;   /* For many builtin functions this    */
				  /* is a value further describing the  */
				  /* function.                          */
    struct obj * ar_arg1;
    struct obj * ar_arg2;
    struct obj * ar_arg3;
};

/* Note that Russell procedure calls look like they pass a single */
/* argument, which is a pointer to the callee's activation        */
/* record.                                                        */

#define MkCF1(nm) \
    /* static */ struct obj * CF(nm) (A) \
    struct ar * A; \
    { \
	return(nm(A -> ar_arg1)); \
    }

#define MkCF2(nm) \
    /* static */ struct obj * CF(nm) (A) \
    struct ar * A; \
    { \
	return(nm(A -> ar_arg1, A -> ar_arg2)); \
    }

#define MkCF3(nm) \
    /* static */ struct obj * CF(nm) (A) \
    struct ar * A; \
    { \
	return(nm(A -> ar_arg1, A -> ar_arg2, A -> ar_arg3)); \
    }

#define MkFVAL0(nm) \
    /* static */ struct funcobj FVAL(nm) \
    = {1,(struct obj *)0,nm}

#define MkFVAL1(nm) \
    MkCF1(nm) \
    /* static */ struct funcobj FVAL(nm) \
    = {2,(struct obj *)0,CF(nm)}

#define MkFVAL2(nm) \
    MkCF2(nm) \
    /* static */ struct funcobj FVAL(nm) \
    = {3,(struct obj *)0,CF(nm)}

#define MkFVAL3(nm) \
    MkCF3(nm) \
    /* static */ struct funcobj FVAL(nm) \
    = {4,(struct obj *)0,CF(nm)}

#define MkEPFVAL0(nm,ip,ep) \
    /* static */ struct funcobj FVAL(nm) \
    = {1,(struct obj *)(ep),ip}

#define MkTVAL(nm) \
    struct funcobj *(nm[])

#ifdef _STDC_
#  define VAR(nm) VAR_##nm
#else
#  define VAR(nm) VAR_/**/nm
#endif

#define MkVAR(nm,sz) \
    word VAR(nm)[sz]
