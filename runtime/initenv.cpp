/**/
/* initenv.cpp   (defines the initial environment)
/*
/* N.B. don't change the name of this file without
/*      changing the `#include initenv.cpp' below!
/**/

#ifndef ENTER
# define ENTER(x) extern struct obj x;
# include "initenv.cpp"
# undef ENTER
# define ENTER(x) (word)(&x),
  extern int trace_flag;

  static word initenv_ar[] = {
        0, /* space for argc */
        (word)(&trace_flag),
#       include "initenv.cpp"
  };
#else

ENTER(FV_Argv)

ENTER(FV_Eof)

ENTER(FV_r_abort)

ENTER(Void)
ENTER(FV_Void_Null)
ENTER(VAR_Void_0)

ENTER(Bool)

ENTER(Short)

ENTER(SFloat)

ENTER(Float)

ENTER(ChStr)

ENTER(FV_Alias)

ENTER(FV_Array)

ENTER(FV_Ref)

ENTER(File)

ENTER(FV_List)

ENTER(FV_LList)

ENTER(Long)

ENTER(FV_Callcc)

ENTER(FV_Signal)

ENTER(FV_UnSignal)

ENTER(FV_expand_hp)

#endif
