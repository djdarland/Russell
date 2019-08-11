typedef long word;
typedef word (*word_func_ptr)();
extern word * GC_malloc();
extern void GC_free();
extern word * GC_malloc_atomic();
#define alloc(sz,result) *(word **)(&result) = GC_malloc((sz)<<2)
#define alloc_nc(sz,result) result = GC_malloc((sz)<<2)
#define alloc_cw(sz,result) result = (word)GC_malloc((sz)<<2)
#define alloc_a(sz,result) *(word **)(&result) = GC_malloc_atomic((sz)<<2)
#define alloc_a_nc(sz,result) result = GC_malloc_atomic((sz)<<2)
#define alloc_a_cw(sz,result) result = (word)GC_malloc_atomic((sz)<<2)
#define abs(x) ((x) > 0? (x) : -(x))
#define shift(x, n) ((n) < 0? (x) >> (-(n)) : (x) << (n))
extern word global_ar[];
extern word m_test();
extern word fn_test_ln1_0();

word m_test(AR)
register word * AR;
{
register word RL;
word TL;
word tmp00;
alloc_cw(3,RL);
tmp00 = (word)((word *)fn_test_ln1_0);
((word *)RL)[(word)2] = (word)((word *)fn_test_ln1_0);
((word *)RL)[(word)0] = (word)1;
return((word)RL);
}

extern word fn_test_ln1_0();

word fn_test_ln1_0(AR)
register word * AR;
{
register word RL;
word TL;
return((word)0);
}

