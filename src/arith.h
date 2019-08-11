/*
 *  mod(a,b)
 *  non_negative a,b; b is a power of 2
 *  Computes a mod b
 */
#define mod(a,b) ((a) & ((b) - 1))

/*
 *  rounddown(a,b)
 *	non-negative a, b;
 *  Compute a rounded down to the nearest multiple of b where b is a 
 *  power of two.
 */
# define rounddown(a,b)   (  (a) & ~((b) - 1)  )

/*
 *	roundup(n,mult)
 *	non-negative  a, b;
 *	Round n up to the nearest multiple of mult.
 *	mult must be a power of two.
 */
# define roundup(n,mult)   rounddown( (n) + (mult) - 1, (mult) )
