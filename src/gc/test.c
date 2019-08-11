/* An incomplete test for the garbage collector.  		*/
/* Some more obscure entry points are not tested at all.	*/
# include <stdlib.h>
# include <stdio.h>
# include "gc.h"
# ifdef PCR
#   include "th/PCR_ThCrSec.h"
#   include "th/PCR_Th.h"
# endif

/* AT_END may be defined to excercise the interior pointer test	*/
/* if the collector is configured with ALL_INTERIOR_POINTERS.   */
/* As it stands, this test should succeed with either		*/
/* configuration.  In the FIND_LEAK configuration, it should	*/
/* find lots of leaks, since we free almost nothing.		*/

struct SEXPR {
    struct SEXPR * sexpr_car;
    struct SEXPR * sexpr_cdr;
};

# ifdef __STDC__
    typedef void * void_star;
# else
    typedef char * void_star;
# endif

typedef struct SEXPR * sexpr;

extern sexpr cons();

# define nil ((sexpr) 0)
# define car(x) ((x) -> sexpr_car)
# define cdr(x) ((x) -> sexpr_cdr)
# define is_nil(x) ((x) == nil)


int extra_count = 0;        /* Amount of space wasted in cons node */

/* Silly implementation of Lisp cons. Intentionally wastes lots of space */
/* to test collector.                                                    */
sexpr cons (x, y)
sexpr x;
sexpr y;
{
    register sexpr r;
    register int *p;
    register my_extra = extra_count;
    
    r = (sexpr) GC_MALLOC(sizeof(struct SEXPR) + my_extra);
    if (r == 0) {
        (void)printf("Out of memory\n");
        exit(1);
    }
    for (p = (int *)r;
         ((char *)p) < ((char *)r) + my_extra + sizeof(struct SEXPR); p++) {
	if (*p) {
	    (void)printf("Found nonzero at %X\n - allocator is broken", p);
	    exit(1);
        }
        *p = 13;
    }
#   ifdef AT_END
	r = (sexpr)((char *)r + (my_extra & ~7));
#   endif
    r -> sexpr_car = x;
    r -> sexpr_cdr = y;
    extra_count = (my_extra + 1) % 5000;
    return(r);
}

/* Return reverse(x) concatenated with y */
sexpr reverse1(x, y)
sexpr x, y;
{
    if (is_nil(x)) {
        return(y);
    } else {
        return( reverse1(cdr(x), cons(car(x), y)) );
    }
}

sexpr reverse(x)
sexpr x;
{
    return( reverse1(x, nil) );
}

sexpr ints(low, up)
int low, up;
{
    if (low > up) {
	return(nil);
    } else {
        return(cons((sexpr)low, ints(low+1, up)));
    }
}

void check_ints(list, low, up)
sexpr list;
int low, up;
{
    if ((int)(car(list)) != low) {
        (void)printf(
           "List reversal produced incorrect list - collector is broken\n");
        exit(1);
    }
    if (low == up) {
        if (cdr(list) != nil) {
           (void)printf("List too long - collector is broken\n");
           exit(1);
        }
    } else {
        check_ints(cdr(list), low+1, up);
    }
}

/* Not used, but useful for debugging: */
void print_int_list(x)
sexpr x;
{
    if (is_nil(x)) {
        (void)printf("NIL\n");
    } else {
        (void)printf("%d", car(x));
        if (!is_nil(cdr(x))) {
            (void)printf(", ");
            (void)print_int_list(cdr(x));
        } else {
            (void)printf("\n");
        }
    }
}

/* Try to force a to be strangely aligned */
struct {
  char dummy;
  sexpr aa;
} A;
#define a A.aa

/*
 * Repeatedly reverse lists built out of very different sized cons cells.
 * Check that we didn't lose anything.
 */
reverse_test()
{
    int i;
    sexpr b;

    a = ints(1, 100);
    b = ints(1, 50);
    for (i = 0; i < 50; i++) {
        b = reverse(reverse(b));
    }
    for (i = 0; i < 10; i++) {
    	/* This maintains the invariant that a always points to a list of */
    	/* 100 integers.  Thus this is thread safe without locks.	  */
        a = reverse(reverse(a));
#	if !defined(AT_END) && !defined(PCR)
	  /* This is not thread safe, since realloc explicitly deallocates */
          if (i & 1) {
            a = (sexpr)GC_REALLOC((void_star)a, 500);
          } else {
            a = (sexpr)GC_REALLOC((void_star)a, 4200);
          }
#	endif
    }
    check_ints(a,1,100);
    check_ints(b,1,50);
    a = b = 0;
}

/*
 * The rest of this builds balanced binary trees, checks that they don't
 * disappear, and tests finalization.
 */
typedef struct treenode {
    int level;
    struct treenode * lchild;
    struct treenode * rchild;
} tn;

int finalizable_count = 0;
int finalized_count = 0;
int dropped_something = 0;

# ifdef __STDC__
  void finalizer(void * obj, void * client_data)
# else
  void finalizer(obj, client_data)
  char * obj;
  char * client_data;
# endif
{
  tn * t = (tn *)obj;
  if ((int)client_data != t -> level) {
     (void)printf("Wrong finalization data - collector is broken\n");
     exit(1);
  }
  finalized_count++;
}

size_t counter = 0;

tn * mktree(n)
int n;
{
    tn * result = (tn *)GC_MALLOC(sizeof(tn));
    
    if (n == 0) return(0);
    if (result == 0) {
        (void)printf("Out of memory\n");
        exit(1);
    }
    result -> level = n;
    result -> lchild = mktree(n-1);
    result -> rchild = mktree(n-1);
    if (counter++ % 119 == 0) {
        GC_REGISTER_FINALIZER((void_star)result, finalizer, (void_star)n,
        		      (GC_finalization_proc *)0, (void_star *)0);
#	ifdef PCR
 	    PCR_ThCrSec_EnterSys();
 	    /* Losing a count here causes erroneous report of failure. */
#	endif
        finalizable_count++;
#	ifdef PCR
 	    PCR_ThCrSec_ExitSys();
#	endif
    }
    return(result);
}

void chktree(t,n)
tn *t;
int n;
{
    if (n == 0 && t != 0) {
        (void)printf("Clobbered a leaf - collector is broken\n");
        exit(1);
    }
    if (n == 0) return;
    if (t -> level != n) {
        (void)printf("Lost a node at level %d - collector is broken\n", n);
        exit(1);
    }
    if (counter++ % 373 == 0) (void) GC_MALLOC(counter%5001);
    chktree(t -> lchild, n-1);
    if (counter++ % 73 == 0) (void) GC_MALLOC(counter%373);
    chktree(t -> rchild, n-1);
}

void alloc_small(n)
int n;
{
    register int i;
    
    for (i = 0; i < n; i += 8) {
        if (GC_MALLOC_ATOMIC(8) == 0) {
            (void)printf("Out of memory\n");
            exit(1);
        }
    }
}

tree_test()
{
    tn * root = mktree(16);
    register int i;
    
    alloc_small(5000000);
    chktree(root, 16);
    if (finalized_count && ! dropped_something) {
        (void)printf("Premature finalization - collector is broken\n");
        exit(1);
    }
    dropped_something = 1;
    root = mktree(16);
    chktree(root, 16);
    for (i = 16; i >= 0; i--) {
        root = mktree(i);
        chktree(root, i);
    }
    alloc_small(5000000);
}

# include "gc_private.h"

int n_tests = 0;

void run_one_test()
{
    DCL_LOCK_STATE;
    
    reverse_test();
    tree_test();
    LOCK();
    n_tests++;
    UNLOCK();
    
}

void check_heap_stats()
{
    (void)printf("Completed %d tests\n", n_tests);
    (void)printf("Finalized %d/%d objects - ",
    		 finalized_count, finalizable_count);
    if (finalized_count > finalizable_count
        || finalized_count < finalizable_count/2) {
        (void)printf ("finalization is probably broken\n");
        exit(1);
    } else {
        (void)printf ("finalization is probably ok\n");
    }
    (void)printf("Total number of bytes allocated is %d\n",
    	         WORDS_TO_BYTES(GC_words_allocd + GC_words_allocd_before_gc));
    (void)printf("Final heap size is %d bytes\n", GC_heapsize);
    if (WORDS_TO_BYTES(GC_words_allocd + GC_words_allocd_before_gc)
        < 33500000*n_tests) {
        (void)printf("Incorrect execution - missed some allocations\n");
        exit(1);
    }
    if (GC_heapsize > 10000000*n_tests) {
        (void)printf("Unexpected heap growth - collector may be broken\n");
        exit(1);
    }
    (void)printf("Collector appears to work\n");
}

#ifndef PCR
main()
{
    n_tests = 0;
    run_one_test();
    check_heap_stats();
    (void)fflush(stdout);
    return(0);
}
# else
test()
{
    PCR_Th_T * th1;
    PCR_Th_T * th2;
    int code;

    n_tests = 0;
    th1 = PCR_Th_Fork(run_one_test, 0);
    th2 = PCR_Th_Fork(run_one_test, 0);
    run_one_test();
    if (PCR_Th_T_Join(th1, &code, NIL, PCR_allSigsBlocked, PCR_waitForever)
        != PCR_ERes_okay || code != 0) {
        (void)printf("Thread 1 failed\n");
    }
    if (PCR_Th_T_Join(th2, &code, NIL, PCR_allSigsBlocked, PCR_waitForever)
        != PCR_ERes_okay || code != 0) {
        (void)printf("Thread 2 failed\n");
    }
    check_heap_stats();
    (void)fflush(stdout);
    return(0);
}
#endif

