/*
 * Copyright (c) 1991-1993 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to copy this garbage collector for any purpose,
 * provided the above notices are retained on all copies.
 * Author: Bill Janssen
 * Modified by: Hans Boehm
 */

/*
 * This is incredibly OS specific code for tracking down data sections in
 * dynamic libraries.  There appears to be no way of doing this quickly
 * without groveling through undocumented data structures.  We would argue
 * that this is a bug in the design of the dlopen interface.  THIS CODE
 * MAY BREAK IN FUTURE OS RELEASES.  If this matters to you, don't hesitate
 * to let your vendor know ...
 */
#include "gc_private.h"
#ifdef DYNAMIC_LOADING
#if !(defined(M68K) && defined(SUNOS)) && !defined(SPARC)
 --> We only know how to find data segments of dynamic libraries under SunOS 4.X
#endif

#include <stdio.h>
#if defined SUNOS5
#   include <sys/elf.h>
#   include <dlfcn.h>
#   include <link.h>
#else
#   include <dlfcn.h>
#   include <link.h>
#   include <a.out.h>
  /* struct link_map field overrides */
#   define l_next	lm_next
#   define l_addr	lm_addr
#   define l_name	lm_name
# endif


#ifdef SUNOS5

static struct link_map *
GC_FirstDLOpenedLinkMap()
{
    extern Elf32_Dyn _DYNAMIC;
    Elf32_Dyn *dp;
    struct r_debug *r;
    static struct link_map * cachedResult = 0;

    if( &_DYNAMIC == 0) {
        return(0);
    }
    if( cachedResult == 0 ) {
        int tag;
        for( dp = ((Elf32_Dyn *)(&_DYNAMIC)); (tag = dp->d_tag) != 0; dp++ ) {
            if( tag == DT_DEBUG ) {
                struct link_map *lm
                        = ((struct r_debug *)(dp->d_un.d_ptr))->r_map;
                if( lm != 0 ) cachedResult = lm->l_next; /* might be NIL */
                break;
            }
        }
    }
    return cachedResult;
}

# endif

# ifdef SUNOS4

static struct link_map *
GC_FirstDLOpenedLinkMap()
{
    extern struct link_dynamic _DYNAMIC;

    if( &_DYNAMIC == 0) {
        return(0);
    }
    return(_DYNAMIC.ld_un.ld_1->ld_loaded);
}


# endif

/* Add dynamic library data sections to the root set.		*/
# if !defined(PCR) && defined(THREADS)
	--> fix mutual exclusion with dlopen
# endif
void GC_register_dynamic_libraries()
{
  struct link_map *lm = GC_FirstDLOpenedLinkMap();
  

  for (lm = GC_FirstDLOpenedLinkMap();
       lm != (struct link_map *) 0;  lm = lm->l_next)
    {
#     ifdef SUNOS4
	struct exec *e;
	 
        e = (struct exec *) lm->lm_addr;
        GC_add_roots_inner(
      		    ((char *) (N_DATOFF(*e) + lm->lm_addr)),
		    ((char *) (N_BSSADDR(*e) + e->a_bss + lm->lm_addr)));
#     endif
#     ifdef SUNOS5
	Elf32_Ehdr * e;
        Elf32_Phdr * p;
        unsigned long offset;
        char * start;
        register int i;
        
	e = (Elf32_Ehdr *) lm->l_addr;
        p = ((Elf32_Phdr *)(((char *)(e)) + e->e_phoff));
        offset = ((unsigned long)(lm->l_addr));
        for( i = 0; i < e->e_phnum; ((i++),(p++)) ) {
          switch( p->p_type ) {
            case PT_LOAD:
              {
                if( !(p->p_flags & PF_W) ) break;
                start = ((char *)(p->p_vaddr)) + offset;
                GC_add_roots_inner(
                  start,
                  start + p->p_memsz
                );
              }
              break;
            default:
              break;
          }
	}
#     endif
    }
}

#else
void GC_register_dynamic_libraries(){}

int GC_no_dynamic_loading;
#endif
