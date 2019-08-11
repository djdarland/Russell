# define OPEN (*
# define CLOSE *)
# define DEBUG(S) write [File$stdout, S]

# include "op_codes.h"

# define mem_var 0	/* a pseudo variable that represents memory */

/* objects that can appear on loclists but are not numbered locs, as such */
# define MEM_OBJ -4
# define NONE_OBJ -3
# define STACK_OBJ -2
# define STATIC_OBJ -1	/* some fns in loclist.r and flocs.r depend */
			/* ... on these values' order */

/* These are the numbers for the standard nodes */
# define GENERICNODE	(0)
# define MEMNODE	(1)
# define NONENODE	(2)
# define STATICNODE	(3)
# define STACKNODE	(4)
# define AVAIL_NODE	(5)	/* first available */

/* mnemonics for categories of op codes */
# define NULLCAT 0
# define ALLOC   1		/* ALH, ALA */
				/* safe allocating CLL, CLC, CLI */
# define ALCNIL 2		/* (nil) DCL, LDN, LDS, LDL, UDC, ADP, TRU, 
									FLS */
# define ASSIGN 3		/* MOV */
# define TOMEM  4		/* PSH, STI, ARG */
# define CATTAR 5		/* TAR */
# define ANY	6		/* LDI */
# define UNSAFE 7		/* unsafe calls to CLL, CLI */
# define ALCSTAT 8		/* LDL and MOV SP - */
# define GARCAT 9		/* GAR - category needed on prepass */
# define ARGCAT 10		/* for ARGs that I assume are preserved */
# define MAYBEALC 11		/* for calls that I assume are allocators */
# define ALCSTACK 12		/* for MOV SP - , I think */
# define ERRCAT	  13		/* for ERRs, after which I can assume the 
									best */
# define WATCHARG 15	/* or ARGs that would otherwise be NULLCAT, but I have
				to see them in final phase, so they have to
				go into qlists. */

# define DEA_update 1		/* these are update note types */
# define STI_update 2
# define KILL_update 3
# define postponed_UDC 4
# define postponed_DEAD 5
# define RL_before 6
# define RL_after 7

# define MAXWATCHARGS 10	/* how many args we watch through a call */

# define TOP (-1)   /* for the top element of the inter. analysis - the
				other values are #s of graph nodes */
