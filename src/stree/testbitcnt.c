# include "parm.h"

# include "stmkfields.m"
# include "streedefs.h"

# include "stformats.h"


main()
{
	int i;
	for( i = 0; i <= 50; i++ ) {
		printf( "kind = %s - # of args = %d \n", kindname(i),
												 bitcnt(stmkfields[i]) );
	}
}
