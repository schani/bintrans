#include <stdio.h>
#include <assert.h>

#include "bintrans.h"

typedef struct
{
    word_32 foreign_addr;
    word_32 live_cr;
    word_32 live_xer;
    word_32 live_gpr;
} ppc_liveness_info_t;

int
main (void)
{
    for (;;)
    {
	ppc_liveness_info_t info;
	int result;

	result = scanf("%08x  cr: %08x xer: %08x gpr: %08x",
		       &info.foreign_addr, &info.live_cr, &info.live_xer, &info.live_gpr);
	if (result != 4)
	    break;

	assert(fwrite(&info, sizeof(ppc_liveness_info_t), 1, stdout) == 1);
    }

    return 0;
}
