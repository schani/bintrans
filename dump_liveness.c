#include <stdio.h>

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
    ppc_liveness_info_t info;

    while (fread(&info, sizeof(ppc_liveness_info_t), 1, stdin) == 1)
	printf("%08x  cr: %08x xer: %08x gpr: %08x\n", info.foreign_addr, info.live_cr, info.live_xer, info.live_gpr);

    return 0;
}
