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
count_bits (word_32 x)
{
    int b = 0;

    while (x != 0)
    {
	b += x & 1;
	x >>= 1;
    }

    return b;
}

int
main (void)
{
    ppc_liveness_info_t info;
    long live_cr = 0, live_xer = 0, live_gpr = 0;
    long num_cr = 0, num_xer = 0, num_gpr = 0;

    while (fread(&info, sizeof(ppc_liveness_info_t), 1, stdin) == 1)
    {
	live_cr += count_bits(info.live_cr);
	live_xer += count_bits(info.live_xer & 0xe0000000);
	live_gpr += count_bits(info.live_gpr);
	num_cr += 32;
	num_xer += 3;
	num_gpr += 32;

	printf("%08x %d %d %d\n",
	       info.foreign_addr,
	       count_bits(info.live_cr),
	       count_bits(info.live_xer & 0xe0000000),
	       count_bits(info.live_gpr));
    }

    /* printf("%ld %ld %ld %ld %ld %ld\n", live_cr, num_cr, live_xer, num_xer, live_gpr, num_gpr); */

    return 0;
}
