#include <stdio.h>
#include <ctype.h>
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
read_bits (FILE *in, word_32 *bits)
{
    int i;

    for (;;)
    {
	int c;

	c = fgetc(in);
	if (c == EOF)
	    return 0;
	if (!isspace(c))
	{
	    ungetc(c, in);
	    break;
	}
    }

    *bits = 0;
    for (i = 31; i >= 0; --i)
    {
	int c = fgetc(in);
	int bit = c - '0';

	assert(bit == 0 || bit == 1);

	*bits |= bit << i;
    }

    return 1;
}

int
main (void)
{
    for (;;)
    {
	ppc_liveness_info_t info;
	int result;

	result = scanf("%08x", &info.foreign_addr);
	if (result != 1)
	    break;

	assert(read_bits(stdin, &info.live_cr));
	assert(read_bits(stdin, &info.live_xer));
	assert(read_bits(stdin, &info.live_gpr));

	assert(fwrite(&info, sizeof(ppc_liveness_info_t), 1, stdout) == 1);
    }

    return 0;
}
