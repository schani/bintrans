#include <stdio.h>
#include <assert.h>
#include "mlgen_macros.h"

typedef unsigned int word_32;
typedef unsigned long long word_64;

word_64
ashiftr8 (word_64 x, word_64 a)
{
    if (x >> 63)
	return (x >> a) | ((word_64)-1LL << (64 - a));
    else
	return x >> a;
}

word_64
bitmask (word_64 s, word_64 l)
{
    if (l == 0LL)
	return 0LL;

    if (s + l >= 64)
	return (word_64)-1LL << s;
    else
	return (((word_64)-1LL) >> (64LL - l)) << s;
}

word_64
width_mask (word_64 w)
{
    switch (w)
    {
	case 1 :
	    return 0xffLL;
	case 2 :
	    return 0xffffLL;
	case 4 :
	    return 0xffffffffLL;
	case 8 :
	    return 0xffffffffffffffffLL;
	default :
	    assert(0);
    }
}

word_64
userop_IsMaskMask (word_64 x, word_64 w)
{
    word_64 m = (1LL << w) - 1LL;

    while (x != 0LL)
    {
	if ((x & m) != 0LL && (x & m) != m)
	    return 0;
	x >>= w;
    }

    return 1LL;
}

word_64
unary_LowMask (word_64 x)
{
    word_64 r = 0LL;

    while (x != 0LL)
    {
	r = (r << 1) | 1LL;
	x >>= 1;
    }

    return r;
}

word_64
unary_HighMask (word_64 x)
{
    word_64 r = 0LL;

    while (x != 0LL)
    {
	r = (r >> 1) | (1LL << 63);
	x <<= 1;
    }

    return r;
}

word_64
sex_32 (word_64 x)
{
    if (x & 0x80000000LL)
	return x | 0xffffffff00000000LL;
    else
	return x & 0xffffffffLL;
}

#include "test.h"

word_32
mask_32 (word_32 begin, word_32 end)
{
    word_32 x = 0;
    word_32 b;
    int i;

    if (end < begin)
    {
	b = 1;
	for (i = 0; i <= end; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}

	b = 1 << begin;
	for (i = begin; i < 32; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}
    }
    else
    {
	b = 1 << begin;
	for (i = 0; i <= end - begin; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}
    }

    return x;
}

word_32
rotl_32 (word_32 x, word_32 i)
{
    assert(i <= 32);

    return (x << i) | (x >> (32 - i));
}

word_64
calc (word_64 sh, word_64 mb, word_64 me, word_64 val)
{
    return rotl_32((word_32)val, (word_32)sh) & mask_32(31 - me, 31 - mb);
}

int
main (void)
{
    word_64 sh, mb, me;

    for (sh = 0; sh < 32; ++sh)
	for (mb = 0; mb < 32; ++mb)
	    for (me = 0; me < 32; ++me)
	    {
		word_64 gen_result = test_rlwinm(sh, mb, me, 0xdeadbeefcafebabeLL);
		word_64 real_result = calc(sh, mb, me, 0xdeadbeefcafebabeLL);

		assert(gen_result == sex_32(real_result));
	    }

    /*
    for (sh = 9; sh < 10; ++sh)
	for (mb = 0; mb < 16; ++mb)
	    for (me = 0; me < 16; ++me)
	    {
		word_64 gen_result = test_rotand(sh, mb, me, 0xdeadbeefcafebabeLL);
	    }
    */

    return 0;
}
