#include <stdio.h>
#include <assert.h>

typedef unsigned int word_32;
typedef unsigned long long word_64;

#define unary_BitNeg(x)              (~(x))

#define unary_ConditionNeg(x)        (!(x))
#define binary_ConditionAnd(x,y)     ((x)&&(y))
#define binary_ConditionOr(x,y)      ((x)||(y))

#define unary_IntZero_8(x)           ((x)==0LL)
#define binary_IntEqual_8(x,y)       ((x)==(y))
#define binary_IntLessU_8(x,y)       ((x)<(y))

#define binary_IntAdd(x,y)           ((x)+(y))
#define binary_IntSub(x,y)           ((x)-(y))
#define binary_IntMul(x,y)           ((x)*(y))

#define binary_ShiftL(x,y)           ((x)<<(y))
#define binary_LShiftR_4(x,y)        ((word_64)((word_32)(x)>>(word_32)(y)))
#define binary_LShiftR_8(x,y)        ((word_64)(x)>>(y))
#define binary_AShiftR_8(x,y)        (ashiftr8((x),(y)))

#define binary_BitAnd(x,y)           ((x)&(y))
#define binary_BitOr(x,y)            ((x)|(y))
#define binary_BitMask(x,y)          (bitmask((x),(y)))

word_64
ashiftr8 (word_64 x, word_64 a)
{
    if (x >> 63)
	return x >> a;
    else
	return (x >> a) | ((word_64)-1LL << (64 - a));
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
test_func (word_64 field_sh, word_64 field_mb, word_64 field_me, word_64 guest_reg_2)
{
    word_64 guest_reg_1;
    word_64 interm_reg_1, interm_reg_2, interm_reg_3, interm_reg_4;

#include "test.new.h"

    return guest_reg_1;
}

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
		word_64 gen_result = test_func(sh, mb, me, 0xdeadbeefcafebabeLL);
		word_64 real_result = calc(sh, mb, me, 0xdeadbeefcafebabeLL);

		assert((gen_result & 0xffffffff) == real_result);
	    }

    return 0;
}
