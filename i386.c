#ifdef EMU_I386

#include <stdio.h>
#include <assert.h>

#include "bintrans.h"

#define I386_PREFIX_LOCK               0x001
#define I386_PREFIX_REPNE              0x002
#define I386_PREFIX_REP                0x004
#define I386_PREFIX_SSE                0x008
#define I386_PREFIX_CS_OVERRIDE        0x010
#define I386_PREFIX_SS_OVERRIDE        0x020
#define I386_PREFIX_DS_OVERRIDE        0x040
#define I386_PREFIX_ES_OVERRIDE        0x080
#define I386_PREFIX_FS_OVERRIDE        0x100
#define I386_PREFIX_GS_OVERRIDE        0x200
#define I386_PREFIX_OP_SIZE_OVERRIDE   0x400
#define I386_PREFIX_ADDR_SIZE_OVERRIDE 0x800

#define I386_NUM_PREFIXES 9

static struct { word_8 prefix; int flag; } prefixes[I386_NUM_PREFIXES] = {
    { 0xf0, I386_PREFIX_LOCK },
    { 0x2e, I386_PREFIX_CS_OVERRIDE },
    { 0x36, I386_PREFIX_SS_OVERRIDE },
    { 0x3e, I386_PREFIX_DS_OVERRIDE },
    { 0x26, I386_PREFIX_ES_OVERRIDE },
    { 0x64, I386_PREFIX_FS_OVERRIDE },
    { 0x65, I386_PREFIX_GS_OVERRIDE },
    { 0x66, I386_PREFIX_OP_SIZE_OVERRIDE },
    { 0x67, I386_PREFIX_ADDR_SIZE_OVERRIDE }
};

void
i386_decode_opcode (interpreter_t *intp, int *prefix_flags, word_8 *opcode1, word_8 *opcode2)
{
    word_8 b;
    int i;

    *prefix_flags = 0;

    for (;;)
    {
	b = mem_get_8(intp, intp->pc++);
	for (i = 0; i < I386_NUM_PREFIXES; ++i)
	    if (prefixes[i].prefix == b)
		break;

	if (i < I386_NUM_PREFIXES)
	{
	    assert(!(*prefix_flags & prefixes[i].flag));
	    *prefix_flags |= prefixes[i].flag;
	}
	else
	{
	    if (b == 0x0f || b == 0xf2 || b == 0xf3)
	    {
		*opcode1 = b;
		*opcode2 = mem_get_8(intp, intp->pc++);
	    }
	    else
		*opcode1 = b;
	    break;
	}
    }
}

void
i386_decode_modrm (interpreter_t *intp, word_8 *_mod, word_8 *_reg, word_8 *_rm,
		   word_8 *_scale, word_8 *_index, word_8 *_base, word_8 *_disp8, word_32 *_disp32)
{
    word_8 modrm = mem_get_8(intp, intp->pc++), sib;
    word_8 mod, reg, rm;
    int need_sib = 0, need_disp8 = 0, need_disp32 = 0;

    *_mod = mod = modrm >> 6;
    *_reg = reg = (modrm >> 3) & 7;
    *_rm = rm = modrm & 7;

    if (mod == 1)
	need_disp8 = 1;
    else if (mod == 2 || (mod == 0 && rm == 5))
	need_disp32 = 1;

    if (mod != 3 && rm == 4)
	need_sib = 1;

    if (need_sib)
    {
	word_8 base;

	sib = mem_get_8(intp, intp->pc++);
	*_scale = sib >> 6;
	*_index = sib >> 3 & 7;
	base = *_base = sib & 7;

	if (base == 5)
	{
	    assert(!need_disp32);
	    need_disp32 = 1;
	}
    }

    if (need_disp8)
	*_disp8 = mem_get_8(intp, intp->pc++);
    else if (need_disp32)
    {
	*_disp32 = mem_get_32_unaligned(intp, intp->pc);
	intp->pc += 4;
    }
}

word_8
i386_decode_imm8 (interpreter_t *intp)
{
    return mem_get_8(intp, intp->pc++);
}

word_16
i386_decode_imm16 (interpreter_t *intp)
{
    word_16 imm16 = mem_get_16_unaligned(intp, intp->pc);

    intp->pc += 2;

    return imm16;
}

word_32
i386_decode_imm32 (interpreter_t *intp)
{
    word_32 imm32 = mem_get_32_unaligned(intp, intp->pc);

    intp->pc += 4;

    return imm32;
}

void
setup_i386_registers (interpreter_t *intp, word_32 stack_bottom)
{
    intp->regs_GPR[4] = stack_bottom;
}

#include "i386_interpreter.c"
#endif
