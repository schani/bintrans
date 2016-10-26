/*
 * i386.c
 *
 * bintrans
 *
 * Copyright (C) 2001 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef EMU_I386

#include <stdio.h>
#include <math.h>

#include "bintrans.h"
#include "i386.h"

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
i386_decode_opcode (interpreter_t *intp, int *prefix_flags, word_8 *opcode1)
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
	    bt_assert(!(*prefix_flags & prefixes[i].flag));
	    *prefix_flags |= prefixes[i].flag;
	}
	else
	{
	    /*
	    if (b == 0x0f || b == 0xf2 || b == 0xf3)
	    {
		*opcode1 = b;
		*opcode2 = mem_get_8(intp, intp->pc++);
	    }
	    else
	    */
		*opcode1 = b;
	    break;
	}
    }
}

void
i386_decode_modrm (interpreter_t *intp, word_8 *_modrm, word_8 *_mod, word_8 *_reg, word_8 *_rm)
{
    word_8 modrm = mem_get_8(intp, intp->pc++);

    *_modrm = modrm;

    *_mod = modrm >> 6;
    *_reg = (modrm >> 3) & 7;
    *_rm = modrm & 7;
}

void
i386_decode_sib (interpreter_t *intp, word_8 modrm, word_8 *_scale, word_8 *_index, word_8 *_base, word_8 *_disp8, word_32 *_disp32)
{
    word_8 mod, reg, rm, sib;
    int need_sib = 0, need_disp8 = 0, need_disp32 = 0;

    mod = modrm >> 6;
    reg = (modrm >> 3) & 7;
    rm = modrm & 7;

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

	if (base == 5 && mod == 0)
	{
	    bt_assert(!need_disp32);
	    need_disp32 = 1;
	}

	if (base == 5)
	    bt_assert(need_disp32 || need_disp8);
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
i386_disassemble_r8 (FILE *out, word_8 reg)
{
    static char *names[] = { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh" };

    bt_assert(reg < 8);
    fputs(names[reg], out);
}

void
i386_disassemble_r16 (FILE *out, word_8 reg)
{
    static char *names[] = { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di" };

    bt_assert(reg < 8);
    fputs(names[reg], out);
}

void
i386_disassemble_r32 (FILE *out, word_8 reg)
{
    static char *names[] = { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi" };

    bt_assert(reg < 8);
    fputs(names[reg], out);
}

void
i386_disassemble_sib (FILE *out, word_8 mod, word_8 scale, word_8 index, word_8 base, word_32 disp32)
{
    if (base == 5 && mod == 0)
	fprintf(out, "0x%x", disp32);

    fputs("(", out);

    if (!(base == 5 && mod == 0))
	i386_disassemble_r32(out, base);

    if (index != 4)
    {
	fputs(",", out);
	i386_disassemble_r32(out, index);
	fprintf(out, ",%d", 1 << scale);
    }

    fputs(")", out);
}

void
i386_disassemble_ea (FILE *out, word_8 mod, word_8 rm, word_8 scale, word_8 index, word_8 base, word_8 disp8, word_32 disp32)
{
    int have_disp;
    word_32 disp;

    if ((mod == 0 && rm == 5) || mod == 2)
    {
	have_disp = 1;
	disp = disp32;
    }
    else if (mod == 1)
    {
	have_disp = 1;
	disp = disp8 | (disp8 & 0x80 ? 0xffffff00 : 0);
    }
    else
	have_disp = 0;

    if (have_disp)
	fprintf(out, "0x%x", disp);

    if (!(mod == 0 && rm == 5))
    {
	if (rm != 4)
	{
	    fputs("(", out);
	    i386_disassemble_r32(out, rm);
	    fputs(")", out);
	}
	else
	    i386_disassemble_sib(out, mod, scale, index, base, disp32);
    }
}

void
i386_disassemble_rm8 (FILE *out, word_8 mod, word_8 rm, word_8 scale, word_8 index, word_8 base, word_8 disp8, word_32 disp32)
{
    bt_assert(mod < 4);

    if (mod == 0 || mod == 1 || mod == 2)
	i386_disassemble_ea(out, mod, rm, scale, index, base, disp8, disp32);
    else
	i386_disassemble_r8(out, rm);
}

void
i386_disassemble_rm16 (FILE *out, word_8 mod, word_8 rm, word_8 scale, word_8 index, word_8 base, word_8 disp8, word_32 disp32)
{
    bt_assert(mod < 4);

    if (mod == 0 || mod == 1 || mod == 2)
	i386_disassemble_ea(out, mod, rm, scale, index, base, disp8, disp32);
    else
	i386_disassemble_r16(out, rm);
}

void
i386_disassemble_rm32 (FILE *out, word_8 mod, word_8 rm, word_8 scale, word_8 index, word_8 base, word_8 disp8, word_32 disp32)
{
    bt_assert(mod < 4);

    if (mod == 0 || mod == 1 || mod == 2)
	i386_disassemble_ea(out, mod, rm, scale, index, base, disp8, disp32);
    else
	i386_disassemble_r32(out, rm);
}

void
setup_i386_registers (interpreter_t *intp, word_32 stack_bottom)
{
    intp->regs_GPR[4] = stack_bottom;
}

#include "i386_interpreter.c"
#include "i386_disassembler.c"
#include "i386_livenesser.c"
#include "i386_jump_analyzer.c"
#endif
