/*
 * i386_to_ppc_compiler.c
 *
 * bintrans
 *
 * Copyright (C) 2002 Mark Probst
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

#define ZERO_REG             0

#define MODE_NIL 0
#define MODE_M16_NOPREFIX 1
#define MODE_M32 2
#define MODE_M64 3
#define MODE_RM8 4
#define MODE_RM16 5
#define MODE_RM32 6
#define MODE_PR16 7
#define MODE_PR32 8
#define MODE_IMM8 9
#define MODE_SIMM8 10
#define MODE_IMM16 11
#define MODE_IMM32 12
#define MODE_AL_IMM8 13
#define MODE_AX_IMM16 14
#define MODE_EAX_IMM32 15
#define MODE_RM8_1 16
#define MODE_RM16_1 17
#define MODE_RM32_1 18
#define MODE_RM8_CL 19
#define MODE_RM16_CL 20
#define MODE_RM32_CL 21
#define MODE_RM8_IMM8 22
#define MODE_RM16_IMM8 23
#define MODE_RM16_IMM16 24
#define MODE_RM32_IMM8 25
#define MODE_RM32_IMM32 26
#define MODE_RM16_SIMM8 27
#define MODE_RM32_SIMM8 28
#define MODE_RM8_R8 29
#define MODE_RM16_R16 30
#define MODE_RM32_R32 31
#define MODE_R8_RM8 32
#define MODE_R16_RM16 33
#define MODE_R32_RM32 34
#define MODE_R16_RM8 35
#define MODE_R32_RM8 36
#define MODE_AX_MOFFS32 37
#define MODE_EAX_MOFFS32 38
#define MODE_MOFFS32_AX 39
#define MODE_MOFFS32_EAX 40
#define MODE_PR8_IMM8 41
#define MODE_PR16_IMM16 42
#define MODE_PR32_IMM32 43
#define MODE_PST 44

#define REG_EAX                   0
#define REG_ECX                   1
#define REG_EDX                   2
#define REG_EBX                   3
#define REG_ESP                   4
#define REG_EBP                   5
#define REG_ESI                   6
#define REG_EDI                   7

#define ref_i386_gpr_r(x)         ref_integer_reg_for_reading(x)
#define ref_i386_gpr_w(x)         ref_integer_reg_for_writing(x)
#define ref_i386_gpr_rw(x)        ref_integer_reg_for_reading_and_writing(x)

#define KILL_OF                   1
#define KILL_CF                   1
#define KILL_SF                   1
#define KILL_ZF                   1

void
move_i386_regs_interpreter_to_compiler (interpreter_t *intp)
{
    *(double*)&constant_area[21 * 2] = (intp->regs_FPST[7]);
    *(double*)&constant_area[20 * 2] = (intp->regs_FPST[6]);
    *(double*)&constant_area[19 * 2] = (intp->regs_FPST[5]);
    *(double*)&constant_area[18 * 2] = (intp->regs_FPST[4]);
    *(double*)&constant_area[17 * 2] = (intp->regs_FPST[3]);
    *(double*)&constant_area[16 * 2] = (intp->regs_FPST[2]);
    *(double*)&constant_area[15 * 2] = (intp->regs_FPST[1]);
    *(double*)&constant_area[14 * 2] = (intp->regs_FPST[0]);
    *(word_16*)&constant_area[9 * 2] = (intp->regs_FSPR[0]);
    *(word_32*)&constant_area[8 * 2] = (intp->regs_SPR[0]);
    *(word_32*)&constant_area[7 * 2] = (intp->regs_GPR[7]);
    *(word_32*)&constant_area[6 * 2] = (intp->regs_GPR[6]);
    *(word_32*)&constant_area[5 * 2] = (intp->regs_GPR[5]);
    *(word_32*)&constant_area[4 * 2] = (intp->regs_GPR[4]);
    *(word_32*)&constant_area[3 * 2] = (intp->regs_GPR[3]);
    *(word_32*)&constant_area[2 * 2] = (intp->regs_GPR[2]);
    *(word_32*)&constant_area[1 * 2] = (intp->regs_GPR[1]);
    *(word_32*)&constant_area[0 * 2] = (intp->regs_GPR[0]);
    *(word_32*)&constant_area[13 * 2] = ((intp->regs_SPR[0] >> 11) & 0x1);
    *(word_32*)&constant_area[12 * 2] = ((intp->regs_SPR[0] >> 7) & 0x1);
    *(word_32*)&constant_area[11 * 2] = ((intp->regs_SPR[0] >> 6) & 0x1);
    *(word_32*)&constant_area[10 * 2] = ((intp->regs_SPR[0] >> 0) & 0x1);
}

void
move_i386_regs_compiler_to_interpreter (interpreter_t *intp)
{
    (intp->regs_FPST[7]) = *(double*)&constant_area[21 * 2];
    (intp->regs_FPST[6]) = *(double*)&constant_area[20 * 2];
    (intp->regs_FPST[5]) = *(double*)&constant_area[19 * 2];
    (intp->regs_FPST[4]) = *(double*)&constant_area[18 * 2];
    (intp->regs_FPST[3]) = *(double*)&constant_area[17 * 2];
    (intp->regs_FPST[2]) = *(double*)&constant_area[16 * 2];
    (intp->regs_FPST[1]) = *(double*)&constant_area[15 * 2];
    (intp->regs_FPST[0]) = *(double*)&constant_area[14 * 2];
    (intp->regs_FSPR[0]) = *(word_16*)&constant_area[9 * 2];
    (intp->regs_SPR[0]) = *(word_32*)&constant_area[8 * 2];
    (intp->regs_GPR[7]) = *(word_32*)&constant_area[7 * 2];
    (intp->regs_GPR[6]) = *(word_32*)&constant_area[6 * 2];
    (intp->regs_GPR[5]) = *(word_32*)&constant_area[5 * 2];
    (intp->regs_GPR[4]) = *(word_32*)&constant_area[4 * 2];
    (intp->regs_GPR[3]) = *(word_32*)&constant_area[3 * 2];
    (intp->regs_GPR[2]) = *(word_32*)&constant_area[2 * 2];
    (intp->regs_GPR[1]) = *(word_32*)&constant_area[1 * 2];
    (intp->regs_GPR[0]) = *(word_32*)&constant_area[0 * 2];
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFF7FF) | ((*(word_32*)&constant_area[13 * 2]) << 11));
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFFF7F) | ((*(word_32*)&constant_area[12 * 2]) << 7));
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFFFBF) | ((*(word_32*)&constant_area[11 * 2]) << 6));
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFFFFE) | ((*(word_32*)&constant_area[10 * 2]) << 0));
}

static word_32 pc;
static int prefix_flags;
static word_8 mod, reg, rm, sib_scale, sib_index, sib_base, disp8, opcode_reg, imm8;
static word_16 imm16;
static word_32 disp32, imm32;
static int op_width;
static int mode;

static reg_t
ref_i386_gpr (reg_t num, int for_reading, int for_writing)
{
    assert(for_reading || for_writing);

    if (for_reading)
    {
	if (for_writing)
	    return ref_i386_gpr_rw(num);
	else
	    return ref_i386_gpr_r(num);
    }
    else
	return ref_i386_gpr_w(num);
}

static void
dispose_integer_reg (reg_t reg)
{
    if (is_tmp_integer_reg(reg))
	free_tmp_integer_reg(reg);
    else
	unref_integer_reg(reg);
}

static int
src_is_imm (word_32 *val)
{
    switch (mode)
    {
	case MODE_RM8_1 :
	case MODE_RM16_1 :
	case MODE_RM32_1 :
	    *val = 1;
	    return 1;

	case MODE_AL_IMM8 :
	case MODE_RM8_IMM8 :
	case MODE_RM32_IMM8 :
	case MODE_PR8_IMM8 :
	    *val = imm8;
	    return 1;

	case MODE_RM16_SIMM8 :
	case MODE_RM32_SIMM8 :
	    *val = SEX32(imm8, 8);
	    return 1;

	case MODE_AX_IMM16 :
	case MODE_RM16_IMM16 :
	case MODE_PR16_IMM16 :
	    *val = imm16;
	    return 1;

	case MODE_EAX_IMM32 :
	case MODE_RM32_IMM32 :
	case MODE_PR32_IMM32 :
	    *val = imm32;
	    return 1;

	default :
	    return 0;
    }
}

static void
gen_sib_address (reg_t *ra, reg_t *rb, word_16 *imm)
{
    if (sib_base == 5)
    {
	if (mod == 0)
	{
	    if (disp32 == 0)
		*ra = 0;
	    else
	    {
		if ((disp32 >> 15) == 0 || (disp32 >> 15) == 0x1ffff)
		{
		    *ra = NO_REG;
		    *imm = disp32;
		}
		else
		{
		    *ra = alloc_tmp_integer_reg();
		    emit_load_integer_32(*ra, disp32);
		    *imm = 0;
		}
	    }
	}
	else
	{
	    *ra = ref_i386_gpr_r(REG_EBP);
	    *imm = 0;
	}
    }
    else
    {
	*ra = ref_i386_gpr_r(sib_base);
	*imm = 0;
    }

    if (sib_index == 4)
    {
	if (*imm == 0)
	{
	    *rb = *ra;
	    *ra = 0;
	}
	else
	    *rb = NO_REG;
    }
    else
    {
	if (sib_scale == 0)
	    *rb = ref_i386_gpr_r(sib_index);
	else
	{
	    reg_t index_reg;

	    *rb = alloc_tmp_integer_reg();
	    index_reg = ref_i386_gpr_r(sib_index);
	    emit(COMPOSE_SLWI(*rb, index_reg, sib_scale));
	    free_tmp_integer_reg(index_reg);
	}

	if (*ra == NO_REG)
	{
	    *ra = *rb;
	    *rb = NO_REG;
	}
    }
}

static void
gen_ea (reg_t *ra, reg_t *rb, word_16 *imm)
{
    switch (mod)
    {
	case 0 :
	    switch (rm)
	    {
		case 4 :
		    gen_sib_address(ra, rb, imm);
		    break;

		case 5 :
		    *ra = 0;
		    *rb = alloc_tmp_integer_reg();
		    *imm = 0;

		    emit_load_integer_32(*ra, disp32);
		    break;

		default :
		    *ra = 0;
		    *rb = ref_i386_gpr_r(rm);
		    *imm = 0;
		    break;
	    }
	    break;
	    
	case 1 :
	    if (rm == 4)
	    {
		gen_sib_address(ra, rb, imm);
		if (disp8 == 0)
		    return;

		if (*rb == NO_REG)
		{
		    word_32 new_imm = SEX32(*imm, 16) + SEX32(disp8, 8);

		    if ((*imm >> 15) == (disp8 >> 7)
			&& (new_imm >> 15) == 1 - (disp8 >> 7))
		    {
			*rb = alloc_tmp_integer_reg();

			emit_load_integer_32(*rb, new_imm);
		    }
		    else
			*imm = new_imm;
		}
		else if (*ra == 0)
		{
		    *ra = *rb;
		    *rb = NO_REG;
		    *imm = SEX16(disp8, 8);
		}
		else
		{
		    reg_t tmp = alloc_tmp_integer_reg();

		    emit(COMPOSE_ADDI(tmp, *ra, SEX16(disp8, 8)));

		    dispose_integer_reg(*ra);

		    *ra = tmp;
		}
	    }
	    else
	    {
		*ra = ref_i386_gpr_r(rm);
		*rb = NO_REG;
		*imm = SEX16(disp8, 8);
	    }
	    break;

	case 2 :
	    if (rm == 4)
	    {
		gen_sib_address(ra, rb, imm);
		if (disp32 == 0)
		    return;

		if (*imm != 0 || *ra == 0)
		{
		    word_32 new_imm = SEX32(*imm, 16) + disp32;

		    if (new_imm == 0)
		    {
			*imm = 0;
			if (*ra != 0)
			{
			    assert(*rb == NO_REG);
			    *rb = *ra;
			    *ra = 0;
			}

			return;
		    }

		    if (*ra == 0)
		    {
			*ra = *rb;
			*rb = NO_REG;
		    }

		    if ((new_imm >> 15) == 0 || (new_imm >> 15) == 0x1ffff)
			*imm = new_imm;
		    else
		    {
			*rb = alloc_tmp_integer_reg();
			emit_load_integer_32(*rb, new_imm);

			*imm = 0;
		    }
		}
		else
		{
		    reg_t tmp = alloc_tmp_integer_reg();

		    assert(*rb != NO_REG && *imm == 0);

		    if ((disp32 >> 15) == 0 || (disp32 >> 15) == 0x1ffff)
			emit(COMPOSE_ADDI(tmp, *rb, disp32 & 0xffff));
		    else if ((disp32 & 0xffff) == 0)
			emit(COMPOSE_ADDIS(tmp, *rb, disp32 >> 16));
		    else
		    {
			emit_load_integer_32(tmp, disp32);
			emit(COMPOSE_ADD(tmp, tmp, *rb));
		    }

		    dispose_integer_reg(*rb);

		    *rb = tmp;
		}
	    }
	    else
	    {
		if (disp32 == 0)
		{
		    *ra = 0;
		    *rb = ref_i386_gpr_r(rm);
		    *imm = 0;
		}
		else
		{
		    *ra = ref_i386_gpr_r(rm);
		    if ((disp32 >> 15) == 0 || (disp32 >> 15) == 0x1ffff)
		    {
			*rb = NO_REG;
			*imm = disp32;
		    }
		    else
		    {
			*rb = alloc_tmp_integer_reg();
			emit_load_integer_32(*rb, disp32);
			*imm = 0;
		    }
		}
	    }
	    break;

	default :
	    assert(0);
    }
}

static void
gen_ea_for_indexing (reg_t *ra, reg_t *rb)
{
    word_16 imm;

    gen_ea(ra, rb, &imm);

    if (imm != 0)
    {
	assert(*rb == NO_REG);
	*rb = alloc_tmp_integer_reg();
	emit_load_integer_32(*rb, imm);
    }
    else
	assert(*rb != NO_REG);
}

static reg_t
ref_i386_gpr_extended (reg_t num, int for_reading, int for_writing, int zexed, int width)
{
    if (zexed)
    {
	reg_t ereg = ref_i386_gpr_r(num);
	reg_t tmp;

	assert(width >= 16);

	tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_EXTRWI(tmp, ereg, width, 32 - width));

	unref_integer_reg(ereg);

	return tmp;
    }
    else
	return ref_i386_gpr(num, for_reading, for_writing);
}

static reg_t
extract_hreg (reg_t num, int for_reading, int for_writing)
{
    if (!for_reading)
	return alloc_tmp_integer_reg();
    else
    {
	reg_t ereg = ref_i386_gpr(opcode_reg - 4, 1, for_writing);

	if (for_writing)
	{
	    emit(COMPOSE_EXTRWI(ereg, ereg, 8, 16));

	    return ereg;
	}
	else
	{
	    reg_t tmp = alloc_tmp_integer_reg();

	    emit(COMPOSE_EXTRWI(tmp, ereg, 8, 16));

	    return tmp;
	}
    }
}

static reg_t
ref_src_op (int zexed, word_32 *imm, int imm_width, int is_simm)
{
    word_32 val;

    if (src_is_imm(&val))
    {
	if (imm != 0
	    && ((!is_simm && (val >> imm_width) == 0)
		|| (is_simm && ((val >> (imm_width - 1)) == 0
				|| (val >> (imm_width - 1)) == ((word_32)-1 >> (imm_width - 1))))))
	{
	    *imm = val & ((1 << imm_width) - 1);
	    return NO_REG;
	}
	else
	{
	    reg_t tmp = alloc_tmp_integer_reg();

	    emit_load_integer_32(tmp, val);

	    return tmp;
	}
    }

    switch (mode)
    {
	case MODE_RM8_CL :
	case MODE_RM32_CL :
	    return ref_i386_gpr_extended(REG_ECX, 1, 0, zexed, 8);

	case MODE_RM8_R8 :
	    if (reg < 4)
		return ref_i386_gpr_extended(reg, 1, 0, zexed, 8);
	    else
		return extract_hreg(reg - 4, 1, 0);

	case MODE_RM16_R16 :
	    return ref_i386_gpr_extended(reg, 1, 0, zexed, 16);

	case MODE_RM32_R32 :
	    return ref_i386_gpr_r(reg);

	case MODE_R8_RM8 :
	case MODE_R16_RM8 :
	case MODE_R32_RM8 :
	    if (mod == 3)
	    {
		if (rm < 4)
		    return ref_i386_gpr_extended(rm, 1, 0, zexed, 8);
		else
		    return extract_hreg(rm - 4, 1, 0);
	    }
	    else
	    {
		reg_t ra, rb, tmp;
		word_16 imm;

		gen_ea(&ra, &rb, &imm);

		tmp = alloc_tmp_integer_reg();

		if (rb == NO_REG)
		    emit(COMPOSE_LBZ(tmp, imm, ra));
		else
		{
		    emit(COMPOSE_LBZX(tmp, ra, rb));

		    dispose_integer_reg(rb);
		}

		dispose_integer_reg(ra);

		return tmp;
	    }

	case MODE_R16_RM16 :
	    if (mod == 3)
		return ref_i386_gpr_extended(rm, 1, 0, zexed, 16);
	    else
	    {
		reg_t ra, rb, tmp;

		gen_ea_for_indexing(&ra, &rb);

		tmp = alloc_tmp_integer_reg();

		emit(COMPOSE_LHBRX(tmp, ra, rb));

		dispose_integer_reg(ra);
		dispose_integer_reg(rb);

		return tmp;
	    }

	case MODE_R32_RM32 :
	    if (mod == 3)
		return ref_i386_gpr_r(rm);
	    else
	    {
		reg_t ra, rb, tmp;

		gen_ea_for_indexing(&ra, &rb);

		tmp = alloc_tmp_integer_reg();

		emit(COMPOSE_LWBRX(tmp, ra, rb));

		dispose_integer_reg(ra);
		dispose_integer_reg(rb);

		return tmp;
	    }

	case MODE_AX_MOFFS32 :
	case MODE_EAX_MOFFS32 :
	    {
		reg_t tmp = alloc_tmp_integer_reg();

		emit_load_integer_32(tmp, imm32);

		emit(COMPOSE_LWBRX(tmp, 0, tmp));

		return tmp;
	    }

	case MODE_MOFFS32_AX :
	    return ref_i386_gpr_extended(REG_EAX, 1, 0, zexed, 16);

	case MODE_MOFFS32_EAX :
	    return ref_i386_gpr_r(REG_EAX);

	case MODE_PST :
	    assert(0);

	default :
	    assert(0);
    }
	    
}

static int
dst_is_imm (word_32 *val)
{
    switch (mode)
    {
	case MODE_IMM8 :
	    *val = imm8;
	    return 1;

	case MODE_SIMM8 :
	    *val = SEX32(imm8, 8);
	    return 1;

	case MODE_IMM16 :
	    *val = imm16;
	    return 1;

	case MODE_IMM32 :
	    *val = imm32;
	    return 1;
    }

    return 0;
}

static reg_t
ref_dst_op (int for_reading, int for_writing, int zexed)
{
    word_32 val;

    assert(for_reading || for_writing);
    if (zexed)
	assert(for_reading);

    if (dst_is_imm(&val))
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit_load_integer_32(tmp, val);

	return tmp;
    }

    switch (mode)
    {
	case MODE_M16_NOPREFIX :
	case MODE_RM16 :
	case MODE_RM16_1 :
	case MODE_RM16_CL :
	case MODE_RM16_IMM8 :
	case MODE_RM16_IMM16 :
	case MODE_RM16_SIMM8 :
	case MODE_RM16_R16 :
	    if (mod == 3)
		return ref_i386_gpr_extended(rm, for_reading, for_writing, zexed, 16);
	    else
	    {
		if (!for_reading)
		    return alloc_tmp_integer_reg();
		else
		{
		    reg_t ra, rb, tmp;

		    gen_ea_for_indexing(&ra, &rb);

		    tmp = alloc_tmp_integer_reg();

		    emit(COMPOSE_LHBRX(tmp, ra, rb));

		    dispose_integer_reg(ra);
		    dispose_integer_reg(rb);

		    return tmp;
		}
	    }

	case MODE_M32 :
	case MODE_RM32 :
	case MODE_RM32_1 :
	case MODE_RM32_CL :
	case MODE_RM32_IMM8 :
	case MODE_RM32_IMM32 :
	case MODE_RM32_SIMM8 :
	case MODE_RM32_R32 :
	    if (mod == 3)
		return ref_i386_gpr_extended(rm, for_reading, for_writing, 0, 32);
	    else
	    {
		if (!for_reading)
		    return alloc_tmp_integer_reg();
		else
		{
		    reg_t ra, rb, tmp;

		    gen_ea_for_indexing(&ra, &rb);

		    tmp = alloc_tmp_integer_reg();

		    emit(COMPOSE_LWBRX(tmp, ra, rb));

		    dispose_integer_reg(ra);
		    dispose_integer_reg(rb);

		    return tmp;
		}
	    }

	case MODE_M64 :
	    assert(0);

	case MODE_RM8 :
	case MODE_RM8_1 :
	case MODE_RM8_CL :
	case MODE_RM8_IMM8 :
	case MODE_RM8_R8 :
	    if (mod == 3)
	    {
		if (rm < 4)
		    return ref_i386_gpr_extended(rm, for_reading, for_writing, zexed, 8);
		else
		    return extract_hreg(rm - 4, for_reading, for_writing);
	    }
	    else
	    {
		if (!for_reading)
		    return alloc_tmp_integer_reg();
		else
		{
		    reg_t ra, rb, tmp;
		    word_16 imm;

		    gen_ea(&ra, &rb, &imm);

		    tmp = alloc_tmp_integer_reg();

		    if (rb == NO_REG)
			emit(COMPOSE_LBZ(tmp, imm, ra));
		    else
		    {
			emit(COMPOSE_LBZX(tmp, ra, rb));

			dispose_integer_reg(rb);
		    }

		    dispose_integer_reg(ra);

		    return tmp;
		}
	    }

	case MODE_PR16 :
	case MODE_PR16_IMM16 :
	    return ref_i386_gpr_extended(opcode_reg, for_reading, for_writing, zexed, 16);

	case MODE_PR32 :
	case MODE_PR32_IMM32 :
	    return ref_i386_gpr(opcode_reg, for_reading, for_writing);

	case MODE_AL_IMM8 :
	    return ref_i386_gpr_extended(REG_EAX, for_reading, for_writing, zexed, 8);

	case MODE_AX_IMM16 :
	case MODE_AX_MOFFS32 :
	    return ref_i386_gpr_extended(REG_EAX, for_reading, for_writing, zexed, 16);

	case MODE_EAX_IMM32 :
	case MODE_EAX_MOFFS32 :
	    return ref_i386_gpr(REG_EAX, for_reading, for_writing);

	case MODE_R8_RM8 :
	    return ref_i386_gpr_extended(reg, for_reading, for_writing, zexed, 8); /* FIXME */

	case MODE_R16_RM16 :
	case MODE_R16_RM8 :
	    return ref_i386_gpr_extended(reg, for_reading, for_writing, zexed, 16);

	case MODE_R32_RM32 :
	case MODE_R32_RM8 :
	    return ref_i386_gpr(reg, for_reading, for_writing);

	case MODE_PR8_IMM8 :
	    if (opcode_reg < 4)
		return ref_i386_gpr_extended(opcode_reg, for_reading, for_writing, zexed, 8);
	    else
		return extract_hreg(opcode_reg - 4, for_reading, for_writing);

	case MODE_PST :
	    assert(0);

	default :
	    assert(0);
    }
}

static void
insert_into_gpr (reg_t num, reg_t src, int length, int begin)
{
    reg_t ereg = ref_i386_gpr_rw(num);

    emit(COMPOSE_INSRWI(ereg, num, length, begin));

    unref_integer_reg(ereg);
}

static void
commit_and_dispose_dst_op (reg_t dst_reg, int for_reading, int zexed, int own_reg)
{
    switch (mode)
    {
	case MODE_M16_NOPREFIX :
	case MODE_RM16 :
	case MODE_RM16_1 :
	case MODE_RM16_CL :
	case MODE_RM16_IMM8 :
	case MODE_RM16_IMM16 :
	case MODE_RM16_SIMM8 :
	case MODE_RM16_R16 :
	    if (mod == 3)
		insert_into_gpr(rm, dst_reg, 16, 16);
	    else
	    {
		reg_t ra, rb;

		gen_ea_for_indexing(&ra, &rb);

		emit(COMPOSE_STHBRX(dst_reg, ra, rb));

		dispose_integer_reg(ra);
		dispose_integer_reg(rb);
	    }
	    break;

	case MODE_M32 :
	case MODE_RM32 :
	case MODE_RM32_1 :
	case MODE_RM32_CL :
	case MODE_RM32_IMM8 :
	case MODE_RM32_IMM32 :
	case MODE_RM32_SIMM8 :
	case MODE_RM32_R32 :
	    if (mod != 3)
	    {
		reg_t ra, rb;

		gen_ea_for_indexing(&ra, &rb);

		emit(COMPOSE_STWBRX(dst_reg, ra, rb));

		dispose_integer_reg(ra);
		dispose_integer_reg(rb);
	    }
	    else if (mod == 3 && own_reg)
	    {
		reg_t ereg = ref_i386_gpr_w(rm);

		emit(COMPOSE_MR(ereg, dst_reg));

		unref_integer_reg(ereg);
	    }
	    break;

	case MODE_M64 :
	    assert(0);

	case MODE_RM8 :
	case MODE_RM8_1 :
	case MODE_RM8_CL :
	case MODE_RM8_IMM8 :
	case MODE_RM8_R8 :
	    if (mod == 3)
		insert_into_gpr(rm, dst_reg, 8, 24);
	    else
	    {
		reg_t ra, rb;
		word_16 imm;

		gen_ea(&ra, &rb, &imm);

		if (rb == NO_REG)
		    emit(COMPOSE_STB(dst_reg, imm, ra));
		else
		{
		    emit(COMPOSE_STBX(dst_reg, ra, rb));

		    dispose_integer_reg(rb);
		}

		dispose_integer_reg(ra);
	    }
	    break;

	case MODE_PR16 :
	case MODE_PR16_IMM16 :
	    insert_into_gpr(opcode_reg, dst_reg, 16, 16);
	    break;

	case MODE_PR32 :
	case MODE_PR32_IMM32 :
	    if (own_reg)
	    {
		reg_t ereg = ref_i386_gpr_w(opcode_reg);

		emit(COMPOSE_MR(ereg, dst_reg));

		unref_integer_reg(ereg);
	    }
	    break;

	case MODE_AL_IMM8 :
	    insert_into_gpr(REG_EAX, dst_reg, 24, 8);
	    break;

	case MODE_AX_IMM16 :
	case MODE_AX_MOFFS32 :
	    insert_into_gpr(REG_EAX, dst_reg, 16, 16);
	    break;

	case MODE_EAX_IMM32 :
	case MODE_EAX_MOFFS32 :
	    if (own_reg)
	    {
		reg_t ereg = ref_i386_gpr_w(REG_EAX);

		emit(COMPOSE_MR(ereg, dst_reg));

		unref_integer_reg(ereg);
	    }
	    break;

	case MODE_R8_RM8 :
	    if (reg < 4)
		insert_into_gpr(reg, dst_reg, 8, 24);
	    else
		insert_into_gpr(reg - 4, dst_reg, 8, 16);
	    break;

	case MODE_R16_RM16 :
	case MODE_R16_RM8 :
	    insert_into_gpr(reg, dst_reg, 16, 16);
	    break;

	case MODE_R32_RM32 :
	case MODE_R32_RM8 :
	    if (own_reg)
	    {
		reg_t ereg = ref_i386_gpr_w(reg);

		emit(COMPOSE_MR(ereg, dst_reg));

		unref_integer_reg(ereg);
	    }
	    break;

	case MODE_PR8_IMM8 :
	    if (opcode_reg < 4)
		insert_into_gpr(opcode_reg, dst_reg, 8, 24);
	    else
		insert_into_gpr(opcode_reg - 4, dst_reg, 8, 16);
	    break;

	case MODE_PST :
	    assert(0);

	default :
	    assert(0);
    }

    dispose_integer_reg(dst_reg);
}

static void
clear_of_cf (void)
{
    if (KILL_OF && KILL_CF)
	emit(COMPOSE_ADDCO(ZERO_REG, ZERO_REG, ZERO_REG));
    else if (KILL_OF)
	emit(COMPOSE_ADDO(ZERO_REG, ZERO_REG, ZERO_REG));
    else if (KILL_CF)
	emit(COMPOSE_ADDC(ZERO_REG, ZERO_REG, ZERO_REG));
}

static void
gen_bit_insn (void (*igen) (reg_t, reg_t, word_16), void (*dgen) (reg_t, reg_t, reg_t), void (*idgen) (reg_t, reg_t, word_16), void (*gen) (reg_t, reg_t, reg_t))
{
    word_32 imm;
    reg_t src = ref_src_op(0, &imm, 16, 0), dst = ref_dst_op(1, 1, 0);

    if (src == NO_REG)
    {
	if ((KILL_SF || KILL_ZF) && idgen != 0)
	    idgen(dst, dst, imm);
	else
	{
	    igen(dst, dst, imm);
	    if (KILL_SF || KILL_ZF)
		emit(COMPOSE_CMPWI(0, dst, 0));
	}
    }
    else
    {
	if (KILL_SF || KILL_ZF)
	    dgen(dst, dst, src);
	else
	    gen(dst, dst, src);
    }

    commit_and_dispose_dst_op(dst, 1, 0, 0);

    dispose_integer_reg(src);

    clear_of_cf();
}

static void
handle_add_insn (void)
{
    assert(0);
}

static void
handle_adc_insn (void)
{
    assert(0);
}

static void
gen_andi (reg_t ra, reg_t rs, word_16 imm)
{
    emit(COMPOSE_ANDID(ra, rs, imm));
}

static void
gen_andd (reg_t ra, reg_t rs, reg_t rb)
{
    emit(COMPOSE_ANDD(ra, rs, rb));
}

static void
gen_andid (reg_t ra, reg_t rs, word_16 imm)
{
    emit(COMPOSE_ANDID(ra, rs, imm));
}

static void
gen_and (reg_t ra, reg_t rs, reg_t rb)
{
    emit(COMPOSE_AND(ra, rs, rb));
}

static void
handle_and_insn (void)
{
    gen_bit_insn(&gen_andi, &gen_andd, &gen_andid, &gen_and);
}

static void
handle_call_insn (void)
{
    reg_t sp = ref_i386_gpr_rw(REG_ESP);
    reg_t tmp = alloc_tmp_integer_reg();

    emit(COMPOSE_ADDI(sp, sp, (word_16)-4));
    emit_load_integer_32(tmp, pc);
    emit(COMPOSE_STWBRX(tmp, 0, sp));

    free_tmp_integer_reg(tmp);
    unref_integer_reg(sp);

    switch (mode)
    {
	case MODE_IMM32 :
	    emit_direct_jump(pc + imm32);
	    break;

	case MODE_RM32 :
	    {
		tmp = ref_dst_op(1, 0, 0);

		emit(COMPOSE_MR(JUMP_TARGET_REG, tmp));

		dispose_integer_reg(tmp);

		emit_indirect_jump();
	    }
	    break;

	default :
	    assert(0);
    }
}

static void
handle_cdq_insn (void)
{
    assert(0);
}

static void
handle_cld_insn (void)
{
    assert(0);
}

static void
handle_cmp_insn (void)
{
    assert(0);
}

static void
handle_dec_insn (void)
{
    assert(0);
}

static void
handle_div_insn (void)
{
    assert(0);
}

static void
handle_fadd_insn (void)
{
    assert(0);
}

static void
handle_fcom_insn (void)
{
    assert(0);
}

static void
handle_fcomp_insn (void)
{
    assert(0);
}

static void
handle_fidivr_insn (void)
{
    assert(0);
}

static void
handle_fild_insn (void)
{
    assert(0);
}

static void
handle_fld_insn (void)
{
    assert(0);
}

static void
handle_fldcw_insn (void)
{
    assert(0);
}

static void
handle_fnstsw_insn (void)
{
    assert(0);
}

static void
handle_fstcw_insn (void)
{
    assert(0);
}

static void
handle_fstp_insn (void)
{
    assert(0);
}

static void
handle_idiv_insn (void)
{
    assert(0);
}

static void
handle_imul_insn (void)
{
    assert(0);
}

static void
handle_inc_insn (void)
{
    assert(0);
}

static void
handle_int_insn (void)
{
    assert(0);
}

static void
handle_ja_insn (void)
{
    assert(0);
}

static void
handle_jae_insn (void)
{
    assert(0);
}

static void
handle_jb_insn (void)
{
    assert(0);
}

static void
handle_jbe_insn (void)
{
    assert(0);
}

static void
handle_je_insn (void)
{
    assert(0);
}

static void
handle_jg_insn (void)
{
    assert(0);
}

static void
handle_jge_insn (void)
{
    assert(0);
}

static void
handle_jl_insn (void)
{
    assert(0);
}

static void
handle_jle_insn (void)
{
    assert(0);
}

static void
handle_jmp_insn (void)
{
    assert(0);
}

static void
handle_jne_insn (void)
{
    assert(0);
}

static void
handle_jns_insn (void)
{
    assert(0);
}

static void
handle_lea_insn (void)
{
    assert(0);
}

static void
handle_mov_insn (void)
{
    word_32 imm;
    reg_t src = ref_src_op(0, &imm, 16, 0);

    if (src == NO_REG)
    {
	reg_t dst = ref_dst_op(0, 1, 0);

	emit(COMPOSE_LI(dst, imm));

	commit_and_dispose_dst_op(dst, 0, 0, 0);
    }
    else
	commit_and_dispose_dst_op(src, 0, 0, 1);
}

static void
handle_movsx_insn (void)
{
    assert(0);
}

static void
handle_movzx_insn (void)
{
    assert(0);
}

static void
handle_mul_insn (void)
{
    assert(0);
}

static void
handle_neg_insn (void)
{
    assert(0);
}

static void
handle_nop_insn (void)
{
    assert(0);
}

static void
handle_not_insn (void)
{
    assert(0);
}

static void
gen_ori (reg_t ra, reg_t rs, word_16 imm)
{
    emit(COMPOSE_ORI(ra, rs, imm));
}

static void
gen_ord (reg_t ra, reg_t rs, reg_t rb)
{
    emit(COMPOSE_ORD(ra, rs, rb));
}

static void
gen_or (reg_t ra, reg_t rs, reg_t rb)
{
    emit(COMPOSE_OR(ra, rs, rb));
}

static void
handle_or_insn (void)
{
    gen_bit_insn(&gen_ori, &gen_ord, 0, &gen_or);
}

static void
handle_pop_insn (void)
{
    reg_t dst = ref_dst_op(1, 0, 0);
    reg_t sp = ref_i386_gpr_rw(REG_ESP);

    emit(COMPOSE_STWBRX(dst, 0, sp));
    emit(COMPOSE_ADDI(sp, sp, 4));

    dispose_integer_reg(sp);
    dispose_integer_reg(dst);
}

static void
handle_push_insn (void)
{
    reg_t sp = ref_i386_gpr_rw(REG_ESP);
    reg_t dst = ref_dst_op(0, 1, 0);

    emit(COMPOSE_ADDI(sp, sp, (word_16)-4));
    emit(COMPOSE_LWBRX(dst, 0, sp));

    dispose_integer_reg(dst);
    dispose_integer_reg(sp);
}

static void
handle_repne_scasb_insn (void)
{
    assert(0);
}

static void
handle_rep_movsb_insn (void)
{
    assert(0);
}

static void
handle_rep_movsd_insn (void)
{
    assert(0);
}

static void
handle_ret_insn (void)
{
    assert(0);
}

static void
handle_sar_insn (void)
{
    assert(0);
}

static void
handle_sete_insn (void)
{
    assert(0);
}

static void
handle_setg_insn (void)
{
    assert(0);
}

static void
handle_setl_insn (void)
{
    assert(0);
}

static void
handle_setle_insn (void)
{
    assert(0);
}

static void
handle_setne_insn (void)
{
    assert(0);
}

static void
handle_shl_insn (void)
{
    assert(0);
}

static void
handle_shr_insn (void)
{
    assert(0);
}

static void
handle_sub_insn (void)
{
    assert(0);
}

static void
handle_test_insn (void)
{
    assert(0);
}

static void
gen_xori (reg_t ra, reg_t rs, word_16 imm)
{
    emit(COMPOSE_XORI(ra, rs, imm));
}

static void
gen_xord (reg_t ra, reg_t rs, reg_t rb)
{
    emit(COMPOSE_XORD(ra, rs, rb));
}

static void
gen_xor (reg_t ra, reg_t rs, reg_t rb)
{
    emit(COMPOSE_XOR(ra, rs, rb));
}

static void
handle_xor_insn (void)
{
    gen_bit_insn(&gen_xori, &gen_xord, 0, &gen_xor);
}

void compile_to_ppc_i386_insn (interpreter_t *intp) {
word_8 opcode, opcode2;
word_32 next_pc;
i386_decode_opcode(intp, &prefix_flags, &opcode, &opcode2);
switch (opcode) {
case 51 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_xor_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_xor_insn();
}
break;
case 50 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_xor_insn();
}
break;
case 49 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_xor_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_xor_insn();
}
break;
case 48 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_xor_insn();
}
break;
case 53 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_xor_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_xor_insn();
}
break;
case 52 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_xor_insn();
}
break;
case 133 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_test_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_test_insn();
}
break;
case 132 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_test_insn();
}
break;
case 169 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_test_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_test_insn();
}
break;
case 168 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_test_insn();
}
break;
case 43 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_sub_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_sub_insn();
}
break;
case 42 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_sub_insn();
}
break;
case 41 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_sub_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_sub_insn();
}
break;
case 40 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_sub_insn();
}
break;
case 45 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_sub_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_sub_insn();
}
break;
case 44 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_sub_insn();
}
break;
case 193 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM8; op_width = 16;
handle_shr_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM8; op_width = 32;
handle_shr_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM8; op_width = 16;
handle_shl_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM8; op_width = 32;
handle_shl_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM8; op_width = 16;
handle_sar_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM8; op_width = 32;
handle_sar_insn();
}
break;
default :
assert(0);
}
}
break;
case 211 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_CL; op_width = 16;
handle_shr_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_CL; op_width = 32;
handle_shr_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_CL; op_width = 16;
handle_shl_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_CL; op_width = 32;
handle_shl_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_CL; op_width = 16;
handle_sar_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_CL; op_width = 32;
handle_sar_insn();
}
break;
default :
assert(0);
}
}
break;
case 209 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_1; op_width = 16;
handle_shr_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_1; op_width = 32;
handle_shr_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_1; op_width = 16;
handle_shl_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_1; op_width = 32;
handle_shl_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_1; op_width = 16;
handle_sar_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_1; op_width = 32;
handle_sar_insn();
}
break;
default :
assert(0);
}
}
break;
case 192 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_shr_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_shl_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_sar_insn();
}
break;
default :
assert(0);
}
}
break;
case 210 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_CL; op_width = 8;
handle_shr_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_CL; op_width = 8;
handle_shl_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_CL; op_width = 8;
handle_sar_insn();
}
break;
default :
assert(0);
}
}
break;
case 208 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_1; op_width = 8;
handle_shr_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_1; op_width = 8;
handle_shl_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_1; op_width = 8;
handle_sar_insn();
}
break;
default :
assert(0);
}
}
break;
case 194 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM16; op_width = 16;
handle_ret_insn();
}
break;
case 195 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_ret_insn();
}
break;
case 243 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 165 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_rep_movsd_insn();
}
break;
case 164 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_rep_movsb_insn();
}
break;
default :
switch (reg) {
default :
assert(0);
}
}
break;
case 242 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 174 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_repne_scasb_insn();
}
break;
default :
switch (reg) {
default :
assert(0);
}
}
break;
case 104 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_push_insn();
}
break;
case 106 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_push_insn();
}
break;
case 80 :
case 81 :
case 82 :
case 83 :
case 84 :
case 85 :
case 86 :
case 87 :
opcode_reg = opcode - 80;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_PR32; op_width = 32;
handle_push_insn();
}
break;
case 88 :
case 89 :
case 90 :
case 91 :
case 92 :
case 93 :
case 94 :
case 95 :
opcode_reg = opcode - 88;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_PR32; op_width = 32;
handle_pop_insn();
}
break;
case 143 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M32; op_width = 32;
handle_pop_insn();
}
break;
default :
assert(0);
}
}
break;
case 11 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_or_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_or_insn();
}
break;
case 10 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_or_insn();
}
break;
case 9 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_or_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_or_insn();
}
break;
case 8 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_or_insn();
}
break;
case 13 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_or_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_or_insn();
}
break;
case 12 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_or_insn();
}
break;
case 144 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_nop_insn();
}
break;
case 199 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_mov_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_mov_insn();
}
break;
default :
assert(0);
}
}
break;
case 198 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_mov_insn();
}
break;
default :
assert(0);
}
}
break;
case 184 :
case 185 :
case 186 :
case 187 :
case 188 :
case 189 :
case 190 :
case 191 :
opcode_reg = opcode - 184;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_PR16_IMM16; op_width = 16;
handle_mov_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_PR32_IMM32; op_width = 32;
handle_mov_insn();
}
break;
case 176 :
case 177 :
case 178 :
case 179 :
case 180 :
case 181 :
case 182 :
case 183 :
opcode_reg = opcode - 176;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_PR8_IMM8; op_width = 8;
handle_mov_insn();
}
break;
case 163 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_MOFFS32_AX; op_width = 16;
handle_mov_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_MOFFS32_EAX; op_width = 32;
handle_mov_insn();
}
break;
case 161 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_MOFFS32; op_width = 16;
handle_mov_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_MOFFS32; op_width = 32;
handle_mov_insn();
}
break;
case 139 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_mov_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_mov_insn();
}
break;
case 138 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_mov_insn();
}
break;
case 137 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_mov_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_mov_insn();
}
break;
case 136 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_mov_insn();
}
break;
case 141 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_lea_insn();
}
break;
case 121 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jns_insn();
}
break;
case 117 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jne_insn();
}
break;
case 233 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jmp_insn();
}
break;
case 235 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jmp_insn();
}
break;
case 126 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jle_insn();
}
break;
case 124 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jl_insn();
}
break;
case 125 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jge_insn();
}
break;
case 127 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jg_insn();
}
break;
case 116 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_je_insn();
}
break;
case 118 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jbe_insn();
}
break;
case 114 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jb_insn();
}
break;
case 115 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_jae_insn();
}
break;
case 119 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_SIMM8; op_width = 32;
handle_ja_insn();
}
break;
case 205 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM8; op_width = 8;
handle_int_insn();
}
break;
case 64 :
case 65 :
case 66 :
case 67 :
case 68 :
case 69 :
case 70 :
case 71 :
opcode_reg = opcode - 64;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_PR16; op_width = 16;
handle_inc_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_PR32; op_width = 32;
handle_inc_insn();
}
break;
case 105 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_imul_insn();
}
break;
case 15 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 149 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_setne_insn();
}
break;
case 158 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_setle_insn();
}
break;
case 156 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_setl_insn();
}
break;
case 159 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_setg_insn();
}
break;
case 148 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_sete_insn();
}
break;
case 182 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM8; op_width = 16;
handle_movzx_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM8; op_width = 32;
handle_movzx_insn();
}
break;
case 190 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM8; op_width = 16;
handle_movsx_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM8; op_width = 32;
handle_movsx_insn();
}
break;
case 137 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jns_insn();
}
break;
case 133 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jne_insn();
}
break;
case 142 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jle_insn();
}
break;
case 140 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jl_insn();
}
break;
case 141 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jge_insn();
}
break;
case 143 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jg_insn();
}
break;
case 132 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_je_insn();
}
break;
case 134 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jbe_insn();
}
break;
case 130 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jb_insn();
}
break;
case 131 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_jae_insn();
}
break;
case 135 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_ja_insn();
}
break;
case 175 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_imul_insn();
}
break;
default :
switch (reg) {
default :
assert(0);
}
}
break;
case 217 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 192 :
case 193 :
case 194 :
case 195 :
case 196 :
case 197 :
case 198 :
case 199 :
opcode_reg = opcode2 - 192;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_PST; op_width = 3;
handle_fld_insn();
}
break;
default :
switch (reg) {
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M16_NOPREFIX; op_width = 16;
handle_fstcw_insn();
}
break;
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M16_NOPREFIX; op_width = 16;
handle_fldcw_insn();
}
break;
default :
assert(0);
}
}
break;
case 221 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 216 :
case 217 :
case 218 :
case 219 :
case 220 :
case 221 :
case 222 :
case 223 :
opcode_reg = opcode2 - 216;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_PST; op_width = 3;
handle_fstp_insn();
}
break;
default :
switch (reg) {
case 3 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M64; op_width = 64;
handle_fstp_insn();
}
break;
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M64; op_width = 64;
handle_fld_insn();
}
break;
default :
assert(0);
}
}
break;
case 219 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M32; op_width = 32;
handle_fild_insn();
}
break;
default :
assert(0);
}
}
break;
case 223 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 224 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_fnstsw_insn();
}
break;
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M16_NOPREFIX; op_width = 16;
handle_fild_insn();
}
break;
default :
assert(0);
}
}
break;
case 218 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M32; op_width = 32;
handle_fidivr_insn();
}
break;
default :
assert(0);
}
}
break;
case 216 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
case 217 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_fcomp_insn();
}
break;
case 208 :
case 209 :
case 210 :
case 211 :
case 212 :
case 213 :
case 214 :
case 215 :
opcode_reg = opcode2 - 208;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_PST; op_width = 3;
handle_fcom_insn();
}
break;
default :
switch (reg) {
default :
assert(0);
}
}
break;
case 220 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M64; op_width = 64;
handle_fcom_insn();
}
break;
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_M64; op_width = 64;
handle_fadd_insn();
}
break;
default :
assert(0);
}
}
break;
case 247 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_test_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_test_insn();
}
break;
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16; op_width = 16;
handle_not_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_not_insn();
}
break;
case 3 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16; op_width = 16;
handle_neg_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_neg_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_mul_insn();
}
break;
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_imul_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_idiv_insn();
}
break;
case 6 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_div_insn();
}
break;
default :
assert(0);
}
}
break;
case 246 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_test_insn();
}
break;
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_not_insn();
}
break;
case 3 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_neg_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_mul_insn();
}
break;
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_imul_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_idiv_insn();
}
break;
case 6 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_div_insn();
}
break;
default :
assert(0);
}
}
break;
case 72 :
case 73 :
case 74 :
case 75 :
case 76 :
case 77 :
case 78 :
case 79 :
opcode_reg = opcode - 72;
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_PR16; op_width = 16;
handle_dec_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_PR32; op_width = 32;
handle_dec_insn();
}
break;
case 254 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_inc_insn();
}
break;
case 1 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8; op_width = 8;
handle_dec_insn();
}
break;
default :
assert(0);
}
}
break;
case 59 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_cmp_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_cmp_insn();
}
break;
case 58 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_cmp_insn();
}
break;
case 57 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_cmp_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_cmp_insn();
}
break;
case 56 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_cmp_insn();
}
break;
case 61 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_cmp_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_cmp_insn();
}
break;
case 60 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_cmp_insn();
}
break;
case 252 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_cld_insn();
}
break;
case 153 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_NIL;
handle_cdq_insn();
}
break;
case 255 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 6 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_push_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_jmp_insn();
}
break;
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16; op_width = 16;
handle_inc_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_inc_insn();
}
break;
case 1 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16; op_width = 16;
handle_dec_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_dec_insn();
}
break;
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32; op_width = 32;
handle_call_insn();
}
break;
default :
assert(0);
}
}
break;
case 232 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_IMM32; op_width = 32;
handle_call_insn();
}
break;
case 35 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_and_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_and_insn();
}
break;
case 34 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_and_insn();
}
break;
case 33 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_and_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_and_insn();
}
break;
case 32 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_and_insn();
}
break;
case 37 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_and_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_and_insn();
}
break;
case 36 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_and_insn();
}
break;
case 19 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_adc_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_adc_insn();
}
break;
case 18 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_adc_insn();
}
break;
case 17 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_adc_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_adc_insn();
}
break;
case 16 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_adc_insn();
}
break;
case 21 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_adc_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_adc_insn();
}
break;
case 20 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_adc_insn();
}
break;
case 3 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_R16_RM16; op_width = 16;
handle_add_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_R32_RM32; op_width = 32;
handle_add_insn();
}
break;
case 2 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_R8_RM8; op_width = 8;
handle_add_insn();
}
break;
case 1 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
next_pc = pc = intp->pc;
mode = MODE_RM16_R16; op_width = 16;
handle_add_insn();
} else {
next_pc = pc = intp->pc;
mode = MODE_RM32_R32; op_width = 32;
handle_add_insn();
}
break;
case 0 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
next_pc = pc = intp->pc;
mode = MODE_RM8_R8; op_width = 8;
handle_add_insn();
}
break;
case 131 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 6 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_xor_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_xor_insn();
}
break;
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_sub_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_sub_insn();
}
break;
case 1 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_or_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_or_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_cmp_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_cmp_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_and_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_and_insn();
}
break;
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_adc_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_adc_insn();
}
break;
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_SIMM8; op_width = 16;
handle_add_insn();
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_SIMM8; op_width = 32;
handle_add_insn();
}
break;
default :
assert(0);
}
}
break;
case 129 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 6 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_xor_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_xor_insn();
}
break;
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_sub_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_sub_insn();
}
break;
case 1 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_or_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_or_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_cmp_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_cmp_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_and_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_and_insn();
}
break;
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_adc_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_adc_insn();
}
break;
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_RM16_IMM16; op_width = 16;
handle_add_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_RM32_IMM32; op_width = 32;
handle_add_insn();
}
break;
default :
assert(0);
}
}
break;
case 128 :
i386_decode_modrm(intp, &opcode2, &mod, &reg, &rm);
switch (opcode2) {
default :
switch (reg) {
case 6 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_xor_insn();
}
break;
case 5 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_sub_insn();
}
break;
case 1 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_or_insn();
}
break;
case 7 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_cmp_insn();
}
break;
case 4 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_and_insn();
}
break;
case 2 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_adc_insn();
}
break;
case 0 :
i386_decode_sib(intp, opcode2, &sib_scale, &sib_index, &sib_base, &disp8, &disp32);
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_RM8_IMM8; op_width = 8;
handle_add_insn();
}
break;
default :
assert(0);
}
}
break;
case 5 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
imm16 = i386_decode_imm16(intp);
next_pc = pc = intp->pc;
mode = MODE_AX_IMM16; op_width = 16;
handle_add_insn();
} else {
imm32 = i386_decode_imm32(intp);
next_pc = pc = intp->pc;
mode = MODE_EAX_IMM32; op_width = 32;
handle_add_insn();
}
break;
case 4 :
if (prefix_flags & I386_PREFIX_OP_SIZE_OVERRIDE) {
assert(0);
} else {
imm8 = i386_decode_imm8(intp);
next_pc = pc = intp->pc;
mode = MODE_AL_IMM8; op_width = 8;
handle_add_insn();
}
break;
default:
assert(0);
}
intp->pc = next_pc;
}
