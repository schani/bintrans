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
#define MODE_R32_RM16 37
#define MODE_AX_MOFFS32 38
#define MODE_EAX_MOFFS32 39
#define MODE_MOFFS32_AX 40
#define MODE_MOFFS32_EAX 41
#define MODE_PR16_AX 42
#define MODE_PR32_EAX 43
#define MODE_PR8_IMM8 44
#define MODE_PR16_IMM16 45
#define MODE_PR32_IMM32 46
#define MODE_PST 47

#define REG_EAX                   0
#define REG_ECX                   1
#define REG_EDX                   2
#define REG_EBX                   3
#define REG_ESP                   4
#define REG_EBP                   5
#define REG_ESI                   6
#define REG_EDI                   7

#define REG_EFLAGS                8
#define REG_FPSW                  9

#define ref_i386_gpr_r(x)         ref_integer_reg_for_reading(x)
#define ref_i386_gpr_w(x)         ref_integer_reg_for_writing(x)
#define ref_i386_gpr_rw(x)        ref_integer_reg_for_reading_and_writing(x)
#define ref_i386_fpsw_r()         ref_integer_reg_for_reading(REG_FPSW)
#define ref_i386_fpsw_w()         ref_integer_reg_for_writing(REG_FPSW)
#define ref_i386_fpsw_rw()        ref_integer_reg_for_reading_and_writing(REG_FPSW)
#define ref_i386_eflags_r()       ref_integer_reg_for_reading(REG_EFLAGS)
#define ref_i386_eflags_w()       ref_integer_reg_for_writing(REG_EFLAGS)
#define ref_i386_eflags_rw()      ref_integer_reg_for_reading_and_writing(REG_EFLAGS)

#define KILL_OF                   ((flags_killed >> 11) & 1)
#define KILL_CF                   (flags_killed & 1)
#define KILL_SF                   ((flags_killed >> 7) & 1)
#define KILL_ZF                   ((flags_killed >> 6) & 1)
#define KILL_SZF                  (KILL_SF || KILL_ZF)

void
move_i386_regs_interpreter_to_compiler (interpreter_t *intp)
{
    *(double*)&constant_area[28] = (intp->regs_FPST[7]);
    *(double*)&constant_area[26] = (intp->regs_FPST[6]);
    *(double*)&constant_area[24] = (intp->regs_FPST[5]);
    *(double*)&constant_area[22] = (intp->regs_FPST[4]);
    *(double*)&constant_area[20] = (intp->regs_FPST[3]);
    *(double*)&constant_area[18] = (intp->regs_FPST[2]);
    *(double*)&constant_area[16] = (intp->regs_FPST[1]);
    *(double*)&constant_area[14] = (intp->regs_FPST[0]);
    *(word_16*)&constant_area[9] = (intp->regs_FSPR[0]);
    *(word_32*)&constant_area[8] = (intp->regs_SPR[0]);
    *(word_32*)&constant_area[7] = (intp->regs_GPR[7]);
    *(word_32*)&constant_area[6] = (intp->regs_GPR[6]);
    *(word_32*)&constant_area[5] = (intp->regs_GPR[5]);
    *(word_32*)&constant_area[4] = (intp->regs_GPR[4]);
    *(word_32*)&constant_area[3] = (intp->regs_GPR[3]);
    *(word_32*)&constant_area[2] = (intp->regs_GPR[2]);
    *(word_32*)&constant_area[1] = (intp->regs_GPR[1]);
    *(word_32*)&constant_area[0] = (intp->regs_GPR[0]);
    *(word_32*)&constant_area[13] = ((intp->regs_SPR[0] >> 11) & 0x1);
    *(word_32*)&constant_area[12] = ((intp->regs_SPR[0] >> 7) & 0x1);
    *(word_32*)&constant_area[11] = ((intp->regs_SPR[0] >> 6) & 0x1);
    *(word_32*)&constant_area[10] = ((intp->regs_SPR[0] >> 0) & 0x1);
}

void
move_i386_regs_compiler_to_interpreter (interpreter_t *intp)
{
    (intp->regs_FPST[7]) = *(double*)&constant_area[28];
    (intp->regs_FPST[6]) = *(double*)&constant_area[26];
    (intp->regs_FPST[5]) = *(double*)&constant_area[24];
    (intp->regs_FPST[4]) = *(double*)&constant_area[22];
    (intp->regs_FPST[3]) = *(double*)&constant_area[20];
    (intp->regs_FPST[2]) = *(double*)&constant_area[18];
    (intp->regs_FPST[1]) = *(double*)&constant_area[16];
    (intp->regs_FPST[0]) = *(double*)&constant_area[14];
    (intp->regs_FSPR[0]) = *(word_16*)&constant_area[9];
    (intp->regs_SPR[0]) = *(word_32*)&constant_area[8];
    (intp->regs_GPR[7]) = *(word_32*)&constant_area[7];
    (intp->regs_GPR[6]) = *(word_32*)&constant_area[6];
    (intp->regs_GPR[5]) = *(word_32*)&constant_area[5];
    (intp->regs_GPR[4]) = *(word_32*)&constant_area[4];
    (intp->regs_GPR[3]) = *(word_32*)&constant_area[3];
    (intp->regs_GPR[2]) = *(word_32*)&constant_area[2];
    (intp->regs_GPR[1]) = *(word_32*)&constant_area[1];
    (intp->regs_GPR[0]) = *(word_32*)&constant_area[0];
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFF7FF) | ((*(word_32*)&constant_area[13]) << 11));
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFFF7F) | ((*(word_32*)&constant_area[12]) << 7));
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFFFBF) | ((*(word_32*)&constant_area[11]) << 6));
    (intp->regs_SPR[0] = (intp->regs_SPR[0] & 0xFFFFFFFE) | ((*(word_32*)&constant_area[10]) << 0));
}

static word_32 pc, this_pc;
static int prefix_flags;
static word_8 mod, reg, rm, sib_scale, sib_index, sib_base, disp8, opcode_reg, imm8;
static word_16 imm16;
static word_32 disp32, imm32;
static int op_width;
static int mode;
static word_32 flags_killed;

static void
gen_interpreter_handle (void)
{
    emit_load_integer_32(3, this_pc);
    emit_interpreter_handle();
}

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
    if (reg == 0)
	return;
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
	case MODE_RM16_IMM8 :
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
	    unref_integer_reg(index_reg);
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

		    emit_load_integer_32(*rb, disp32);
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
	emit_load_integer_32(*rb, SEX32(imm, 16));
    }
    else
	assert(*rb != NO_REG);
}

static reg_t
ref_i386_gpr_extended (reg_t num, int for_reading, int for_writing, int zexed, int width)
{
    assert(for_reading || for_writing);

    if (zexed)
    {
	reg_t ereg = ref_i386_gpr_r(num);
	reg_t tmp;

	tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_CLRLWI(tmp, ereg, 32 - width));

	unref_integer_reg(ereg);

	return tmp;
    }
    else
    {
	if (width != 32 && for_writing)
	{
	    if (!for_reading)
		return alloc_tmp_integer_reg();
	    else
	    {
		reg_t tmp = alloc_tmp_integer_reg();
		reg_t ereg = ref_i386_gpr_r(num);

		emit(COMPOSE_MR(tmp, ereg));

		unref_integer_reg(ereg);

		return tmp;
	    }
	}
	else
	    return ref_i386_gpr(num, for_reading, for_writing);
    }
}

static reg_t
extract_hreg (reg_t num, int for_reading, int for_writing)
{
    if (!for_reading)
	return alloc_tmp_integer_reg();
    else
    {
	reg_t ereg = ref_i386_gpr(num, 1, for_writing);
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_EXTRWI(tmp, ereg, 8, 16));

	unref_integer_reg(ereg);

	return tmp;
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
	case MODE_R32_RM16 :
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
	    {
		reg_t tmp = alloc_tmp_integer_reg();

		emit_load_integer_32(tmp, imm32);

		emit(COMPOSE_LHBRX(tmp, 0, tmp));

		return tmp;
	    }

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
ref_dst_op (int for_reading, int for_writing, int zexed,
	    word_32 *imm, int imm_width, int is_simm)
{
    word_32 val;

    assert(imm == 0);

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

	case MODE_MOFFS32_AX :
	    {
		reg_t tmp = alloc_tmp_integer_reg();

		if (for_reading)
		{
		    reg_t addr = alloc_tmp_integer_reg();

		    emit_load_integer_32(addr, imm32);

		    emit(COMPOSE_LHBRX(tmp, 0, addr));

		    free_tmp_integer_reg(addr);
		}

		return tmp;
	    }

	case MODE_MOFFS32_EAX :
	    {
		reg_t tmp = alloc_tmp_integer_reg();

		if (for_reading)
		{
		    reg_t addr = alloc_tmp_integer_reg();

		    emit_load_integer_32(addr, imm32);

		    emit(COMPOSE_LWBRX(tmp, 0, addr));

		    free_tmp_integer_reg(addr);
		}

		return tmp;
	    }

	case MODE_R8_RM8 :
	    return ref_i386_gpr_extended(reg, for_reading, for_writing, zexed, 8); /* FIXME */

	case MODE_R16_RM16 :
	case MODE_R16_RM8 :
	    return ref_i386_gpr_extended(reg, for_reading, for_writing, zexed, 16);

	case MODE_R32_RM32 :
	case MODE_R32_RM8 :
	case MODE_R32_RM16 :
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

static reg_t
ref_dst_op_to_reg (int for_reading, int for_writing, int zexed)
{
    return ref_dst_op(for_reading, for_writing, zexed, 0, 0, 0);
}

static void
insert_into_gpr (reg_t num, reg_t src, int length, int begin)
{
    reg_t ereg = ref_i386_gpr_rw(num);

    emit(COMPOSE_INSRWI(ereg, src, length, begin));

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
	    {
		if (rm < 4)
		    insert_into_gpr(rm, dst_reg, 8, 24);
		else
		    insert_into_gpr(rm - 4, dst_reg, 8, 16);
	    }
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
	    insert_into_gpr(REG_EAX, dst_reg, 8, 24);
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

	case MODE_MOFFS32_AX :
	    {
		reg_t addr = alloc_tmp_integer_reg();

		emit_load_integer_32(addr, imm32);

		emit(COMPOSE_STHBRX(dst_reg, 0, addr));

		free_tmp_integer_reg(addr);
	    }
	    break;

	case MODE_MOFFS32_EAX :
	    {
		reg_t addr = alloc_tmp_integer_reg();

		emit_load_integer_32(addr, imm32);

		emit(COMPOSE_STWBRX(dst_reg, 0, addr));

		free_tmp_integer_reg(addr);
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
	case MODE_R32_RM16 :
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

static reg_t
ref_fpsw_top (void)
{
    reg_t fpsw = ref_i386_fpsw_r();
    reg_t top = alloc_tmp_integer_reg();

    emit(COMPOSE_EXTRWI(top, fpsw, 3, 18));

    unref_integer_reg(fpsw);

    return top;
}

static void
commit_fpsw_top (reg_t top)
{
    reg_t fpsw = ref_i386_fpsw_rw();

    emit(COMPOSE_INSRWI(fpsw, top, 3, 18));

    unref_integer_reg(fpsw);
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
    reg_t src = ref_src_op(0, &imm, 16, 0), dst = ref_dst_op_to_reg(1, 1, 0);

    if (src == NO_REG)
    {
	if (KILL_SZF && idgen != 0)
	    idgen(dst, dst, imm);
	else
	{
	    igen(dst, dst, imm);
	    if (KILL_SZF)
		emit(COMPOSE_CMPWI(0, dst, 0));
	}
    }
    else
    {
	if (KILL_SZF)
	    dgen(dst, dst, src);
	else
	    gen(dst, dst, src);

	dispose_integer_reg(src);
    }

    commit_and_dispose_dst_op(dst, 1, 0, 0);

    clear_of_cf();
}

static void
gen_add_with_imm (reg_t dst, word_16 imm)
{
    assert(!KILL_OF);
    if (KILL_SZF)
	emit(COMPOSE_ADDICD(dst, dst, imm));
    else if (KILL_CF)
	emit(COMPOSE_ADDIC(dst, dst, imm));
    else
	emit(COMPOSE_ADDI(dst, dst, imm));
}

static void
handle_add_insn (void)
{
    word_32 imm;
    int shift = op_width != 32 && (KILL_CF || KILL_OF || KILL_SZF);
    int no_imm = KILL_OF || shift;
    reg_t src = ref_src_op(0, no_imm ? 0 : &imm, 16, 1);
    reg_t dst = ref_dst_op_to_reg(1, 1, 0);

    if (shift)
    {
	reg_t tmp = alloc_tmp_integer_reg();

	assert(src != NO_REG);

	emit(COMPOSE_SLWI(tmp, src, 32 - op_width));
	dispose_integer_reg(src);
	src = tmp;

	emit(COMPOSE_SLWI(dst, dst, 32 - op_width));
    }

    if (src == NO_REG)
    {
	assert(!shift);
	gen_add_with_imm(dst, imm);
    }
    else
    {
	if (KILL_CF && KILL_OF && KILL_SZF)
	    emit(COMPOSE_ADDCOD(dst, dst, src));
	else if (KILL_CF && KILL_OF)
	    emit(COMPOSE_ADDCO(dst, dst, src));
	else if (KILL_CF && KILL_SZF)
	    emit(COMPOSE_ADDCD(dst, dst, src));
	else if (KILL_CF)
	    emit(COMPOSE_ADDC(dst, dst, src));
	else if (KILL_OF && KILL_SZF)
	    emit(COMPOSE_ADDOD(dst, dst, src));
	else if (KILL_OF)
	    emit(COMPOSE_ADDO(dst, dst, src));
	else if (KILL_SZF)
	    emit(COMPOSE_ADDD(dst, dst, src));
	else
	    emit(COMPOSE_ADD(dst, dst, src));

	dispose_integer_reg(src);

	if (shift)
	    emit(COMPOSE_SRWI(dst, dst, 32 - op_width));
    }

    commit_and_dispose_dst_op(dst, 1, 0, 0);
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
handle_bsr_insn (void)
{
    reg_t src = ref_src_op(1, 0, 0, 0);
    reg_t dst = ref_dst_op_to_reg(0, 1, 0);

    if (KILL_ZF)
	emit(COMPOSE_CMPWI(0, src, 0));
    emit(COMPOSE_CNTLZW(dst, src));
    emit(COMPOSE_SUBFIC(dst, dst, 31));

    commit_and_dispose_dst_op(dst, 0, 1, 0);
    dispose_integer_reg(src);
}

static void
handle_bt_insn (void)
{
    gen_interpreter_handle();
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
		tmp = ref_dst_op_to_reg(1, 0, 0);

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
    reg_t eax = ref_i386_gpr_r(REG_EAX);
    reg_t edx = ref_i386_gpr_w(REG_EDX);

    emit(COMPOSE_SRAWI(edx, eax, 31));

    unref_integer_reg(edx);
    unref_integer_reg(eax);
}

static void
handle_cld_insn (void)
{
    reg_t eflags = ref_i386_eflags_rw();

    emit(COMPOSE_RLWINM(eflags, eflags, 0, 22, 20));

    unref_integer_reg(eflags);
}

static void
handle_cmp_insn (void)
{
    if (KILL_SZF || KILL_OF || KILL_CF)
    {
	reg_t src = ref_src_op(0, 0, 0, 0);
	reg_t dst = ref_dst_op_to_reg(1, 0, 0);
	reg_t tmp;

	if (op_width != 32)
	{
	    tmp = alloc_tmp_integer_reg();
	    emit(COMPOSE_SLWI(tmp, src, 32 - op_width));
	    dispose_integer_reg(src);
	    src = tmp;

	    tmp = alloc_tmp_integer_reg();
	    emit(COMPOSE_SLWI(tmp, dst, 32 - op_width));
	    dispose_integer_reg(dst);
	    dst = tmp;
	}

	tmp = alloc_tmp_integer_reg();

	if (KILL_CF)
	{
	    emit(COMPOSE_NOR(tmp, dst, dst));
	    emit(COMPOSE_ADDC(tmp, src, tmp));
	}

	if (KILL_SZF && KILL_OF)
	    emit(COMPOSE_SUBFOD(tmp, src, dst));
	else if (KILL_SZF)
	    emit(COMPOSE_SUBFD(tmp, src, dst));
	else if (KILL_OF)
	    emit(COMPOSE_SUBFO(tmp, src, dst));
	else
	    emit(COMPOSE_SUBF(tmp, src, dst));

	free_tmp_integer_reg(tmp);

	dispose_integer_reg(dst);
	dispose_integer_reg(src);
    }
}

static void
handle_dec_insn (void)
{
    reg_t dst = ref_dst_op_to_reg(1, 1, 0);
    reg_t tmp;

    assert(op_width == 32);

    if (KILL_OF)
    {
	tmp = alloc_tmp_integer_reg();
	emit(COMPOSE_LI(tmp, (word_16)-1));
    }

    if (KILL_OF && KILL_SZF)
	emit(COMPOSE_ADDOD(dst, dst, tmp));
    else if (KILL_OF)
	emit(COMPOSE_ADDO(dst, dst, tmp));
    else
    {
	emit(COMPOSE_ADDI(dst, dst, (word_16)-1));
	if (KILL_SZF)
	    emit(COMPOSE_CMPWI(0, dst, 0));
    }

    if (KILL_OF)
	free_tmp_integer_reg(tmp);

    commit_and_dispose_dst_op(dst, 1, 0, 0);
}

static void
gen_div_insn (int is_signed)
{
    reg_t dst = ref_dst_op_to_reg(1, 0, 0);
    reg_t eax = ref_i386_gpr_rw(REG_EAX);
    reg_t edx = ref_i386_gpr_w(REG_EDX);
    reg_t tmp = alloc_tmp_integer_reg();
    reg_t tmp2 = alloc_tmp_integer_reg();

    assert(op_width == 32);

    if (is_signed)
	emit(COMPOSE_DIVW(tmp, eax, dst));
    else
	emit(COMPOSE_DIVWU(tmp, eax, dst));
    emit(COMPOSE_MULLW(tmp2, tmp, dst));
    emit(COMPOSE_SUBF(edx, tmp2, eax));
    emit(COMPOSE_MR(eax, tmp));

    free_tmp_integer_reg(tmp2);
    free_tmp_integer_reg(tmp);
    unref_integer_reg(edx);
    unref_integer_reg(eax);
    dispose_integer_reg(dst);
}

static void
handle_div_insn (void)
{
    gen_div_insn(0);
}

static void
handle_fadd_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_faddp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fcom_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fcomp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fcompp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fdiv_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fdivp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fdivrp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fidivr_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fild_insn (void)
{
    /*
    reg_t top = ref_fpsw_top();
    reg_t dst = ref_dst_op_to_reg(1, 0, 1);

    emit(COMPOSE_ADDI(top, top, (word_16)-1));
    emit(COMPOSE_CLRLWI(top, top, 29));
    commit_fpsw_top(top);
    */

    gen_interpreter_handle();
}

static void
handle_fistp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fld_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fldcw_insn (void)
{
    reg_t dst = ref_dst_op_to_reg(1, 0, 1);
    reg_t fpsw = ref_i386_fpsw_w();

    emit(COMPOSE_MR(fpsw, dst));

    unref_integer_reg(fpsw);
    dispose_integer_reg(dst);
}

static void
handle_fldz_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fmul_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fmulp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fnstsw_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fstcw_insn (void)
{
    reg_t fpsw = ref_i386_fpsw_r();

    commit_and_dispose_dst_op(fpsw, 0, 1, 1);
}

static void
handle_fstp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fsub_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fucompp_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_fxch_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_idiv_insn (void)
{
    gen_div_insn(1);
}

static void
gen_mul64 (void (*gen_high) (reg_t, reg_t, reg_t), int single_op)
{
    reg_t dst = ref_dst_op_to_reg(1, 0, 0);
    reg_t tmp = alloc_tmp_integer_reg();
    reg_t src;
    reg_t low, high;

    assert(op_width == 32);

    if (single_op)
    {
	src = ref_i386_gpr_rw(REG_EAX);
	high = ref_i386_gpr_w(REG_EDX);
	low = src;
    }
    else
    {
	src = ref_src_op(0, 0, 0, 0);
	if (KILL_OF || KILL_CF)
	    high = alloc_tmp_integer_reg();
	low = dst;
    }

    emit(COMPOSE_MULLW(tmp, src, dst));
    if (single_op || KILL_OF || KILL_CF)
	gen_high(high, src, dst);
    emit(COMPOSE_MR(low, tmp));

    free_tmp_integer_reg(tmp);
    dispose_integer_reg(src);
    dispose_integer_reg(dst);

    if (KILL_OF || KILL_CF)
    {
	label_t label = alloc_label();

	emit(COMPOSE_CMPWI(1, high, 0));
	emit(COMPOSE_ADDCO(ZERO_REG, ZERO_REG, ZERO_REG));
	emit_branch(COMPOSE_BEQ(2, 0), label);

	dispose_integer_reg(high);

	tmp = alloc_tmp_integer_reg();
	emit(COMPOSE_MFXER(tmp));
	emit(COMPOSE_ORIS(tmp, tmp, 0x6000));
	emit(COMPOSE_MTXER(tmp));
	free_tmp_integer_reg(tmp);

	emit_label(label);
	free_label(label);
    }
    else
	if (single_op)
	    dispose_integer_reg(high);
}

static void
gen_mulhw (reg_t d, reg_t a, reg_t b)
{
    emit(COMPOSE_MULHW(d, a, b));
}

static void
handle_imul_insn (void)
{
    assert(op_width == 32);

    if (mode == MODE_RM32)
	gen_mul64(&gen_mulhw, 1);
    else
	gen_mul64(&gen_mulhw, 0);
}

static void
handle_inc_insn (void)
{
    reg_t dst = ref_dst_op_to_reg(1, 1, 0);
    reg_t tmp;
    int shift = op_width != 32 && (KILL_CF || KILL_OF || KILL_SZF)
	? 32 - op_width : 0;

    if (shift != 0)
	emit(COMPOSE_SLWI(dst, dst, shift));

    if (KILL_OF)
    {
	tmp = alloc_tmp_integer_reg();
	emit_load_integer_32(tmp, 1 << shift);
    }

    if (KILL_OF && KILL_SZF)
	emit(COMPOSE_ADDOD(dst, dst, tmp));
    else if (KILL_OF)
	emit(COMPOSE_ADDO(dst, dst, tmp));
    else
    {
	emit(COMPOSE_ADDI(dst, dst, 1));
	if (KILL_SZF)
	    emit(COMPOSE_CMPWI(0, dst, 0));
    }

    if (KILL_OF)
	free_tmp_integer_reg(tmp);

    if (shift != 0)
	emit(COMPOSE_SRWI(dst, dst, shift));

    commit_and_dispose_dst_op(dst, 1, 0, 0);
}

static void
handle_int_insn (void)
{
    assert(mode == MODE_IMM8 && imm8 == 0x80);

    emit_system_call();

    emit_direct_jump(pc);
}

static void
gen_cr_jump (void (*gen) (label_t))
{
    word_32 rel;
    int is_imm = dst_is_imm(&rel);
    label_t label = alloc_label();

    assert(is_imm);

    gen(label);

    emit_direct_jump(pc + rel);

    emit_label(label);

    free_label(label);
}

static void
gen_ba (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit(COMPOSE_CROR(6, 2, 6));
    emit_branch(COMPOSE_BEQ(6, 0), label);
}

static void
handle_ja_insn (void)
{
    gen_cr_jump(&gen_ba);
}

static void
gen_bca (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit_branch(COMPOSE_BEQ(6, 0), label);
}

static void
handle_jae_insn (void)
{
    gen_cr_jump(&gen_bca);
}

static void
gen_bnca (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit_branch(COMPOSE_BNE(6, 0), label);
}

static void
handle_jb_insn (void)
{
    gen_cr_jump(&gen_bnca);
}

static void
gen_bor_ca_eq (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit(COMPOSE_CROR(6, 2, 6));
    emit_branch(COMPOSE_BNE(6, 0), label);
}

static void
handle_jbe_insn (void)
{
    gen_cr_jump(&gen_bor_ca_eq);
}

static void
gen_bne_2 (label_t label)
{
    emit_branch(COMPOSE_BNE(2, 0), label);
}

static void
handle_je_insn (void)
{
    gen_cr_jump(&gen_bne_2);
}

static void
gen_bnor_zf_ne_sf_of (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit(COMPOSE_CREQV(5, 0, 5));
    emit(COMPOSE_CRORC(5, 2, 5));
    emit_branch(COMPOSE_BEQ(5, 0), label);
}

static void
handle_jg_insn (void)
{
    gen_cr_jump(&gen_bnor_zf_ne_sf_of);
}

static void
gen_bne_lt_ov (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit(COMPOSE_CREQV(5, 0, 5));
    emit_branch(COMPOSE_BNE(5, 0), label);
}

static void
handle_jge_insn (void)
{
    gen_cr_jump(&gen_bne_lt_ov);
}

static void
gen_beq_lt_ov (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit(COMPOSE_CREQV(5, 0, 5));
    emit_branch(COMPOSE_BEQ(5, 0), label);
}

static void
handle_jl_insn (void)
{
    gen_cr_jump(&gen_beq_lt_ov);
}

static void
gen_bor_zf_ne_sf_of (label_t label)
{
    emit(COMPOSE_MCRXR(1));
    emit(COMPOSE_CREQV(5, 0, 5));
    emit(COMPOSE_CRORC(5, 2, 5));
    emit_branch(COMPOSE_BNE(5, 0), label);
}

static void
handle_jle_insn (void)
{
    gen_cr_jump(&gen_bor_zf_ne_sf_of);
}

static void
handle_jmp_insn (void)
{
    if (mode == MODE_RM32)
    {
	reg_t dst = ref_dst_op_to_reg(1, 0, 0);

	emit(COMPOSE_MR(JUMP_TARGET_REG, dst));

	dispose_integer_reg(dst);

	emit_indirect_jump();
    }
    else
    {
	word_32 rel;
	int is_imm = dst_is_imm(&rel);

	assert(is_imm);

	emit_direct_jump(pc + rel);
    }
}

static void
gen_beq_2 (label_t label)
{
    emit_branch(COMPOSE_BEQ(2, 0), label);
}

static void
handle_jne_insn (void)
{
    gen_cr_jump(&gen_beq_2);
}

static void
gen_bns (label_t label)
{
    emit_branch(COMPOSE_BEQ(0, 0), label);
}

static void
handle_jns_insn (void)
{
    gen_cr_jump(&gen_bns);
}

static void
handle_lea_insn (void)
{
    reg_t ra, rb;
    word_16 imm;
    reg_t dst = ref_dst_op_to_reg(0, 1, 0);

    gen_ea(&ra, &rb, &imm);

    if (rb == NO_REG)
	emit(COMPOSE_ADDI(dst, ra, imm));
    else
    {
	assert(imm == 0);
	emit(COMPOSE_ADD(dst, ra, rb));
	dispose_integer_reg(rb);
    }

    dispose_integer_reg(ra);

    commit_and_dispose_dst_op(dst, 0, 0, 0);
}

static void
handle_mov_insn (void)
{
    word_32 imm;
    reg_t src = ref_src_op(0, &imm, 16, 0);

    if (src == NO_REG)
    {
	reg_t dst = ref_dst_op_to_reg(0, 1, 0);

	emit(COMPOSE_LI(dst, imm));

	commit_and_dispose_dst_op(dst, 0, 0, 0);
    }
    else
	commit_and_dispose_dst_op(src, 0, 0, 1);
}

static void
handle_movsx_insn (void)
{
    reg_t src = ref_src_op(0, 0, 0, 0);
    reg_t dst = ref_dst_op_to_reg(0, 1, 0);
    int width;

    if (mode == MODE_R16_RM8 || mode == MODE_R32_RM8)
	width = 8;
    else if (mode == MODE_R32_RM16)
	width = 16;
    else
	assert(0);

    emit(COMPOSE_SLWI(dst, src, 32 - width));
    emit(COMPOSE_SRAWI(dst, dst, 32 - width));

    dispose_integer_reg(src);
    commit_and_dispose_dst_op(dst, 0, 0, 0);
}

static void
handle_movzx_insn (void)
{
    handle_mov_insn();
}

static void
gen_mulhwu (reg_t d, reg_t a, reg_t b)
{
    emit(COMPOSE_MULHWU(d, a, b));
}

static void
handle_mul_insn (void)
{
    gen_mul64(&gen_mulhwu, 1);
}

static void
handle_neg_insn (void)
{
    reg_t dst = ref_dst_op_to_reg(1, 1, 0);

    if (KILL_CF)
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_ADDI(tmp, 0, 0xffff)); /* tmp = 0xffffffff */
	emit(COMPOSE_ADDC(tmp, tmp, dst));

	free_tmp_integer_reg(tmp);
    }

    if (KILL_OF && KILL_SZF)
	emit(COMPOSE_NEGOD(dst, dst));
    else if (KILL_OF)
	emit(COMPOSE_NEGO(dst, dst));
    else if (KILL_SZF)
	emit(COMPOSE_NEGD(dst, dst));
    else
	emit(COMPOSE_NEG(dst, dst));

    commit_and_dispose_dst_op(dst, 1, 0, 0);
}

static void
handle_not_insn (void)
{
    reg_t dst = ref_dst_op_to_reg(1, 1, 0);

    emit(COMPOSE_NOR(dst, dst, dst));

    commit_and_dispose_dst_op(dst, 1, 0, 0);
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
    reg_t sp = ref_i386_gpr_rw(REG_ESP);

    if (mode == MODE_PR32)
    {
	if (opcode_reg == REG_ESP)
	    emit(COMPOSE_LWBRX(sp, 0, sp));
	else
	{
	    reg_t dst = ref_i386_gpr_w(opcode_reg);

	    emit(COMPOSE_LWBRX(dst, 0, sp));
	    emit(COMPOSE_ADDI(sp, sp, 4));

	    unref_integer_reg(dst);
	}
    }
    else
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_LWBRX(tmp, 0, sp));
	emit(COMPOSE_ADDI(sp, sp, 4));

	commit_and_dispose_dst_op(tmp, 0, 0, 1);
    }

    unref_integer_reg(sp);
}

static void
handle_push_insn (void)
{
    reg_t sp = ref_i386_gpr_rw(REG_ESP);
    reg_t dst = ref_dst_op_to_reg(1, 0, 0);

    if ((mode == MODE_PR32 && opcode_reg == REG_ESP)
	|| (mode == MODE_RM32 && mod == 3 && rm == REG_ESP))
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_LI(tmp, (word_16)-4));
	emit(COMPOSE_STWBRX(dst, sp, tmp));

	free_tmp_integer_reg(tmp);

	emit(COMPOSE_ADDI(sp, sp, (word_16)-4));
    }
    else
    {
	emit(COMPOSE_ADDI(sp, sp, (word_16)-4));
	emit(COMPOSE_STWBRX(dst, 0, sp));
    }

    dispose_integer_reg(dst);
    dispose_integer_reg(sp);
}

static void
gen_asm_func (int num)
{
    reg_t tmp = alloc_tmp_integer_reg();

    emit(COMPOSE_LWZ(tmp, num * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_MTLR(tmp));

    free_tmp_integer_reg(tmp);

    emit(COMPOSE_BLRL());
}

static void
handle_repe_cmpsb_insn (void)
{
    gen_asm_func(REPE_CMPSB_CONST);
}

static void
handle_rep_movsb_insn (void)
{
    gen_asm_func(REP_MOVSB_CONST);
}

static void
handle_rep_movsd_insn (void)
{
    gen_asm_func(REP_MOVSD_CONST);
}

static void
handle_repne_scasb_insn (void)
{
    gen_asm_func(REPNE_SCASB_CONST);
}

static void
handle_rep_stosb_insn (void)
{
    gen_asm_func(REP_STOSB_CONST);
}

static void
handle_rep_stosd_insn (void)
{
    gen_asm_func(REP_STOSD_CONST);
}

static void
handle_ret_insn (void)
{
    reg_t esp = ref_i386_gpr_rw(REG_ESP);
    word_32 offset;

    emit(COMPOSE_LWBRX(JUMP_TARGET_REG, 0, esp));

    if (mode == MODE_IMM16)
	offset = imm16 + 4;
    else
	offset = 4;

    assert((offset & ~0x7fff) == 0);

    emit(COMPOSE_ADDI(esp, esp, offset));

    unref_integer_reg(esp);

    emit_indirect_jump();
}

static void
gen_shift_insn (void (*gen_id) (reg_t, reg_t, word_32),
		void (*gen_i) (reg_t, reg_t, word_32),
		void (*gen_d) (reg_t, reg_t, reg_t),
		void (*gen) (reg_t, reg_t, reg_t))
{
    word_32 imm;
    reg_t src = ref_src_op(0, &imm, 8, 0);
    reg_t dst = ref_dst_op_to_reg(1, 1, 1);

    if (src == NO_REG)
    {
	if (KILL_SZF && op_width == 32)
	    gen_id(dst, dst, imm & 0x1f);
	else
	    gen_i(dst, dst, imm & 0x1f);
    }
    else
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_ANDID(tmp, src, 0x1f));
	if (KILL_SZF && op_width == 32)
	    gen_d(dst, dst, tmp);
	else
	    gen(dst, dst, tmp);

	free_tmp_integer_reg(tmp);
	dispose_integer_reg(src);
    }

    /* this can be implemented faster if !KILL_SF, namely
       by using the dot form of the shift above.  */
    if (op_width != 32 && KILL_SZF)
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_RLWINMD(tmp, dst, 32 - op_width, 0, 31));

	free_tmp_integer_reg(tmp);
    }

    commit_and_dispose_dst_op(dst, 1, 1, 0);
}

static void
gen_srawid (reg_t dst, reg_t src, word_32 n)
{
    emit(COMPOSE_SRAWID(dst, src, n));
}

static void
gen_srawi (reg_t dst, reg_t src, word_32 n)
{
    emit(COMPOSE_SRAWI(dst, src, n));
}

static void
gen_srawd (reg_t dst, reg_t src, reg_t n)
{
    emit(COMPOSE_SRAWD(dst, src, n));
}

static void
gen_sraw (reg_t dst, reg_t src, reg_t n)
{
    emit(COMPOSE_SRAW(dst, src, n));
}

static void
handle_sar_insn (void)
{
    gen_shift_insn(&gen_srawid, &gen_srawi, &gen_srawd, &gen_sraw);
}

static void
gen_set_insn (void (*gen) (label_t))
{
    reg_t tmp = alloc_tmp_integer_reg();
    label_t label = alloc_label();

    emit(COMPOSE_LI(tmp, 0));
    gen(label);
    emit(COMPOSE_LI(tmp, 1));
    emit_label(label);
    free_label(label);

    commit_and_dispose_dst_op(tmp, 0, 0, 1);
}

static void
handle_sete_insn (void)
{
    gen_set_insn(&gen_bne_2);
}

static void
handle_setg_insn (void)
{
    gen_set_insn(&gen_bnor_zf_ne_sf_of);
}

static void
handle_setl_insn (void)
{
    gen_set_insn(&gen_beq_lt_ov);
}

static void
handle_setle_insn (void)
{
    gen_set_insn(&gen_bor_zf_ne_sf_of);
}

static void
handle_setne_insn (void)
{
    gen_set_insn(&gen_beq_2);
}

static void
gen_slwid (reg_t dst, reg_t src, word_32 n)
{
    emit(COMPOSE_SLWID(dst, src, n));
}

static void
gen_slwi (reg_t dst, reg_t src, word_32 n)
{
    emit(COMPOSE_SLWI(dst, src, n));
}

static void
gen_slwd (reg_t dst, reg_t src, reg_t n)
{
    emit(COMPOSE_SLWD(dst, src, n));
}

static void
gen_slw (reg_t dst, reg_t src, reg_t n)
{
    emit(COMPOSE_SLW(dst, src, n));
}

static void
handle_shl_insn (void)
{
    gen_shift_insn(&gen_slwid, &gen_slwi, &gen_slwd, &gen_slw);
}

static void
handle_shld_insn (void)
{
    gen_interpreter_handle();
}

static void
gen_srwid (reg_t dst, reg_t src, word_32 n)
{
    emit(COMPOSE_SRWID(dst, src, n));
}

static void
gen_srwi (reg_t dst, reg_t src, word_32 n)
{
    emit(COMPOSE_SRWI(dst, src, n));
}

static void
gen_srwd (reg_t dst, reg_t src, reg_t n)
{
    emit(COMPOSE_SRWD(dst, src, n));
}

static void
gen_srw (reg_t dst, reg_t src, reg_t n)
{
    emit(COMPOSE_SRW(dst, src, n));
}

static void
handle_shr_insn (void)
{
    gen_shift_insn(&gen_srwid, &gen_srwi, &gen_srwd, &gen_srw);
}

static void
handle_shrd_insn (void)
{
    gen_interpreter_handle();
}

static void
handle_sub_insn (void)
{
    word_32 imm;
    reg_t src = ref_src_op(0, KILL_OF && KILL_SZF ? 0 : &imm, 15, 1); /* we could actually do more than 15 bits */
    reg_t dst = ref_dst_op_to_reg(1, 1, 0);

    assert(op_width == 32);

    if (src == NO_REG)
	imm = SEX16(imm, 15);

    if (KILL_CF)
    {
	reg_t tmp = alloc_tmp_integer_reg();

	emit(COMPOSE_NOR(tmp, dst, dst));
	if (src == NO_REG)
	    emit(COMPOSE_ADDIC(tmp, tmp, imm));
	else
	    emit(COMPOSE_ADDC(tmp, tmp, src));

	free_tmp_integer_reg(tmp);
    }

    if (src == NO_REG)
	imm = -SEX32(imm, 16) & 0xffff;

    if (src == NO_REG)
    {
	assert(!KILL_SZF && !KILL_OF);

	emit(COMPOSE_ADDI(dst, dst, imm));
    }
    else
    {
	if (KILL_OF && KILL_SZF)
	    emit(COMPOSE_SUBFOD(dst, src, dst));
	else if (KILL_OF)
	    emit(COMPOSE_SUBFO(dst, src, dst));
	else if (KILL_SZF)
	    emit(COMPOSE_SUBFD(dst, src, dst));
	else
	    emit(COMPOSE_SUBF(dst, src, dst));

	dispose_integer_reg(src);
    }

    commit_and_dispose_dst_op(dst, 1, 0, 0);
}

static void
handle_test_insn (void)
{
    /* we only load the operands if we have to set the flags.  this is
       not correct if loading the operands would cause a trap.  */
    if (KILL_SZF)
    {
	word_32 imm;
	reg_t src = ref_src_op(1, &imm, 16, 0);
	reg_t dst = ref_dst_op_to_reg(1, 0, 1);
	reg_t tmp = alloc_tmp_integer_reg();

	if (src == NO_REG)
	    emit(COMPOSE_ANDID(tmp, dst, imm));
	else
	{
	    if (!KILL_SZF || op_width != 32)
		emit(COMPOSE_AND(tmp, dst, src));
	    else
		emit(COMPOSE_ANDD(tmp, dst, src));
	    dispose_integer_reg(src);

	    if (op_width != 32)
		emit(COMPOSE_SLWID(tmp, tmp, 32 - op_width));
	}

	dispose_integer_reg(dst);
	free_tmp_integer_reg(tmp);
    }
    if (KILL_CF || KILL_OF)
	clear_of_cf();
}

static void
handle_xchg_insn (void)
{
    reg_t src, dst, tmp;

    if ((mode == MODE_PR16_AX || mode == MODE_PR32_EAX) && opcode_reg == 0)
	return;			/* nop */

    dst = ref_dst_op_to_reg(1, 1, 0);

    if (mode == MODE_PR16_AX || mode == MODE_PR32_EAX)
	src = ref_i386_gpr_rw(REG_EAX);
    else if (mode == MODE_RM8_R8 && reg >= 4)
	src = ref_i386_gpr_rw(reg - 4);
    else
	src = ref_i386_gpr_rw(reg);

    tmp = alloc_tmp_integer_reg();

    emit(COMPOSE_MR(tmp, dst));
    emit(COMPOSE_MR(dst, src));

    if (op_width == 32)
	emit(COMPOSE_MR(src, tmp));
    else if (op_width == 16)
	emit(COMPOSE_INSRWI(src, tmp, 16, 16));
    else
    {
	assert(op_width == 8);

	if (reg < 4)
	    emit(COMPOSE_INSRWI(src, tmp, 8, 24));
	else
	    emit(COMPOSE_INSRWI(src, tmp, 8, 16));
    }

    commit_and_dispose_dst_op(dst, 1, 0, 0);
    dispose_integer_reg(src);
    free_tmp_integer_reg(tmp);
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

#define SKELETON_FUNC_NAME    compile_to_ppc_i386_insn
#define SKELETON_FUNC_ARGS    , word_32 _flags_killed
#ifdef COLLECT_STATS
#define SKELETON_PRE_DECODE   ++num_translated_insns; flags_killed = _flags_killed; this_pc = intp->pc;
#else
#define SKELETON_PRE_DECODE   flags_killed = _flags_killed; this_pc = intp->pc;
#endif

#include "i386_skeleton.c"
