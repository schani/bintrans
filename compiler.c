/*
 * compiler.c
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

#include <assert.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

#include "bintrans.h"

/* #define NO_REGISTER_CACHING */
/* #define NO_PATCHING */

#ifdef NEED_COMPILER
#include "compiler.h"
#include "fragment_hash.h"

#include "alpha_composer.h"
#include "alpha_disassembler.c"

#define MAX_LABELS               8
#define MAX_BACKPATCHES          8
#define MAX_CONSTANTS        16384

#define MAX_UNRESOLVED_JUMPS 16384 /* should be a lot less if we actually do resolve branches */

#define JUMP_TARGET_REG        16
#define RETURN_ADDR_REG        26
#define CONSTANT_AREA_REG      27
#define PROCEDURE_VALUE_REG    27

#define NUM_REG_CONSTANTS          (NUM_EMU_REGISTERS * 2 + 2) /* registers + scratch area */

#define DIRECT_DISPATCHER_CONST    (NUM_REG_CONSTANTS + 0)
#define INDIRECT_DISPATCHER_CONST  (NUM_REG_CONSTANTS + 2)
#define SYSTEM_CALL_CONST          (NUM_REG_CONSTANTS + 4)
#define ISYNC_CONST                (NUM_REG_CONSTANTS + 6)
#define C_STUB_CONST               (NUM_REG_CONSTANTS + 8)
#define LEADING_ZEROS_CONST        (NUM_REG_CONSTANTS + 10)
#define DIV_UNSIGNED_64_CONST      (NUM_REG_CONSTANTS + 12)
#define DIV_SIGNED_64_CONST        (NUM_REG_CONSTANTS + 14)
#define MOD_UNSIGNED_64_CONST      (NUM_REG_CONSTANTS + 16)
#define MOD_SIGNED_64_CONST        (NUM_REG_CONSTANTS + 18)
#define DIV_UNSIGNED_32_CONST      (NUM_REG_CONSTANTS + 20)
#define DIV_SIGNED_32_CONST        (NUM_REG_CONSTANTS + 22)
#define MOD_UNSIGNED_32_CONST      (NUM_REG_CONSTANTS + 24)
#define MOD_SIGNED_32_CONST        (NUM_REG_CONSTANTS + 26)
#define DIV_UNSIGNED_16_CONST      (NUM_REG_CONSTANTS + 28)
#define DIV_SIGNED_16_CONST        (NUM_REG_CONSTANTS + 30)
#define MOD_UNSIGNED_16_CONST      (NUM_REG_CONSTANTS + 32)
#define MOD_SIGNED_16_CONST        (NUM_REG_CONSTANTS + 34)
#define DIV_UNSIGNED_8_CONST       (NUM_REG_CONSTANTS + 36)
#define DIV_SIGNED_8_CONST         (NUM_REG_CONSTANTS + 38)
#define MOD_UNSIGNED_8_CONST       (NUM_REG_CONSTANTS + 40)
#define MOD_SIGNED_8_CONST         (NUM_REG_CONSTANTS + 42)
#ifdef COUNT_INSNS
#define EMULATED_INSNS_CONST       (NUM_REG_CONSTANTS + 44)
#define NATIVE_INSNS_CONST         (NUM_REG_CONSTANTS + 46)
#define FRAGMENTS_CONST            (NUM_REG_CONSTANTS + 48)
#define PREMATURE_EXITS_CONST      (NUM_REG_CONSTANTS + 50)
#define END_EXITS_CONST            (NUM_REG_CONSTANTS + 52)
#define EMULATED_TRACE_INSNS_CONST (NUM_REG_CONSTANTS + 54)
#endif

typedef struct
{
    int begin;
    int end;
} reg_range_t;

#if defined(EMU_PPC)
reg_range_t integer_reg_range[] = { { 1, 16 }, { 18, 26 }, { 0, 0 } };
#define NUM_INTEGER_REGS                       23
#elif defined(EMU_I386)
reg_range_t integer_reg_range[] = { { 15, 16 }, { 18, 26 }, { 0, 0 } };
#define NUM_INTEGER_REGS                        9
#define FIRST_NATIVE_INTEGER_HOST_REG           1
#define NUM_NATIVE_INTEGER_HOST_REGS           14 /* FIXME: this should be in the machine definition (NUM_INTEGER_EMU_REGISTERS) */
#endif

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
#define FIRST_FLOAT_REG        28
#define NUM_FLOAT_REGS          3
#else
#define FIRST_FLOAT_REG         1
#define NUM_FLOAT_REGS         15
#endif

#define REG_TYPE_INTEGER        1
#define REG_TYPE_FLOAT          2

#define MAX_ALLOC_DEPTH         4

typedef struct
{
    int free;
    int emitted;
    word_64 address;
    int num_backpatches;
    word_64 backpatches[MAX_BACKPATCHES];
} label_info_t;

typedef struct
{
    reg_t native_reg;
    int type;
    int foreign_reg;		/* -1 if allocated to the code generator */
    int free;
    int modified;		/* only relevant for foreign regs */
    int refcount;		/* (refcount == 0 && !free) means native reg holds foreign reg,
				   but is not currently needed by code generator */
    unsigned long timestamp;	/* timestamp of last reference */
} register_alloc_t;

#ifdef COLLECT_STATS
typedef struct
{
    int num_translated;
    int num_generated_insns;
    int num_generated_consts;
} insn_info_t;

insn_info_t insn_infos[NUM_INSNS];

#ifdef COLLECT_PPC_FPR_STATS
unsigned int fpr_uses[32];
#endif
#endif

label_info_t label_infos[MAX_LABELS];
word_32 code_area[MAX_CODE_INSNS];
word_32 *emit_loc;
word_64 fragment_start;

int num_constants = NUM_EMU_REGISTERS * 2 + 2;
int num_constants_init;
word_32 constant_area[NUM_EMU_REGISTERS * 2 + 2 + MAX_CONSTANTS];

word_64 unresolved_jump_addrs[MAX_UNRESOLVED_JUMPS];
word_32 unresolved_jump_targets[MAX_UNRESOLVED_JUMPS];

register_alloc_t integer_regs[MAX_ALLOC_DEPTH][NUM_INTEGER_REGS];
register_alloc_t float_regs[MAX_ALLOC_DEPTH][NUM_FLOAT_REGS];
int alloc_sp = 0;
unsigned long register_timestamp = 0;

int have_jumped = 0;
int generated_insn_index;

interpreter_t *compiler_intp = 0;
#ifdef CROSSDEBUGGER
interpreter_t *debugger_intp = 0;
#endif

#ifdef COLLECT_STATS
#ifdef COMPILER_THRESHOLD
unsigned long num_patched_direct_jumps = 0;
#else
unsigned long num_direct_jumps = 0;
#endif
unsigned long num_direct_and_indirect_jumps = 0;
unsigned long num_translated_blocks = 0;
unsigned long num_translated_traces = 0;
unsigned long num_translated_insns = 0;
unsigned long num_translated_trace_insns = 0;
unsigned long num_load_store_reg_insns = 0;
unsigned long num_stub_calls = 0;
unsigned long num_loop_profiler_calls = 0;
unsigned long num_const_adds = 0;
#endif

void (*branch_profile_func) (void) = 0;

#ifdef MEASURE_TIME
long compiler_time = 0;
struct timeval measure_start;

void
start_timer (void)
{
    gettimeofday(&measure_start, 0);
}

void
stop_timer (void)
{
    struct timeval stop;

    gettimeofday(&stop, 0);

    compiler_time += (stop.tv_sec - measure_start.tv_sec) * 1000000 + stop.tv_usec - measure_start.tv_usec;
}
#else
#define start_timer()
#define stop_timer()
#endif

void
emit (word_32 insn)
{
    assert(emit_loc < code_area + MAX_CODE_INSNS);
    *emit_loc++ = insn;
}

void
load_reg (register_alloc_t *reg)
{
#ifdef COLLECT_STATS
    ++num_load_store_reg_insns;
#endif

    switch (reg->type)
    {
	case REG_TYPE_INTEGER :
	    emit(COMPOSE_LDL(reg->native_reg, reg->foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	case REG_TYPE_FLOAT :
	    emit(COMPOSE_LDT(reg->native_reg, reg->foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	default :
	    assert(0);
    }
}

void
store_reg (register_alloc_t *reg)
{
    assert(reg->modified);

#ifdef COLLECT_STATS
    ++num_load_store_reg_insns;
#endif

    switch (reg->type)
    {
	case REG_TYPE_INTEGER :
	    emit(COMPOSE_STL(reg->native_reg, reg->foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	case REG_TYPE_FLOAT :
	    emit(COMPOSE_STT(reg->native_reg, reg->foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	default:
	    assert(0);
    }

    reg->modified = 0;
}

void
store_and_free_reg (register_alloc_t *reg)
{
    assert(!reg->free && reg->refcount == 0 && reg->foreign_reg != -1);

    if (reg->modified)
	store_reg(reg);
    reg->free = 1;
}

int
free_some_reg (register_alloc_t *regs, int num_regs)
{
    int i, index = -1;

    for (i = 0; i < num_regs; ++i)
	if (regs[i].refcount == 0 && !regs[i].free && regs[i].foreign_reg != -1)
	    if (index == -1 || regs[index].timestamp > regs[i].timestamp)
		index = i;

    assert(index != -1);

    store_and_free_reg(&regs[index]);

    return index;
}

void
store_and_free_all_foreign_regs (void)
{
    int i;

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
	if (!integer_regs[alloc_sp][i].free && integer_regs[alloc_sp][i].foreign_reg != -1)
	    store_and_free_reg(&integer_regs[alloc_sp][i]);

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
	if (!float_regs[alloc_sp][i].free && float_regs[alloc_sp][i].foreign_reg != -1)
	    store_and_free_reg(&float_regs[alloc_sp][i]);
}

void
store_all_foreign_regs (void)
{
    int i;

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
	if (!integer_regs[alloc_sp][i].free && integer_regs[alloc_sp][i].foreign_reg != -1 && integer_regs[alloc_sp][i].modified)
	    store_reg(&integer_regs[alloc_sp][i]);

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
	if (!float_regs[alloc_sp][i].free && float_regs[alloc_sp][i].foreign_reg != -1 && float_regs[alloc_sp][i].modified)
	    store_reg(&float_regs[alloc_sp][i]);
}

void
push_alloc (void)
{
    int i;

    ++alloc_sp;
    assert(alloc_sp < MAX_ALLOC_DEPTH);

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
	integer_regs[alloc_sp][i] = integer_regs[alloc_sp - 1][i];
    for (i = 0; i < NUM_FLOAT_REGS; ++i)
	float_regs[alloc_sp][i] = float_regs[alloc_sp - 1][i];
}

void
revert_reg_state (register_alloc_t *reg, register_alloc_t *old_reg)
{
    if (reg->free)
    {
	if (!old_reg->free)
	{
	    assert(old_reg->foreign_reg != -1);
	    load_reg(old_reg);
	}
    }
    else
    {
	if (reg->foreign_reg == -1)
	{
	    assert(!old_reg->free);
	    assert(old_reg->foreign_reg == -1);
	    assert(reg->refcount == old_reg->refcount);
	    assert(reg->modified == old_reg->modified);
	}
	else
	{
	    if (!old_reg->free)
	    {
		if (reg->foreign_reg == old_reg->foreign_reg)
		{
		    if (reg->modified && !old_reg->modified)
			store_reg(reg);
		}
		else
		{
		    if (reg->modified)
			store_reg(reg);
		    load_reg(old_reg);
		}
	    }
	    else
	    {
		if (reg->modified)
		    store_reg(reg);
	    }
	}
    }
}

void
pop_alloc (void)
{
    int i;

    assert(alloc_sp > 0);

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
	revert_reg_state(&integer_regs[alloc_sp][i], &integer_regs[alloc_sp - 1][i]);

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
	revert_reg_state(&float_regs[alloc_sp][i], &float_regs[alloc_sp - 1][i]);

    --alloc_sp;
}

register_alloc_t*
ref_reg (register_alloc_t *regs, int num_regs, int foreign_reg, int modify, int *newly_allocated)
{
    int free_reg = -1;
    int index = -1;
    int i;

    for (i = 0; i < num_regs; ++i)
    {
	if (regs[i].free)
	{
	    free_reg = i;
	    if (foreign_reg == -1) /* reg for the code generator */
		break;
	}
	else if (regs[i].foreign_reg == foreign_reg)
	{
	    index = i;
	    if (foreign_reg != -1)
		break;
	}
    }

    if (foreign_reg == -1)
    {
	if (free_reg == -1)
	    index = free_some_reg(regs, num_regs);
	else
	    index = free_reg;
    }
    else
	if (index == -1)
	{
	    if (free_reg != -1)
		index = free_reg;
	    else
		index = free_some_reg(regs, num_regs);
	}

    *newly_allocated = regs[index].free;

    regs[index].foreign_reg = foreign_reg;
    regs[index].free = 0;
    regs[index].modified = regs[index].modified || modify;
    ++regs[index].refcount;
    regs[index].timestamp = register_timestamp++;

    return &regs[index];
}

void
unref_reg (register_alloc_t *regs, int num_regs, reg_t reg)
{
    int i;

    for (i = 0; i < num_regs; ++i)
	if (regs[i].native_reg == reg)
	    break;

    assert(i < num_regs);

    assert(regs[i].refcount > 0);

    if (--regs[i].refcount == 0 && regs[i].foreign_reg == -1)
	regs[i].free = 1;
}

reg_t
ref_integer_reg (int foreign_reg, int reading, int writing)
{
    int newly_allocated;
    register_alloc_t *reg;

    if (foreign_reg >= 0 && (foreign_reg & NEED_NATIVE))
    {
	assert(!reading);
	return foreign_reg & ~NEED_NATIVE;
    }

    if (reading)
	assert(foreign_reg != -1);

#ifdef EMU_I386
    if (foreign_reg >= 0)
    {
	assert(foreign_reg < NUM_NATIVE_INTEGER_HOST_REGS);
	return foreign_reg + FIRST_NATIVE_INTEGER_HOST_REG;
    }
#endif

    reg = ref_reg(integer_regs[alloc_sp], NUM_INTEGER_REGS, foreign_reg, writing, &newly_allocated);
    if (reading && newly_allocated)
	load_reg(reg);
    return reg->native_reg;
}

void
unref_integer_reg (reg_t reg)
{
#ifdef EMU_I386
    if (reg >= FIRST_NATIVE_INTEGER_HOST_REG && reg < FIRST_NATIVE_INTEGER_HOST_REG + NUM_NATIVE_INTEGER_HOST_REGS)
	return;
#endif

    unref_reg(integer_regs[alloc_sp], NUM_INTEGER_REGS, reg);
}

reg_t
ref_float_reg (int foreign_reg, int reading, int writing)
{
    int newly_allocated;
    register_alloc_t *reg;

    if (foreign_reg >= 0 && (foreign_reg & NEED_NATIVE))
    {
	assert(!reading);
	return foreign_reg & ~NEED_NATIVE;
    }

    if (reading)
	assert(foreign_reg != -1);

#if defined(COLLECT_STATS) && defined(COLLECT_PPC_FPR_STATS)
    if (foreign_reg >= 0)
    {
	assert(foreign_reg >= 5 + 32 && foreign_reg < 5 + 32 + 32);
	++fpr_uses[foreign_reg - 5 - 32];
    }
#endif

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
    if (foreign_reg >= 0)
    {
	int fpr_num = foreign_reg - 5 - 32;

	assert(foreign_reg >= 5 + 32 && foreign_reg < 5 + 32 + 32);

	if (fpr_num < 14)
	    return fpr_num;
	if (fpr_num >= 18)
	    return fpr_num - 4;
    }
#endif

    reg = ref_reg(float_regs[alloc_sp], NUM_FLOAT_REGS, foreign_reg, writing, &newly_allocated);
    if (reading && newly_allocated)
	load_reg(reg);
    return reg->native_reg;
}

void
unref_float_reg (reg_t reg)
{
#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
    if (reg < 28)
	return;
#endif

    unref_reg(float_regs[alloc_sp], NUM_FLOAT_REGS, reg);
}

void
emit_load_integer_32 (reg_t reg, word_32 val)
{
    if ((val >> 15) == 0 || (val >> 15) == 0x1ffff)
	emit(COMPOSE_LDA(reg, val & 0xffff, 31));
    else if ((val & 0xffff) == 0)
	emit(COMPOSE_LDAH(reg, val >> 16, 31));
    else
    {
#if defined(EMU_PPC)
	assert(num_constants < MAX_CONSTANTS);

	emit(COMPOSE_LDL(reg, num_constants * 4, CONSTANT_AREA_REG));
	constant_area[num_constants++] = val;
#elif defined(EMU_I386)
	emit(COMPOSE_LDA(reg, val & 0xffff, 31));
	if (val & 0x8000)
	    emit(COMPOSE_ZAPNOT_IMM(reg, 3, reg));
	emit(COMPOSE_LDAH(reg, val >> 16, reg));
#endif
    }
}

void
emit_load_integer_64 (reg_t reg, word_64 val)
{
    if ((val >> 31) == 0 || (val >> 31) == 0x1ffffffff)
	emit_load_integer_32(reg, (word_32)val);
#ifdef EMU_I386
    else if ((val >> 32) == 0)
    {
	if ((val >> 15) == 0 || (val >> 15) == 0x1ffff)
	{
	    emit(COMPOSE_LDA(reg, val & 0xffff, 31));
	    emit(COMPOSE_ZAPNOT_IMM(reg, 15, reg));
	}
	else
	{
	    emit_load_integer_32(reg, (word_32)val);
	    emit(COMPOSE_ZAPNOT_IMM(reg, 15, reg));
	}
    }
#endif
    else
    {
	assert(num_constants + 2 < MAX_CONSTANTS);

	if (num_constants % 2 == 1)
	    constant_area[num_constants++] = 0xdeadbeef;

	emit(COMPOSE_LDQ(reg, num_constants * 4, CONSTANT_AREA_REG));
	constant_area[num_constants++] = val & 0xffffffff;
	constant_area[num_constants++] = val >> 32;

	/*
	printf("  load $%u,0x%lx\n", reg, val);
	assert(0);
	*/
    }
}

label_t
alloc_label (void)
{
    label_t i;

    for (i = 0; i < MAX_LABELS; ++i)
	if (label_infos[i].free)
	    break;
    assert(i < MAX_LABELS);

    label_infos[i].free = 0;
    label_infos[i].emitted = 0;
    label_infos[i].num_backpatches = 0;

    return i;
}

void
free_label (label_t label)
{
    int i;
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->free);
    assert(l->emitted);

    for (i = 0; i < l->num_backpatches; ++i)
    {
	sword_64 disp = l->address - l->backpatches[i] - 4;
	word_32 field;

	assert(disp >= -(1 << 22) && disp < (1 << 22));
	field = (disp >> 2) & 0x1fffff;

	*(word_32*)(l->backpatches[i]) |= field;
    }

    label_infos[label].free = 1;
}

void
emit_label (label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->free);
    assert(!l->emitted);

    l->address = (word_64)emit_loc;
    l->emitted = 1;

    /* printf("label %d:\n", label); */
}

void
emit_branch (word_32 insn, label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->free);

    assert(l->num_backpatches < MAX_BACKPATCHES);
    l->backpatches[l->num_backpatches++] = (word_64)emit_loc;

    emit(insn);
}

void
emit_direct_jump (word_32 target)
{
    int i;

    if (branch_profile_func != 0)
	branch_profile_func();

    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, DIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	if (unresolved_jump_addrs[i] == 0)
	    break;

    assert(i < MAX_UNRESOLVED_JUMPS);

    unresolved_jump_addrs[i] = (word_64)emit_loc;
    unresolved_jump_targets[i] = target;

    have_jumped = 1;
}

void
emit_indirect_jump (void)
{
    if (branch_profile_func != 0)
	branch_profile_func();

    /* we assume that the target address is already in $7 */

    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, INDIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(31, PROCEDURE_VALUE_REG));

    have_jumped = 1;
}

void
emit_isync (void)
{
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, ISYNC_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(31, PROCEDURE_VALUE_REG));

    have_jumped = 1;
}

void
emit_system_call (void)
{
    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, SYSTEM_CALL_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));
}

void
finish_fragment (void)
{
    int i;

    /*
    for (i = 0; i < num_constants; ++i)
    {
	word_64 disp;

	*emit_loc = constants[i];
	disp = (word_64)emit_loc - fragment_start;
	assert(disp < 32768);

	*constant_loads[i] |= disp & 0xffff;

	++emit_loc;
    }

    num_constants = 0;
    */

    assert(alloc_sp == 0);

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
    {
	if (!integer_regs[0][i].free)
	{
	    assert(integer_regs[0][i].foreign_reg != -1);
	    assert(!integer_regs[0][i].modified);
	    assert(integer_regs[0][i].refcount == 0);

	    integer_regs[0][i].free = 1;
	}
    }

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
    {
	if (!float_regs[0][i].free)
	{
	    assert(float_regs[0][i].foreign_reg != -1);
	    assert(!float_regs[0][i].modified);
	    assert(float_regs[0][i].refcount == 0);

	    float_regs[0][i].free = 1;
	}
    }

    fragment_start = (word_64)emit_loc;
}

void
emit_store_mem_8 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));
#endif
    emit(COMPOSE_STB(value_reg, 0, addr_reg));
}

void
emit_store_mem_64 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    reg_t tmp_reg;

    emit(COMPOSE_STL(value_reg, 4, addr_reg));
    tmp_reg = ref_integer_reg_for_writing(-1);
    emit(COMPOSE_SRL_IMM(value_reg, 32, tmp_reg));
    emit(COMPOSE_STL(tmp_reg, 0, addr_reg));
    unref_integer_reg(tmp_reg);
#else
    emit(COMPOSE_STQ(value_reg, 0, addr_reg));
#endif
}

void
emit_store_mem_16 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));
#endif
    emit(COMPOSE_STW(value_reg, 0, addr_reg));
}

void
emit_load_mem_8 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));
#endif
    emit(COMPOSE_LDBU(value_reg, 0, addr_reg));
}

void
emit_load_mem_16 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));
#endif
    emit(COMPOSE_LDWU(value_reg, 0, addr_reg));
}

void
emit_load_mem_64 (reg_t value_reg, reg_t addr_reg)
{
#ifdef DIFFERENT_BYTEORDER
    reg_t tmp_reg = ref_integer_reg_for_writing(-1);

    emit(COMPOSE_LDL(tmp_reg, 0, addr_reg));
    emit(COMPOSE_SLL_IMM(tmp_reg, 32, tmp_reg));
    emit(COMPOSE_LDL(value_reg, 4, addr_reg));
    emit(COMPOSE_ZAPNOT_IMM(value_reg, 15, value_reg));
    emit(COMPOSE_BIS(value_reg, tmp_reg, value_reg));
    unref_integer_reg(tmp_reg);
#else
    emit(COMPOSE_LDQ(value_reg, 0, addr_reg));
#endif
}

#if defined(EMU_PPC)
#ifndef USE_HAND_TRANSLATOR
#include "ppc_compiler.c"
#else
#include "ppc_to_alpha_compiler.c"
#endif
#elif defined(EMU_I386)
#include "i386.h"
#include "i386_compiler.c"
#endif

void
add_const_64 (word_64 val)
{
    constant_area[num_constants++] = val & 0xffffffff;
    constant_area[num_constants++] = val >> 32;
}

word_64
get_const_64 (int index)
{
    /* FIXME: this is endian dependent */
    return (word_64)constant_area[index] + (((word_64)constant_area[index + 1]) << 32);
}

word_64
lookup_fragment (word_32 addr)
{
    fragment_hash_entry_t *entry = fragment_hash_get(addr);

    if (entry == 0)
	return 0;
    return entry->native_addr;
}

#ifndef COMPILER_THRESHOLD
void
enter_fragment (word_32 foreign_addr, word_64 native_addr)
{
    fragment_hash_entry_t entry;
#ifdef PROFILE_LOOPS
    int i;
#endif

#ifdef PERIODIC_STAT_DUMP
    if (num_translated_blocks % 100 == 0)
	print_compiler_stats();
#endif

    init_fragment_hash_entry(&entry);
    entry.native_addr = native_addr;
    fragment_hash_put(foreign_addr, &entry);
}
#endif

word_64
div_unsigned_64 (word_64 a, word_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_64
div_signed_64 (sword_64 a, sword_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_64
mod_unsigned_64 (word_64 a, word_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_64
mod_signed_64 (sword_64 a, sword_64 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_32
div_unsigned_32 (word_32 a, word_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_32
div_signed_32 (sword_32 a, sword_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_32
mod_unsigned_32 (word_32 a, word_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_32
mod_signed_32 (sword_32 a, sword_32 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_16
div_unsigned_16 (word_16 a, word_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_16
div_signed_16 (sword_16 a, sword_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_16
mod_unsigned_16 (word_16 a, word_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_16
mod_signed_16 (sword_16 a, sword_16 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_8
div_unsigned_8 (word_8 a, word_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

sword_8
div_signed_8 (sword_8 a, sword_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a / b;
}

word_8
mod_unsigned_8 (word_8 a, word_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

sword_8
mod_signed_8 (sword_8 a, sword_8 b)
{
#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif
    return a % b;
}

word_32
count_leading_zeros (word_32 w)
{
    word_32 m = 0x80000000;
    word_32 i;

#ifdef COLLECT_STATS
    ++num_stub_calls;
#endif

    for (i = 0; i < 32; ++i)
    {
	if (w & m)
	    break;
	m >>= 1;
    }

    return i;
}

void
init_compiler (interpreter_t *intp, interpreter_t *dbg_intp)
{
    int i, j, k;

    compiler_intp = intp;
#ifdef CROSSDEBUGGER
    debugger_intp = dbg_intp;
#endif

    i = 0;
    for (j = 0; integer_reg_range[j].end > 0; ++j)
	for (k = integer_reg_range[j].begin; k < integer_reg_range[j].end; ++k)
	{
	    integer_regs[0][i].native_reg = k;
	    integer_regs[0][i].type = REG_TYPE_INTEGER;
	    integer_regs[0][i].free = 1;
	    integer_regs[0][i].modified = 0;
	    integer_regs[0][i].refcount = 0;
	    ++i;
	}

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
    {
	float_regs[0][i].native_reg = FIRST_FLOAT_REG + i;
	float_regs[0][i].type = REG_TYPE_FLOAT;
	float_regs[0][i].free = 1;
	float_regs[0][i].modified = 0;
	float_regs[0][i].refcount = 0;
    }

    for (i = 0; i < MAX_LABELS; ++i)
	label_infos[i].free = 1;

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	unresolved_jump_addrs[i] = 0;

    emit_loc = code_area;
    fragment_start = (word_64)emit_loc;

    add_const_64((word_64)direct_dispatcher);
    add_const_64((word_64)indirect_dispatcher);
    add_const_64((word_64)system_call_entry);
    add_const_64((word_64)isync_entry);
    add_const_64((word_64)c_stub);
    add_const_64((word_64)count_leading_zeros);
    add_const_64((word_64)div_unsigned_64);
    add_const_64((word_64)div_signed_64);
    add_const_64((word_64)mod_unsigned_64);
    add_const_64((word_64)mod_signed_64);
    add_const_64((word_64)div_unsigned_32);
    add_const_64((word_64)div_signed_32);
    add_const_64((word_64)mod_unsigned_32);
    add_const_64((word_64)mod_signed_32);
    add_const_64((word_64)div_unsigned_16);
    add_const_64((word_64)div_signed_16);
    add_const_64((word_64)mod_unsigned_16);
    add_const_64((word_64)mod_signed_16);
    add_const_64((word_64)div_unsigned_8);
    add_const_64((word_64)div_signed_8);
    add_const_64((word_64)mod_unsigned_8);
    add_const_64((word_64)mod_signed_8);

#ifdef COUNT_INSNS
    add_const_64(0);		/* emulated insns executed */
    add_const_64(0);		/* native insns executed */
    add_const_64(0);		/* fragments executed */
    add_const_64(0);		/* premature exits */
    add_const_64(0);		/* end exits */
    add_const_64(0);		/* emulated trace insns executed */

    for (i = 0; i < NUM_INSNS; ++i)
    {
	insn_infos[i].num_translated = 0;
	insn_infos[i].num_generated_insns = 0;
	insn_infos[i].num_generated_consts = 0;
    }
#endif

    num_constants_init = num_constants;
}

void
disassemble_alpha_code (word_64 start, word_64 end)
{
    word_64 x;

    for (x = start; x < end; x += 4)
    {
	printf("%016lx  ", x);
	disassemble_alpha_insn(*(word_32*)x, x);
	printf("\n");
    }
}

word_64
compile_until_jump (word_32 *addr, int optimize_taken_jump, label_t taken_jump_label, word_32 *target_addr)
{
    word_64 start = (word_64)emit_loc;
    word_32 insnp = *addr;
#if defined(DUMP_CODE) || defined(COLLECT_STATS)
    word_64 x = start;
#endif
#if defined(EMU_I386) || defined(DUMP_CODE)
    int i;
#endif

#ifdef EMU_I386
    compute_liveness(compiler_intp, insnp);

    i = 0;

    compiler_intp->pc = insnp;
#endif

    have_jumped = 0;

    while (!have_jumped)
    {
#if defined(EMU_PPC)
#ifndef USE_HAND_TRANSLATOR
	compile_ppc_insn(mem_get_32(compiler_intp, insnp), insnp, optimize_taken_jump, taken_jump_label);
#else
	compile_to_alpha_ppc_insn(mem_get_32(compiler_intp, insnp), insnp, optimize_taken_jump, taken_jump_label);
#endif
#elif defined(EMU_I386)
	word_32 insn_addr = compiler_intp->pc;

	compile_i386_insn(compiler_intp, block_insns[i++].flags_killed);
#endif

#ifdef NO_REGISTER_CACHING
	store_and_free_all_foreign_regs();
#endif

#ifdef COLLECT_STATS
	assert(generated_insn_index < NUM_INSNS);
	++insn_infos[generated_insn_index].num_translated;
	insn_infos[generated_insn_index].num_generated_insns += ((word_64)emit_loc - x) / 4;
#endif

#ifdef DUMP_CODE
	printf("++++++++++++++++\n%08x  ", insnp);
#if defined(EMU_PPC)
	disassemble_ppc_insn(mem_get_32(compiler_intp, insnp), insnp);
#elif defined(EMU_I386)
	compiler_intp->pc = insn_addr;
	disassemble_i386_insn(compiler_intp);
	printf("       0x%08x", block_insns[i - 1].flags_killed);
#endif
	printf("\n- - - - - - - - \n");
	disassemble_alpha_code(x, emit_loc);
	x = (word_64)emit_loc;
#endif

#if defined(COLLECT_STATS) || defined(DUMP_CODE)
	x = (word_64)emit_loc;
#endif

#if defined(EMU_PPC)
	insnp += 4;
#elif defined(EMU_I386)
	insnp = compiler_intp->pc;
#endif
    }

#ifdef DUMP_CODE
    printf("++++++++++++++++\nepilogue\n- - - - - - - - \n");
    disassemble_alpha_code(x, (word_64)emit_loc);
    x = (word_64)emit_loc;
#endif

#ifdef EMU_PPC
    if (optimize_taken_jump)
    {
	int num_targets, can_jump_indirectly, can_fall_through;
	word_32 target;

	jump_analyze_ppc_insn(mem_get_32(compiler_intp, insnp - 4), insnp - 4, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	assert(!can_jump_indirectly);
	assert(num_targets == 1);

	*target_addr = target;
    }
#endif

    *addr = insnp;

    return start;
}

#ifdef DUMP_CODE
void
dump_constants (int old_num_constants)
{
    int i;

    for (i = old_num_constants; i < num_constants; ++i)
	printf("%4d    %08x\n", i * 4, constant_area[i]);
}
#endif

void
emit_const_add (int const_index, int amount, word_32 **lda_loc)
{
    assert(amount >= -32768 && amount < 32768);

    emit(COMPOSE_LDQ(16, const_index * 4, CONSTANT_AREA_REG));
    if (lda_loc != 0)
	*lda_loc = emit_loc;
    emit(COMPOSE_LDA(16, amount, 16));
    emit(COMPOSE_STQ(16, const_index * 4, CONSTANT_AREA_REG));

#ifdef COLLECT_STATS
    ++num_const_adds;
#endif
}

word_32
patch_mem_disp (word_32 insn, word_16 val)
{
    return (insn & 0xffff0000) | val;
}

#if defined(COLLECT_STATS) || defined(COUNT_INSNS)
unsigned long old_num_translated_insns;
#endif

word_64
compile_basic_block (word_32 addr, int as_trace)
{
    word_64 native_addr;
#ifdef DUMP_CODE
    int old_num_constants = num_constants;
    word_64 dump_start;
#endif
#ifdef COUNT_INSNS
    word_32 *emulated_lda_insn, *native_lda_insn, *old_emit_loc;

    old_num_translated_insns = num_translated_insns;
#endif

    start_timer();

    while (((emit_loc - code_area) & 3) != 0)
	emit(0);

    native_addr = (word_64)emit_loc;

#ifdef COUNT_INSNS
    emit_const_add(FRAGMENTS_CONST, 1, 0);
    if (as_trace)
    {
	emit_const_add(END_EXITS_CONST, 1, 0);
	emit_const_add(EMULATED_TRACE_INSNS_CONST, 0, &emulated_lda_insn);
    }
    else
	emit_const_add(EMULATED_INSNS_CONST, 0, &emulated_lda_insn);
    emit_const_add(NATIVE_INSNS_CONST, 0, &native_lda_insn);

    old_emit_loc = emit_loc;
#endif

    compile_until_jump(&addr, 0, 0, 0);

#ifdef DUMP_CODE
    dump_start = (word_64)emit_loc;
#endif

    store_all_foreign_regs();
    emit_direct_jump(addr);	/* this is not necessary if the jump at the end of the basic block was unconditional */

#ifdef DUMP_CODE
    disassemble_alpha_code(dump_start, (word_64)emit_loc);
#endif

    finish_fragment();

    flush_icache();

    stop_timer();

#ifdef DUMP_CODE
    dump_constants(old_num_constants);
#endif

#ifdef COLLECT_STATS
    if (as_trace)
	++num_translated_traces;
    else
	++num_translated_blocks;
#endif

#ifdef COUNT_INSNS
    *emulated_lda_insn = patch_mem_disp(*emulated_lda_insn, num_translated_insns - old_num_translated_insns);
    *native_lda_insn = patch_mem_disp(*native_lda_insn, emit_loc - old_emit_loc);
#endif

    return native_addr;
}

void
branch_profile_within_trace (void)
{
#ifdef COUNT_INSNS
    emit_const_add(PREMATURE_EXITS_CONST, 1, 0);
    emit_const_add(EMULATED_TRACE_INSNS_CONST, num_translated_insns - old_num_translated_insns, 0);
#endif
}

void
branch_profile_end_trace (void)
{
#ifdef COUNT_INSNS
    emit_const_add(END_EXITS_CONST, 1, 0);
    emit_const_add(EMULATED_TRACE_INSNS_CONST, num_translated_insns - old_num_translated_insns, 0);
#endif
}

word_64
compile_trace (word_32 addr, int length, int bits)
{
    word_64 start = (word_64)emit_loc;
    int bit;
#ifdef DUMP_CODE
    int old_num_constants = num_constants;
    word_64 dump_start;
#endif

#ifdef COLLECT_STATS
    old_num_translated_insns = num_translated_insns;
#endif

    start_timer();

    branch_profile_func = branch_profile_within_trace;

    for (bit = 1 << (length - 1); bit != 0; bit >>= 1)
    {
	if (bits & bit)
	{
	    label_t taken_jump_label = alloc_label();
	    word_32 target_addr;

	    compile_until_jump(&addr, 1, taken_jump_label, &target_addr);

#ifdef DUMP_CODE
	    dump_start = (word_64)emit_loc;
#endif

	    push_alloc();
	    store_all_foreign_regs();
	    emit_direct_jump(addr);
	    pop_alloc();

	    emit_label(taken_jump_label);
	    free_label(taken_jump_label);

#ifdef DUMP_CODE
	    disassemble_alpha_code(dump_start, (word_64)emit_loc);
#endif

	    addr = target_addr;
	}
	else
	    compile_until_jump(&addr, 0, 0, 0);
    }

    branch_profile_func = branch_profile_end_trace;

    compile_until_jump(&addr, 0, 0, 0);

#ifdef DUMP_CODE
    dump_start = (word_64)emit_loc;
#endif

    store_all_foreign_regs();
    emit_direct_jump(addr);	/* this is not necessary if the jump at the end of the basic block was unconditional */

    branch_profile_func = 0;

    finish_fragment();

    flush_icache();

    stop_timer();

#ifdef DUMP_CODE
    disassemble_alpha_code(dump_start, (word_64)emit_loc);
    dump_constants(old_num_constants);
#endif

#ifdef COLLECT_STATS
    ++num_translated_traces;

    num_translated_trace_insns += num_translated_insns - old_num_translated_insns;
#endif

    return start;
}

#ifdef CROSSDEBUGGER
void
compare_register_sets (void)
{
    int diff = 0;
    int i;

#if defined(EMU_PPC)
    for (i = 0; i < 5; ++i)
	if (compiler_intp->regs_SPR[i] != debugger_intp->regs_SPR[i])
	    diff = 1;
    for (i = 0; i < 32; ++i)
    {
	if (compiler_intp->regs_GPR[i] != debugger_intp->regs_GPR[i])
	    diff = 1;
	if (*(word_64*)&compiler_intp->regs_FPR[i] != *(word_64*)&debugger_intp->regs_FPR[i])
	    diff = 1;
    }
#elif defined(EMU_I386)
    /*
    if (compiler_intp->regs_SPR[0] != debugger_intp->regs_SPR[0])
	diff = 1;
    */
    for (i = 0; i < 8; ++i)
    {
	if (compiler_intp->regs_GPR[i] != debugger_intp->regs_GPR[i])
	    diff = 1;
	if (compiler_intp->regs_FPST[i] != debugger_intp->regs_FPST[i])
	    diff = 1;
    }
    if (compiler_intp->regs_FSPR[0] != debugger_intp->regs_FSPR[0])
	diff = 1;
#endif

    if (diff)
    {
	printf("*** compiler\n");
	dump_registers(compiler_intp);
	printf("*** interpreter\n");
	dump_registers(debugger_intp);
	assert(0);
    }
}
#endif

#ifdef COMPILER_THRESHOLD
word_64
interpret_until_threshold (word_32 addr)
{
    fragment_hash_entry_t *entry;

    move_regs_compiler_to_interpreter(compiler_intp);

    compiler_intp->pc = addr;

    for (;;)
    {
	entry = fragment_hash_get(compiler_intp->pc);
	if (entry == 0)
	{
	    fragment_hash_entry_t new;

	    init_fragment_hash_entry(&new);
	    new.native_addr = 0;
	    new.times_executed = 1;
	    fragment_hash_put(compiler_intp->pc, &new);
	}
	else
	{
	    if (entry->native_addr != 0)
	    {
		move_regs_interpreter_to_compiler(compiler_intp);
		return entry->native_addr;
	    }

	    ++entry->times_executed;
	    if (entry->times_executed > COMPILER_THRESHOLD)
	    {
		entry->native_addr = compile_basic_block(compiler_intp->pc, 0);
		move_regs_interpreter_to_compiler(compiler_intp);
		return entry->native_addr;
	    }
	}

	/* printf("interpreting from 0x%08x\n", compiler_intp->pc); */

	compiler_intp->have_jumped = 0;
	while (!compiler_intp->have_jumped)
	    interpret_insn(compiler_intp);

	/* printf("jumping to 0x%08x\n", compiler_intp->pc); */
    }
}
#endif

word_64
provide_fragment (word_32 addr)
{
    word_64 native_addr = lookup_fragment(addr);

    /*
    move_regs_compiler_to_interpreter(compiler_intp);
    dump_registers(compiler_intp);
    */

#ifdef COLLECT_STATS
    ++num_direct_and_indirect_jumps;
#endif

#ifdef CROSSDEBUGGER
    printf("*** jumping to %08x\n", addr);
    debugger_intp->have_jumped = 0;
    while (!debugger_intp->have_jumped)
	interpret_insn(debugger_intp);
    move_regs_compiler_to_interpreter(compiler_intp);
    compare_register_sets();
    assert(debugger_intp->pc == addr);
#endif

    if (native_addr != 0)
	return native_addr;

#ifdef COMPILER_THRESHOLD
#ifdef PROFILE_LOOPS
    return loop_profiler(compiler_intp, addr);
#else
    return interpret_until_threshold(addr);
#endif
#else
    native_addr = compile_basic_block(addr, 0);
    enter_fragment(addr, native_addr);

    return native_addr;
#endif
}

word_64
provide_fragment_and_patch (word_64 jump_addr)
{
    int i;
    word_64 native_addr;
    sword_64 disp;
    word_32 field;
    word_32 foreign_addr;

#if defined(COLLECT_STATS) && !defined(COMPILER_THRESHOLD)
    ++num_direct_jumps;
#endif

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	if (unresolved_jump_addrs[i] == jump_addr)
	    break;

    assert(i < MAX_UNRESOLVED_JUMPS);

    foreign_addr = unresolved_jump_targets[i];

#ifdef COMPILER_THRESHOLD
    native_addr = lookup_fragment(foreign_addr);

    if (native_addr == 0)
#ifdef PROFILE_LOOPS
	return loop_profiler(compiler_intp, foreign_addr);
#else
	return interpret_until_threshold(foreign_addr);
#endif
    else
#endif
    {
#if defined(COLLECT_STATS) && defined(COMPILER_THRESHOLD)
	++num_patched_direct_jumps;
#endif

#ifndef COMPILER_THRESHOLD
	native_addr = provide_fragment(foreign_addr);
#endif

#if !defined(CROSSDEBUGGER) && !defined(NO_PATCHING)
	/* printf("patching at %lx\n", jump_addr); */

	jump_addr -= 8;

	disp = native_addr - jump_addr - 4;
	assert(disp >= -(1 << 22) && disp < (1 << 22));
	field = (disp >> 2) & 0x1fffff;

	*(word_32*)jump_addr = COMPOSE_BR(31, field);

	unresolved_jump_addrs[i] = 0;

	flush_icache();
#endif

	return native_addr;
    }
}

word_64
compile_and_return_first_native_addr (word_32 addr, int regs_in_compiler)
{
    word_64 native_addr;

    if (!regs_in_compiler)
	move_regs_interpreter_to_compiler(compiler_intp);

#ifdef COMPILER_THRESHOLD
#ifdef PROFILE_LOOPS
    native_addr = loop_profiler(compiler_intp, addr);
#else
    native_addr = interpret_until_threshold(addr);
#endif
#else
    native_addr = compile_basic_block(addr, 0);
#endif

    return native_addr;
}

void
start_compiler (word_32 addr)
{
    start_execution(compile_and_return_first_native_addr(addr, 0)); /* this call never returns */
}

word_64
isync_handler (word_32 addr)
{
    int i;

    printf("isync!!!!!!!!!!!!!!!!\n");

    num_constants = num_constants_init;
    emit_loc = code_area;

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	unresolved_jump_addrs[i] = 0;

    init_fragment_hash();

    return compile_and_return_first_native_addr(addr, 1);
}

void
print_compiler_stats (void)
{
#ifdef COLLECT_STATS
    int i;

#ifdef COMPILER_THRESHOLD
    printf("patched direct jumps:          %lu\n", num_patched_direct_jumps);
    printf("indirect jumps:                %lu\n", num_direct_and_indirect_jumps);
#else
    printf("patched direct jumps:          %lu\n", num_direct_jumps);
    printf("indirect jumps:                %lu\n", num_direct_and_indirect_jumps - num_direct_jumps);
#endif
    printf("fragment hash misses:          %lu\n", num_fragment_hash_misses);
    printf("translated blocks:             %lu\n", num_translated_blocks);
    printf("translated traces:             %lu\n", num_translated_traces);
    printf("translated insns:              %lu\n", num_translated_insns);
    printf("translated insns in traces:    %lu\n", num_translated_trace_insns);
    printf("generated insns:               %lu\n", emit_loc - code_area - 3 * num_const_adds);
    printf("load/store reg insns:          %lu\n", num_load_store_reg_insns);
    printf("constants:                     %d\n", num_constants - (NUM_EMU_REGISTERS * 2 + 2));
    printf("stub calls:                    %lu\n", num_stub_calls);
    printf("loop profiler calls:           %lu\n", num_loop_profiler_calls);

#ifdef COLLECT_PPC_FPR_STATS
    for (i = 0; i < 32; ++i)
	printf("f%-2d: %u\n", i, fpr_uses[i]);
#endif
#endif

#ifdef COUNT_INSNS
    printf("blocks executed:               %lu\n", get_const_64(FRAGMENTS_CONST));
    printf("premature trace exits:         %lu\n", get_const_64(PREMATURE_EXITS_CONST));
    printf("end trace exits:               %lu\n", get_const_64(END_EXITS_CONST));
    printf("emulated block insns executed: %lu\n", get_const_64(EMULATED_INSNS_CONST));
    printf("emulated trace insns executed: %lu\n", get_const_64(EMULATED_TRACE_INSNS_CONST));
    printf("native block insns executed:   %lu\n", get_const_64(NATIVE_INSNS_CONST));
#endif

#ifdef COLLECT_STATS
#ifndef USE_HAND_TRANSLATOR
    for (i = 0; i < NUM_INSNS; ++i)
	if (insn_infos[i].num_translated > 0)
	    printf("  %-20s       %10d  %10d\n", insn_names[i], insn_infos[i].num_translated, insn_infos[i].num_generated_insns);
#endif
#endif

#ifdef MEASURE_TIME
    printf("time spent in compiler: %lu\n", compiler_time);
#endif
}

void
handle_compiler_system_call (void)
{
    move_regs_compiler_to_interpreter(compiler_intp);
    /*
    printf("*** system call\n");
    dump_ppc_registers(compiler_intp);
    */

    handle_system_call(compiler_intp);

    move_regs_interpreter_to_compiler(compiler_intp);
}
#endif
