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
#include <string.h>

#include "bintrans.h"

/* #define NO_REGISTER_CACHING */
/* #define NO_PATCHING */

#ifdef NEED_COMPILER
#include "compiler.h"
#include "fragment_hash.h"

#include "alpha_composer.h"
#include "alpha_disassembler.c"

#define MAX_LABELS              32
#define MAX_BACKPATCHES          8
#define MAX_CONSTANTS        16384

#define MAX_UNRESOLVED_JUMPS 16384 /* should be a lot less if we actually do resolve branches */

#define MAX_FRAGMENT_EMU_INSNS          256
#define MAX_FRAGMENT_INSNS              512
#define MAX_REG_FIELDS                 1024
#define MAX_REG_LOCKS                  2048
#define MAX_FRAGMENT_UNRESOLVED_JUMPS    16

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
reg_range_t integer_reg_range[] = { { 4, 16 }, { 18, 26 }, { 0, 0 } };
#define NUM_INTEGER_REGS                       20
#elif defined(EMU_I386)
reg_range_t integer_reg_range[] = { { 15, 16 }, { 18, 26 }, { 0, 0 } };
#define NUM_INTEGER_REGS                        9
#define FIRST_NATIVE_INTEGER_HOST_REG           1
#define NUM_NATIVE_INTEGER_HOST_REGS           14 /* FIXME: this should be in the machine definition (NUM_INTEGER_EMU_REGISTERS) */
#endif
#define FIRST_TMP_INTEGER_REG   1
#define NUM_TMP_INTEGER_REGS    3

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
#define FIRST_FLOAT_REG        28
#define NUM_FLOAT_REGS          3
#else
#define FIRST_FLOAT_REG         2
#define NUM_FLOAT_REGS         14
#endif

#define FIRST_TMP_FLOAT_REG     1
#define NUM_TMP_FLOAT_REGS      1

int native_integer_regs[NUM_INTEGER_REGS];
int native_float_regs[NUM_FLOAT_REGS];

#define REG_TYPE_INTEGER        1
#define REG_TYPE_FLOAT          2

#define MAX_ALLOC_DEPTH         4

typedef struct
{
    int emitted;
    int insn_num;
    int num_backpatches;
    int backpatches[MAX_BACKPATCHES];
} label_info_t;

typedef struct
{
    reg_t native_reg;
    int free;
} tmp_register_t;

typedef struct
{
    int type;
    int used;
    int locked;
    int dirty;
    int first_insn_num;
    int last_insn_num;
    int live_at_start;
    int live_at_end;
    reg_t native_reg;
} emu_register_t;

#define REG_LOCK_READ         1
#define REG_LOCK_WRITE        2
#define REG_LOCK_READ_WRITE   3
#define REG_LOCK_UNLOCK       4

typedef struct
{
    int type;
    reg_t reg;
    int insn_num;
} reg_lock_t;

typedef struct
{
    int insn_num;
    reg_t reg;
    int shift;
} reg_field_t;

#define EMU_INSN_INSN         1
#define EMU_INSN_EPILOGUE     2
#define EMU_INSN_INTERLUDE    3
#define EMU_INSN_STORE_REGS   4
#define EMU_INSN_CONTINUE     5

typedef struct
{
    int type;
    word_32 addr;
    int native_index;
} emu_insn_t;

typedef struct
{
    int insn_num;
    word_32 target;
} unresolved_jump_t;

#ifdef COLLECT_STATS
typedef struct
{
    int num_translated;
    int num_generated_insns;
    int num_generated_consts;
} insn_info_t;

insn_info_t insn_infos[NUM_INSNS];

word_32 fragment_insns[MAX_FRAGMENT_INSNS];
int num_fragment_insns;

emu_insn_t fragment_emu_insns[MAX_FRAGMENT_EMU_INSNS];
int num_fragment_emu_insns;

reg_lock_t reg_locks[MAX_REG_LOCKS];
int num_reg_locks;

reg_field_t reg_fields[MAX_REG_FIELDS];
int num_reg_fields;

label_info_t label_infos[MAX_LABELS];
int num_labels;

unresolved_jump_t fragment_unresolved_jumps[MAX_FRAGMENT_UNRESOLVED_JUMPS];
int num_fragment_unresolved_jumps;

#ifdef COLLECT_PPC_FPR_STATS
unsigned int fpr_uses[32];
#endif
#endif

word_32 code_area[MAX_CODE_INSNS];
word_32 *emit_loc;

int num_constants = NUM_EMU_REGISTERS * 2 + 2;
int old_num_constants;
int num_constants_init;
word_32 constant_area[NUM_EMU_REGISTERS * 2 + 2 + MAX_CONSTANTS] __attribute__ ((aligned (8)));

word_64 unresolved_jump_addrs[MAX_UNRESOLVED_JUMPS];
word_32 unresolved_jump_targets[MAX_UNRESOLVED_JUMPS];

tmp_register_t tmp_integer_regs[NUM_TMP_INTEGER_REGS];
tmp_register_t tmp_float_regs[NUM_TMP_FLOAT_REGS];

emu_register_t emu_regs[NUM_EMU_REGISTERS];

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
unsigned long num_isyncs = 0;
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
    assert(num_fragment_insns < MAX_FRAGMENT_INSNS);

    fragment_insns[num_fragment_insns++] = insn;

    /*
    assert(emit_loc < code_area + MAX_CODE_INSNS);
    *emit_loc++ = insn;
    */
}

void
start_emu_insn (int type, word_32 addr)
{
    assert(num_fragment_emu_insns < MAX_FRAGMENT_EMU_INSNS);

    fragment_emu_insns[num_fragment_emu_insns].type = type;
    fragment_emu_insns[num_fragment_emu_insns].addr = addr;
    fragment_emu_insns[num_fragment_emu_insns].native_index = num_fragment_insns;

    ++num_fragment_emu_insns;
}

void
emit_store_regs (int follow_type)
{
    start_emu_insn(EMU_INSN_STORE_REGS, 0);
    emit(0);
    start_emu_insn(follow_type, 0);
}

void
note_insn_reg (reg_t reg, int shift)
{
    assert(num_reg_fields < MAX_REG_FIELDS);
    assert(reg & FIELD_REG_BIT);

    reg_fields[num_reg_fields].insn_num = num_fragment_insns;
    reg_fields[num_reg_fields].reg = reg & ~FIELD_REG_BIT;
    reg_fields[num_reg_fields].shift = shift;

    ++num_reg_fields;
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

void
dump_fragment_code (word_64 start, word_64 end)
{
#ifdef DUMP_CODE
    int i;
    word_64 index = start;

    for (i = 0; i < num_fragment_emu_insns; ++i)
    {
	word_64 last;

	printf("++++++++++++++++\n");

	switch (fragment_emu_insns[i].type)
	{
	    case EMU_INSN_INSN :
		printf("%08x  ", fragment_emu_insns[i].addr);
#if defined(EMU_PPC)
		disassemble_ppc_insn(mem_get_32(compiler_intp, fragment_emu_insns[i].addr), fragment_emu_insns[i].addr);
#elif defined(EMU_I386)
		compiler_intp->pc = fragment_emu_insns[i].addr;
		disassemble_i386_insn(compiler_intp);
		printf("       0x%08x", block_insns[i - 1].flags_killed);
#endif
		break;

	    case EMU_INSN_EPILOGUE :
		printf("epilogue");
		break;

	    case EMU_INSN_STORE_REGS :
		printf("store regs");
		break;
	}

	printf("\n- - - - - - - - \n");

	if (i < num_fragment_emu_insns - 1)
	    last = start + fragment_emu_insns[i + 1].native_index * 4;
	else
	    last = end;

	disassemble_alpha_code(index, last);

	index = last;
    }

    for (i = old_num_constants; i < num_constants; ++i)
	printf("%4d    %08x\n", i * 4, constant_area[i]);
#endif
}

void
direct_emit (word_32 insn)
{
    assert(emit_loc < code_area + MAX_CODE_INSNS);

    *emit_loc++ = insn;
}

void
load_reg (int type, reg_t native_reg, reg_t foreign_reg)
{
#ifdef COLLECT_STATS
    ++num_load_store_reg_insns;
#endif

    switch (type)
    {
	case REG_TYPE_INTEGER :
	    direct_emit(COMPOSE_LDL(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	case REG_TYPE_FLOAT :
	    direct_emit(COMPOSE_LDT(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	default :
	    assert(0);
    }
}

void
store_reg (int type, reg_t native_reg, reg_t foreign_reg)
{
#ifdef COLLECT_STATS
    ++num_load_store_reg_insns;
#endif

    switch (type)
    {
	case REG_TYPE_INTEGER :
	    direct_emit(COMPOSE_STL(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	case REG_TYPE_FLOAT :
	    direct_emit(COMPOSE_STT(native_reg, foreign_reg * 8, CONSTANT_AREA_REG));
	    break;

	default:
	    assert(0);
    }
}

reg_t
ref_reg (int foreign_reg, int reading, int writing, int type)
{
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);
    assert(emu_regs[foreign_reg].type == type);
    assert(reading || writing);

    if (!emu_regs[foreign_reg].used)
    {
	emu_regs[foreign_reg].used = 1;
	emu_regs[foreign_reg].live_at_start = reading;
	emu_regs[foreign_reg].live_at_end = 0;
	emu_regs[foreign_reg].first_insn_num = num_fragment_insns;
    }

    if (emu_regs[foreign_reg].locked)
    {
	int i;

	assert(num_reg_locks > 0);

	for (i = num_reg_locks - 1; i >= 0; --i)
	    if (reg_locks[i].reg == foreign_reg)
	    {
		if (!(reg_locks[i].type & REG_LOCK_READ))
		    assert(!reading);
		if (writing)
		    reg_locks[i].type |= REG_LOCK_WRITE;
		break;
	    }

	assert(i >= 0);
    }
    else
    {
	reg_locks[num_reg_locks].reg = foreign_reg;
	reg_locks[num_reg_locks].type = (reading ? REG_LOCK_READ : 0) | (writing ? REG_LOCK_WRITE : 0);
	reg_locks[num_reg_locks].insn_num = num_fragment_insns;

	++num_reg_locks;
    }

    ++emu_regs[foreign_reg].locked;

    emu_regs[foreign_reg].live_at_end = emu_regs[foreign_reg].live_at_end || writing;

    return foreign_reg | FIELD_REG_BIT;
}

void
unref_reg (reg_t reg)
{
    int foreign_reg;

    assert(reg | FIELD_REG_BIT);

    foreign_reg = reg & ~FIELD_REG_BIT;
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);

    assert(emu_regs[foreign_reg].used);
    assert(emu_regs[foreign_reg].locked > 0);

    emu_regs[foreign_reg].last_insn_num = num_fragment_insns;

    if (--emu_regs[foreign_reg].locked == 0)
    {
	reg_locks[num_reg_locks].reg = foreign_reg;
	reg_locks[num_reg_locks].type = REG_LOCK_UNLOCK;
	reg_locks[num_reg_locks].insn_num = num_fragment_insns;

	++num_reg_locks;
    }
}

reg_t
ref_integer_reg (int foreign_reg, int reading, int writing)
{
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);
#ifdef EMU_I386
    assert(foreign_reg < NUM_NATIVE_INTEGER_HOST_REGS);
    return foreign_reg + FIRST_NATIVE_INTEGER_HOST_REG;
#else
    return ref_reg(foreign_reg, reading, writing, REG_TYPE_INTEGER);
#endif
}

void
unref_integer_reg (reg_t reg)
{
#ifdef EMU_I386
    assert(reg > FIRST_NATIVE_INTEGER_HOST_REG && reg < FIRST_NATIVE_INTEGER_HOST_REG + NUM_NATIVE_INTEGER_HOST_REGS);
#else
    unref_reg(reg);
#endif
}

reg_t
ref_float_reg (int foreign_reg, int reading, int writing)
{
    assert(foreign_reg >= 0 && foreign_reg < NUM_EMU_REGISTERS);

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
    {
	int fpr_num = foreign_reg - 5 - 32;

	assert(foreign_reg >= 5 + 32 && foreign_reg < 5 + 32 + 32);

	if (fpr_num < 14)
	    return fpr_num;
	if (fpr_num >= 18)
	    return fpr_num - 4;
    }
#else
    return ref_reg(foreign_reg, reading, writing, REG_TYPE_FLOAT);
#endif
}

void
unref_float_reg (reg_t reg)
{
#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
    if (reg < 28)
	return;
#else
    unref_reg(reg);
#endif
}

reg_t
alloc_tmp_reg (tmp_register_t *tmp_regs, int num_regs)
{
    int i;

    for (i = 0; i < num_regs; ++i)
	if (tmp_regs[i].free)
	{
	    tmp_regs[i].free = 0;
	    return tmp_regs[i].native_reg;
	}

    assert(0);

    return -1;
}

void
free_tmp_reg (tmp_register_t *tmp_regs, int num_regs, reg_t native_reg)
{
    int i;

    for (i = 0; i < num_regs; ++i)
	if (tmp_regs[i].native_reg == native_reg)
	{
	    assert(!tmp_regs[i].free);
	    tmp_regs[i].free = 1;
	    return;
	}

    assert(0);
}

reg_t
alloc_tmp_integer_reg (void)
{
    return alloc_tmp_reg(tmp_integer_regs, NUM_TMP_INTEGER_REGS);
}

void
free_tmp_integer_reg (reg_t native_reg)
{
    free_tmp_reg(tmp_integer_regs, NUM_TMP_INTEGER_REGS, native_reg);
}

reg_t
alloc_tmp_float_reg (void)
{
    return alloc_tmp_reg(tmp_float_regs, NUM_TMP_FLOAT_REGS);
}

void
free_tmp_float_reg (reg_t native_reg)
{
    free_tmp_reg(tmp_float_regs, NUM_TMP_FLOAT_REGS, native_reg);
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
    assert(num_labels < MAX_LABELS);

    label_infos[num_labels].emitted = 0;
    label_infos[num_labels].num_backpatches = 0;

    return num_labels++;
}

void
free_label (label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(l->emitted);

    /*
    int i;
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->free);

    for (i = 0; i < l->num_backpatches; ++i)
    {
	sword_64 disp = l->address - l->backpatches[i] - 4;
	word_32 field;

	assert(disp >= -(1 << 22) && disp < (1 << 22));
	field = (disp >> 2) & 0x1fffff;

	*(word_32*)(l->backpatches[i]) |= field;
    }

    label_infos[label].free = 1;
    */
}

void
emit_label (label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(!l->emitted);

    l->insn_num = num_fragment_insns;
    l->emitted = 1;

    /* printf("label %d:\n", label); */
}

void
emit_branch (word_32 insn, label_t label)
{
    label_info_t *l;

    assert(label >= 0 && label < MAX_LABELS);
    l = &label_infos[label];

    assert(l->num_backpatches < MAX_BACKPATCHES);
    l->backpatches[l->num_backpatches++] = num_fragment_insns;

    emit(insn);
}

void
emit_direct_jump (word_32 target)
{
    if (branch_profile_func != 0)
	branch_profile_func();

    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, DIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
    emit(COMPOSE_JMP(RETURN_ADDR_REG, PROCEDURE_VALUE_REG));

    assert(num_fragment_unresolved_jumps < MAX_FRAGMENT_UNRESOLVED_JUMPS);

    fragment_unresolved_jumps[num_fragment_unresolved_jumps].insn_num = num_fragment_insns;
    fragment_unresolved_jumps[num_fragment_unresolved_jumps].target = target;

    ++num_fragment_unresolved_jumps;

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
start_fragment (void)
{
    int i;

    num_fragment_insns = 0;
    num_fragment_emu_insns = 0;
    num_reg_locks = 0;
    num_reg_fields = 0;
    num_labels = 0;
    num_fragment_unresolved_jumps = 0;
    old_num_constants = num_constants;

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
    {
	emu_regs[i].used = 0;
	emu_regs[i].locked = 0;
	emu_regs[i].native_reg = NO_REG;
    }
}

void
simple_reg_alloc (void)
{
    int i, ii = 0, fi = 0;

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	if (emu_regs[i].used)
	{
	    if (emu_regs[i].type == REG_TYPE_INTEGER)
		emu_regs[i].native_reg = native_integer_regs[ii++];
	    else if (emu_regs[i].type == REG_TYPE_FLOAT)
		emu_regs[i].native_reg = native_float_regs[fi++];
	    else
		assert(0);
	}
}

reg_t
alloc_native_reg_for_emu_reg (reg_t emu_reg, int current_insn_num)
{
    int l;
    reg_t native_reg;

    for (l = 0; l < NUM_EMU_REGISTERS; ++l)
	if (emu_regs[l].type == emu_regs[emu_reg].type && emu_regs[l].used
	    && !emu_regs[l].locked && emu_regs[l].native_reg != NO_REG
	    && emu_regs[l].last_insn_num <= current_insn_num)
	    break;

    if (l == NUM_EMU_REGISTERS)
	for (l = 0; l < NUM_EMU_REGISTERS; ++l)
	    if (emu_regs[l].type == emu_regs[emu_reg].type && emu_regs[l].used
		&& !emu_regs[l].locked && emu_regs[l].native_reg != NO_REG)
		break;

    assert(l < NUM_EMU_REGISTERS);

    if (emu_regs[l].dirty)
	store_reg(emu_regs[l].type, emu_regs[l].native_reg, l);

    native_reg = emu_regs[l].native_reg;

    emu_regs[l].native_reg = NO_REG;

    return native_reg;
}

void
finish_fragment (void)
{
    int i;
    int num_integer_regs = 0, num_float_regs = 0;
#ifdef DUMP_CODE
    word_64 start = (word_64)emit_loc, body_start;
#endif
    int jumps_index;
    int rf, rl;

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
    {
	if (emu_regs[i].used)
	{
	    if (emu_regs[i].type == REG_TYPE_INTEGER)
		++num_integer_regs;
	    else if (emu_regs[i].type == REG_TYPE_FLOAT)
		++num_float_regs;
	    else
		assert(0);
	}
    }

#ifdef DUMP_CODE
    printf("%d integer regs, %d float regs\n", num_integer_regs, num_float_regs);
#endif

    /* assert(num_float_regs == 0); */

    if (num_integer_regs <= NUM_INTEGER_REGS && num_float_regs <= NUM_FLOAT_REGS)
	simple_reg_alloc();
    else
    {
	int ii = 0, fi = 0;

	for (i = 0; i < num_reg_fields; ++i)
	{
	    if (emu_regs[reg_fields[i].reg].native_reg == NO_REG)
	    {
		if (emu_regs[reg_fields[i].reg].type == REG_TYPE_INTEGER)
		{
		    if (ii < NUM_INTEGER_REGS)
			emu_regs[reg_fields[i].reg].native_reg = native_integer_regs[ii++];
		}
		else if (emu_regs[reg_fields[i].reg].type == REG_TYPE_FLOAT)
		{
		    if (fi < NUM_FLOAT_REGS)
			emu_regs[reg_fields[i].reg].native_reg = native_float_regs[fi++];
		}
		else
		    assert(0);

		if (ii == NUM_INTEGER_REGS && fi == NUM_FLOAT_REGS)
		    break;
	    }
	}
    }

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
    {
	if (emu_regs[i].used && emu_regs[i].live_at_start && emu_regs[i].native_reg != NO_REG)
	    load_reg(emu_regs[i].type, emu_regs[i].native_reg, i);
    }

    body_start = (word_64)emit_loc;
    jumps_index = 0;
    rf = 0;
    rl = 0;

    /* instructions are patched and emitted; register allocation happens simultaneously */
    for (i = 0; i < num_fragment_emu_insns; ++i)
    {
	int len, first, last;
	int j;

	first = fragment_emu_insns[i].native_index;

	if (i < num_fragment_emu_insns - 1)
	    len = fragment_emu_insns[i + 1].native_index - first;
	else
	    len = num_fragment_insns - first;

	last = first + len;

	fragment_emu_insns[i].native_index = emit_loc - (word_32*)body_start;

	if (fragment_emu_insns[i].type == EMU_INSN_STORE_REGS)
	{
	    assert(first + 1 == last);

	    fragment_insns[first] = emit_loc - code_area;

	    for (j = 0; j < NUM_EMU_REGISTERS; ++j)
		if (emu_regs[j].used && emu_regs[j].live_at_end && emu_regs[j].native_reg != NO_REG)
		    store_reg(emu_regs[j].type, emu_regs[j].native_reg, j);
	}
	else
	{
	    for (j = first; j < last; ++j)
	    {
		word_32 *first_insn = emit_loc;

		while (rl < num_reg_locks && reg_locks[rl].insn_num == j)
		{
		    if (reg_locks[rl].type == REG_LOCK_UNLOCK)
			emu_regs[reg_locks[rl].reg].locked = 0;
		    else
		    {
			assert(!emu_regs[reg_locks[rl].reg].locked);

			emu_regs[reg_locks[rl].reg].locked = 1;

			if (emu_regs[reg_locks[rl].reg].native_reg == NO_REG)
			{
			    emu_regs[reg_locks[rl].reg].native_reg = alloc_native_reg_for_emu_reg(reg_locks[rl].reg, j);

			    if (reg_locks[rl].type & REG_LOCK_READ)
				load_reg(emu_regs[reg_locks[rl].reg].type, emu_regs[reg_locks[rl].reg].native_reg,
					 reg_locks[rl].reg);
			    emu_regs[reg_locks[rl].reg].dirty = (reg_locks[rl].type & REG_LOCK_WRITE) ? 1 : 0;
			}
			else
			    emu_regs[reg_locks[rl].reg].dirty |= (reg_locks[rl].type & REG_LOCK_WRITE) ? 1 : 0;
		    }

		    ++rl;
		}

		while (rf < num_reg_fields && reg_fields[rf].insn_num == j)
		{
		    assert(emu_regs[reg_fields[rf].reg].locked);

		    fragment_insns[j] |= emu_regs[reg_fields[rf].reg].native_reg << reg_fields[rf].shift;

		    ++rf;
		}

		direct_emit(fragment_insns[j]);

		/* the insn is replaced by its code_area index */
		fragment_insns[j] = first_insn - code_area;

		if (jumps_index < num_fragment_unresolved_jumps && j + 1 == fragment_unresolved_jumps[jumps_index].insn_num)
		{
		    int k;

		    for (k = 0; k < MAX_UNRESOLVED_JUMPS; ++k)
			if (unresolved_jump_addrs[k] == 0)
			    break;

		    assert(k < MAX_UNRESOLVED_JUMPS);

		    unresolved_jump_addrs[k] = (word_64)emit_loc;
		    unresolved_jump_targets[k] = fragment_unresolved_jumps[jumps_index].target;

		    ++jumps_index;
		}
	    }
	}
    }

    /* jumps are patched */
    for (i = 0; i < num_labels; ++i)
    {
	label_info_t *l = &label_infos[i];
	int j;

	for (j = 0; j < l->num_backpatches; ++j)
	{
	    sword_64 disp = (fragment_insns[l->insn_num] - fragment_insns[l->backpatches[j]] - 1) * 4;
	    word_32 field;

	    assert(disp >= -(1 << 22) && disp < (1 << 22));
	    field = (disp >> 2) & 0x1fffff;

	    code_area[fragment_insns[l->backpatches[j]]] |= field;
	}
    }

#ifdef DUMP_CODE
    printf("++++++++++++++++\nprologue\n- - - - - - - - \n");

    disassemble_alpha_code(start, body_start);

    dump_fragment_code(body_start, (word_64)emit_loc);
#endif
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
    tmp_reg = alloc_tmp_integer_reg();
    emit(COMPOSE_SRL_IMM(value_reg, 32, tmp_reg));
    emit(COMPOSE_STL(tmp_reg, 0, addr_reg));
    free_tmp_integer_reg(tmp_reg);
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
    reg_t tmp_reg = alloc_tmp_integer_reg();

    emit(COMPOSE_LDL(tmp_reg, 0, addr_reg));
    emit(COMPOSE_SLL_IMM(tmp_reg, 32, tmp_reg));
    emit(COMPOSE_LDL(value_reg, 4, addr_reg));
    emit(COMPOSE_ZAPNOT_IMM(value_reg, 15, value_reg));
    emit(COMPOSE_BIS(value_reg, tmp_reg, value_reg));
    free_tmp_integer_reg(tmp_reg);
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
    int i, j, k, range;
    int native;

    compiler_intp = intp;
#ifdef CROSSDEBUGGER
    debugger_intp = dbg_intp;
#endif

    i = 0;
    for (range = 0; integer_reg_range[range].end > 0; ++range)
    {
	for (native = integer_reg_range[range].begin; native < integer_reg_range[range].end; ++native)
	    native_integer_regs[i++] = native;
    }

    i = 0;
    for (native = FIRST_FLOAT_REG; native < FIRST_FLOAT_REG + NUM_FLOAT_REGS; ++native)
	native_float_regs[i++] = native;

    for (i = 0; i < NUM_TMP_INTEGER_REGS; ++i)
    {
	tmp_integer_regs[i].native_reg = FIRST_TMP_INTEGER_REG + i;
	tmp_integer_regs[i].free = 1;
    }

    for (i = 0; i < NUM_TMP_FLOAT_REGS; ++i)
    {
	tmp_float_regs[i].native_reg = FIRST_TMP_FLOAT_REG + i;
	tmp_float_regs[i].free = 1;
    }

    for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	if (i >= 5 + 32 && i < 5 + 32 + 32)
	    emu_regs[i].type = REG_TYPE_FLOAT;
	else
	    emu_regs[i].type = REG_TYPE_INTEGER;

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	unresolved_jump_addrs[i] = 0;

    emit_loc = code_area;

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

word_64
compile_until_jump (word_32 *addr, int optimize_taken_jump, label_t taken_jump_label, word_32 *target_addr)
{
    word_64 start = (word_64)emit_loc;
    word_32 insnp = *addr;
#ifdef COLLECT_STATS
    word_64 x = start;
#endif
#ifdef EMU_I386
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
	start_emu_insn(EMU_INSN_INSN, insnp);

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
	store_and_free_all_foreign_regs(); /* FIXME */
#endif

#ifdef COLLECT_STATS
	assert(generated_insn_index < NUM_INSNS);
	++insn_infos[generated_insn_index].num_translated;
	insn_infos[generated_insn_index].num_generated_insns += ((word_64)emit_loc - x) / 4;
#endif

#if defined(COLLECT_STATS)
	x = (word_64)emit_loc;
#endif

#if defined(EMU_PPC)
	insnp += 4;
#elif defined(EMU_I386)
	insnp = compiler_intp->pc;
#endif
    }

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
#ifdef COUNT_INSNS
    word_32 *emulated_lda_insn, *native_lda_insn, *old_emit_loc;

    old_num_translated_insns = num_translated_insns;
#endif

    start_timer();

    start_fragment();

    /*
    while (((emit_loc - code_area) & 3) != 0)
	emit(0);
    */

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

    emit_store_regs(EMU_INSN_EPILOGUE);

    emit_direct_jump(addr);	/* this is not necessary if the jump at the end of the basic block was unconditional */

    finish_fragment();

    flush_icache();

    stop_timer();

#ifdef COLLECT_STATS
    if (as_trace)
	++num_translated_traces;
    else
	++num_translated_blocks;

    /*
    {
	int i, used = 0;

	for (i = 0; i < NUM_EMU_REGISTERS; ++i)
	    if (regs_used_in_fragment[i])
		++used;

	printf("regs used in block: %d\n", used);
    }
    */
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

#ifdef COLLECT_STATS
    old_num_translated_insns = num_translated_insns;
#endif

    start_timer();

    start_fragment();

    branch_profile_func = branch_profile_within_trace;

    for (bit = 1 << (length - 1); bit != 0; bit >>= 1)
    {
	if (bits & bit)
	{
	    label_t taken_jump_label = alloc_label();
	    word_32 target_addr;

	    compile_until_jump(&addr, 1, taken_jump_label, &target_addr);

	    emit_store_regs(EMU_INSN_INTERLUDE);

	    emit_direct_jump(addr);

	    emit_label(taken_jump_label);
	    free_label(taken_jump_label);

	    addr = target_addr;
	}
	else
	    compile_until_jump(&addr, 0, 0, 0);
    }

    branch_profile_func = branch_profile_end_trace;

    compile_until_jump(&addr, 0, 0, 0);

    emit_store_regs(EMU_INSN_EPILOGUE);

    emit_direct_jump(addr);	/* this is not necessary if the jump at the end of the basic block was unconditional */

    branch_profile_func = 0;

    finish_fragment();

    flush_icache();

    stop_timer();

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
    reset_mem_trace();
    trace_mem = 1;
    debugger_intp->have_jumped = 0;
    while (!debugger_intp->have_jumped)
	interpret_insn(debugger_intp);
    trace_mem = 0;
    move_regs_compiler_to_interpreter(compiler_intp);
    compare_register_sets();
    compare_mem_writes(debugger_intp, compiler_intp);
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

#ifdef COLLECT_STATS
    ++num_isyncs;
#endif

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
    printf("isyncs:                        %lu\n", num_isyncs);

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
