#include <assert.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

#include "bintrans.h"

#include "alpha_composer.h"
#include "alpha_disassembler.c"

typedef word_32 reg_t;
typedef int label_t;

#define MAX_LABELS               8
#define MAX_BACKPATCHES          8
#define MAX_CONSTANTS        16384

#define MAX_UNRESOLVED_JUMPS 16384 /* should be a lot less if we actually do resolve branches */

#define MAX_CODE_INSNS     600000

#define FRAGMENT_HASH_TABLE_SIZE  16384
#define FRAGMENT_HASH_OVERFLOW     8192

#define HASH_ADDR(addr)         (((addr) >> 2) & (FRAGMENT_HASH_TABLE_SIZE - 1))

#define JUMP_TARGET_REG        16
#define RETURN_ADDR_REG        26
#define CONSTANT_AREA_REG      27
#define PROCEDURE_VALUE_REG    27

#define NUM_REG_CONSTANTS          (NUM_EMU_REGISTERS * 2 + 2) /* registers + scratch area */

#define DIRECT_DISPATCHER_CONST    (NUM_REG_CONSTANTS + 0)
#define INDIRECT_DISPATCHER_CONST  (NUM_REG_CONSTANTS + 2)
#define SYSTEM_CALL_CONST          (NUM_REG_CONSTANTS + 4)
#define C_STUB_CONST               (NUM_REG_CONSTANTS + 6)
#define LEADING_ZEROS_CONST        (NUM_REG_CONSTANTS + 8)
#define DIV_UNSIGNED_64_CONST      (NUM_REG_CONSTANTS + 10)
#define DIV_SIGNED_64_CONST        (NUM_REG_CONSTANTS + 12)
#define MOD_UNSIGNED_64_CONST      (NUM_REG_CONSTANTS + 14)
#define MOD_SIGNED_64_CONST        (NUM_REG_CONSTANTS + 16)
#define DIV_UNSIGNED_32_CONST      (NUM_REG_CONSTANTS + 18)
#define DIV_SIGNED_32_CONST        (NUM_REG_CONSTANTS + 20)
#define MOD_UNSIGNED_32_CONST      (NUM_REG_CONSTANTS + 22)
#define MOD_SIGNED_32_CONST        (NUM_REG_CONSTANTS + 24)
#define DIV_UNSIGNED_16_CONST      (NUM_REG_CONSTANTS + 26)
#define DIV_SIGNED_16_CONST        (NUM_REG_CONSTANTS + 28)
#define MOD_UNSIGNED_16_CONST      (NUM_REG_CONSTANTS + 30)
#define MOD_SIGNED_16_CONST        (NUM_REG_CONSTANTS + 32)
#define DIV_UNSIGNED_8_CONST       (NUM_REG_CONSTANTS + 34)
#define DIV_SIGNED_8_CONST         (NUM_REG_CONSTANTS + 36)
#define MOD_UNSIGNED_8_CONST       (NUM_REG_CONSTANTS + 38)
#define MOD_SIGNED_8_CONST         (NUM_REG_CONSTANTS + 40)

#if defined(EMU_PPC)
#define FIRST_INTEGER_REG       1
#define NUM_INTEGER_REGS       15
#define REG_TYPE_INTEGER        1
#elif defined(EMU_I386)
#define FIRST_NATIVE_INTEGER_HOST_REG           1
#define NUM_NATIVE_INTEGER_HOST_REGS            10 /* FIXME: this should be in the machine definition (NUM_INTEGER_EMU_REGISTERS) */
#define FIRST_INTEGER_REG                       (FIRST_NATIVE_INTEGER_HOST_REG + NUM_NATIVE_INTEGER_HOST_REGS)
#define NUM_INTEGER_REGS                        (15 - 10) /* FIXME: see above */
#define REG_TYPE_INTEGER                        1
#endif

#define FIRST_FLOAT_REG         1
#define NUM_FLOAT_REGS         15
#define REG_TYPE_FLOAT          2

#define MAX_ALLOC_DEPTH         4

#define NEED_NATIVE        0x1000

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
    word_32 foreign_addr;
    int next;
    word_64 native_addr;
} fragment_hash_entry_t;

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
#endif

label_info_t label_infos[MAX_LABELS];
word_32 code_area[MAX_CODE_INSNS];
word_32 *emit_loc;
word_64 fragment_start;

int num_constants = NUM_EMU_REGISTERS * 2 + 2;
word_32 constant_area[NUM_EMU_REGISTERS * 2 + 2 + MAX_CONSTANTS];

word_64 unresolved_jump_addrs[MAX_UNRESOLVED_JUMPS];
word_32 unresolved_jump_targets[MAX_UNRESOLVED_JUMPS];

fragment_hash_entry_t fragment_hash_table[FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW];
int first_free_overflow;

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

#ifdef EMU_I386
typedef struct
{
    word_32 addr;
    word_32 flags_live;
    word_32 flags_killed;
} i386_insn_t;

#define MAX_BLOCK_INSNS          1024
#define MAX_AFTER_BRANCH_INSNS     15

i386_insn_t block_insns[MAX_BLOCK_INSNS + MAX_AFTER_BRANCH_INSNS];
int num_block_insns = 0;
#endif

#ifdef COLLECT_STATS
unsigned long num_direct_jumps = 0;
unsigned long num_direct_and_indirect_jumps = 0;
unsigned long num_fragment_hash_misses = 0;
unsigned long num_translated_fragments = 0;
unsigned long num_translated_insns = 0;
unsigned long num_load_store_reg_insns = 0;
unsigned long num_divisions = 0;
#endif

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

#define ref_integer_reg_for_reading(f)                    ref_integer_reg(f,1,0)
#define ref_integer_reg_for_writing(f)                    ref_integer_reg(f,0,1)
#define ref_integer_reg_for_reading_and_writing(f)        ref_integer_reg(f,1,1)

#define ref_gpr_reg_for_reading                           ref_integer_reg_for_reading
#define ref_gpr_reg_for_writing                           ref_integer_reg_for_writing

void
unref_integer_reg (reg_t reg)
{
#ifdef EMU_I386
    if (reg >= FIRST_NATIVE_INTEGER_HOST_REG && reg < FIRST_NATIVE_INTEGER_HOST_REG + NUM_NATIVE_INTEGER_HOST_REGS)
	return;
#endif

    unref_reg(integer_regs[alloc_sp], NUM_INTEGER_REGS, reg);
}

#define unref_gpr_reg(f)                                  unref_integer_reg(f)

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

    reg = ref_reg(float_regs[alloc_sp], NUM_FLOAT_REGS, foreign_reg, writing, &newly_allocated);
    if (reading && newly_allocated)
	load_reg(reg);
    return reg->native_reg;
}

#define ref_float_reg_for_reading(f)                      ref_float_reg(f,1,0)
#define ref_float_reg_for_writing(f)                      ref_float_reg(f,0,1)
#define ref_float_reg_for_reading_and_writing(f)          ref_float_reg(f,1,1)

void
unref_float_reg (reg_t reg)
{
    unref_reg(float_regs[alloc_sp], NUM_FLOAT_REGS, reg);
}

void
emit_load_integer_32 (reg_t reg, word_32 val)
{
    if ((val >> 15) == 0 || (val >> 15) == (0xffffffff >> 15))
	emit(COMPOSE_LDA(reg, val & 0xffff, 31));
    else if ((val & 0xffff) == 0)
	emit(COMPOSE_LDAH(reg, val >> 16, 31));
    else
    {
	assert(num_constants < MAX_CONSTANTS);

	emit(COMPOSE_LDL(reg, num_constants * 4, CONSTANT_AREA_REG));
	constant_area[num_constants++] = val;

	/*
	printf("  load $%u,%u\n", reg, val);
	assert(0);
	*/
    }
}

void
emit_load_integer_64 (reg_t reg, word_64 val)
{
    if ((val >> 15) == 0 || (val >> 15) == ((word_64)-1 >> 15))
	emit(COMPOSE_LDA(reg, val & 0xffff, 31));
    else if ((val & 0xffff) == 0 && ((val >> 31) == 0 || (val >> 31) == ((word_64)-1 >> 31)))
	emit(COMPOSE_LDAH(reg, (val >> 16) & 0xffff, 31));
    else
    {
	assert(num_constants + 2 < MAX_CONSTANTS);

	if (num_constants % 2 == 1)
	    constant_area[num_constants++] = 0xdeadbeef;

	emit(COMPOSE_LDQ(reg, num_constants * 4, CONSTANT_AREA_REG));
	constant_area[num_constants++] = val & 0xffffffff;
	constant_area[num_constants++] = val >> 32;

	/*
	printf("  load $%u,%u\n", reg, val);
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
    /* we assume that the target address is already in $7 */

    emit(COMPOSE_LDQ(PROCEDURE_VALUE_REG, INDIRECT_DISPATCHER_CONST * 4, CONSTANT_AREA_REG));
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

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
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

#define emit_store_mem_32(val,addr)    emit(COMPOSE_STL((val),0,(addr)))
#define emit_load_mem_32(val,addr)     emit(COMPOSE_LDL((val),0,(addr)))

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
#include "ppc_compiler.c"
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
lookup_fragment (word_32 addr)
{
    int index = HASH_ADDR(addr);

    if (fragment_hash_table[index].native_addr == 0)
	return 0;

    if (fragment_hash_table[index].foreign_addr == addr)
	return fragment_hash_table[index].native_addr;

#ifdef COLLECT_STATS
    ++num_fragment_hash_misses;
#endif

    index = fragment_hash_table[index].next;
    while (index != -1)
    {
	if (fragment_hash_table[index].foreign_addr == addr)
	    return fragment_hash_table[index].native_addr;
	index = fragment_hash_table[index].next;
    }

#ifdef COLLECT_STATS
    --num_fragment_hash_misses;
#endif

    return 0;
}

void
enter_fragment (word_32 foreign_addr, word_64 native_addr)
{
    int index = HASH_ADDR(foreign_addr);

#ifdef COLLECT_STATS
    ++num_translated_fragments;

#ifdef PERIODIC_STAT_DUMP
    if (num_translated_fragments % 100 == 0)
	print_compiler_stats();
#endif
#endif

    if (fragment_hash_table[index].native_addr == 0)
    {
	fragment_hash_table[index].foreign_addr = foreign_addr;
	fragment_hash_table[index].native_addr = native_addr;
	fragment_hash_table[index].next = -1;
    }
    else
    {
	int new = first_free_overflow;

	assert(new != -1);
	first_free_overflow = fragment_hash_table[new].next;

	fragment_hash_table[new].foreign_addr = foreign_addr;
	fragment_hash_table[new].native_addr = native_addr;
	fragment_hash_table[new].next = -1;

	while (fragment_hash_table[index].next != -1)
	    index = fragment_hash_table[index].next;

	fragment_hash_table[index].next = new;
    }
}

word_64
div_unsigned_64 (word_64 a, word_64 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

sword_64
div_signed_64 (sword_64 a, sword_64 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

word_64
mod_unsigned_64 (word_64 a, word_64 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

sword_64
mod_signed_64 (sword_64 a, sword_64 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

word_32
div_unsigned_32 (word_32 a, word_32 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

sword_32
div_signed_32 (sword_32 a, sword_32 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

word_32
mod_unsigned_32 (word_32 a, word_32 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

sword_32
mod_signed_32 (sword_32 a, sword_32 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

word_16
div_unsigned_16 (word_16 a, word_16 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

sword_16
div_signed_16 (sword_16 a, sword_16 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

word_16
mod_unsigned_16 (word_16 a, word_16 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

sword_16
mod_signed_16 (sword_16 a, sword_16 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

word_8
div_unsigned_8 (word_8 a, word_8 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

sword_8
div_signed_8 (sword_8 a, sword_8 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a / b;
}

word_8
mod_unsigned_8 (word_8 a, word_8 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

sword_8
mod_signed_8 (sword_8 a, sword_8 b)
{
#ifdef COLLECT_STATS
    ++num_divisions;
#endif
    return a % b;
}

void
init_compiler (interpreter_t *intp, interpreter_t *dbg_intp)
{
    int i;

    compiler_intp = intp;
#ifdef CROSSDEBUGGER
    debugger_intp = dbg_intp;
#endif

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
    {
	integer_regs[0][i].native_reg = FIRST_INTEGER_REG + i;
	integer_regs[0][i].type = REG_TYPE_INTEGER;
	integer_regs[0][i].free = 1;
	integer_regs[0][i].modified = 0;
	integer_regs[0][i].refcount = 0;
    }

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
    {
	float_regs[0][i].native_reg = FIRST_INTEGER_REG + i;
	float_regs[0][i].type = REG_TYPE_FLOAT;
	float_regs[0][i].free = 1;
	float_regs[0][i].modified = 0;
	float_regs[0][i].refcount = 0;
    }

    for (i = 0; i < MAX_LABELS; ++i)
	label_infos[i].free = 1;

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	unresolved_jump_addrs[i] = 0;

    for (i = 0; i < FRAGMENT_HASH_TABLE_SIZE; ++i)
	fragment_hash_table[i].native_addr = 0;
    for (i = FRAGMENT_HASH_TABLE_SIZE; i < FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW - 1; ++i)
	fragment_hash_table[i].next = i + 1;
    fragment_hash_table[i].next = -1;
    first_free_overflow = FRAGMENT_HASH_TABLE_SIZE;

    emit_loc = code_area;
    fragment_start = (word_64)emit_loc;

    add_const_64((word_64)direct_dispatcher);
    add_const_64((word_64)indirect_dispatcher);
    add_const_64((word_64)system_call_entry);
    add_const_64((word_64)c_stub);
    add_const_64((word_64)leading_zeros);
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

#ifdef COLLECT_STATS
    for (i = 0; i < NUM_INSNS; ++i)
    {
	insn_infos[i].num_translated = 0;
	insn_infos[i].num_generated_insns = 0;
	insn_infos[i].num_generated_consts = 0;
    }
#endif
}

#ifdef EMU_I386
void
compute_liveness (interpreter_t *intp, word_32 addr)
{
    word_32 old_pc = intp->pc;
    word_32 live;
    int i;
    int num_targets;
    word_32 targets[2];
    int can_fall_through, can_jump_indirectly;

    num_block_insns = 0;

    intp->pc = addr;

    while (num_block_insns < MAX_BLOCK_INSNS)
    {
	block_insns[num_block_insns++].addr = intp->pc;

	jump_analyze_i386_insn(intp, &num_targets, targets, &can_fall_through, &can_jump_indirectly);

	if (num_targets > 0 || can_jump_indirectly || !can_fall_through)
	    break;
    }

    assert(num_targets > 0 || can_jump_indirectly || !can_fall_through);

    if (!can_jump_indirectly)
    {
	int target;
	word_32 dummy_target;

	/* printf("checking out after branch\n"); */

	live = 0;

	if (can_fall_through)
	    targets[num_targets++] = intp->pc;

	for (target = 0; target < num_targets; ++target)
	{
	    word_32 block_live = 0xffffffff;
	    word_32 dummy;

	    intp->pc = targets[target];

	    for (i = num_block_insns; i < num_block_insns + MAX_AFTER_BRANCH_INSNS; ++i)
	    {
		int num_block_targets;

		block_insns[i].addr = intp->pc;

		jump_analyze_i386_insn(intp, &num_block_targets, &dummy_target, &can_fall_through, &can_jump_indirectly);

		if (num_block_targets > 0 || can_jump_indirectly || !can_fall_through)
		{
		    ++i;
		    break;
		}
	    }

	    for (--i; i >= num_block_insns; --i)
	    {
		intp->pc = block_insns[i].addr;
		liveness_i386_insn(intp, &block_live, &dummy);
	    }

	    live |= block_live;
	}
    }
    else
	live = 0xffffffff;

    for (i = num_block_insns - 1; i >= 0; --i)
    {
	intp->pc = block_insns[i].addr;
	liveness_i386_insn(intp, &live, &block_insns[i].flags_killed);
	block_insns[i].flags_live = live;
    }

    intp->pc = old_pc;
}

void
print_liveness (interpreter_t *intp)
{
    int i;

    for (i = 0; i < num_block_insns; ++i)
	printf("0x%08x   0x%08x 0x%08x\n", block_insns[i].addr, block_insns[i].flags_live, block_insns[i].flags_killed);
}
#endif

word_64
compile_basic_block (word_32 addr)
{
    word_64 start = (word_64)emit_loc;
    word_32 insnp = addr;
#if defined(DUMP_CODE) || defined(COLLECT_STATS)
    word_64 x = start;
    int old_num_constants = num_constants;
#endif
#ifdef EMU_I386
    int i;
#endif

    start_timer();

#ifdef EMU_I386
    compute_liveness(compiler_intp, addr);

    i = 0;

    compiler_intp->pc = addr;
#endif

    have_jumped = 0;

    while (!have_jumped)
    {
#if defined(EMU_PPC)
	compile_ppc_insn(mem_get_32(compiler_intp, insnp), insnp);
#elif defined(EMU_I386)
	word_32 insn_addr = compiler_intp->pc;

	compile_i386_insn(compiler_intp, block_insns[i++].flags_killed);
#endif

#ifdef COLLECT_STATS
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
	while (x < (word_64)emit_loc)
	{
	    printf("%016lx  ", x);
	    disassemble_alpha_insn(*(word_32*)x, x);
	    printf("\n");
	    x += 4;
	}
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

    store_all_foreign_regs();
    emit_direct_jump(insnp);	/* this is not necessary if the jump at the end of the basic block was unconditional */

#ifdef DUMP_CODE
    printf("++++++++++++++++\nepilogue\n- - - - - - - - \n");
    while (x < (word_64)emit_loc)
    {
	printf("%016lx  ", x);
	disassemble_alpha_insn(*(word_32*)x, x);
	printf("\n");
	x += 4;
    }
#endif

    finish_fragment();

#ifdef DUMP_CODE
    for (i = old_num_constants; i < num_constants; ++i)
	printf("%4d    %08x\n", i * 4, constant_area[i]);
#endif

    enter_fragment(addr, start);

    flush_icache();

    stop_timer();

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

    return compile_basic_block(addr);
}

word_64
provide_fragment_and_patch (word_64 jump_addr)
{
    int i;
    word_64 native_addr;
    sword_64 disp;
    word_32 field;

#ifdef COLLECT_STATS
    ++num_direct_jumps;
#endif

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	if (unresolved_jump_addrs[i] == jump_addr)
	    break;

    assert(i < MAX_UNRESOLVED_JUMPS);

    native_addr = provide_fragment(unresolved_jump_targets[i]);

#ifndef CROSSDEBUGGER
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

void
start_compiler (word_32 addr)
{
    word_64 native_addr = compile_basic_block(addr);

    move_regs_interpreter_to_compiler(compiler_intp);

    start_execution(native_addr); /* this call never returns */
}

void
print_compiler_stats (void)
{
#ifdef COLLECT_STATS
    int i;

    printf("patched direct jumps:   %lu\n", num_direct_jumps);
    printf("indirect jumps:         %lu\n", num_direct_and_indirect_jumps - num_direct_jumps);
    printf("fragment hash misses:   %lu\n", num_fragment_hash_misses);
    printf("translated fragments:   %lu\n", num_translated_fragments);
    printf("translated insns:       %lu\n", num_translated_insns);
    printf("generated insns:        %lu\n", emit_loc - code_area);
    printf("load/store reg insns:   %lu\n", num_load_store_reg_insns);
    printf("constants:              %d\n", num_constants - (NUM_EMU_REGISTERS * 2 + 2));
    printf("divisions:              %lu\n", num_divisions);

    for (i = 0; i < NUM_INSNS; ++i)
	if (insn_infos[i].num_translated > 0)
	    printf("  %-10s       %10d  %10d\n", insn_names[i], insn_infos[i].num_translated, insn_infos[i].num_generated_insns);
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
