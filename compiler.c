#include <assert.h>
#include <stdio.h>

#include "bintrans.h"

#include "alpha_composer.h"
#include "alpha_disassembler.c"

typedef word_32 reg_t;
typedef int label_t;

#define MAX_LABELS              8
#define MAX_BACKPATCHES         8
#define MAX_CONSTANTS        8192

#define MAX_UNRESOLVED_JUMPS 2048

#define MAX_CODE_INSNS      65536

#define FRAGMENT_HASH_TABLE_SIZE   2048
#define FRAGMENT_HASH_OVERFLOW     1024

#define HASH_ADDR(addr)         (((addr) >> 2) & (FRAGMENT_HASH_TABLE_SIZE - 1))

#define FIRST_INTEGER_REG       1
#define NUM_INTEGER_REGS        5
#define REG_SAVE_AREA_REG       6
#define JUMP_TARGET_REG         7
#define RETURN_ADDR_REG        26
#define CONSTANT_AREA_REG      27
#define PROCEDURE_VALUE_REG    27

#define DIRECT_DISPATCHER_CONST    0
#define INDIRECT_DISPATCHER_CONST  2
#define SYSTEM_CALL_CONST          4
#define C_STUB_CONST               6
#define LEADING_ZEROS_CONST        8
#define DIV_UNSIGNED_32_CONST     10
#define DIV_SIGNED_32_CONST       12

#define FIRST_FLOAT_REG       1
#define NUM_FLOAT_REGS        5

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

label_info_t label_infos[MAX_LABELS];
word_32 code_area[MAX_CODE_INSNS];
word_32 *emit_loc;
word_64 fragment_start;

int num_constants = 0;
word_32 constant_area[MAX_CONSTANTS];

word_64 unresolved_jump_addrs[MAX_UNRESOLVED_JUMPS];
word_32 unresolved_jump_targets[MAX_UNRESOLVED_JUMPS];

fragment_hash_entry_t fragment_hash_table[FRAGMENT_HASH_TABLE_SIZE + FRAGMENT_HASH_OVERFLOW];
int first_free_overflow;

int integer_reg_free[NUM_INTEGER_REGS] = { 1, 1, 1, 1, 1 };
int float_reg_free[NUM_FLOAT_REGS] = { 1, 1, 1, 1, 1 };

int have_jumped = 0;

interpreter_t *compiler_intp = 0;
#ifdef CROSSDEBUGGER
interpreter_t *debugger_intp = 0;
#endif

reg_t
alloc_integer_reg (void)
{
    int i;

    for (i = 0; i < NUM_INTEGER_REGS; ++i)
	if (integer_reg_free[i])
	{
	    integer_reg_free[i] = 0;
	    return FIRST_INTEGER_REG + i;
	}

    assert(0);
    return -1;
}

void
free_integer_reg (reg_t reg)
{
    assert(reg >= FIRST_INTEGER_REG && reg < FIRST_INTEGER_REG + NUM_INTEGER_REGS);

    assert(!integer_reg_free[reg - FIRST_INTEGER_REG]);

    integer_reg_free[reg - FIRST_INTEGER_REG] = 1;
}

reg_t
alloc_float_reg (void)
{
    int i;

    for (i = 0; i < NUM_FLOAT_REGS; ++i)
	if (float_reg_free[i])
	{
	    float_reg_free[i] = 0;
	    return FIRST_FLOAT_REG + i;
	}

    assert(0);
    return -1;
}

void
free_float_reg (reg_t reg)
{
    assert(reg >= FIRST_FLOAT_REG && reg < FIRST_FLOAT_REG + NUM_FLOAT_REGS);

    assert(!float_reg_free[reg - FIRST_FLOAT_REG]);

    float_reg_free[reg - FIRST_FLOAT_REG] = 1;
}

void
emit (word_32 insn)
{
    assert(emit_loc < code_area + MAX_CODE_INSNS);
    *emit_loc++ = insn;

    /*
    printf("  ");
    disassemble_alpha_insn(insn, 0);
    printf("\n");
    */
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
load_reg_integer_32 (reg_t reg, word_64 reg_index)
{
    emit(COMPOSE_LDL(reg, reg_index * 8, REG_SAVE_AREA_REG));
}

void
store_reg_integer_32 (word_64 reg_index, reg_t reg)
{
    emit(COMPOSE_STL(reg, reg_index * 8, REG_SAVE_AREA_REG));
}

void
load_reg_float_64 (reg_t reg, word_64 reg_index)
{
    emit(COMPOSE_LDT(reg, reg_index * 8, REG_SAVE_AREA_REG));
}

void
store_reg_float_64 (word_64 reg_index, reg_t reg)
{
    /*
    reg_t int_reg = alloc_integer_reg();

    emit(COMPOSE_FTOIT(reg, int_reg));
    emit(COMPOSE_STQ(int_reg, reg_index * 8, REG_SAVE_AREA_REG));

    free_integer_reg(int_reg);
    */

    emit(COMPOSE_STT(reg, reg_index * 8, REG_SAVE_AREA_REG));
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
    /*
    int i;

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

    fragment_start = (word_64)emit_loc;
}

#define emit_store_mem_32(val,addr)    emit(COMPOSE_STL((val),0,(addr)))
#define emit_load_mem_32(val,addr)     emit(COMPOSE_LDL((val),0,(addr)))

void
emit_store_mem_8 (reg_t value_reg, reg_t addr_reg)
{
    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));
    emit(COMPOSE_STB(value_reg, 0, addr_reg));
}

void
emit_store_mem_64 (reg_t value_reg, reg_t addr_reg)
{
    reg_t tmp_reg = alloc_integer_reg();

    emit(COMPOSE_STL(value_reg, 4, addr_reg));
    emit(COMPOSE_SRL_IMM(value_reg, 32, tmp_reg));
    emit(COMPOSE_STL(tmp_reg, 0, addr_reg));
    free_integer_reg(tmp_reg);
}

void
emit_store_mem_16 (reg_t value_reg, reg_t addr_reg)
{
    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));
    emit(COMPOSE_STW(value_reg, 0, addr_reg));
}

void
emit_load_mem_8 (reg_t value_reg, reg_t addr_reg)
{
    emit(COMPOSE_XOR_IMM(addr_reg, 3, addr_reg));
    emit(COMPOSE_LDBU(value_reg, 0, addr_reg));
}

void
emit_load_mem_16 (reg_t value_reg, reg_t addr_reg)
{
    emit(COMPOSE_XOR_IMM(addr_reg, 2, addr_reg));
    emit(COMPOSE_LDWU(value_reg, 0, addr_reg));
}

void
emit_load_mem_64 (reg_t value_reg, reg_t addr_reg)
{
    reg_t tmp_reg = alloc_integer_reg();

    emit(COMPOSE_LDL(tmp_reg, 0, addr_reg));
    emit(COMPOSE_SLL_IMM(tmp_reg, 32, tmp_reg));
    emit(COMPOSE_LDL(value_reg, 4, addr_reg));
    emit(COMPOSE_BIS(value_reg, tmp_reg, value_reg));
    free_integer_reg(tmp_reg);
}

#include "ppc_compiler.c"

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

    index = fragment_hash_table[index].next;
    while (index != -1)
    {
	if (fragment_hash_table[index].foreign_addr == addr)
	    return fragment_hash_table[index].native_addr;
	index = fragment_hash_table[index].next;
    }

    return 0;
}

void
enter_fragment (word_32 foreign_addr, word_64 native_addr)
{
    int index = HASH_ADDR(foreign_addr);

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

word_32
div_unsigned_32 (word_32 a, word_32 b)
{
    return a / b;
}

sword_32
div_signed_32 (sword_32 a, sword_32 b)
{
    return a / b;
}

void
init_compiler (interpreter_t *intp, interpreter_t *dbg_intp)
{
    int i;

    compiler_intp = intp;
#ifdef CROSSDEBUGGER
    debugger_intp = dbg_intp;
#endif

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
    add_const_64((word_64)div_unsigned_32);
    add_const_64((word_64)div_signed_32);
}

word_64
compile_basic_block (word_32 addr)
{
    word_64 start = (word_64)emit_loc;
    word_32 insnp = addr;
    word_64 x;
    int old_num_constants = num_constants;
    int i;

    have_jumped = 0;

    while (!have_jumped)
    {
	compile_ppc_insn(mem_get_32(compiler_intp, insnp), insnp);
	insnp += 4;
    }
    emit_direct_jump(insnp);	/* this is not necessary if the jump at the end of the basic block was unconditional */

    finish_fragment();

    /*
    printf("-----\n");
    for (x = start; x < (word_64)emit_loc; x += 4)
    {
	printf("%016lx  ", x);
	disassemble_alpha_insn(*(word_32*)x, x);
	printf("\n");
    }

    for (i = old_num_constants; i < num_constants; ++i)
	printf("%4d    %08x\n", i * 4, constant_area[i]);
    */

    enter_fragment(addr, start);

    flush_icache();

    return start;
}

#ifdef CROSSDEBUGGER
void
compare_register_sets (void)
{
    int diff = 0;
    int i;

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

    if (diff)
    {
	dump_ppc_registers(debugger_intp);
	assert(0);
    }
}
#endif

word_64
provide_fragment (word_32 addr)
{
    word_64 native_addr = lookup_fragment(addr);

    /*
    move_ppc_regs_compiler_to_interpreter(compiler_intp);
    printf("*** jumping to %08x\n", addr);
    dump_ppc_registers(compiler_intp);
    */

#ifdef CROSSDEBUGGER
    debugger_intp->have_jumped = 0;
    while (!debugger_intp->have_jumped)
	interpret_ppc_insn(debugger_intp);
    assert(debugger_intp->pc == addr);
    compare_register_sets();
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

    for (i = 0; i < MAX_UNRESOLVED_JUMPS; ++i)
	if (unresolved_jump_addrs[i] == jump_addr)
	    break;

    assert(i < MAX_UNRESOLVED_JUMPS);

    native_addr = provide_fragment(unresolved_jump_targets[i]);

    jump_addr -= 8;

    disp = native_addr - jump_addr - 4;
    assert(disp >= -(1 << 22) && disp < (1 << 22));
    field = (disp >> 2) & 0x1fffff;

    *(word_32*)jump_addr = COMPOSE_BR(31, field);

    unresolved_jump_addrs[i] = 0;

    flush_icache();

    return native_addr;
}

void
start_compiler (word_32 addr)
{
    word_64 native_addr = compile_basic_block(addr);

    move_ppc_regs_interpreter_to_compiler(compiler_intp);
    start_execution(native_addr); /* this call never returns */
}

void
handle_compiler_system_call (void)
{
    move_ppc_regs_compiler_to_interpreter(compiler_intp);
    /*
    printf("*** system call\n");
    dump_ppc_registers(compiler_intp);
    */

    handle_system_call(compiler_intp);

    move_ppc_regs_interpreter_to_compiler(compiler_intp);
}
