/*
 * liveness.c
 *
 * bintrans
 *
 * Copyright (C) 2001,2002 Mark Probst
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

#include <stdio.h>
#include <assert.h>

#include "bintrans.h"
#include "compiler.h"
#include "fragment_hash.h"

#ifdef EMU_I386
i386_insn_t block_insns[MAX_TRACE_INSNS + MAX_AFTER_BRANCH_INSNS];

int
compute_liveness (interpreter_t *intp, word_32 addr, word_32 *addrs)
{
    word_32 old_pc = intp->pc;
    word_32 live;
    int i;
    int num_targets;
    word_32 targets[2];
    int can_fall_through, can_jump_indirectly;
    int num_block_insns;

    num_block_insns = 0;

    intp->pc = addr;

    while (num_block_insns < MAX_BLOCK_INSNS)
    {
	addrs[num_block_insns++] = intp->pc;

	jump_analyze_i386_insn(intp, &num_targets, targets, &can_fall_through, &can_jump_indirectly);

	if (num_targets > 0 || can_jump_indirectly || !can_fall_through)
	    break;
    }

    assert(num_targets > 0 || can_jump_indirectly || !can_fall_through);

    if (!can_jump_indirectly)
    {
	int target;

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
		word_32 target;

		addrs[i] = intp->pc;

		jump_analyze_i386_insn(intp, &num_block_targets, &target, &can_fall_through, &can_jump_indirectly);

		if (!can_jump_indirectly && !can_fall_through && num_block_targets == 1 && target != 0)
		    intp->pc = target;
		else if (num_block_targets > 0 || can_jump_indirectly || !can_fall_through)
		{
		    ++i;
		    break;
		}
	    }

	    for (--i; i >= num_block_insns; --i)
	    {
		intp->pc = addrs[i];
		liveness_i386_insn(intp, &block_live, &dummy);
	    }

	    live |= block_live;
	}
    }
    else
	live = 0xffffffff;

    for (i = num_block_insns - 1; i >= 0; --i)
    {
	intp->pc = addrs[i];
	liveness_i386_insn(intp, &live, &block_insns[i].flags_killed);
	block_insns[i].flags_live = live;
    }

    intp->pc = old_pc;

    return num_block_insns;
}

void
print_liveness (interpreter_t *intp, word_32 *addrs, int num_block_insns)
{
    int i;

    for (i = 0; i < num_block_insns; ++i)
	printf("0x%08x   0x%08x 0x%08x\n", addrs[i], block_insns[i].flags_live, block_insns[i].flags_killed);
}
#endif

#ifdef EMU_PPC
#include "ppc_livenesser.c"

ppc_insn_t block_insns[MAX_TRACE_INSNS];

int
compute_iterative_liveness (interpreter_t *intp, word_32 addr, word_32 *addrs, word_32 *live_cr, word_32 *live_xer, word_32 *live_gpr)
{
    word_32 last_addr;
    int can_fall_through, can_jump_indirectly;
    int num_targets;
    word_32 target;
    int length, i;

    last_addr = addr;
    length = 0;
    for (;;)
    {
	jump_analyze_ppc_insn(mem_get_32(intp, last_addr), last_addr, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	assert(length < MAX_TRACE_INSNS);

	addrs[length++] = last_addr;
	last_addr += 4;

	if (num_targets > 0 || !can_fall_through || can_jump_indirectly)
	    break;
    }

#ifdef COLLECT_LIVENESS
    if (can_jump_indirectly)
	*live_cr = *live_xer = *live_gpr = 0xffffffff;
    else
    {
	fragment_hash_entry_t *entry;
	fragment_hash_supplement_t *supplement;

	if (can_fall_through)
	{
	    entry = fragment_hash_get(last_addr + 4, &supplement);
	    if (entry == 0)
		*live_cr = *live_xer = *live_gpr = 0xffffffff;
	    else
	    {
		*live_cr = supplement->live_cr;
		*live_xer = supplement->live_xer;
		*live_gpr = supplement->live_gpr;
	    }
	}
	else
	{
	    *live_cr = *live_xer = *live_gpr = 0;

	    assert(num_targets > 0);
	}

	if (num_targets > 0)
	{
	    assert(num_targets == 1);

	    entry = fragment_hash_get(target, &supplement);

	    if (entry == 0)
		*live_cr = *live_xer = *live_gpr = 0xffffffff;
	    else
	    {
		*live_cr |= supplement->live_cr;
		*live_xer |= supplement->live_xer;
		*live_gpr |= supplement->live_gpr;
	    }
	}
    }

    for (i = length - 1; i >= 0; --i)
    {
	liveness_ppc_insn(mem_get_32(intp, addrs[i]), addrs[i],
			  live_cr, &block_insns[i].killed_cr,
			  live_xer, &block_insns[i].killed_xer,
			  live_gpr, &block_insns[i].killed_gpr);

	block_insns[i].live_cr = *live_cr;
	block_insns[i].live_xer = *live_xer;
	block_insns[i].live_gpr = *live_gpr;
    }
#endif

    return length;
}
#endif

#if defined(EMU_PPC) && defined(DYNAMO_TRACES)
#include "ppc_consumer.c"

#define MAX_ALT_DEPTH          40

void
compute_limited_liveness (interpreter_t *intp, word_32 pc, int max_depth, word_32 *live_cr, word_32 *live_xer)
{
    word_32 needed_cr = 0, needed_xer = 0;
    word_32 dead_cr = 0, dead_xer = 0;
    int i;

    for (i = 0; i < max_depth; ++i)
    {
	word_32 consumed_cr, consumed_xer;
	word_32 killed_cr, killed_xer;
	word_32 insn = mem_get_32(intp, pc);
	int num_targets;
	word_32 target;
	int can_fall_through, can_jump_indirectly;

	consume_ppc_insn(insn, pc, &consumed_cr, &consumed_xer);
	kill_ppc_insn(insn, pc, &killed_cr, &killed_xer);

	needed_cr |= consumed_cr & ~dead_cr;
	needed_xer |= consumed_xer & ~dead_xer;

	dead_cr |= killed_cr;
	dead_xer |= killed_xer;

	jump_analyze_ppc_insn(insn, pc, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	if (can_jump_indirectly)
	    break;

	if (!can_fall_through)
	{
	    assert(num_targets == 1);
	    pc = target;
	}
	else if (num_targets == 0)
	    pc += 4;
	else
	{
	    assert(num_targets == 1);
	    break;
	}
    }

    *live_cr = ~dead_cr | needed_cr;
    *live_xer = ~dead_xer | needed_xer;
}

void
compute_liveness_for_trace (interpreter_t *intp, word_32 *addrs, int length)
{
    word_32 live_cr, live_xer, live_gpr, killed_cr, killed_xer, killed_gpr;
    /* word_32 real_killed_cr, real_killed_xer; */
    int i;
    int num_targets;
    word_32 target;
    int can_fall_through, can_jump_indirectly;

    assert(length <= MAX_DYNAMO_TRACE);

    /* first check the end of the trace */
    jump_analyze_ppc_insn(mem_get_32(intp, addrs[length - 1]), addrs[length - 1], &num_targets, &target, &can_fall_through, &can_jump_indirectly);
    if (can_jump_indirectly)
	live_cr = live_xer = 0xffffffff;
    else
    {
	if (can_fall_through)
	    compute_limited_liveness(intp, addrs[length - 1] + 4, MAX_ALT_DEPTH, &live_cr, &live_xer);
	else
	    live_cr = live_xer = 0;

	if (num_targets > 0)
	{
	    word_32 alt_live_cr, alt_live_xer;

	    assert(num_targets == 1);

	    compute_limited_liveness(intp, target, MAX_ALT_DEPTH, &alt_live_cr, &alt_live_xer);

	    live_cr |= alt_live_cr;
	    live_xer |= alt_live_xer;
	}
    }

    for (i = length - 1; i >= 0; --i)
    {
	word_32 pc = addrs[i], insn = mem_get_32(intp, pc);

	live_gpr = 0xffffffff;
	liveness_ppc_insn(insn, pc, &live_cr, &killed_cr, &live_xer, &killed_xer, &live_gpr, &killed_gpr);
	/* kill_ppc_insn(insn, pc, &real_killed_cr, &real_killed_xer); */

	block_insns[i].killed_cr = killed_cr;
	block_insns[i].killed_xer = killed_xer;

	jump_analyze_ppc_insn(insn, pc, &num_targets, &target, &can_fall_through, &can_jump_indirectly);

	if (can_jump_indirectly)
	    live_cr = live_xer = 0xffffffff;
	else if (num_targets > 0 && can_fall_through && i < length - 1)
	{
	    word_32 alt_pc;
	    word_32 alt_live_cr, alt_live_xer;

	    assert(num_targets == 1);

	    if (addrs[i + 1] == pc + 4)
		alt_pc = target;
	    else
	    {
		assert(addrs[i + 1] == target);

		alt_pc = pc + 4;
	    }

	    compute_limited_liveness(intp, alt_pc, MAX_ALT_DEPTH, &alt_live_cr, &alt_live_xer);

	    printf("alt analysis from %08x: %08x\n", alt_pc, alt_live_cr);

	    live_cr |= alt_live_cr;
	    live_xer |= alt_live_xer;
	}

	block_insns[i].live_cr = live_cr;
	block_insns[i].live_xer = live_xer;
    }
}
#endif

#if defined(EMU_PPC) && defined(COLLECT_LIVENESS)

typedef struct
{
    word_32 foreign_addr;
    word_32 live_cr;
    word_32 live_xer;
    word_32 live_gpr;
} ppc_liveness_info_t;

void
load_liveness_info (void)
{
    FILE *in = fopen("liveness.out", "r");
    ppc_liveness_info_t info;

    if (in == 0)
	return;

    while (fread(&info, sizeof(ppc_liveness_info_t), 1, in) == 1)
    {
	fragment_hash_entry_t entry;
	fragment_hash_supplement_t supplement;

	init_fragment_hash_entry(&entry, &supplement);
	entry.foreign_addr = info.foreign_addr;
	supplement.live_cr = info.live_cr;
	supplement.live_xer = info.live_xer;
	supplement.live_gpr = info.live_gpr;

	fragment_hash_put(info.foreign_addr, &entry, &supplement);
    }

    fclose(in);
}

void
save_liveness_info (void)
{
    FILE *out = fopen("liveness.out", "w");
    int i;

    assert(out != 0);

    for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
	if (fragment_hash_table[i].foreign_addr != (word_32)-1
	    && (fragment_hash_supplement[i].live_cr != 0xffffffff
		|| fragment_hash_supplement[i].live_xer != 0xffffffff
		|| fragment_hash_supplement[i].live_gpr != 0xffffffff))
	{
	    ppc_liveness_info_t info;

	    info.foreign_addr = fragment_hash_table[i].foreign_addr;
	    info.live_cr = fragment_hash_supplement[i].live_cr;
	    info.live_xer = fragment_hash_supplement[i].live_xer;
	    info.live_gpr = fragment_hash_supplement[i].live_gpr;

	    assert(fwrite(&info, sizeof(ppc_liveness_info_t), 1, out) == 1);
	}

    fclose(out);

    /*
      for (i = 0; i < FRAGMENT_HASH_ENTRIES; ++i)
      if (fragment_hash_table[i].foreign_addr != (word_32)-1)
      printf("%08x  cr: %08x xer: %08x gpr: %08x\n", fragment_hash_table[i].foreign_addr,
      fragment_hash_supplement[i].live_cr, fragment_hash_supplement[i].live_xer, fragment_hash_supplement[i].live_gpr);
    */
}

#endif
