/*
 * liveness.c
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

#include <stdio.h>
#include <assert.h>

#include "bintrans.h"
#include "compiler.h"

#ifdef EMU_I386
i386_insn_t block_insns[MAX_BLOCK_INSNS + MAX_AFTER_BRANCH_INSNS];
int num_block_insns = 0;

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

		block_insns[i].addr = intp->pc;

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

#if defined(EMU_PPC) && defined(DYNAMO_TRACES)
#include "ppc_consumer.c"

#define MAX_ALT_DEPTH          40

ppc_insn_t block_insns[MAX_DYNAMO_TRACE];

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
    word_32 live_cr, live_xer, killed_cr, killed_xer;
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

	liveness_ppc_insn(insn, pc, &live_cr, &killed_cr, &live_xer, &killed_xer);
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