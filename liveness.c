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
