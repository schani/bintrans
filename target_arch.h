/*
 * target_arch.h
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

#ifdef ARCH_ALPHA

#include "alpha_types.h"

#define NUM_NATIVE_INTEGER_REGS      31
#define NUM_NATIVE_FLOAT_REGS        31

#if defined(EMU_PPC)
#define NUM_FREE_INTEGER_REGS                  20
#elif defined(EMU_I386)
#define NUM_FREE_INTEGER_REGS                   9
#endif

#if defined(EMU_PPC) && defined(FAST_PPC_FPR)
#define NUM_FREE_FLOAT_REGS     3
#else
#define NUM_FREE_FLOAT_REGS    14
#endif

#endif /* ARCH_ALPHA */

#ifdef ARCH_I386
#include "i386_types.h"
#endif
