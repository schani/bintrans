/*
 * emu_arch.h
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

#if defined(EMU_PPC)
#include "ppc_defines.h"

#ifdef USE_HAND_TRANSLATOR
#undef NUM_EMU_REGISTERS
#define NUM_EMU_REGISTERS (5 + 32 + 32 + 4 + 2)
#endif
#elif defined(EMU_I386)
#include "i386_defines.h"
#else
#error no emulator specified
#endif
