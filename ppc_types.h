/*
 * ppc_types.h
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

typedef unsigned int word_32;
typedef unsigned short word_16;
typedef unsigned long long word_64;
typedef unsigned char word_8;
typedef unsigned int word_1;
typedef unsigned int word_2;
typedef unsigned int word_3;
typedef unsigned int word_4;
typedef unsigned int word_5;
typedef word_8 byte;
typedef signed int sword_32;
typedef signed short sword_16;
typedef signed long long sword_64;
typedef signed char sword_8;

typedef unsigned int addr_t;

#ifdef EMU_LITTLE_ENDIAN
#define DIFFERENT_BYTEORDER
#endif
