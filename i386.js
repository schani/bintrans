/*
 * i386.js
 *
 * bintrans
 *
 * Copyright (C) 2016 Mark Probst
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

var assert = require('assert');

var I386_PREFIX_LOCK               = 0x001;
var I386_PREFIX_REPNE              = 0x002;
var I386_PREFIX_REP                = 0x004;
var I386_PREFIX_SSE                = 0x008;
var I386_PREFIX_CS_OVERRIDE        = 0x010;
var I386_PREFIX_SS_OVERRIDE        = 0x020;
var I386_PREFIX_DS_OVERRIDE        = 0x040;
var I386_PREFIX_ES_OVERRIDE        = 0x080;
var I386_PREFIX_FS_OVERRIDE        = 0x100;
var I386_PREFIX_GS_OVERRIDE        = 0x200;
var I386_PREFIX_OP_SIZE_OVERRIDE   = 0x400;
exports.PREFIX_OP_SIZE_OVERRIDE = I386_PREFIX_OP_SIZE_OVERRIDE;
var I386_PREFIX_ADDR_SIZE_OVERRIDE = 0x800;

var PREFIXES = [
    { prefix: 0xf0, flag: I386_PREFIX_LOCK },
    { prefix: 0x2e, flag: I386_PREFIX_CS_OVERRIDE },
    { prefix: 0x36, flag: I386_PREFIX_SS_OVERRIDE },
    { prefix: 0x3e, flag: I386_PREFIX_DS_OVERRIDE },
    { prefix: 0x26, flag: I386_PREFIX_ES_OVERRIDE },
    { prefix: 0x64, flag: I386_PREFIX_FS_OVERRIDE },
    { prefix: 0x65, flag: I386_PREFIX_GS_OVERRIDE },
    { prefix: 0x66, flag: I386_PREFIX_OP_SIZE_OVERRIDE },
    { prefix: 0x67, flag: I386_PREFIX_ADDR_SIZE_OVERRIDE }
];

function i386_decode_opcode ()
{
    var prefix_flags = 0;
    var opcode;
    
    var b;
    var i;

    for (;;)
    {
	b = this.mem_get_8(this.pc++);
	for (i = 0; i < PREFIXES.length; ++i)
	    if (PREFIXES[i].prefix == b)
		break;

	if (i < PREFIXES.length)
	{
	    assert(!(prefix_flags & PREFIXES[i].flag));
	    prefix_flags |= PREFIXES[i].flag;
	}
	else
	{
	    opcode = b;
	    break;
	}
    }
    //console.log("prefix flags %d  opcode %d", prefix_flags, opcode);
    return { prefix_flags: prefix_flags, opcode: opcode };
}

function i386_decode_modrm ()
{
    var modrm = this.mem_get_8(this.pc++);
    //console.log("modrm %d", modrm);
    return { modrm: modrm, mod: modrm >> 6, reg: (modrm >> 3) & 7, rm: modrm & 7 };
}

function i386_decode_sib (modrm)
{
    var scale, index, base, disp8, disp32;
    var need_sib = 0, need_disp8 = 0, need_disp32 = 0;

    var mod = modrm >> 6;
    var reg = (modrm >> 3) & 7;
    var rm = modrm & 7;

    if (mod == 1)
	need_disp8 = 1;
    else if (mod == 2 || (mod == 0 && rm == 5))
	need_disp32 = 1;

    if (mod != 3 && rm == 4)
	need_sib = 1;

    if (need_sib)
    {
	var sib = this.mem_get_8(this.pc++);
	scale = sib >> 6;
	index = sib >> 3 & 7;
	base = sib & 7;

	if (base == 5 && mod == 0)
	{
	    assert(!need_disp32);
	    need_disp32 = 1;
	}

	if (base == 5)
	    assert(need_disp32 || need_disp8);
    }

    if (need_disp8)
	disp8 = this.mem_get_8(this.pc++);
    else if (need_disp32)
    {
	disp32 = this.mem_get_32_unaligned(this.pc);
	this.pc += 4;
    }

    return { scale: scale, index: index, base: base, disp8: disp8, disp32: disp32 };
}

function i386_decode_imm8 ()
{
    return this.mem_get_8(this.pc++);
}

function i386_decode_imm16 ()
{
    var imm16 = this.mem_get_16_unaligned(this.pc);

    this.pc += 2;

    return imm16;
}

function i386_decode_imm32 ()
{
    var imm32 = this.mem_get_32_unaligned(this.pc);

    this.pc += 4;

    return imm32;
}

function i386_disassemble_r8 (reg)
{
    var names = [ "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh" ];
    assert(reg < 8);
    return names[reg];
}
exports.disassemble_r8 = i386_disassemble_r8;

function i386_disassemble_r16 (reg)
{
    var names = [ "ax", "cx", "dx", "bx", "sp", "bp", "si", "di" ];
    assert(reg < 8);
    return names[reg];
}
exports.disassemble_r16 = i386_disassemble_r16;

function i386_disassemble_r32 (reg)
{
    var names = [ "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi" ];
    assert(reg < 8);
    return names[reg];
}
exports.disassemble_r32 = i386_disassemble_r32;

function i386_disassemble_sib (mod, scale, index, base, disp32)
{
    var output = "";

    if (base == 5 && mod == 0)
	output += "0x" + disp32.toString(16);

    output += "(";

    if (!(base == 5 && mod == 0))
	output += i386_disassemble_r32(base);

    if (index != 4)
    {
	output += ",";
	output += i386_disassemble_r32(index);
	output += "," + (1 << scale).toString(10);
    }

    output += ")";

    return output;
}

function i386_disassemble_ea (mod, rm, scale, index, base, disp8, disp32)
{
    var have_disp;
    var disp;

    if ((mod == 0 && rm == 5) || mod == 2)
    {
	have_disp = 1;
	disp = disp32;
    }
    else if (mod == 1)
    {
	have_disp = 1;
	disp = disp8 | (disp8 & 0x80 ? 0xffffff00 : 0);
    }
    else
	have_disp = 0;

    var output = "";

    if (have_disp)
	output += "0x" + disp.toString(16);

    if (!(mod == 0 && rm == 5))
    {
	if (rm != 4)
	{
	    output += "(";
	    output += i386_disassemble_r32(rm);
	    output += ")";
	}
	else
	    output += i386_disassemble_sib(mod, scale, index, base, disp32);
    }

    return output;
}

exports.disassemble_rm8 = function i386_disassemble_rm8 (mod, rm, scale, index, base, disp8, disp32)
{
    assert(mod < 4);

    if (mod == 0 || mod == 1 || mod == 2)
	return i386_disassemble_ea(mod, rm, scale, index, base, disp8, disp32);
    else
	return i386_disassemble_r8(rm);
};

exports.disassemble_rm16 = function i386_disassemble_rm16 (mod, rm, scale, index, base, disp8, disp32)
{
    assert(mod < 4);

    if (mod == 0 || mod == 1 || mod == 2)
	return i386_disassemble_ea(mod, rm, scale, index, base, disp8, disp32);
    else
	return i386_disassemble_r16(rm);
};

exports.disassemble_rm32 = function i386_disassemble_rm32 (mod, rm, scale, index, base, disp8, disp32)
{
    assert(mod < 4);

    if (mod == 0 || mod == 1 || mod == 2)
	return i386_disassemble_ea(mod, rm, scale, index, base, disp8, disp32);
    else
	return i386_disassemble_r32(rm);
};

function mem_get_8 (addr)
{
    return this.mem.getUint8(addr);
}

function mem_get_16_unaligned (addr)
{
    return this.mem.getUint16(addr, true);
}

function mem_get_32_unaligned (addr)
{
    return this.mem.getUint32(addr, true);
}

exports.new_interpreter = function new_interpreter ()
{
    var buffer = new ArrayBuffer(1024 * 1024);
    var view = new DataView(buffer);

    return {
	mem: view,
	pc: 0,
	decode_opcode: i386_decode_opcode,
	decode_modrm: i386_decode_modrm,
	decode_sib: i386_decode_sib,
	decode_imm8: i386_decode_imm8,
	decode_imm16: i386_decode_imm16,
	decode_imm32: i386_decode_imm32,
	mem_get_8: mem_get_8,
	mem_get_16_unaligned: mem_get_16_unaligned,
	mem_get_32_unaligned: mem_get_32_unaligned
    };
};
