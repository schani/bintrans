/*
 * ppc_asm.S
 *
 * bintrans
 *
 * Copyright (C) 2002 Mark Probst
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

#define eax 14
#define ecx 15
#define edx 16
#define ebx 17
#define esp 18
#define ebp 19
#define esi 20
#define edi 21

#define LOAD_NATIVE_HOST_REGISTERS	\
	lwz	14,0(31)	;\
	lwz	15,4(31)	;\
	lwz	16,8(31)	;\
	lwz	17,12(31)	;\
	lwz	18,16(31)	;\
	lwz	19,20(31)	;\
	lwz	20,24(31)	;\
	lwz	21,28(31)	;\
	lwz	22,32(31)	;\
	lhz	23,36(31)	;\
				\
	mfcr	9		;\
	lis	8,0xf000	;\
	andc	9,9,8		;\
	lwz	8,44(31) /*zf*/	;\
	insrwi	9,8,1,2		;\
	lwz	8,48(31) /*sf*/	;\
	insrwi	9,8,1,0		;\
	mtcr	9		;\
				\
	mfxer	9		;\
	lis	8,0x6000	;\
	andc	9,9,8		;\
	lwz	8,40(31) /*cf*/	;\
	insrwi	9,8,1,2		;\
	lwz	8,52(31) /*of*/;\
	insrwi	9,8,1,1		;\
	mtxer	9

#define STORE_NATIVE_HOST_REGISTERS	\
	stw	14,0(31)	;\
	stw	15,4(31)	;\
	stw	16,8(31)	;\
	stw	17,12(31)	;\
	stw	18,16(31)	;\
	stw	19,20(31)	;\
	stw	20,24(31)	;\
	stw	21,28(31)	;\
	stw	22,32(31)	;\
	sth	23,36(31)	;\
				\
	mfcr	9		;\
	extrwi	8,9,1,2		;\
	stw	8,44(31)	;\
	extrwi	8,9,1,0		;\
	stw	8,48(31)	;\
				\
	mfxer	9		;\
	extrwi	8,9,1,2		;\
	stw	8,40(31)	;\
	extrwi	8,9,1,1		;\
	stw	8,52(31)

	.section	".text"

	# r3 ... first address to flush
	# r4 ... address after last address to flush
	.align	2
	.globl	flush_icache
flush_icache:
	clrrwi	3,3,5
	addi	4,4,31
	clrrwi	4,4,5
	subf	5,3,4

	mr	6,5
dcbst_loop:
	dcbst	3,6
	addic.	6,6,-32
	bge	dcbst_loop

	sync

	mr	6,5
icbi_loop:
	icbi	3,6
	addic.	6,6,-32
	bge	icbi_loop

	sync
	isync
	blr


	# lr holds the address following the jump to this
	# routine.  we call a c function which looks up the
	# target.  it returns with the native address which
	# we have to jump to.
	.align	2
	.globl	direct_dispatcher
direct_dispatcher:
	STORE_NATIVE_HOST_REGISTERS
	mflr	3
	bl	provide_fragment_and_patch
	# r3 now holds the native target address

	li	0,0
	LOAD_NATIVE_HOST_REGISTERS
	mtlr	3
	blr


	# r3 contains the foreign target address.  we call
	# a c function which returns the native address.
	.align	2	
	.globl	indirect_dispatcher
indirect_dispatcher:
#ifdef COMPILER
	lis	8,fragment_hash_table@ha
	la	8,fragment_hash_table@l(8)
	rlwinm	9,3,2,12,27
	add	9,8,9
	lwz	8,8(9)		# r8 = native_addr
	cmpwi	1,8,0
	mtlr	8
	beq	1,call_provide_fragment
	lwz	7,0(9)		# r7 = foreign_addr
	cmpw	1,3,7
	beqlr	1
#endif

call_provide_fragment:
	STORE_NATIVE_HOST_REGISTERS
	bl	provide_fragment
	# r3 now holds the native target address

	li	0,0
	LOAD_NATIVE_HOST_REGISTERS
	mtlr	3
	blr


	.align	2
	.globl	system_call_entry
system_call_entry:
	mflr	30
	STORE_NATIVE_HOST_REGISTERS
	bl	handle_compiler_system_call

	li	0,0
	LOAD_NATIVE_HOST_REGISTERS
	mtlr	30
	blr


	.align	2
	.globl	isync_entry
isync_entry:
	blr


	.align	2
	.globl	c_stub
c_stub:
	blr


	# r3 holds the address of the instruction to be interpreted.
	.align	2
	.globl	interpreter_handle
interpreter_handle:
	mflr	30
	STORE_NATIVE_HOST_REGISTERS
	bl	handle_insn_in_interpreter

	li	0,0
	LOAD_NATIVE_HOST_REGISTERS
	mtlr	30
	blr


	.align	2
	.globl	repe_cmpsb_entry
repe_cmpsb_entry:
	cmpwi	ecx,0
	beqlr			# finish if ecx == 0
	addi	esi,esi,-1
	addi	edi,edi,-1
repe_cmpsb_loop:
	lbzu	8,1(esi)	# 8 = mem(++esi)
	lbzu	9,1(edi)	# 9 = mem(++edi)
	addic.	ecx,ecx,-1	# --ecx, ecx == 0
	cmpw	1,8,9		# src == dst
	crorc	2,2,6		# ecx == 0 || src == dst
	bne	repe_cmpsb_loop	# repeat unless ecx == 0 || src == dst
	addi	esi,esi,1
	addi	edi,edi,1
	slwi	8,8,24
	slwi	9,9,24
	nor	7,9,9		# calc cf
	addc	7,8,7		# calc cf
	subfo.	7,8,9		# calc sf,zf,of
	blr


	.align	2
	.globl	rep_movsb_entry
rep_movsb_entry:
	cmpwi	ecx,0
	beqlr			# finish if ecx == 0
	mtctr	ecx
	addi	esi,esi,-1
	addi	edi,edi,-1
rep_movsb_loop:
	lbzu	9,1(esi)
	stbu	9,1(edi)
	bdnz	rep_movsb_loop
	li	ecx,0
	addi	esi,esi,1
	addi	edi,edi,1
	blr

	
	.align	2
	.globl	rep_movsd_entry
rep_movsd_entry:
	cmpwi	ecx,0
	beqlr			# finish if ecx == 0
	mtctr	ecx
	addi	esi,esi,-4
	addi	edi,edi,-4
rep_movsd_loop:
	lwzu	9,4(esi)
	stwu	9,4(edi)
	bdnz	rep_movsd_loop
	li	ecx,0
	addi	esi,esi,4
	addi	edi,edi,4
	blr


	.align	2
	.globl	repne_scasb_entry
repne_scasb_entry:
	cmpwi	ecx,0
	beqlr			# finish if ecx == 0
	andi.	8,eax,0xff	# 8 = al
	addi	edi,edi,-1
repne_scasb_loop:
	lbzu	9,1(edi)	# 9 = mem(++edi)
	addic.	ecx,ecx,-1	# --ecx	,  ecx == 0 ?
	cmpw	1,9,8		# tmp == al ?
	cror	2,6,2		# tmp == al || ecx == 0
	bne	repne_scasb_loop	# repeat unless tmp == al || ecx == 0
	addi	edi,edi,1
	slwi	9,9,24
	slwi	8,8,24
	subf.	7,9,8		# set sf,zf
	neg	8,8
	addc	7,8,9		# set cf
	blr


	.align	2
	.globl	rep_stosb_entry
rep_stosb_entry:
	cmpwi	ecx,0
	beqlr			# finish if ecx == 0
	mtctr	ecx		# repeat ecx times
	addi	edi,edi,-1
rep_stosb_loop:
	stbu	eax,1(edi)
	bdnz	rep_stosb_loop
	li	ecx,0
	addi	edi,edi,1
	blr


	.align	2
	.globl	rep_stosd_entry
rep_stosd_entry:
	cmpwi	ecx,0
	beqlr			# finish if ecx == 0
	mtctr	ecx		# repeat ecx times
	addi	edi,edi,-4
rep_stosd_loop:
	stwu	eax,4(edi)
	bdnz	rep_stosd_loop
	li	ecx,0
	addi	edi,edi,4
	blr


	# r3 contains the address of the generated code we
	# have to jump to.  the generated code never returns.
	.align	2
	.globl	start_execution
start_execution:
	lis 31,constant_area@ha
	la 31,constant_area@l(31)

	li	0,0
	LOAD_NATIVE_HOST_REGISTERS
	mtlr	3
	blr
