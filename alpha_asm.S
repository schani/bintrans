/*
 * alpha_asm.S
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

#include "config_defs.h"
	
#ifdef NEED_COMPILER
#define v0	$0
#define a0	$16
#define a1	$17
#define a2	$18
#define a3	$19
#define a4	$20
#define a5	$21
#define t8	$22
#define ra	$26
#define pv	$27
#define gp	$29
#define sp	$30

#ifdef EMU_I386
#define LOAD_NATIVE_HOST_REGISTERS	\
	ldl	$1,0(pv)	;\
	ldl	$2,8(pv)	;\
	ldl	$3,16(pv)	;\
	ldl	$4,24(pv)	;\
	ldl	$5,32(pv)	;\
	ldl	$6,40(pv)	;\
	ldl	$7,48(pv)	;\
	ldl	$8,56(pv)	;\
	ldl	$9,64(pv)	;\
	ldl	$10,72(pv)	;\
	ldl	$11,80(pv)	;\
	ldl	$12,88(pv)	;\
	ldl	$13,96(pv)	;\
	ldl	$14,104(pv)
#define STORE_NATIVE_HOST_REGISTERS(o)	\
	stl	$1,0(o)	;\
	stl	$2,8(o)	;\
	stl	$3,16(o)	;\
	stl	$4,24(o)	;\
	stl	$5,32(o)	;\
	stl	$6,40(o)	;\
	stl	$7,48(o)	;\
	stl	$8,56(o)	;\
	stl	$9,64(o)	;\
	stl	$10,72(o)	;\
	stl	$11,80(o)	;\
	stl	$12,88(o)	;\
	stl	$13,96(o)	;\
	stl	$14,104(o)
#else
#define LOAD_NATIVE_HOST_REGISTERS
#define STORE_NATIVE_HOST_REGISTERS(o)
#endif

#ifdef FAST_PPC_FPR
#define LOAD_NATIVE_PPC_FPRS	\
	ldt	$f0,296(pv)	;\
	ldt	$f1,304(pv)	;\
	ldt	$f2,312(pv)	;\
	ldt	$f3,320(pv)	;\
	ldt	$f4,328(pv)	;\
	ldt	$f5,336(pv)	;\
	ldt	$f6,344(pv)	;\
	ldt	$f7,352(pv)	;\
	ldt	$f8,360(pv)	;\
	ldt	$f9,368(pv)	;\
	ldt	$f10,376(pv)	;\
	ldt	$f11,384(pv)	;\
	ldt	$f12,392(pv)	;\
	ldt	$f13,400(pv)	;\
	ldt	$f14,440(pv)	;\
	ldt	$f15,448(pv)	;\
	ldt	$f16,456(pv)	;\
	ldt	$f17,464(pv)	;\
	ldt	$f18,472(pv)	;\
	ldt	$f19,480(pv)	;\
	ldt	$f20,488(pv)	;\
	ldt	$f21,496(pv)	;\
	ldt	$f22,504(pv)	;\
	ldt	$f23,512(pv)	;\
	ldt	$f24,520(pv)	;\
	ldt	$f25,528(pv)	;\
	ldt	$f26,536(pv)	;\
	ldt	$f27,544(pv)
#define STORE_NATIVE_PPC_FPRS(o)	\
	stt	$f0,296(o)	;\
	stt	$f1,304(o)	;\
	stt	$f2,312(o)	;\
	stt	$f3,320(o)	;\
	stt	$f4,328(o)	;\
	stt	$f5,336(o)	;\
	stt	$f6,344(o)	;\
	stt	$f7,352(o)	;\
	stt	$f8,360(o)	;\
	stt	$f9,368(o)	;\
	stt	$f10,376(o)	;\
	stt	$f11,384(o)	;\
	stt	$f12,392(o)	;\
	stt	$f13,400(o)	;\
	stt	$f14,440(o)	;\
	stt	$f15,448(o)	;\
	stt	$f16,456(o)	;\
	stt	$f17,464(o)	;\
	stt	$f18,472(o)	;\
	stt	$f19,480(o)	;\
	stt	$f20,488(o)	;\
	stt	$f21,496(o)	;\
	stt	$f22,504(o)	;\
	stt	$f23,512(o)	;\
	stt	$f24,520(o)	;\
	stt	$f25,528(o)	;\
	stt	$f26,536(o)	;\
	stt	$f27,544(o)
#else
#define LOAD_NATIVE_PPC_FPRS
#define STORE_NATIVE_PPC_FPRS(o)
#endif

	.set	noat
	.set	noreorder
	
	# extern
	.globl	provide_fragment_and_patch
	.globl	provide_fragment
	.globl	constant_area
	.globl	leading_zeros
	.globl	handle_compiler_system_call
	.globl	fragment_hash_table

	
.text
	.align	4
	.globl	flush_icache	
	.ent	flush_icache
flush_icache:
	call_pal	0x86	# i-stream memory barrier
	ret	$31,(ra)
	.end	flush_icache


	# we enter here from the generated machine code.
	# ra holds the address of the instruction directly
	# following the jump.  we call a c function which
	# looks up the target of the jump.  if it is found,
	# the jump is patched.  if it isnt, the code for the
	# branch target is generated.  either way, the function
	# returns the native target address.
	.align	4
	.globl	direct_dispatcher
	.ent	direct_dispatcher
direct_dispatcher:
	ldgp	gp,0(pv)

#ifdef EMU_I386
	lda	a0,constant_area
	STORE_NATIVE_HOST_REGISTERS(a0)
#else
#ifdef FAST_PPC_FPR
	lda	a0,constant_area
	STORE_NATIVE_PPC_FPRS(a0)
#endif
#endif

	mov	ra,a0
	jsr	ra,provide_fragment_and_patch
	ldgp	gp,0(ra)
	# v0 now holds the native target address

	lda	pv,constant_area

	LOAD_NATIVE_HOST_REGISTERS
	LOAD_NATIVE_PPC_FPRS

	jmp	$31,(v0)
	.end	direct_dispatcher


	# we enter here from the generated machine code.
	# a0 holds the address of the jump target.  we call
	# a c function which looks up the native target
	# address.  if it isnt found, that function generates
	# the code.  either way, it returns the address of
	# the native code.
	.align	4
	.globl	indirect_dispatcher
	.ent	indirect_dispatcher
indirect_dispatcher:
	ldgp	gp,0(pv)

#ifdef COMPILER
	lda	a1,fragment_hash_table
	ldiq	a2,16383*4
	and	a0,a2,a2
#if defined(PROFILE_FRAGMENTS) && defined(PROFILE_LOOPS)
	sll	a2,4,a3
	addq	a3,a1,a1
#elif defined(PROFILE_FRAGMENTS)
	s8addq	a2,a1,a1
#else
	s4addq	a2,a1,a1
#endif
	ldq	a2,8(a1)	# a2 = native_addr
	beq	a2,call_provide_fragment
	ldl	a3,0(a1)	# a3 = foreign_addr
	cmpeq	a3,a0,a4
	beq	a4,call_provide_fragment
	lda	pv,constant_area
	jmp	$31,(a2)

call_provide_fragment:
#endif
#ifdef EMU_I386
	lda	a1,constant_area
	STORE_NATIVE_HOST_REGISTERS(a1)
#else
#ifdef FAST_PPC_FPR
	lda	a1,constant_area
	STORE_NATIVE_PPC_FPRS(a1)
#endif	
#endif
	jsr	ra,provide_fragment
	ldgp	gp,0(ra)
	# v0 now holds the native target address

	lda	pv,constant_area

	LOAD_NATIVE_HOST_REGISTERS

	jmp	$31,(v0)
	.end	indirect_dispatcher


	# this is easy.  we enter here from generated machine
	# code.  we just invoke the system call handler.  then
	# we restore some registers and return to the generated
	# code.
	.align	4
	.globl	system_call_entry
	.ent	system_call_entry
system_call_entry:
	ldgp	gp,0(pv)
	lda	sp,-16(sp)
	stq	ra,0(sp)

#ifdef EMU_I386
	lda	a0,constant_area
	STORE_NATIVE_HOST_REGISTERS(a0)
#else
#ifdef FAST_PPC_FPR
	lda	a0,constant_area
	STORE_NATIVE_PPC_FPRS(a0)
#endif
#endif
	jsr	ra,handle_compiler_system_call
	ldgp	gp,0(ra)

	lda	pv,constant_area

	LOAD_NATIVE_HOST_REGISTERS
	LOAD_NATIVE_PPC_FPRS

	ldq	ra,0(sp)
	lda	sp,16(sp)
	ret	$31,(ra)
	.end	system_call_entry


	# this is where execution of generated code begins.
	# in a0 we get the address of the generated code.  we load
	# some registers and then jump there, never returning.
	.align	4
	.globl	start_execution
	.ent	start_execution
start_execution:
	ldgp	gp,0(pv)

	lda	pv,constant_area

	LOAD_NATIVE_HOST_REGISTERS
	LOAD_NATIVE_PPC_FPRS

	jmp	$31,(a0)
	.end	start_execution

	# this calls a c function.  the address of the c function is
	# passed in $0.  arguments to the c function are passed in the
	# standard argument registers.  the return value is passed back
	# in $0.
	.align	4
	.globl	c_stub
	.ent	c_stub
c_stub:
	ldgp	gp,0(pv)
	lda	sp,-192(sp)
	stq	ra,0(sp)
#ifdef EMU_I386
	lda	t8,constant_area
	STORE_NATIVE_HOST_REGISTERS(t8)
#else
	stq	$1,8(sp)
	stq	$2,16(sp)
	stq	$3,24(sp)
	stq	$4,32(sp)
	stq	$5,40(sp)
	stq	$6,48(sp)
	stq	$7,56(sp)
	stq	$8,64(sp)
	stq	$18,72(sp)
	stq	$19,80(sp)
	stq	$20,88(sp)
	stq	$21,96(sp)
	stq	$22,104(sp)
	stq	$23,112(sp)
	stq	$24,120(sp)
	stq	$25,128(sp)
#ifdef FAST_PPC_FPR
	lda	t8,constant_area
	STORE_NATIVE_PPC_FPRS(t8)
#else
	stt	$f10,136(sp)
	stt	$f11,144(sp)
	stt	$f12,152(sp)
	stt	$f13,160(sp)
	stt	$f14,168(sp)
	stt	$f15,176(sp)
#endif
#endif

	mov	$0,pv
	jsr	ra,(pv)
	ldgp	gp,0(ra)

	lda	pv,constant_area

#ifdef EMU_I386
	LOAD_NATIVE_HOST_REGISTERS
#else
	ldq	$1,8(sp)
	ldq	$2,16(sp)
	ldq	$3,24(sp)
	ldq	$4,32(sp)
	ldq	$5,40(sp)
	ldq	$6,48(sp)
	ldq	$7,56(sp)
	ldq	$8,64(sp)
	ldq	$18,72(sp)
	ldq	$19,80(sp)
	ldq	$20,88(sp)
	ldq	$21,96(sp)
	ldq	$22,104(sp)
	ldq	$23,112(sp)
	ldq	$24,120(sp)
	ldq	$25,128(sp)
#ifdef FAST_PPC_FPR
	LOAD_NATIVE_PPC_FPRS
#else
	ldt	$f10,136(sp)
	ldt	$f11,144(sp)
	ldt	$f12,152(sp)
	ldt	$f13,160(sp)
	ldt	$f14,168(sp)
	ldt	$f15,176(sp)
#endif
#endif
	ldq	ra,0(sp)
	lda	sp,192(sp)
	ret	$31,(ra)
	.end	c_stub
#endif
