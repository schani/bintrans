#define v0	$0
#define a0	$16
#define ra	$26
#define pv	$27
#define gp	$29
#define sp	$30

	.set	noat
	.set	noreorder
	
	# extern
	.globl	provide_fragment_and_patch
	.globl	provide_fragment
	.globl	constant_area
	.globl	register_save_area
	.globl	leading_zeros
	.globl	handle_compiler_system_call

	
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

	mov	ra,a0
	jsr	ra,provide_fragment_and_patch
	ldgp	gp,0(ra)
	# v0 now holds the native target address

	lda	pv,constant_area
	lda	$6,register_save_area

	jmp	$31,(v0)
	.end	direct_dispatcher


	# we enter here from the generated machine code.
	# $7 holds the address of the jump target.  we call
	# a c function which looks up the native target
	# address.  if it isnt found, that function generates
	# the code.  either way, it returns the address of
	# the native code.
	.align	4
	.globl	indirect_dispatcher
	.ent	indirect_dispatcher
indirect_dispatcher:
	ldgp	gp,0(pv)

	mov	$7,a0
	jsr	ra,provide_fragment
	ldgp	gp,0(ra)
	# v0 now holds the native target address

	lda	pv,constant_area
	lda	$6,register_save_area

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

	jsr	ra,handle_compiler_system_call
	ldgp	gp,0(ra)

	lda	pv,constant_area
	lda	$6,register_save_area

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
	lda	$6,register_save_area

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
	lda	sp,-96(sp)
	stq	ra,0(sp)
	stq	$1,8(sp)
	stq	$2,16(sp)
	stq	$3,24(sp)
	stq	$4,32(sp)
	stq	$5,40(sp)
	stt	$f1,48(sp)
	stt	$f2,56(sp)
	stt	$f3,64(sp)
	stt	$f4,72(sp)
	stt	$f5,80(sp)

	mov	$0,pv
	jsr	ra,(pv)
	ldgp	gp,0(ra)

	lda	pv,constant_area
	lda	$6,register_save_area

	ldq	ra,0(sp)
	ldq	$1,8(sp)
	ldq	$2,16(sp)
	ldq	$3,24(sp)
	ldq	$4,32(sp)
	ldq	$5,40(sp)
	ldt	$f1,48(sp)
	ldt	$f2,56(sp)
	ldt	$f3,64(sp)
	ldt	$f4,72(sp)
	ldt	$f5,80(sp)
	ret	$31,(ra)
	.end	c_stub
