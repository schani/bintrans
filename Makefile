#EMU = I386
EMU = PPC
LOCATION = -DCOMPLANG

#MODE = INTERPRETER
MODE = COMPILER
#MODE = DEBUGGER
#MODE = CROSSDEBUGGER

ARCH = -DARCH_ALPHA
#ARCH = -DARCH_I386
#ARCH = -DARCH_PPC

ASM_OBJS = alpha_asm.o
#ASM_OBJS = ppc_asm.o

COMPILER_OBJS = compiler.o

#DIET = diet
#LDOPTS = -Wl,-Ttext,0x70000000

#DEFINES = -DUSE_HAND_TRANSLATOR -DCOLLECT_STATS -DDYNAMO_TRACES -DDYNAMO_THRESHOLD=50 -DSYNC_BLOCKS
#-DDUMP_CODE
DEFINES = -DUSE_HAND_TRANSLATOR -DCOLLECT_STATS -DCOLLECT_LIVENESS -DLIVENESS_DEPTH=0
#-DCOUNT_INSNS -DCOUNT_LOAD_STORES
#-DDUMP_CFG
#-DDUMP_CODE
#-DSYNC_BLOCKS
#-DMEASURE_TIME
#DEFINES = -DUSE_HAND_TRANSLATOR -DCOLLECT_STATS -DDUMP_CODE -DSYNC_BLOCKS
#DEFINES = -DUSE_HAND_TRANSLATOR -DPROFILE_LOOPS -DCOMPILER_THRESHOLD=50 -DCOLLECT_STATS -DCOUNT_INSNS
# -DDUMP_CODE
# 
#  -DFAST_PPC_FPR
#  -DCOLLECT_PPC_FPR_STATS
#DEFINES = -DUSE_HAND_TRANSLATOR -DDUMP_CODE
#DEFINES =
#DEFINES = -O -DPROFILE_LOOPS -DPROFILE_FRAGMENTS -DCOLLECT_STATS
#DEFINES = -DEMULATED_MEM
#DEFINES = -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOLLECT_STATS -DPERIODIC_STAT_DUMP
#DEFINES = -DDUMP_CODE
#DEFINES = -O
#DEFINES = -DCOMPILER_THRESHOLD=20 -DCOLLECT_STATS
#DEFINES = -O -DCOLLECT_STATS
#-DMEASURE_TIME
#-DCOUNT_INSNS
#-DDUMP_CODE
# -DCOLLECT_STATS
# 
#DEFINES = -O -fno-inline
# -DMEASURE_TIME
# -DCOLLECT_STATS
#
#
# 
# 

ifeq ($(EMU),PPC)
EMU_DEFS = -DEMU_PPC -DEMU_BIG_ENDIAN
EMU_OBJS = unaligned.o
endif
ifeq ($(EMU),I386)
EMU_DEFS = -DEMU_I386 -DEMU_LITTLE_ENDIAN
EMU_OBJS = i386.o
XXX_COMPILER_OBJS += i386_add_rm32_simm8_compiler.o i386_add_rm32_r32_compiler.o \
		i386_inc_pr32_compiler.o i386_inc_rm32_compiler.o \
		i386_lea_r32_rm32_compiler.o \
		i386_mov_eax_moffs32_compiler.o i386_mov_moffs32_eax_compiler.o i386_mov_rm32_imm32_compiler.o i386_mov_rm32_r32_compiler.o \
			i386_mov_r32_rm32_compiler.o \
		i386_pop_pr32_compiler.o i386_pop_m32_compiler.o \
		i386_push_imm32_compiler.o i386_push_pr32_compiler.o i386_push_simm8_compiler.o i386_push_rm32_compiler.o \
		i386_sub_rm32_r32_compiler.o \
		i386_xor_al_imm8_compiler.o i386_xor_ax_imm16_compiler.o i386_xor_eax_imm32_compiler.o i386_xor_rm32_imm32_compiler.o \
			i386_xor_rm32_simm8_compiler.o i386_xor_rm32_r32_compiler.o i386_xor_r32_rm32_compiler.o
endif

ifeq ($(MODE),INTERPRETER)
MODE_DEFS = -DINTERPRETER
OBJS = $(EMU_OBJS)
endif
ifeq ($(MODE),COMPILER)
MODE_DEFS = -DCOMPILER
OBJS = $(EMU_OBJS) $(COMPILER_OBJS) $(ASM_OBJS)
endif
ifeq ($(MODE),DEBUGGER)
MODE_DEFS = -DDEBUGGER
OBJS = $(EMU_OBJS)
endif
ifeq ($(MODE),CROSSDEBUGGER)
MODE_DEFS = -DCROSSDEBUGGER
OBJS = $(EMU_OBJS) $(COMPILER_OBJS) $(ASM_OBJS)
endif

CFLAGS = $(MODE_DEFS) $(DEFINES) $(ARCH) $(EMU_DEFS) $(LOCATION) -fno-inline

all : bintrans dump_liveness undump_liveness convert_liveness

bintrans : ppc.o mm.o fragment_hash.o loops.o liveness.o lispreader.o $(OBJS)
	$(DIET) gcc $(LDOPTS) -o bintrans ppc.o mm.o fragment_hash.o loops.o liveness.o lispreader.o $(OBJS)
#	gcc -o bintrans ppc.o mm.o fragment_hash.o loops.o liveness.o lispreader.o $(OBJS)

dump_liveness : dump_liveness.c bintrans.h compiler.h
	$(DIET) gcc $(CFLAGS) -Wall -o dump_liveness dump_liveness.c

undump_liveness : undump_liveness.c bintrans.h compiler.h
	$(DIET) gcc $(CFLAGS) -Wall -o undump_liveness undump_liveness.c

convert_liveness : convert_liveness.c bintrans.h compiler.h
	$(DIET) gcc $(CFLAGS) -Wall -o convert_liveness convert_liveness.c

ppc.o : ppc.c ppc_interpreter.c ppc_disassembler.c alpha_types.h bintrans.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c ppc.c

i386.o : i386.c i386_interpreter.c i386_disassembler.c i386_livenesser.c bintrans.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c i386.c

compiler.o : compiler.c alpha_composer.h ppc_to_alpha_compiler.c ppc_jump_analyzer.c i386_compiler.c i386_to_ppc_compiler.c alpha_disassembler.c alpha_types.h bintrans.h fragment_hash.h compiler.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c compiler.c

mm.o : mm.c bintrans.h alpha_types.h ppc_defines.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c mm.c

unaligned.o : unaligned.c bintrans.h compiler.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c unaligned.c

fragment_hash.o : fragment_hash.c fragment_hash.h bintrans.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c fragment_hash.c

loops.o : loops.c fragment_hash.h bintrans.h compiler.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c loops.c

liveness.o : liveness.c bintrans.h
	$(DIET) gcc $(CFLAGS) -Wall -g -c liveness.c

lispreader.o : lispreader.c lispreader.h
	$(DIET) gcc $(CFLAGS) -I. -Wall -g -c lispreader.c

alpha_asm.o : alpha_asm.S Makefile
	$(DIET) gcc $(MODE_DEFS) $(DEFINES) $(EMU_DEFS) -g -c alpha_asm.S

ppc_asm.o : ppc_asm.S Makefile
	$(DIET) gcc $(MODE_DEFS) $(DEFINES) $(EMU_DEFS) -g -c ppc_asm.S

i386_%_compiler.o : i386_%_compiler.c bintrans.h alpha_composer.h
	$(DIET) gcc $(CFLAGS) -g -c $<

elfer : elfer.c
	gcc -o elfer elfer.c

clean :
	rm -f *~ core *.o bintrans
