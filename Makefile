#EMU = I386
EMU = PPC
#LOCATION = -DCOMPLANG

ARCH = -DARCH_ALPHA
#ARCH = -DARCH_I386
ASM_OBJS = alpha_asm.o
COMPILER_OBJS = compiler.o
#DEFINES = -DINTERPRETER
#DEFINES = -DINTERPRETER -DEMULATED_MEM
#DEFINES = -DCROSSDEBUGGER -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOMPILER -DCOLLECT_STATS -DDUMP_CODE
#DEFINES = -DCOMPILER -DCOLLECT_STATS -DPERIODIC_STAT_DUMP
#DEFINES = -DDEBUGGER -DEMULATED_MEM
#DEFINES = -DCOMPILER -DDUMP_CODE
DEFINES = -O -DCOMPILER
# -DCOLLECT_STATS
# -DMEASURE_TIME
#DEFINES = -O -fno-inline -DCOMPILER
# -DMEASURE_TIME
# -DCOLLECT_STATS
#
#
# 
# 

ifeq ($(EMU),PPC)
EMU_DEFS = -DEMU_PPC -DEMU_BIG_ENDIAN -DDIFFERENT_BYTEORDER
EMU_OBJS = unaligned.o
endif
ifeq ($(EMU),I386)
EMU_DEFS = -DEMU_I386 -DEMU_LITTLE_ENDIAN
EMU_OBJS = i386.o \
		i386_add_rm32_simm8_compiler.o i386_add_rm32_r32_compiler.o \
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

CFLAGS = $(DEFINES) $(ARCH) $(EMU_DEFS) $(LOCATION)

bintrans : ppc.o mm.o $(EMU_OBJS) $(COMPILER_OBJS) $(ASM_OBJS) 
	gcc -o bintrans ppc.o mm.o $(EMU_OBJS) $(COMPILER_OBJS) $(ASM_OBJS)

ppc.o : ppc.c ppc_interpreter.c ppc_disassembler.c alpha_types.h bintrans.h
	gcc $(CFLAGS) -Wall -g -c ppc.c

i386.o : i386.c i386_interpreter.c i386_disassembler.c i386_livenesser.c bintrans.h
	gcc $(CFLAGS) -Wall -g -c i386.c

compiler.o : compiler.c alpha_composer.h ppc_compiler.c i386_compiler.c alpha_disassembler.c alpha_types.h bintrans.h
	gcc $(CFLAGS) -Wall -g -c compiler.c

mm.o : mm.c bintrans.h alpha_types.h ppc_defines.h
	gcc $(CFLAGS) -Wall -g -c mm.c

unaligned.o : unaligned.c bintrans.h
	gcc $(CFLAGS) -Wall -g -c unaligned.c

alpha_asm.o : alpha_asm.S
	gcc $(DEFINES) $(EMU_DEFS) -g -c alpha_asm.S

i386_%_compiler.o : i386_%_compiler.c bintrans.h alpha_composer.h
	gcc $(CFLAGS) -g -c $<

elfer : elfer.c
	gcc -o elfer elfer.c

clean :
	rm -f *~ core *.o bintrans
