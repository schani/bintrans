EMU = I386
#EMU = PPC
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
DEFINES = -DDEBUGGER -DEMULATED_MEM
#DEFINES = -DCOMPILER -DDUMP_CODE
#DEFINES = -DCOMPILER -DCOLLECT_STATS
#DEFINES = -O -DCOMPILER
# -DCOLLECT_STATS
#-DMEASURE_TIME
#
# 
# 

ifeq ($(EMU),PPC)
EMU_DEFS = -DEMU_PPC -DEMU_BIG_ENDIAN -DDIFFERENT_BYTEORDER
EMU_OBJS = unaligned.o
endif
ifeq ($(EMU),I386)
EMU_DEFS = -DEMU_I386 -DEMU_LITTLE_ENDIAN
EMU_OBJS = i386.o
endif

CFLAGS = $(DEFINES) $(ARCH) $(EMU_DEFS) $(LOCATION)

bintrans : ppc.o mm.o $(EMU_OBJS) $(COMPILER_OBJS) $(ASM_OBJS) 
	gcc -o bintrans ppc.o mm.o $(EMU_OBJS) $(COMPILER_OBJS) $(ASM_OBJS)

ppc.o : ppc.c ppc_interpreter.c ppc_disassembler.c alpha_types.h bintrans.h
	gcc $(CFLAGS) -Wall -g -c ppc.c

i386.o : i386.c i386_interpreter.c i386_disassembler.c bintrans.h
	gcc $(CFLAGS) -Wall -g -c i386.c

compiler.o : compiler.c alpha_composer.h ppc_compiler.c alpha_disassembler.c alpha_types.h bintrans.h
	gcc $(CFLAGS) -Wall -g -c compiler.c

mm.o : mm.c bintrans.h alpha_types.h ppc_defines.h
	gcc $(CFLAGS) -Wall -g -c mm.c

unaligned.o : unaligned.c bintrans.h
	gcc $(CFLAGS) -Wall -g -c unaligned.c

alpha_asm.o : alpha_asm.S
	gcc $(DEFINES) -g -c alpha_asm.S

elfer : elfer.c
	gcc -o elfer elfer.c

clean :
	rm -f *~ core *.o bintrans
