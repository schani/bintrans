interp : ppc.c interpreter.c
	gcc -Wall -g -o interp ppc.c

elfer : elfer.c
	gcc -o elfer elfer.c
