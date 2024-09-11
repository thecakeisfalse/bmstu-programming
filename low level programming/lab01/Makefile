Y_FLAGS := -f elf64
YD_FLAGS := -g dwarf2 -l main.lst

all: link

link: compile
	@ ld main.o -o main

compile:
	@ yasm ${Y_FLAGS} ${YD_FLAGS} main.asm
