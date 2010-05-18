.SUFFIXES:
.SUFFIXES: .o .asm .cpp .c

.asm.o:
	$(AS) $(ASFLAGS) $<

AS = nasm
ASFLAGS = -f elf
CC = gcc
CCFLAGS = -Wall -m32
LIBDIR = ../nasm
LIBS = $(LIBDIR)/driver.c $(LIBDIR)/asm_io.o $(LIBDIR)/cdecl.h

all:
	make mstrebl

mstrebl: strebl.o mstrebl.o
	$(CC) $(CCFLAGS) strebl.o mstrebl.o $(LIBS) -o $@

