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
	make encrypt
	make htod
	make dtoh

encrypt: encrypt.o
	$(CC) $(CCFLAGS) $? $(LIBS) -o $@

htod: htod.o
	$(CC) $(CCFLAGS) $? $(LIBS) -o $@

dtoh: dtoh.o
	$(CC) $(CCFLAGS) $? $(LIBS) -o $@
