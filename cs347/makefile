AS = nasm
ASFLAGS = -f elf
CC = gcc
CCFLAGS = -Wall -m32
LIBDIR = libs

.SUFFIXES:
.SUFFIXES: .o .asm .cpp .c

.asm.o:
	$(AS) $(ASFLAGS) $<

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
