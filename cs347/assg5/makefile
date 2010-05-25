.SUFFIXES:
.SUFFIXES: .o .asm .cpp .c

.asm.o:
	$(AS) $(ASFLAGS) $<

.c.o:
	$(CC) $(CCFLAGS) -c $<

AS = nasm
ASFLAGS = -f elf
CC = gcc
CCFLAGS = -Wall -m32

all:
	make assg5

assg5: driver.o assg5.o asm_io.o
	$(CC) $(CCFLAGS) -o $@ $?

asm_io.o : asm_io.asm
	$(AS) $(ASFLAGS) -d ELF_TYPE asm_io.asm
	
clean:
	rm *.o
