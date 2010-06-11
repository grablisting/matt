CC=gnatmake
SRC=src
BIN=bin
OBJ=obj

all:
	make binary_main

binary_main:
	gnatmake -O2 -o $(BIN)/assg4 -D $(OBJ) $(SRC)/binary_main

clean:
	rm $(OBJ)/*.ali $(OBJ)/*.o
