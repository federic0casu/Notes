SRC = cat.c
ASM = _syscall.S
OUT = cat

CC  = gcc

all: $(SRC)
	$(CC) $(SRC) -o $(OUT)

assembly: $(SRC) $(ASM)
	$(CC) $(SRC) $(ASM) -o $(OUT)

clean:
	rm -rf $(OUT)
