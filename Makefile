SRC=main
EXE=cn-asm

all:
	ghc -o $(EXE) $(SRC).hs

.PHONY: clean
clean:
	rm -f $(EXE) $(SRC).o $(SRC).hi *~
