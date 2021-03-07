
all: 1check.p 1check.n

1check.p: 1check
	pucrunch -c20 1check 1check.p

1check.n: 1check2
	pucrunch -c20 1check2 1check.n


1check: 1check.asm sin.h
	dasm 1check.asm -o1check -v3 -l1check.lst -MSYSTEM=PAL

1check2: 1check.asm sin.h
	dasm 1check.asm -o1check2 -v3 -l1check2.lst -MSYSTEM=NTSC

sin.h: sinus.c
	gcc -o sinus sinus.c -lm ; ./sinus >sin.h

