
all: colscr.p colscr.n

colscr.p: colscr
	pucrunch -c20 colscr colscr.p

colscr.n: colscr2
	pucrunch -c20 colscr2 colscr.n


colscr: colscr.asm sin.h
	dasm colscr.asm -ocolscr -v3 -lcolscr.lst -MSYSTEM=PAL

colscr2: colscr.asm sin.h
	dasm colscr.asm -ocolscr2 -v3 -lcolscr2.lst -MSYSTEM=NTSC

sin.h: sinus.c
	gcc -o sinus sinus.c -lm ; ./sinus >sin.h

