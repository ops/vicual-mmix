all: wave.p wave.n
# dowave

wavec: wavec.c
	gcc -O -o wavec wavec.c -lm

dowave: wavec
	./wavec


wave.p: wave
	pucrunch -c20 wave wave.p

wave.n: wave2
	pucrunch -c20 wave2 wave.n


wave: wave.asm sin.h
	dasm wave.asm -owave -v3 -lwave.lst -MSYSTEM=PAL

wave2: wave.asm sin.h
	dasm wave.asm -owave2 -v3 -lwave2.lst -MSYSTEM=NTSC

sin.h: sinus.c
	gcc -o sinus sinus.c -lm ; ./sinus >sin.h
