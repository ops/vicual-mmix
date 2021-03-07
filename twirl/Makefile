
all: twirl.p twirl.n

twirl.p: twirl
	pucrunch -c20 twirl twirl.p

twirl.n: twirl2
	pucrunch -c20 twirl2 twirl.n

twirl: twirl.asm sin.h
	dasm twirl.asm -otwirl -v3 -ltwirl.lst -MSYSTEM=PAL

twirl2: twirl.asm sin.h
	dasm twirl.asm -otwirl2 -v3 -ltwirl2.lst -MSYSTEM=NTSC

sin.h: sinus.c
	gcc -o sinus sinus.c -lm ; ./sinus > sin.h
