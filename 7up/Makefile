
all: 7up.p 7up.n

7up.p: 7up
	pucrunch -c20 7up 7up.p

7up.n: 7up2
	pucrunch -c20 7up2 7up.n

7up: 7up.asm
	dasm 7up.asm -o7up -v3 -l7up.lst -MSYSTEM=PAL

7up2: 7up.asm
	dasm 7up.asm -o7up2 -v3 -l7up2.lst -MSYSTEM=NTSC


