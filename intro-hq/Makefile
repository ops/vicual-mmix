
all: intro.p intro.n

intro.p: intro
	pucrunch -c20 intro intro.p -fshort -m5

intro.n: intro2
	pucrunch -c20 intro2 intro.n -fshort -m5

intro: intro.asm packed.bin map.bin
	dasm intro.asm -ointro -v3 -lintro.lst -MSYSTEM=PAL

intro2: intro.asm packed.bin map.bin
	dasm intro.asm -ointro2 -v3 -lintro2.lst -MSYSTEM=NTSC

# 00 - back, 01 - border, 10 - character, 11 - aux
bigfont.bin: bigfont.ppm
	../c/ppmtologo -0 bigfont.ppm bigfont.bin
#	../c/ppmtomcol -b1230301230222222 -0 bigfont.ppm bigfont.bin
#                      0123456789abcdef

packed.bin: bigfont.bin charpack
	./charpack bigfont.bin packed.bin map.bin

map.bin: bigfont.bin charpack
	./charpack bigfont.bin packed.bin map.bin

charpack: charpack.c
	gcc -o charpack charpack.c
