
all: credits.p credits.n

credits.p: credits
	pucrunch -x0x1001 -c20 credits credits.p

credits.n: credits2
	pucrunch -x0x1001 -c20 credits2 credits.n

credits: credits.asm font_h.bin map_h.bin
	dasm credits.asm -ocredits -v3 -lcredits.lst -MSYSTEM=PAL

credits2: credits.asm font_h.bin map_h.bin
	dasm credits.asm -ocredits2 -v3 -lcredits2.lst -MSYSTEM=NTSC


# 00 - back, 01 - border, 10 - character, 11 - aux
#bigfont_h.bin: bigfont_h.ppm Makefile
#	../c/ppmtomcol -b0321210321333333 -0 bigfont_h.ppm bigfont_h.bin
#	../c/ppmtomcol -b1230301230222222 -0 bigfont_h.ppm bigfont_h.bin
#                      0123456789abcdef

bigfont_h.bin: big.ppm Makefile
	../c/ppmtologo -0 big.ppm bigfont_h.bin

font_h.bin: charpack bigfont_h.bin
	./charpack bigfont_h.bin font_h.bin map_h.bin

map_h.bin: charpack bigfont_h.bin
	./charpack bigfont_h.bin font_h.bin map_h.bin

charpack: charpack.c
	gcc -Wall -o charpack charpack.c
