all: spiral.p spiral.d spiral.n
# dospiral

spiralc: spiral.c
	gcc -O -o spiralc spiral.c -lm

spiral.p: spiral
	pucrunch -c20 spiral spiral.p

spiral.n: spiral2
	pucrunch -c20 spiral2 spiral.n

screen.h: spiralc Makefile
	./spiralc -o screen.h -p -s 0 32

#screen.h2: spiralc Makefile
#	./spiralc -o screen.h2 -n -p -s 0 32

spiral: spiral.asm screen.h
	dasm spiral.asm -ospiral -v3 -lspiral.lst -MSYSTEM=PAL

spiral2: spiral.asm screen.h
	dasm spiral.asm -ospiral2 -v3 -lspiral2.lst -MSYSTEM=NTSC

mkdata: mkdata.c
	gcc -O -o mkdata mkdata.c

d4x32: spiralc
	./spiralc -o d4x32 -d -p -s 4 32

d8x16: spiralc
	./spiralc -o d8x16 -d -p -s 8 16

d4x8: spiralc
	./spiralc -o d4x8 -d -p -s 4 8

ds1: spiralc Makefile
#	./spiralc -o ds1 -d -s 128 2
	./spiralc -o ds1 -d -s 4 8 8
ds2: spiralc Makefile
	./spiralc -o ds2 -d -p -s 4 16 1
ds3: spiralc Makefile
	./spiralc -o ds3 -d -s 8 32 2
#	./spiralc -o ds3 -d -s 4 16 1

dring16: spiralc
	./spiralc -o dring16 -d -p -s 0 16

dring32: spiralc
	./spiralc -o dring32 -d -p -s 0 32

dx2: spiralc Makefile
	#./spiralc -o dx -d -x 12 128 6
	./spiralc -o dx2 -d -x 128 4 0

dlogo: spiralc Makefile
#	./spiralc -o dlogo -d -v 24 12 6
	./spiralc -o dlogo -d -v 36 7 6

DATS = d4x32 ds3 ds2 dx2 dlogo 
spiral.d: mkdata $(DATS) Makefile
	./mkdata $(DATS) >spiral.d
