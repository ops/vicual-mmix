
all: loader.p loader.n image

loader.p: loader
	pucrunch -c20 loader loader.p -fshort -m5

loader.n: loader2
	pucrunch -c20 loader2 loader.n -fshort -m5

loader: loader.asm
	dasm loader.asm -oloader -v3 -lloader.lst -MSYSTEM=PAL

loader2: loader.asm
	dasm loader.asm -oloader2 -v3 -lloader2.lst -MSYSTEM=NTSC

image:
	c1541 -format 2009,04 d64 pal.d64
	c1541 -attach pal.d64 -write loader.p vicual-mmix -write ../intro-hq/intro.p -write ../spiral/spiral.p -write ../spiral/spiral.d -write ../1check/1check.p -write ../colscr/colscr.p -write ../wave/wave.p -write ../twirl/twirl.p -write ../7up/7up.p -write ../credits/credits.p
	c1541 -format 2009,04 d64 ntsc.d64
	c1541 -attach ntsc.d64 -write loader.n vicual-mmix-ntsc -write ../intro-hq/intro.n -write ../spiral/spiral.n -write ../spiral/spiral.d -write ../1check/1check.n -write ../colscr/colscr.n -write ../wave/wave.n -write ../twirl/twirl.n -write ../7up/7up.n -write ../credits/credits.n
	c1541 -format 2009,04 d81 pal.d81
	c1541 -attach pal.d81 -write loader.p vicual-mmix -write ../intro-hq/intro.p -write ../spiral/spiral.p -write ../spiral/spiral.d -write ../1check/1check.p -write ../colscr/colscr.p -write ../wave/wave.p -write ../twirl/twirl.p -write ../7up/7up.p -write ../credits/credits.p
	c1541 -format 2009,04 d81 ntsc.d81
	c1541 -attach ntsc.d81 -write loader.n vicual-mmix -write ../intro-hq/intro.n -write ../spiral/spiral.n -write ../spiral/spiral.d -write ../1check/1check.n -write ../colscr/colscr.n -write ../wave/wave.n -write ../twirl/twirl.n -write ../7up/7up.n -write ../credits/credits.n



