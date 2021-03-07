SAME_MUS = 1

CO0 = $0	; char (background)
CO3 = CO0	; border


  processor 6502

MATRIX  = $200
COLOR   = $9400+(MATRIX & 512)
CHARMEM = $1000	; Use $2000..$2c00 for ROM font


;lda #$0c	; -> video matrix $0200, chars $1000


eff0_ys = $00	; $00..$3f
;	  	; $40..$bf unused
hor_map = $c0	; $c0..$e0	; must be aligned! $e0 included!
;	  	; $e1..$f0 unused
xhoriz = $f1
columncnt_h = $f2
h_code = $f3	; now stores 0,2,4,..2*MIDCOL-2
effect_cnt = $f4
effect_arg = $f5

map_h	 = $100	; $100..$17f ($54)
back     = $160	; $160..$1df	; does not need to be aligned, but no page wrap
		; $1e0..$1ff stack space
		; $200..$2ef video matrix (can be upto $313, for example 9x30)
;page3org	; $316..$3fb (currently)

;techsin must be aligned now.
techsin  = $9400	; $9400..$94ff 00..3f 40..7f 80..bf c0..ff
	 	; $9500..$95ff unused
		; $9600..$96ef color memory (upto $9713)
	 	; $9700..$97ff unused

; $0000-01df	tables
; $01e0-01ff	stack space
; $0200-02ef	video matrix (ROWS*COLUMNS 8*30)
; $02f0-0313	unused
; $0314-0315	IRQ vectors
; $0316-03ff	code
; $0400-1000	--open space--
; $1000-12ff	init code, character data (MIDROWS*MIDCOLS 16-byte chars)
; $1300-18bf	font (packed), first 32 bytes are zero (empty char)
; $18c0-1cdd	code
; $1cde-2000	music data, music player (currenty upto $1ed3)


NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34

COLUMNS = 31	; columns used for video matrix (must be even and <=MIDROWS*16)
	  	; now works with odd also..
MIDCOLS = 24	; actual printed columns. columns between chars are empty
MIDROWS = 2
ROWS = 8	; total rows used for video matrix

#else

LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER = 26

COLUMNS = 27	; columns used for video matrix (must be even and <=MIDROWS*16)
	  	; now works with odd also..
MIDCOLS = 20	; actual printed columns. columns between chars are empty
MIDROWS = 2	; 2 chars per scroll
ROWS = 4	; total rows used for video matrix

#endif

RASTER	= 41

TIMER_VALUE = LINES * CYCLES_PER_LINE - 2	; P $5685 N $4245


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm
	.org $1001

	; Code here will be overwritten by charset
	; so this is only available at startup.
	ldx #32-1
11$	txa
	sta hor_map,x
	lda backsrc,x
	sta back+0,x
	sta back+64,x
	lda backsrc+32,x
	sta back+32,x
	sta back+96,x

	lda eff0_ys_src,x
	sta eff0_ys,x
	lda eff0_ys_src+32,x
	sta eff0_ys+32,x
#if SYSTEM == NTSC
	lda techsrc3,x
	sta techsin+7*32,x
	sta techsin+6*32,x
	sta techsin+5*32,x
	sta techsin+4*32,x
	sta techsin+3*32,x
	sta techsin+2*32,x	; quarter tech
#else
	lda techsrc1,x
	sta techsin+7*32,x
	sta techsin+6*32,x	; full tech

	lda techsrc2,x
	sta techsin+5*32,x
	sta techsin+4*32,x	; half tech

	lda techsrc3,x
	sta techsin+3*32,x
	sta techsin+2*32,x	; quarter tech
#endif

	lda #4
	sta techsin+1*32,x
	sta techsin+0*32,x	; no tech
	dex
	bpl 11$

	stx hor_map+32		; negative value as an EOF to hor_map+32!
	stx effect_cnt		; $ff

;inithoriz:
#if SYSTEM == NTSC
	lda #SCRCENTER-COLUMNS+1
#else
	lda #SCRCENTER-COLUMNS-1
#endif
	sta xhoriz

	ldx #0
	stx columncnt_h
	stx h_code
	stx $900f
	stx $9002	; 0 columns
	stx $9003	; 0 rows

#if SAME_MUS
#else
	sei
#endif
	;$0c = video matrix $0200, chars $1000
	lda #((MATRIX/$400)*$10)+(CHARMEM/$400)^$88
	sta $9005

#if SYSTEM == NTSC
	lda #RASTER+10
#else
	lda #RASTER+10-(MIDROWS*16-COLUMNS+1)/2	; borrow 2 lines from top line..
#endif
	sta $9001

#if COLUMNS == 32
	ldx #COLUMNS*ROWS-1
	lda #CO0
cm$	sta COLOR,x
	dex
	cpx #255
	bne cm$
#else
	ldx #COLUMNS*ROWS
	lda #CO0
cm$	sta COLOR-1,x
	dex
	bne cm$
#endif

	ldx #map_end-map_hsrc
10$	lda map_hsrc,x	; 7-bit values
	;and #254	; charcodes are already, but make sure
	sta map_h,x
	dex
	bpl 10$

	ldx #page3end-page3src
14$	lda page3src-1,x
	sta page3org-1,x
	dex
	bne 14$

	lda #$7f
	sta $913e	; disable and acknowledge interrupts / NMIs
	sta $912d
	sta $911e	; disable NMIs (Restore key)
	;ldx #0		; disable Timer A free run
	stx $912b


	; Will be overwritten by character data
#if COLUMNS == 32
	ldx #COLUMNS*ROWS-1
	lda #((font_h-CHARMEM)/16)
1$	sta MATRIX,x
	dex
	cpx #255
	bne 1$
#else
	ldx #COLUMNS*ROWS
	lda #((font_h-CHARMEM)/16)
1$	sta MATRIX-1,x
	dex
	bne 1$
#endif

#if SAME_MUS
	sei
#endif
;synchronize with the screen

sync:	ldx #RASTER	; wait for this raster line (times 2)
5$	cpx $9004
	beq 5$	; wait until it has passed..
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	samepage sync
	ldy #9
	bit $24
sloop	ldx $9004
	txa
	bit $24
#if SYSTEM & PAL
	ldx #24
#endif
#if SYSTEM & NTSC
	bit $24
	ldx #21
#endif
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne sloop
	samepage sloop
	; now it is fully synchronized
	; 6 cycles have passed since last $9004 change
	; and we are on line 2(28+9)=74

#if SYSTEM == NTSC
	ldy #5
wa	dey
	bne wa
	samepage wa
#endif

	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts
	;lda #$82
	;sta $911e	; enable Restore key

	jmp contstart
	; end of one-shot code..



page3src
#rorg $316	; start after the IRQ pointers
page3org

irq:	lda #<(TIMER_VALUE-46+2)	; 2 for reload time
	sec
	sbc $9124	; 46 to 53 cycles delay at this stage
			; 90..83/23..16 in $9124 for PAL/NTSC
	; A = 0..7	0=wait 7 cycles .. 7=wait 0 cycles
	sta *+4
	bne *+2
	nop
	lda #$a9
	lda #$a9
	lda #$a9
	bit $ea
	; now we are synchronized 18 cycles from the IRQ
	samepage irq

#if SYSTEM == NTSC
	jsr wait19
#else
	lda techp1+1	;4
	sta techp2+1	;4
	jsr wait12	;+12=20, close enough
#endif

	ldy #0

loop0
#if SYSTEM == NTSC
	nop
	nop
	nop
	nop
	nop
	clc
	ldx hor_map,y	; 4	; colors follow effects
	lda back+32,x	; 4	; +8 black +7 white
#else
	lda COLOR+0*COLUMNS,y
	sta COLOR+6*COLUMNS,y
	sta COLOR+7*COLUMNS,y	; 14 cycles, some can be written twice
	nop
	clc
	ldx hor_map+32-COLUMNS/2*2,y	; 4	; colors follow effects
	lda back+32,x	; 4	; +8 black +7 white
#endif
eorp0	eor #0		; color bars + colored chars
	sta $900f	; 4
#if SYSTEM == NTSC
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	nop
	nop
#else
	lda MATRIX+0*COLUMNS,y	;4	plot chars to third scroller
	sta MATRIX+6*COLUMNS,y	;5
	adc #1			;2
	sta MATRIX+7*COLUMNS,y	;5

	lda MATRIX+2*COLUMNS,y	;4	plot chars to third scroller
	sta MATRIX+4*COLUMNS,y	;5
	adc #1			;2
	sta MATRIX+5*COLUMNS,y	;5
#endif
	iny
#if SYSTEM == NTSC
	cpy #(MIDROWS*16)
#else
	cpy #(COLUMNS/2)*2	;(MIDROWS*15)	;2
#endif
	bne loop0
	samepage loop0
outloop0:


	ldy #0
loop	ldx hor_map,y	; 4	; colors follow effects

techp1	lda techsin,y	; note: 15 does not yet work!
	clc
#if SYSTEM == NTSC
	adc #1+4	; +1 to round up 3 to 1 cycle, 7 to 2 cycles etc.
#else
	adc #1		; +1 to round up 3 to 1 cycle, 7 to 2 cycles etc.
#endif
	and #12
	lsr
	lsr		; and #12 causes lsr's to shift 0 to C for next adc
	adc xhoriz	; 3
	sta $9000

backp	lda back+0,x	; 4
	sta $900f	; 4

#if SYSTEM == NTSC
	nop
	nop
	jsr wait19
#else
	jsr wait29
#endif
	iny
	cpy #(MIDROWS*16)	;2
	beq outloop
	jmp loop
	samepage loop
outloop:

#if SYSTEM == NTSC
#else
	ldy #0
loop2	ldx hor_map,y	; 4	; colors follow effects

techp2	lda techsin,y	; note: 15 does not yet work!
	clc
	adc #1		; +1 to round up 3 to 1 cycle, 7 to 2 cycles etc.
	and #12
	lsr
	lsr		; and #12 causes lsr's to shift 0 to C for next adc
	adc xhoriz	; 3
	sta $9000

backp2	lda back+0,x	; 4
	sta $900f	; 4

#if SYSTEM == NTSC
	jsr wait19
	nop
	nop
#else
	jsr wait29
#endif
	iny
	cpy #(MIDROWS*16)	;2
	beq outloop2
	jmp loop2
	samepage loop2
outloop2:
#endif
	;bit $ea
#if SYSTEM == NTSC
#if 0
	lda #1		;2
	clc		;2
	adc xhoriz	;3
	sta $9000	;4
#else
	lda #4
	sta $9000
#endif
#else
	lda #3		;2
	clc		;2
	adc xhoriz	;3
	sta $9000	;4
#endif

	ldy #2*ROWS+1	;16-line chars
	sty $9003
	ldx #COLUMNS+(MATRIX & 512)/4
	stx $9002

eorp3	lda #$d0	;set up the first two lines for the next field
	sta $900f

	jsr scrollhoriz

	jmp copyloop

wait29	bit $ea
	pha ; wait a bit
	pla
wait19	pha
	pla
wait12	rts   ; 6	; jsr takes 6



effects:
	lda effect_cnt
	bpl effect_type

a$	lda #0		; if no effect, scroll colors
	inc a$+1
	lsr
	lsr
	;clc	; does not matter, always rounds up
	adc #<back
#if SYSTEM == PAL
	sta backp2+1
	adc #32
#endif
	sta backp+1
	rts

effect_type:
	ldy #3
	lda efptrhi,y
	pha
	lda efptrlo,y
	pha
	lda effect_cnt	; preload value for all effects
	rts		; jump to one effect or another

efptrhi	dc.b >(effect0-1),>(effect1-1),>(effect2-1),>(effectNone-1)
efptrlo	dc.b <(effect0-1),<(effect1-1),<(effect2-1),<(effectNone-1)
#rend
page3end
	lda #page3end-page3src+$16



backsrc:
	dc.b $00,$20,$20,$40,$80,$90,$70,$10,$00,$20,$20,$40,$80,$90,$70,$f0
	dc.b $10,$10,$f0,$70,$90,$80,$40,$20,$20,$00,$10,$70,$90,$80,$40,$20

	dc.b $00,$60,$20,$40,$40,$30,$b0,$10
	dc.b $00,$60,$40,$40,$e0,$30,$b0,$10,$10,$b0,$30,$e0,$40,$40,$60,$00
	dc.b $10,$b0,$30,$e0,$40,$40,$60	;,$00

	; shift by 2 not used!
techsrc1:
	dc.b $0,$0,$0,$0,$0,$1,$3,$4,$5,$7,$8,$9,$b,$c,$d,$d
	dc.b $d,$d,$c,$b,$9,$8,$7,$5,$4,$3,$1 ;$0,$0,$0,$0,$0
	; full tech 16 * 2.2 = 35.2 lines
techsrc2:
	dc.b $0,$0,$0,$0,$0,$1,$1,$3,$3,$4,$5,$5,$7,$7,$8,$8
	dc.b $8,$8,$7,$7,$5,$5,$4,$3,$3,$1,$1 ;$0,$0,$0,$0,$0
	; half 16 * 2.2 = 35.2 lines
techsrc3:
	dc.b $0,$0,$0,$0,$0,$1,$1,$1,$3,$3,$3,$4,$4,$4,$5,$5
	dc.b $5,$5,$4,$4,$4,$3,$3,$3,$1,$1,$1,$0,$0,$0,$0,$0
	; quarter 16 * 2.2 = 35.2 lines

eff0_ys_src:
	dc.b 31,31,31,31,31,31,30,29,28,26,25,24,22,20,19,18
	dc.b 15,14,13,12,10, 8, 7, 6, 4, 3, 2, 1, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 1, 1, 2, 3, 4, 6, 7, 8,10,12,13,14
	dc.b 17,18,19,20,22,24,25,26,28,29,30,31,31,31,31,31

map_hsrc:
#incbin "map_h.bin"
map_end





#if SYSTEM == NTSC
MOVE_OVER = 9
#else
MOVE_OVER = 0
#endif
	.org CHARMEM+MIDCOLS*MIDROWS*16 +MOVE_OVER*16
	; Actually, 2 empty chars would be needed because video matrix
	; scrolling plots ascending char codes according to the first line.
	; But the first "column" in the font is for space anyway so it's okay.

font_h:
	; Because of the tech-tech and the empty spaces between characters
	; and shift by 2 is not used, the font has to have 1 rightmost and
	; leftmost pixels be empty only. Thus 22-pixel-wide chars.
;-rw-r--r--  1 albert  albert     84 Feb 28 11:50 map_h.bin
;-rw-r--r--  1 albert  albert   1536 Feb 28 11:50 font_h.bin
;-rw-r--r--  1 albert  albert     84 Apr 14 23:01 map_h.bin
;-rw-r--r--  1 albert  albert   1344 Apr 14 23:01 font_h.bin
#incbin "font_h.bin"


; Do NOT CHANGE code! Addresses are modified by code!
copyloop:
	ldx #0		; 2
	ldy hor_map	; 3
loop$
	lda font_h,y			; copyloop+4,5,6
	sta CHARMEM+0*16*MIDROWS,x		; 7,8,9
	lda font_h,y			; 10,11,12
	sta CHARMEM+1*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+2*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+3*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+4*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+5*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+6*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+7*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+8*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+9*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+10*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+11*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+12*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+13*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+14*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+15*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+16*16*MIDROWS,x
	lda font_h,y
	sta CHARMEM+17*16*MIDROWS,x
#if MIDCOLS > 18
	lda font_h,y
	sta CHARMEM+18*16*MIDROWS,x
#endif
#if MIDCOLS > 19
	lda font_h,y
	sta CHARMEM+19*16*MIDROWS,x
#endif
#if MIDCOLS > 20
	lda font_h,y
	sta CHARMEM+20*16*MIDROWS,x
#endif
#if MIDCOLS > 21
	lda font_h,y
	sta CHARMEM+21*16*MIDROWS,x
#endif
#if MIDCOLS > 22
	lda font_h,y
	sta CHARMEM+22*16*MIDROWS,x
#endif
#if MIDCOLS > 23
	lda font_h,y
	sta CHARMEM+23*16*MIDROWS,x
#endif
#if MIDCOLS > 24
	lda font_h,y
	sta CHARMEM+24*16*MIDROWS,x
#endif
#if MIDCOLS > 25
	lda font_h,y
	sta CHARMEM+25*16*MIDROWS,x
#endif
#if MIDCOLS > 26
	lda font_h,y
	sta CHARMEM+26*16*MIDROWS,x
#endif
#if MIDCOLS > 27
	lda font_h,y
	sta CHARMEM+27*16*MIDROWS,x
#endif
#if MIDCOLS > 28
	lda font_h,y
	sta CHARMEM+28*16*MIDROWS,x
#endif
	inx		;2
	ldy hor_map,x	;4	; negative value=end marker, saves 24*2 cycles
	bmi 0$			;2
	jmp loop$	;3 = 24*9+11 -> 32*227 = 102.3 lines
0$


#if SYSTEM == NTSC
	lda #1
#else
	lda #3
#endif
	clc
	adc xhoriz	; 3
	sta $9000


lrts2
eorp4	lda #$00	;set up the first two lines for the next field
	sta $900f

	jsr effects

	;lda #$08	;CO0
	;sta $900f
#if 1 ;SYSTEM == PAL
	jmp tech
#else
#if SAME_MUS
	jmp MUSVEC
#else
	;inc $900f
	jsr player_update
	;dec $900f
	jmp $eb18	; return from IRQ
#endif

#endif

scrollhoriz:
#if 0
	lda $9111	; VIA#1 port A
	and #$20	; fire?
	beq lrts2
#endif
	; faster scroll (1 cycle / frame) -- too fast
	dec xhoriz
	lda xhoriz
#if SYSTEM == NTSC
	cmp #SCRCENTER-COLUMNS	; 4? -> 6
#else
	cmp #SCRCENTER-COLUMNS-2	; 4? -> 6
#endif
	bne scs$
	jsr new$
	jmp copy$

scs$	; scroll video matrix
#if SYSTEM == NTSC
	lda #SCRCENTER-COLUMNS+1	; 6
#else
	lda #SCRCENTER-COLUMNS-1	; 6
#endif
	sta xhoriz

	ldx #0
	clc

sc$	lda MATRIX + 1 + 2*COLUMNS,x	; 4
	sta MATRIX + 2*COLUMNS,x	; 5
	adc #1			; 2 cycles instead of 4
	sta MATRIX + 3*COLUMNS,x	; 5

	lda MATRIX + 1 + 0*COLUMNS,x
	sta MATRIX + 0*COLUMNS,x
	adc #1
	sta MATRIX + 1*COLUMNS,x
#if 1
	lda COLOR+1+0*COLUMNS,x	; color scrolling 5.9 lines more
	sta COLOR+0*COLUMNS,x
	sta COLOR+1*COLUMNS,x
#endif
	inx 	    		; 2
	cpx #COLUMNS-1		; 2	clears Carry
	bne sc$		  	; 3
	    			; 37 * 29 = 1073 = 15.1 lines
	rts

new$
	dec columncnt_h
	bpl ch0$		; one empty between chars to make MIDCOLS last
	lda #((font_h-CHARMEM)/16)
	sta MATRIX + 3*COLUMNS-1
	sta MATRIX + 4*COLUMNS-1

	sta MATRIX + 1*COLUMNS-2
	sta MATRIX + 2*COLUMNS-2

#if 1
	lda #0
	sta COLOR+1*COLUMNS-2	; column between chars to black
	sta COLOR+2*COLUMNS-2	; column between chars to black
	;beq newcol$

again$	inc newcol$+1
newcol$	lda #0
	and #7
	beq again$	; black not allowed as character color
	sta COLOR+1*COLUMNS-1
#if SYSTEM == NTSC
	sta COLOR+2*COLUMNS-1
#endif

	bne scroll$	;jmp scroll$
#else
	bne scroll$	; counter negative -- get next character
#endif

ch0$	lda h_code
	clc
	sta MATRIX + 3*COLUMNS-1
	adc #1
	sta MATRIX + 4*COLUMNS-1
	adc #1 	    ; h_code increment+2 % 2*MIDCOLS
	cmp #2*MIDCOLS
	bcc 0$
	lda #0
0$	sta h_code

	rts

scroll$	inc ptr$+1
ptr$	ldx #255
	lda text_h,x
	bpl 5$
	cmp #255	; eof?
	beq reset$

	jsr seteffect
	jmp repl$

reset$	sta ptr$+1

	lda eorp0+1
	eor #8
	sta eorp0+1
	sta eorp4+1
	;lda eorp3+1	; they are in-phase, so load not needed
	;eor #8
	sta eorp3+1

	dec FADEOUT+1	; trigger fadeout at some point

repl$	lda #" "	; replace eof with space

5$	ldy #3 		; normally 24 pixels wide chars
	cmp #"I"
	beq narrow$
	cmp #"L"
	beq narrow$
	cmp #" "
	bne 6$
	;inc newcol$+1
narrow$	dey		; I L and space are narrower chars
6$	and #31
	sta add$+1
	asl
	;clc
add$	adc #0		; A = 3*A
	sta ccode$+1
	sty columncnt_h

	rts

copy$	lda h_code	;2x
	asl 		;4x
	adc h_code	;6x
	tay

ccode$	ldx #0
	lda map_h,x
	lsr
	lsr
	lsr
	lsr
	;and #15		; XXXX to A
	pha				; hi(font_h + map_hsrc*16)
	lda map_h,x	; zzzzxxx0
	asl		; zzzxxx0-
	asl		; zzxxx0--
	asl		; zxxx0---
	asl		; xxx0----
	clc
	adc #<font_h			; lo(font_h + map_hsrc*16)
	sta copyloop+5,y	;5
	pla
	adc #>font_h
	sta copyloop+6,y
	inc ccode$+1

	; extra scroller, use the chars directly
	; charcode = (font_h + map_hsrc*16 - CHARMEM) / 16
	; 	   = (font_h-CHARMEM)/16 + map_hsrc
	lda map_h,x
	clc
	adc #((font_h-CHARMEM)/16)
	sta MATRIX + 1*COLUMNS-1
	;adc #1
	;sta MATRIX + 2*COLUMNS-1
#endif

	rts


contstart:
#if SAME_MUS
	jsr player_update
#else
	jsr player_init
#endif
	lda $9124
	cli
loop$	jmp loop$



E_ROTATE   = 128
E_MODULATE = 129
E_PIPE     = 130
E_TECH     = 131

; Z is 2, [ is 3, G is 6, Y is 9
; W, Q not used
; Note, length of text must be <256
text_h
	dc.b "       VICUAL MMIX BY", E_ROTATE, "ALBERT      "
	dc.b "MUSIC BY ZAPAC     "
	dc.b "I HOPE YOU ENJOYED THE DEMO     ", E_TECH
	dc.b "GREETINGS TO   ZAPAC ", E_MODULATE
	dc.b "ALEKSI EEBEN  MARKO MAKELA  BRITELITE  VIZNUT  MERMAID  "
	dc.b E_TECH
	dc.b "AND ALL OTHER", E_PIPE
	dc.b "VICZO FREAKS OUT THERE         ", E_TECH
	dc.b 255


techone:
	lda CHARMEM+MIDCOLS*16*MIDROWS-16*MIDROWS,x
	lsr
	ror CHARMEM+0*16*MIDROWS,x
	ror CHARMEM+1*16*MIDROWS,x
	ror CHARMEM+2*16*MIDROWS,x
	ror CHARMEM+3*16*MIDROWS,x
	ror CHARMEM+4*16*MIDROWS,x
	ror CHARMEM+5*16*MIDROWS,x
	ror CHARMEM+6*16*MIDROWS,x
	ror CHARMEM+7*16*MIDROWS,x
	ror CHARMEM+8*16*MIDROWS,x
	ror CHARMEM+9*16*MIDROWS,x
	ror CHARMEM+10*16*MIDROWS,x
	ror CHARMEM+11*16*MIDROWS,x
	ror CHARMEM+12*16*MIDROWS,x
	ror CHARMEM+13*16*MIDROWS,x
	ror CHARMEM+14*16*MIDROWS,x
	ror CHARMEM+15*16*MIDROWS,x
	ror CHARMEM+16*16*MIDROWS,x
	ror CHARMEM+17*16*MIDROWS,x
#if MIDCOLS > 18
	ror CHARMEM+18*16*MIDROWS,x
#endif
#if MIDCOLS > 19
	ror CHARMEM+19*16*MIDROWS,x
#endif
#if MIDCOLS > 20
	ror CHARMEM+20*16*MIDROWS,x
#endif
#if MIDCOLS > 21
	ror CHARMEM+21*16*MIDROWS,x
#endif
#if MIDCOLS > 22
	ror CHARMEM+22*16*MIDROWS,x
#endif
#if MIDCOLS > 23
	ror CHARMEM+23*16*MIDROWS,x
#endif
#if MIDCOLS > 24
	ror CHARMEM+24*16*MIDROWS,x
#endif
#if MIDCOLS > 25
	ror CHARMEM+25*16*MIDROWS,x
#endif
#if MIDCOLS > 26
	ror CHARMEM+26*16*MIDROWS,x
#endif
#if MIDCOLS > 27
	ror CHARMEM+27*16*MIDROWS,x
#endif
#if MIDCOLS > 28
	ror CHARMEM+28*16*MIDROWS,x
#endif
#if MIDCOLS > 29
	ror CHARMEM+29*16*MIDROWS,x
#endif
	rts	; 24*6+12= 2.2 lines


; X = 0..31, Y can be used
techleft:
#if 0	; MIDCOLS == 24
	txa ;2
	pha ;3
	clc ;2
	adc #7*16*MIDROWS	;2
	pha ;3

	lda CHARMEM+0*16*MIDROWS,x
	asl
	rol CHARMEM+23*16*MIDROWS,x
	rol CHARMEM+22*16*MIDROWS,x
	rol CHARMEM+21*16*MIDROWS,x
	rol CHARMEM+20*16*MIDROWS,x
	rol CHARMEM+19*16*MIDROWS,x
	rol CHARMEM+18*16*MIDROWS,x
	rol CHARMEM+17*16*MIDROWS,x
	rol CHARMEM+16*16*MIDROWS,x
	rol CHARMEM+15*16*MIDROWS,x
	rol CHARMEM+14*16*MIDROWS,x
	ldy #2	;2
2$
	pla	;4
	tax	;2
	rol CHARMEM+6*16*MIDROWS,x	; 6 13
	rol CHARMEM+5*16*MIDROWS,x
	rol CHARMEM+4*16*MIDROWS,x
	rol CHARMEM+3*16*MIDROWS,x
	rol CHARMEM+2*16*MIDROWS,x
	rol CHARMEM+1*16*MIDROWS,x
	rol CHARMEM+0*16*MIDROWS,x	; 0 7
	dey 	;2
	bne 2$	;3 = 35 cycles more = half a line per techleft..
	; 13 bytes more, saves 21-13=8 bytes :-|
#else
	lda CHARMEM+0*16*MIDROWS,x
	asl
#if MIDCOLS > 29
	rol CHARMEM+29*16*MIDROWS,x
#endif
#if MIDCOLS > 28
	rol CHARMEM+28*16*MIDROWS,x
#endif
#if MIDCOLS > 27
	rol CHARMEM+27*16*MIDROWS,x
#endif
#if MIDCOLS > 26
	rol CHARMEM+26*16*MIDROWS,x
#endif
#if MIDCOLS > 25
	rol CHARMEM+25*16*MIDROWS,x
#endif
#if MIDCOLS > 24
	rol CHARMEM+24*16*MIDROWS,x
#endif
#if MIDCOLS > 23
	rol CHARMEM+23*16*MIDROWS,x
#endif
#if MIDCOLS > 22
	rol CHARMEM+22*16*MIDROWS,x
#endif
#if MIDCOLS > 21
	rol CHARMEM+21*16*MIDROWS,x
#endif
#if MIDCOLS > 20
	rol CHARMEM+20*16*MIDROWS,x
#endif
#if MIDCOLS > 19
	rol CHARMEM+19*16*MIDROWS,x
#endif
#if MIDCOLS > 18
	rol CHARMEM+18*16*MIDROWS,x
#endif
	rol CHARMEM+17*16*MIDROWS,x
	rol CHARMEM+16*16*MIDROWS,x
	rol CHARMEM+15*16*MIDROWS,x
	rol CHARMEM+14*16*MIDROWS,x
	rol CHARMEM+13*16*MIDROWS,x
	rol CHARMEM+12*16*MIDROWS,x
	rol CHARMEM+11*16*MIDROWS,x
	rol CHARMEM+10*16*MIDROWS,x
	rol CHARMEM+9*16*MIDROWS,x
	rol CHARMEM+8*16*MIDROWS,x
	rol CHARMEM+7*16*MIDROWS,x
	rol CHARMEM+6*16*MIDROWS,x
	rol CHARMEM+5*16*MIDROWS,x
	rol CHARMEM+4*16*MIDROWS,x
	rol CHARMEM+3*16*MIDROWS,x
	rol CHARMEM+2*16*MIDROWS,x
	rol CHARMEM+1*16*MIDROWS,x
	rol CHARMEM+0*16*MIDROWS,x
#endif
	rts	; 24*6+12= 2.2 lines


;
; 0 4  8 12: no shift
; 1 5  9 13: 1 x techone
; 2 6 10 14: 2 x techone -> not used
; 3 7 11 15: 1 x techleft + 1 cycle


tech
#if 1
FADEOUT	lda #128+2	; fade out music, continue the scroll..
	bmi 0$

	lsr
	lsr
	lsr
	lsr
	sta $900e	; 7 -> 0
	dec FADEOUT+1
0$
#endif

y$	lda #0
	dec y$+1
	and #31
techoff	ora #0

	sta techp0+1
	sta techp1+1
	;sta techp2+1	; copied at start of irq

	ldx #0
tloop0	;ldy hor_map,x	; tech-tech follow effects. But we can't use it.
	     		; It would take too much rastertime, because
			; more lines are required to be shifted..
techp0	lda techsin,x	; shift 3 right is equal to shift 1 left (+correction)
	and #3		; but there is no space for such code, so avoid
	beq 1$		; 0
	cmp #2
	bcc 2$
;	beq 3$
	jsr techleft	; may use Y
	jmp 1$
;3$	jsr techone	; shift by 2 not used
2$	jsr techone	; may use Y

1$	inx
	cpx #32
	bne tloop0
#if SAME_MUS
	jmp MUSVEC
#else
	;inc $900f
	jsr player_update
	;dec $900f
	jmp $eb18	; return from IRQ
#endif


;	effect 0	rotate
;	effect 1	modulate
;	effect 2	pipe

effect0:		; rotate	$10d
	and #63
	tax
	ldy eff0_ys,x	; start position
	and #31
	tax
	lda eff0_la,x	; fractional line height
	sta la$+1

	lda #0
	ldx #32/4-1
0$	sta hor_map+0,x		; 4 sta zp,x
	sta hor_map+16,x	; no unrolling: 288 cycles = 4.1 lines
	sta hor_map+8,x		; two-unrolled: 208 cycles = 2.9 lines
	sta hor_map+24,x
	dex
	bpl 0$	; =168 cycles = 2.4 lines

	lda #$ca
	cpy #16
	bcs 11$
	lda #$e8
11$	sta ya$		; go either up or down..
	tya
	tax

	lda #E_LASC*31
next$	tay			;2
	lsr			;2
	lsr			;2
	lsr			;2	/8-table would save ~1 line
	sta hor_map+0,x		;4 (sta zero-page,x)
ya$	dex			;2 dey/iny
	tya			;2
	;sec ; difference too small to notice (1/32 per round)
la$	sbc #E_LASC*32/32		;2
	bcs next$		;3= 21 -> max 32*21 = 672 cycles = 9.5 lines

	inc effect_cnt
	bmi effectOff
	rts

effectNone:
	lda #$40
	clc
	adc techoff+1
	sta techoff+1	; 9 bytes
effectOff:
	lda #255
	sta effect_cnt
cont	rts



eff1_tab:   ; Used for both effect1 and effect2 (31+32 needed for effect1)
	dc.b  1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 2, 1, 2, 1, 2, 2
	dc.b  1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 0, 1, 1, 0, 0, 0
	dc.b  1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 2, 1, 2, 1, 2
	dc.b  2, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 0, 1, 1, 0

effect1:		; modulate	$3b+eff3_tab($42)
	and #31
	tay
	lda #32-1
	;bne efcom		; jump always
	dc.b $2c		; save a byte: bit $abs ignores "ldy #16"
effect2:		; pipe		$36+eff3_tab($42)
	ldy #16
efcom	ldx #32-1
	bne st$	; jump always

0$	sbc eff1_tab,y ; 4
	bpl ok$	       ; 3
st$	and #31	       ; 2
	sec 	       ; 2
ok$	sta hor_map+0,x	 ; 4
	iny 		 ; 2
	dex		 ; 2
	bpl 0$		 ; 3
	    		 ; = 18*32+5 = 581 = 8.2 lines

	inc effect_cnt
	bpl lrts

	dec effect_arg
	bmi lrts
	inx	;ldx #0
	stx effect_type+1	; ->0 rotate
	inc effect_arg		; ->1 once

setcnt	stx effect_cnt
lrts	rts


seteffect:
	ldy effect_cnt
	bpl lrts		; do not activate if effect already active

	and #3
	sta effect_type+1
	tax
	inx	;ldx #1, 2, 3
	stx effect_arg
	ldx #0
	beq setcnt	;stx effect_cnt
	; jump always


eff0_la:
E_LASC = 8	; should be 8
	dc.b E_LASC*32/32,E_LASC*32/32,E_LASC*32/31,E_LASC*32/31
	dc.b E_LASC*32/30,E_LASC*32/28,E_LASC*32/27,E_LASC*32/25
	dc.b E_LASC*32/23,E_LASC*32/20,E_LASC*32/18,E_LASC*32/15
	dc.b E_LASC*32/12,E_LASC*32/9,E_LASC*32/6,E_LASC*32/3

	dc.b 255,E_LASC*32/3,E_LASC*32/6,E_LASC*32/9
	dc.b E_LASC*32/12,E_LASC*32/15,E_LASC*32/18,E_LASC*32/20
	dc.b E_LASC*32/23,E_LASC*32/25,E_LASC*32/27,E_LASC*32/28
	dc.b E_LASC*32/30,E_LASC*32/31,E_LASC*32/31,E_LASC*32/32







#if SAME_MUS

MUSVEC = $1c80
player_update = MUSVEC+6

#else

	 .org $1c80
INCSONG equ 1
#include "../music/acplay2.a65"
;#include "../music/cutie.sng"
;#include "../music/canon3.sng"
;#include "../music/mmix.sng"
;#include "../music/vicual.sng"
#include "../music/voyage.sng"

#endif
