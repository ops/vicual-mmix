SAME_MUS = 0
END_MUS = 0

;SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3
FADE = $3cf
FADENOT = $3ea


  processor 6502


; This part by Albert of Pu-239

;loadersave  $68..$ef
chmask = $f1
textptr = $f2	; $f2/f3, $f4/$f5, $f6/f7, $f8/f9, $fa/fb, $fc/fd, $fe/ff

colors = $200	; $200..$23f
cstab  = $240	; $240..$27f
next   = $280	; $280..$280+ROWS*4+1 ($2ed)
fontb  = $100	; $0100..$0180 2 nybbles
fontn  = $9780	; $9780..$9800 1 nybble

	; 00 - background, 01 - border, 11 - aux, 10 - character
; $00..$54 zpcode
; $68..$ef   loadersave
; $0100-017f	fontb
; $0180-01c8	main
; $0200-02ff	colors
; $031a-03ff	loader / charset3
; $1000-12d8	video matrix 0
; $1378-1400	charset 0
; $1400-16d8	video matrix 1
; $1778-1800	charset 1
; $1b78-1c00	charset 2
; $1cd0-2000
; $9780-9800	fontn

NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER = 34
#else
LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER = 26
#endif
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

#if SYSTEM & PAL
RASTER	= 20	;24		; effect at RASTER + 10 (+ 1)
#else
RASTER	= 12	;24		; effect at RASTER + 10 (+ 1)
#endif


#if SYSTEM & PAL
SCROLLS		EQU 7	;7 max (not parameterized everywhere)
COLUMNS		EQU SCROLLS*4-1
ROWS		EQU 26 ;27	;33 max (part of color memory used for tables)
#else
SCROLLS		EQU 5
COLUMNS		EQU SCROLLS*4-1
ROWS		EQU 22
#endif


; The BASIC line - Note: it is overwritten by the video matrix quite quickly

matrix0 = $1000

	.org $1001	; for the unexpanded Vic-20
basic:
	.word 0$	; link to next line
	.word 2002	; line number
	.byte $9E	; SYS token

; SYS digits

	.if (start) / 10000
	.byte $30 + (start) / 10000
	.endif
	.if (start) / 1000
	.byte $30 + (start) % 10000 / 1000
	.endif
	.if (start) / 100
	.byte $30 + (start) % 1000 / 100
	.endif
	.if (start) / 10
	.byte $30 + (start) % 100 / 10
	.endif
	.byte $30 + (start) % 10
0$	.byte 0,0,0	; end of BASIC program


start
#if SAME_MUS
#else
	sei	; to prevent keyboard scan to overwrite some data
#endif
	lda #0
	sta $9002	; columns + 9th bit of video matrix/color memory

	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)
	lda #$00	; disable Timer A free run
	sta $912b

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315

	ldx #0
	lda #8+1	; multicolor
0$	sta $9400,x
	sta $9500,x
	sta $9400+ROWS*COLUMNS-256,x
	;sta $9700,x
	dex
	bne 0$


	ldx #2*16-1
2$	txa
	and #15
	asl
	tay
	lda border,y
	ora back,y
	sta colors+$00,x	; odd lines

	iny
	lda border,y
	ora back,y
	sta colors+$20,x	; even lines
	lda cstabsrc+0,x
	sta cstab+0,x
	lda cstabsrc+32,x
	sta cstab+32,x

	dex
	bpl 2$


	ldx #0
	ldy #0
10$
	txa
	sec
	sbc #1
	and #15
	sta next,x

	inx
	cpx #ROWS*4+1	;108
	bne 10$

	ldx #0
	ldy #0
11$	lda fontsrc,x
	sta fontb,y
	inx
	lda fontsrc,x
	sta fontn,y
	iny
	inx
	bne 11$

loadersave = ($f0-$88)
	; save loader so we can use the space for cs3
	ldx #$88
12$	lda $378-1,x
	sta loadersave-1,x
	dex
	bne 12$

#if (zpend-zpsrc) > loadersave
    	err ;
#endif
	ldx #(zpend-zpsrc)
53$	lda zpsrc-1,x
	sta zpcode-1,x
	dex
	bne 53$


mkcharsets:
	lda #>cs0
	sta src0$+2
	lda #>cs1
	sta trg1$+2

again$	ldy #2
	lda cstrg+1,y
	sta src0$+2
	lda cstrg+0,y
	sta trg1$+2

	ldx #0
1$	txa
	and #7
	tay
	txa
	and #$f8
	ora scramble1,y
	tay
src0$	lda cs0,y
trg1$	sta cs1,x
	inx
	bpl 1$

	dec again$+1
	bpl again$


	ldx #7
	lda #$55
13$	sta cs3-8,x
	dex
	bpl 13$

	lda #31
	sta chmask

	lda #RASTER+10
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #ROWS*2	; rows & 8x8 chars
	sta $9003
	lda #$cc
	sta $9005
	lda #$08
	sta $900f

inittxt:
	ldx #SCROLLS*2-1
0$	lda textptro,x
	sta textptr,x
	dex
	bpl 0$

	ldx #(mainend-mainsrc)	;$3e right now
52$	lda mainsrc-1,x
	sta main-1,x
	dex
	bne 52$

#if SAME_MUS
	sei
#else
	jsr player_init
#endif

;synchronize with the screen

sync:	ldx #RASTER	; wait for this raster line (times 2)
5$	cpx $9004
	beq 5$
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
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


	; extra delay so it does not have to be in the irq
#if SYSTEM & PAL
	nop
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
#else
	ldy #10
wa	dey
	bne wa
	samepage wa
	nop
	nop
#endif


;initialize the timers
timers:
	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	jmp main



scramble1:
	dc.b 2,3,4,5,6,7,0,1
cstrg:
	dc.b >cs3, >cs2, >cs1, >cs0

mainsrc
#rorg $180	; $42? right now
main
	jsr setvideomem

#if SAME_MUS
	;jsr player_update
	jsr player_update
#endif
	lda $9124
	cli
loadnext
	lda #0
	beq loadnext

	ldx #$88
2$	lda loadersave-1,x
	sta $378-1,x
	dex
	bne 2$

	lda LOADER
	cmp #$8d
	bne *

	sei
	lda #<irq2	; set the raster IRQ routine pointer
	sta $314
	lda #>irq2
	sta $315
	cli

	lda #3		; only clear $9000..$9003
	ldx #<nextFile
	ldy #>nextFile
	jsr LOADER
1$	bcs 1$		; failed to load!

#if END_MUS == 0
	ldx #<MUSVEC	;$1e00
	lda #>MUSVEC	;$1e00
	jmp FADENOT
#else
	; then fade sound
	lda $900e
	and #$0f
	ldx #RASTER
	jmp FADE
#endif

nextFile:
#if SYSTEM == NTSC
	dc.b "CREDITS.N"		; 8 significant chars
#else
	dc.b "CREDITS.P"		; 8 significant chars
#endif

irq2	jsr player_update
	jmp $eb15	; return from IRQ
#rend
mainend

	; 11b0 .. 12d8 free for init data (overwritten)

#if SYSTEM == NTSC
	org $1000+ROWS*COLUMNS+32
#else
	org $1000+ROWS*COLUMNS
#endif

text0:	dc.b "  time flies like an arrow^ fruit flies like a banana^  ", 0
text1:	dc.b "  quick brown fox jumps over the lazy god ", 0
#if SCROLLS == 5
text2:	dc.b "  five upscrollers ^^^ please keep asking whats possible ", 0
#else
text2:	dc.b "  seven upscrollers ^^^ please keep asking whats possible ", 0
#endif
	; 0 free..

	org $1378
	dc.b $55,$55,$55,$55,$55,$55,$55,$55	;   01 = border
	org $1380
	; charset 0
cs0:	dc.b $00,$00,$00,$00,$00,$00,$00,$00	; 0 00 = back
	dc.b $e0,$e0,$e0,$e0,$00,$00,$00,$00	; 1 11 = aux
	dc.b $0e,$0e,$0e,$0e,$00,$00,$00,$00	; 2 10 = color
	dc.b $ee,$ee,$ee,$ee,$00,$00,$00,$00	; 3
	dc.b $00,$00,$00,$00,$e0,$e0,$e0,$e0	; 4
	dc.b $e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0	; 5
	dc.b $0e,$0e,$0e,$0e,$e0,$e0,$e0,$e0	; 6
	dc.b $ee,$ee,$ee,$ee,$e0,$e0,$e0,$e0	; 7
	dc.b $00,$00,$00,$00,$0e,$0e,$0e,$0e	; 8
	dc.b $e0,$e0,$e0,$e0,$0e,$0e,$0e,$0e	; 9
	dc.b $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e	; 10
	dc.b $ee,$ee,$ee,$ee,$0e,$0e,$0e,$0e	; 11
	dc.b $00,$00,$00,$00,$ee,$ee,$ee,$ee	; 12
	dc.b $e0,$e0,$e0,$e0,$ee,$ee,$ee,$ee	; 13
	dc.b $0e,$0e,$0e,$0e,$ee,$ee,$ee,$ee	; 14
	dc.b $ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee	; 15

	org $1400
	; video matrix 1
matrix1:

fontsrc:
	dc.b $00,$0,$00,$0,$00,$0,$00,$0
	dc.b $cc,$4,$05,$b,$05,$a,$cd,$7
	dc.b $cc,$c,$5a,$a,$5a,$a,$be,$1
	dc.b $8c,$4,$70,$b,$50,$a,$b0,$7
	dc.b $cc,$c,$50,$a,$50,$a,$bc,$7
	dc.b $cc,$c,$5a,$a,$5a,$a,$52,$a
	dc.b $cc,$c,$0a,$a,$0a,$a,$02,$a
	dc.b $8c,$4,$70,$b,$55,$a,$b5,$7
	dc.b $cc,$c,$0a,$0,$0a,$0,$ce,$c
	dc.b $00,$0,$40,$8,$73,$b,$00,$0
	dc.b $80,$0,$70,$0,$50,$0,$bc,$c
	dc.b $cc,$c,$0a,$0,$87,$d,$70,$2
	dc.b $cc,$c,$50,$0,$50,$0,$50,$0
	dc.b $cc,$4,$00,$b,$23,$b,$cc,$7
	dc.b $cc,$c,$08,$7,$87,$0,$fc,$c
	dc.b $8c,$4,$70,$b,$50,$a,$bc,$7
	dc.b $cc,$c,$05,$a,$05,$a,$0b,$7
	dc.b $8c,$4,$70,$b,$54,$a,$bc,$7
	dc.b $cc,$c,$05,$a,$85,$a,$7b,$7
	dc.b $80,$4,$7a,$b,$5a,$a,$b7,$2
	dc.b $00,$8,$00,$a,$33,$b,$00,$a
	dc.b $8c,$c,$70,$0,$50,$0,$bc,$c
	dc.b $00,$8,$8e,$3,$bc,$0,$02,$b
	dc.b $cc,$c,$b4,$0,$87,$0,$fc,$c
	dc.b $40,$8,$bc,$7,$8f,$4,$70,$b
	dc.b $00,$8,$08,$7,$3b,$4,$00,$b
	dc.b $c0,$8,$75,$a,$5b,$a,$52,$f
	dc.b $84,$0,$7b,$a,$5a,$a,$52,$7	;2 in [
	dc.b $40,$8,$5a,$a,$5a,$a,$b7,$7	;3 in \
	dc.b $08,$4,$57,$b,$55,$a,$bf,$7	;9 in ]
	dc.b $00,$0,$c0,$0,$30,$0,$00,$0	;. in ^
	;dc.b $8c,$4,$5c,$a,$5f,$a,$9a,$6	;@ in _

back:
#if 0
	;dc.b $00, $20,$20,$80,$80,$90,$70,$10
	dc.b $00, $20,$20,$80,$80,$90,$70,$10,$10,$10,$70,$90,$80,$80,$20,$20
	dc.b $00,$20,$20,$80,$20,$80,$80,$80,$90,$90,$70,$70,$f0,$f0,$10,$10
	dc.b $10,$10,$f0,$f0,$f0,$70,$70,$90,$90,$80,$80,$80,$20,$80,$20,$20
	dc.b $00, $20,$20,$80,$80,$90,$70,$10,$10,$10,$70,$90,$80,$80,$20,$20
	;dc.b $00, $10,$10,$70,$90,$80,$80,$20
#endif
	;dc.b $08,$28,$28,$48,$58,$38,$78,$18,$08,$28,$28,$48,$58,$38,$78,$18
	;dc.b $18,$18,$78,$38,$58,$48,$28,$28,$08,$18,$78,$38,$58,$48,$28,$08

	dc.b $08,$28,$28,$48,$88,$98,$78,$18,$08,$28,$28,$48,$88,$98,$78,$f8
	dc.b $18,$18,$f8,$78,$98,$88,$48,$28,$28,$08,$18,$78,$98,$88,$48,$28

	;dc.b $08,$28,$28,$88,$88,$98,$78,$18,$08,$28,$28,$88,$88,$98,$78,$18
	;dc.b $18,$18,$78,$98,$88,$88,$28,$28,$08,$18,$18,$78,$98,$88,$88,$28


cstabsrc:
#if 0
	; No scroll
	dc.b $cc,$cc,$cc,$cc, $cc,$cc,$cc,$cc, $cc,$cc,$cc,$cc, $cc,$cc,$cc,$cc
	; scroll by 2
	dc.b $dd,$cd,$cd,$cd, $dd,$cd,$cd,$cd, $dd,$cd,$cd,$cd, $dd,$cd,$cd,$cd
	; scroll by 4
	dc.b $de,$de,$ce,$ce, $de,$de,$ce,$ce, $de,$de,$ce,$ce, $de,$de,$ce,$ce
	; scroll by 6
	dc.b $df,$df,$df,$cf, $df,$df,$df,$cf, $df,$df,$df,$cf, $df,$df,$df,$cf
#else
	; No scroll
	dc.b $cc,$cc,$cc,$cc, $cc,$cc,$cc,$cc, $cc,$cc,$cc,$cc, $cc,$cc,$cc,$cc
	; scroll by 2
	dc.b $dd,$cd,$cd,$cd, $dd,$cd,$cd,$cd, $dd,$cd,$cd,$cd, $dd,$cd,$cd,$cd
	; scroll by 4
	dc.b $de,$de,$ce,$ce, $de,$de,$ce,$ce, $de,$de,$ce,$ce, $de,$de,$ce,$ce
	; scroll by 6
	dc.b $d8,$d8,$d8,$c8, $d8,$d8,$d8,$c8, $d8,$d8,$d8,$c8, $d8,$d8,$d8,$c8
#endif

	; 1560 .. 16d8 free for init data (overwritten)

	org $1400+ROWS*COLUMNS

text3:	dc.b " greetings to other vic[o groups and coders in finland and"
	dc.b " around the world ^^   keep it up ^^  ", 0
#if SCROLLS > 4
text4:	dc.b "       independent vertical scrolling for unexpanded vic[o   "
	dc.b 0
#endif

	; 0 free

	org $1778
	dc.b $55,$55,$55,$55,$55,$55,$55,$55	;   01 = border
	org $1780
	; charset 1
cs1:	dc.b 0
	org $1800


setvideomem:
	lda #$6f	; see-through character
	ldx #0
0$	sta matrix0,x
	sta matrix1,x
	sta matrix0+ROWS*COLUMNS-256,x	; some locations are written twice,
	sta matrix1+ROWS*COLUMNS-256,x	; but only upto ROWS*COLUMNS
#if ROWS*COLUMNS > 512
	sta matrix0+ROWS*COLUMNS-512,x
	sta matrix1+ROWS*COLUMNS-512,x
#endif
	inx
	bne 0$

	rts

	;align 256,0
irq:
; irq (event)	; > 7 + at least 2 cycles of last instruction (9 to 16 total)
; pha		; 3
; txa		; 2
; pha		; 3
; tya		; 2
; pha		; 3
; tsx		; 2
; lda $0104,x	; 4
; and #xx	; 2
; beq 		; 3
; jmp ($314)	; 5
		; ---
		; 38 to 45 cycles delay at this stage

	lda #<(TIMER_VALUE-46+2)	; 2 for reload time
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

	nop

#if COLUMNS < 28
	nop
#endif
#if COLUMNS < 26
	nop
#endif
#if COLUMNS < 24
	nop
#endif
#if COLUMNS < 22
	nop
#endif
#if COLUMNS < 20
	nop
#endif

lines	ldx #1	; (ROWS*4)
	ldy #3
rloop
	ldy next,x	; 4
cohptr	lda colors+$20,y	;ROWS*4,x	; 4
	sta $900f	; 4

ptab0	lda cstab+0,y	; 4
	sta $9005 	; 4
ptab1	lda cstab+0,y	; 4
	sta $9005	; 4
ptab2	lda cstab+0,y	; 4
	sta $9005	; 4
ptab3	lda cstab+0,y	; 4
	sta $9005	; 4
ptab4	lda cstab+0,y	; 4
	sta $9005	; 4
ptab5	lda cstab+0,y	; 4
	sta $9005	; 4
#if SYSTEM & PAL
ptab6	lda cstab+0,y	; 4
	sta $9005	; 4
	sta ptac6a+1	; 4
#else
	nop
	nop
	nop
#endif
	bit $ea

colptr	lda colors+$00,y
	sta $900f

ptac0	lda cstab+0,y	; 4
	sta $9005	; 4
ptac1	lda cstab+0,y	; 4
	sta $9005	; 4
ptac2	lda cstab+0,y	; 4
	sta $9005	; 4
ptac3	lda cstab+0,y	; 4
	sta $9005	; 4
ptac4	lda cstab+0,y	; 4
	sta $9005	; 4
ptac5	lda cstab+0,y	; 4
	sta $9005	; 4
#if SYSTEM & PAL
ptac6a	lda #0		; 2
	dex
	sta $9005	; 4
#else
	dex
#endif
	bne rloop	;3/2
	samepage rloop

9$	pha
	pla
	lda #8
	sta $900f
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory

	jsr chaux

	;jsr player_update
	jsr player_update

	jsr colscroll

#if 0
	dec $900f
	inc $900f
#endif

#if 1
	; Check keyboard
	lsr $9121
	bcs 95$
	; run/stop
	lda #255
	sta fadeout+1	;inc KEYWAIT+1
95$
#endif
	jmp $eb18	; return from IRQ

;nexty:	dc.b 1,2,3,0



scount:	dc.b 4,2,0,0,2,0,4	; this config calls scroll
sspeed:	dc.b 4,2,1,6,3,5,2	; max 3 times / frame

scroll:
	ldy #SCROLLS-1
0$	lda scount,y
	clc
	adc sspeed,y
	cmp #8
	and #7		; does not touch Carry flag
	sta scount,y
	bcc fine$	; 0..7
	jsr scrollit$
fine$	dey
	bpl 0$

	ldy scount+0
	lda csptr,y
	sta ptab0+1
	sta ptac0+1

	ldy scount+1
	lda csptr,y
	sta ptab1+1
	sta ptac1+1

	ldy scount+2
	lda csptr,y
	sta ptab2+1
	sta ptac2+1

	ldy scount+3
	lda csptr,y
	sta ptab3+1
	sta ptac3+1

#if SCROLLS > 4
	ldy scount+4
	lda csptr,y
	sta ptab4+1
	sta ptac4+1
#endif
#if SCROLLS > 5
	ldy scount+5
	lda csptr,y
	sta ptab5+1
	sta ptac5+1
#endif
#if SCROLLS > 6
	ldy scount+6
	lda csptr,y
	sta ptab6+1
	;sta ptac6+1
#endif
	rts

ptr$	dc.b 0,0,0,0,0,0,0
;fadecnt$
;	dc.b 0
scrollit$
	sty y$+1
	tya
	asl
	asl

	;jsr scrollcols	; scroll columns 0..2
;scrollcols:	; Must preserve Y
	tax
	;clc
	adc #3
	sta end$+1

	; Scroll the right 3 columns up by one location
20$	lda matrix1+0*COLUMNS,x	; first row m1 -> m0
	sta matrix0+0*COLUMNS,x

	lda matrix1+1*COLUMNS,x
	sta matrix1+0*COLUMNS,x
	sta matrix0+1*COLUMNS,x

	lda matrix1+2*COLUMNS,x
	sta matrix1+1*COLUMNS,x
	sta matrix0+2*COLUMNS,x

	lda matrix1+3*COLUMNS,x
	sta matrix1+2*COLUMNS,x
	sta matrix0+3*COLUMNS,x

	lda matrix1+4*COLUMNS,x
	sta matrix1+3*COLUMNS,x
	sta matrix0+4*COLUMNS,x
	lda matrix1+5*COLUMNS,x
	sta matrix1+4*COLUMNS,x
	sta matrix0+5*COLUMNS,x
	lda matrix1+6*COLUMNS,x
	sta matrix1+5*COLUMNS,x
	sta matrix0+6*COLUMNS,x
	lda matrix1+7*COLUMNS,x
	sta matrix1+6*COLUMNS,x
	sta matrix0+7*COLUMNS,x

	lda matrix1+8*COLUMNS,x
	sta matrix1+7*COLUMNS,x
	sta matrix0+8*COLUMNS,x
	lda matrix1+9*COLUMNS,x
	sta matrix1+8*COLUMNS,x
	sta matrix0+9*COLUMNS,x
	lda matrix1+10*COLUMNS,x
	sta matrix1+9*COLUMNS,x
	sta matrix0+10*COLUMNS,x
	lda matrix1+11*COLUMNS,x
	sta matrix1+10*COLUMNS,x
	sta matrix0+11*COLUMNS,x

	lda matrix1+12*COLUMNS,x
	sta matrix1+11*COLUMNS,x
	sta matrix0+12*COLUMNS,x
	lda matrix1+13*COLUMNS,x
	sta matrix1+12*COLUMNS,x
	sta matrix0+13*COLUMNS,x
	lda matrix1+14*COLUMNS,x
	sta matrix1+13*COLUMNS,x
	sta matrix0+14*COLUMNS,x
	lda matrix1+15*COLUMNS,x
	sta matrix1+14*COLUMNS,x
	sta matrix0+15*COLUMNS,x

	lda matrix1+16*COLUMNS,x
	sta matrix1+15*COLUMNS,x
	sta matrix0+16*COLUMNS,x
	lda matrix1+17*COLUMNS,x
	sta matrix1+16*COLUMNS,x
	sta matrix0+17*COLUMNS,x
	lda matrix1+18*COLUMNS,x
	sta matrix1+17*COLUMNS,x
	sta matrix0+18*COLUMNS,x
#if ROWS > 19
	lda matrix1+19*COLUMNS,x
	sta matrix1+18*COLUMNS,x
	sta matrix0+19*COLUMNS,x
#endif
#if ROWS > 20
	lda matrix1+20*COLUMNS,x
	sta matrix1+19*COLUMNS,x
	sta matrix0+20*COLUMNS,x
#endif
#if ROWS > 21
	lda matrix1+21*COLUMNS,x
	sta matrix1+20*COLUMNS,x
	sta matrix0+21*COLUMNS,x
#endif
#if ROWS > 22
	lda matrix1+22*COLUMNS,x
	sta matrix1+21*COLUMNS,x
	sta matrix0+22*COLUMNS,x
#endif
#if ROWS > 23
	lda matrix1+23*COLUMNS,x
	sta matrix1+22*COLUMNS,x
	sta matrix0+23*COLUMNS,x
#endif
#if ROWS > 24
	lda matrix1+24*COLUMNS,x
	sta matrix1+23*COLUMNS,x
	sta matrix0+24*COLUMNS,x
#endif
#if ROWS > 25
	lda matrix1+25*COLUMNS,x
	sta matrix1+24*COLUMNS,x
	sta matrix0+25*COLUMNS,x
#endif
#if ROWS > 26
	lda matrix1+26*COLUMNS,x
	sta matrix1+25*COLUMNS,x
	sta matrix0+26*COLUMNS,x
#endif
#if ROWS > 27
	lda matrix1+27*COLUMNS,x
	sta matrix1+26*COLUMNS,x
	sta matrix0+27*COLUMNS,x
#endif
#if ROWS > 28
	lda matrix1+28*COLUMNS,x
	sta matrix1+27*COLUMNS,x
	sta matrix0+28*COLUMNS,x
#endif
#if ROWS > 29
	lda matrix1+29*COLUMNS,x
	sta matrix1+28*COLUMNS,x
	sta matrix0+29*COLUMNS,x
#endif
#if ROWS > 30
	lda matrix1+30*COLUMNS,x
	sta matrix1+29*COLUMNS,x
	sta matrix0+30*COLUMNS,x
#endif
#if ROWS > 31
	lda matrix1+31*COLUMNS,x
	sta matrix1+30*COLUMNS,x
	sta matrix0+31*COLUMNS,x
#endif
	inx		; 240 bytes for 26*lda+sta+sta + lda+sta
end$	cpx #3
	beq rts$
	jmp 20$		; (ROWS*14+4)*3 = 15.5 lines for 26 rows
rts$	;rts		; 13+9*ROWS bytes (256 for 27 rows)


	; plot new row to m1
	ldx ptr$,y
	tya
	asl
	asl
	tay

	lda fontb,x
	pha
	lsr
	lsr
	lsr
	lsr
	ora #$70
	sta matrix1+ROWS*COLUMNS-COLUMNS+0,y

	;lda fontb,x
	pla
	and #$0f
	ora #$70
	sta matrix1+ROWS*COLUMNS-COLUMNS+1,y

	lda fontn,x
	and #$0f
	ora #$70
	sta matrix1+ROWS*COLUMNS-COLUMNS+2,y
	inx

y$	ldy #0
	txa
	sta ptr$,y
	tya
	tax
	dec cnt$,x
	beq newchar$
	rts

cnt$	dc.b 4,4,4,4,4,4,4

reset$	lda textptro+0,x
	sta textptr+0,x
	lda textptro+1,x
	sta textptr+1,x

	cpy #2	; when scroll2 wraps..
	bne newchar$
	;dec fadecnt$
	;bpl newchar$
	lda AUTO
	bne newchar$

	lda #128+2*ROWS
	sta fadeout+1	;inc KEYWAIT+1

newchar$
	tya
	asl
	tax
	lda (textptr,x)
	beq reset$

	and chmask
	asl
	asl
	sta ptr$,y

	lda #4
	sta cnt$,y

	inc textptr+0,x
	bne nohi$
	inc textptr+1,x
nohi$	rts




textptro:
	dc.b <text0,>text0, <text1,>text1, <text2,>text2, <text3,>text3
#if SCROLLS > 4
	dc.b <text4,>text4
#endif
#if SCROLLS > 5
	dc.b <text5,>text5
#endif
#if SCROLLS > 6
	dc.b <text6,>text6
#endif

csptr:	dc.b <(cstab+0),<(cstab+0),<(cstab+16),<(cstab+16)
	dc.b <(cstab+32),<(cstab+32),<(cstab+48),<(cstab+48)
	samepage csptr

#if 1

chaux:	; color luminance order
	; 062485371
0$	ldy #0
cnt$	lda #0
	asl
acc$	adc #0
	sta acc$+1
	bcc 1$
	iny
1$	lda $900e
	and #15
	ora COLO1$,y
	sta $900e
	inc cnt$+1
	inc cnt$+1
	bpl 2$
	lda #0
	sta cnt$+1

	ldx 0$+1
	inx
	cpx #11
	bne 4$
	ldx #0
4$	stx 0$+1

2$	rts

COLO1$	dc.b $80,$40,$60,$80,$90,$50, $30,$70,$c0,$90,$40, $80

#endif


border:
	dc.b $00, $06,$02,$04,$04,$03,$03,$01,$01,$03,$03,$04,$04,$02,$06,$00
	dc.b $00, $06,$02,$04,$04,$03,$03,$01,$01,$03,$03,$04,$04,$02,$06,$00
	dc.b $00


fadeout	lda #0
	beq nofade$
	lsr
	and #$1f
	tay
	lda #0
	sta colors,y
	sta colors+32,y
	sta border,y
	sta chmask
	dec fadeout+1
	bne nofade$
	inc loadnext+1
nofade$	jmp scroll	;32 bytes


#if SCROLLS > 6
text6:	dc.b "       pu[\] on the roll again^^", 0
#endif

	; 1b4a..1b77 free

	org $1b78
	dc.b $55,$55,$55,$55,$55,$55,$55,$55	;   01 = border
	org $1b80
	; charset 2
cs2:
	;dc.b 0
	; 1b80..1bff free for init code/data (overwritten)

zpsrc
#rorg $00
zpcode
colscroll:
	dec cnt$+1
cnt$	lda #6
	lsr
	lsr
	and #$0f
	bcc cn0$
	sta colptr+1	;0..15
	ora #$20
	sta cohptr+1	;16..31
	bne cn1$	;always

cn0$	sta cohptr+1	;0..15
	;clc
	adc #$1f	;15..30
	sta colptr+1
cn1$	;29 bytes

#if 1
	inc cc$+1
	ldx #7	;15	ABA (Art by accident)?
2$	txa
	asl
cc$	adc #0
	and #31
	tay
	lda colors+$00,x
	and #$f0
	ora border,y
	sta colors+$00,x	; odd lines
	sta colors+$10,x	; odd lines

	lda colors+$20,x
	and #$f0
	ora border+1,y
	sta colors+$20,x	; even lines
	sta colors+$30,x	; even lines

	dex
	bpl 2$
	;42 bytes
#endif

fadein$	lda lines+1	;1..(ROWS*4)
	cmp #ROWS*4
	bcc ok$
	jmp fadeout
ok$	inc lines+1
	rts
	; 14 bytes
#rend
zpend

	org $1c00

#if SCROLLS > 5
text5:	dc.b "     a colorful stretch and tech scroll coming next ", 0
#endif

	; charset 3
cs3 = $380


	; 1c35..2000
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

	org $2000

