OCOL = $4
SAME_MUS = 0	; 0 for debugging this part alone
END_MUS = 0

DOUBLEY = 1
QUADY = 1
DOUBLEX = 1

;SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3
FADE = $3cf
FADENOT = $3ea

  processor 6502


; This part by Albert of Pu-239

	; 00 - background, 01 - border, 11 - aux, 10 - character

; $1000-11f7	video matrix (init code)
; $1200-12ff	sini
; $1300-13ff	bitrev
; $1400-15ff	charset
; $1600-163f	left+right
; $1640-16bf	masks
; $16c0-174f	amplitudesrc
; $1750-176f	sinmap
; $1770-1bc1	code
; $1e00-.. music


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
RASTER	= 16	;24		; effect at RASTER + 10 (+ 1)
#else
RASTER	= 4+2	;24		; effect at RASTER + 10 (+ 1)
#endif


#if SYSTEM & PAL
COLUMNS		EQU 28
ROWS		EQU 18
#else
COLUMNS		EQU 25
ROWS		EQU 14
#endif


;zpcount = $f0

modi	= $f2
acc0	= $f3
acc1	= $f4
y1	= $f5

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


setcolmem:
#if SAME_MUS
#else
	sei	; to prevent keyboard scan to overwrite some data
#endif
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

	lda #0
	sta $9002	; columns + 9th bit of video matrix/color memory
	sta $9003	; rows & 16x8 chars

	ldx #0
	lda col2+1
0$	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	dex
	bne 0$

#if SYSTEM == NTSC
	lda #11		; RASTER not the same as the interrupt!!
#else
	lda #12		; RASTER not the same as the interrupt!!
#endif
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #$cd
	sta $9005	; c=$1000 d = $1400
#if SAME_MUS
	lda #$37
	sta $900e
#else
	lda #$30
	sta $900e
#endif
	lda col1+1
	sta col1
	lda col2+1
	sta col2

	ldx #0
2$	lda mainsrc,x
	sta main,x
	inx
	cpx #mainend-mainsrc
	bne 2$

#if SAME_MUS
	rts
#else
	jmp player_init
	;rts
#endif

mainsrc
#rorg $100
main
	lda LOADER
	cmp #$8d
	bne *

loadnext
	lda #0
	beq loadnext

	sei
	lda #<MUSVEC	;irq2	; set the raster IRQ routine pointer
	sta $314
	lda #>MUSVEC	;irq2
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
	dc.b "1CHECK.N"
#else
	dc.b "1CHECK.P"
#endif

;irq2	lda $9124
;	;jsr player_update
;	jsr player_update
;	jmp $eb18	; return from IRQ
#rend
mainend

	org $1000+ROWS*COLUMNS
	dc.b 0

	org $1200
sini:
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

	align 256,0
bitrev:
	dc.b $00,$80,$40,$c0,$20,$a0,$60,$e0,$10,$90,$50,$d0,$30,$b0,$70,$f0
	dc.b $08,$88,$48,$c8,$28,$a8,$68,$e8,$18,$98,$58,$d8,$38,$b8,$78,$f8
	dc.b $04,$84,$44,$c4,$24,$a4,$64,$e4,$14,$94,$54,$d4,$34,$b4,$74,$f4
	dc.b $0c,$8c,$4c,$cc,$2c,$ac,$6c,$ec,$1c,$9c,$5c,$dc,$3c,$bc,$7c,$fc
	dc.b $02,$82,$42,$c2,$22,$a2,$62,$e2,$12,$92,$52,$d2,$32,$b2,$72,$f2
	dc.b $0a,$8a,$4a,$ca,$2a,$aa,$6a,$ea,$1a,$9a,$5a,$da,$3a,$ba,$7a,$fa
	dc.b $06,$86,$46,$c6,$26,$a6,$66,$e6,$16,$96,$56,$d6,$36,$b6,$76,$f6
	dc.b $0e,$8e,$4e,$ce,$2e,$ae,$6e,$ee,$1e,$9e,$5e,$de,$3e,$be,$7e,$fe
	dc.b $01,$81,$41,$c1,$21,$a1,$61,$e1,$11,$91,$51,$d1,$31,$b1,$71,$f1
	dc.b $09,$89,$49,$c9,$29,$a9,$69,$e9,$19,$99,$59,$d9,$39,$b9,$79,$f9
	dc.b $05,$85,$45,$c5,$25,$a5,$65,$e5,$15,$95,$55,$d5,$35,$b5,$75,$f5
	dc.b $0d,$8d,$4d,$cd,$2d,$ad,$6d,$ed,$1d,$9d,$5d,$dd,$3d,$bd,$7d,$fd
	dc.b $03,$83,$43,$c3,$23,$a3,$63,$e3,$13,$93,$53,$d3,$33,$b3,$73,$f3
	dc.b $0b,$8b,$4b,$cb,$2b,$ab,$6b,$eb,$1b,$9b,$5b,$db,$3b,$bb,$7b,$fb
	dc.b $07,$87,$47,$c7,$27,$a7,$67,$e7,$17,$97,$57,$d7,$37,$b7,$77,$f7
	dc.b $0f,$8f,$4f,$cf,$2f,$af,$6f,$ef,$1f,$9f,$5f,$df,$3f,$bf,$7f,$ff


	org $1400
	; charset	32*4 = 128 bytes used
cs0:
#if DOUBLEX
	ds.b 2*4*4*16
#else
	ds.b 4*4*16
#endif

left:	ds.b 32
right:	ds.b 32

masks:
#if DOUBLEX
	dc.b $00,$00,$ff,$ff	;  0 00000000 00000000 1111111111111111
	dc.b $00,$01,$ff,$fe	;  1 00000000 00000001 1111111111111110
	dc.b $00,$03,$ff,$fc	;  2 00000000 00000011 1111111111111100
	dc.b $00,$07,$ff,$f8	;  3 00000000 00000111 1111111111111000
	dc.b $00,$0f,$ff,$f0	;  4 00000000 00001111 1111111111110000
	dc.b $00,$1f,$ff,$e0	;  5 00000000 00011111 1111111111100000
	dc.b $00,$3f,$ff,$c0	;  6 00000000 00111111 1111111111000000
	dc.b $00,$7f,$ff,$80	;  7 00000000 01111111 1111111110000000
	dc.b $00,$ff,$ff,$00	;  8 00000000 11111111 1111111100000000
	dc.b $01,$ff,$fe,$00	;  9 00000001 11111111 1111111000000000
	dc.b $03,$ff,$fc,$00	; 10 00000011 11111111 1111110000000000
	dc.b $07,$ff,$f8,$00	; 11 00000111 11111111 1111100000000000
	dc.b $0f,$ff,$f0,$00	; 12 00001111 11111111 1111000000000000
	dc.b $1f,$ff,$e0,$00	; 13 00011111 11111111 1110000000000000
	dc.b $3f,$ff,$c0,$00	; 14 00111111 11111111 1100000000000000
	dc.b $7f,$ff,$80,$00	; 15 01111111 11111111 1000000000000000
	dc.b $ff,$ff,$00,$00	;-16 11111111 11111111 0000000000000000
	dc.b $ff,$fe,$00,$01	;-15 11111111 11111110 0000000000000001
	dc.b $ff,$fc,$00,$03	;-14 11111111 11111100 0000000000000011
	dc.b $ff,$f8,$00,$07	;-13 11111111 11111000 0000000000000111
	dc.b $ff,$f0,$00,$0f	;-12 11111111 11110000 0000000000001111
	dc.b $ff,$e0,$00,$1f	;-11 11111111 11100000 0000000000011111
	dc.b $ff,$c0,$00,$3f	;-10 11111111 11000000 0000000000111111
	dc.b $ff,$80,$00,$7f	; -9 11111111 10000000 0000000001111111
	dc.b $ff,$00,$00,$ff	; -8 11111111 00000000 0000000011111111
	dc.b $fe,$00,$01,$ff	; -7 11111110 00000000 0000000111111111
	dc.b $fc,$00,$03,$ff	; -6 11111100 00000000 0000001111111111
	dc.b $f8,$00,$07,$ff	; -5 11111000 00000000 0000011111111111
	dc.b $f0,$00,$0f,$ff	; -4 11110000 00000000 0000111111111111
	dc.b $e0,$00,$1f,$ff	; -3 11100000 00000000 0001111111111111
	dc.b $c0,$00,$3f,$ff	; -2 11000000 00000000 0011111111111111
	dc.b $80,$00,$7f,$ff	; -1 10000000 00000000 0111111111111111
#else
	dc.b $00,$ff	;  0 00000000 11111111
	dc.b $01,$fe	;  1 00000001 11111110
	dc.b $03,$fc	;  2 00000011 11111100
	dc.b $07,$f8	;  3 00000111 11111000
	dc.b $0f,$f0	;  4 00001111 11110000
	dc.b $1f,$e0	;  5 00011111 11100000
	dc.b $3f,$c0	;  6 00111111 11000000
	dc.b $7f,$80	;  7 01111111 10000000
	dc.b $ff,$00	; -8 11111111 00000000
	dc.b $fe,$01	; -7 11111110 00000001
	dc.b $fc,$03	; -6 11111100 00000011
	dc.b $f8,$07	; -5 11111000 00000111
	dc.b $f0,$0f	; -4 11110000 00001111
	dc.b $e0,$1f	; -3 11100000 00011111
	dc.b $c0,$3f	; -2 11000000 00111111
	dc.b $80,$7f	; -1 10000000 01111111
#endif


ampsrc:
#if DOUBLEX
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0a,$0b,$0b,$0b,$0c
	dc.b $00,$02,$04,$06,$08,$0a,$0c,$0e,$0f,$11,$12,$13,$14,$15,$16,$16
	dc.b $00,$02,$05,$08,$0b,$0d,$10,$12,$14,$16,$18,$19,$1b,$1c,$1c,$1d
	dc.b $00,$03,$06,$09,$0c,$0f,$11,$14,$16,$18,$1a,$1c,$1d,$1e,$1f,$1f
	dc.b $00,$02,$05,$08,$0b,$0d,$10,$12,$14,$16,$18,$19,$1b,$1c,$1c,$1d
	dc.b $00,$02,$04,$06,$08,$0a,$0c,$0e,$0f,$11,$12,$13,$14,$15,$16,$16
	dc.b $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0a,$0b,$0b,$0b,$0c
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
#else
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$02,$04,$06,$08,$0a,$0b,$0b,$0c,$0b,$0b,$0a,$08,$06,$04,$02
	dc.b $00,$04,$08,$0c,$0f,$12,$14,$16,$16,$16,$14,$12,$0f,$0c,$08,$04
	dc.b $00,$05,$0b,$10,$14,$18,$1b,$1c,$1d,$1c,$1b,$18,$14,$10,$0b,$05
	dc.b $00,$06,$0c,$11,$16,$1a,$1d,$1f,$1f,$1f,$1d,$1a,$16,$11,$0c,$06
	dc.b $00,$05,$0b,$10,$14,$18,$1b,$1c,$1d,$1c,$1b,$18,$14,$10,$0b,$05
	dc.b $00,$04,$08,$0c,$0f,$12,$14,$16,$16,$16,$14,$12,$0f,$0c,$08,$04
	dc.b $00,$02,$04,$06,$08,$0a,$0b,$0b,$0c,$0b,$0b,$0a,$08,$06,$04,$02
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
#endif

sinmap:
#if DOUBLEX
	dc.b 0,2,4,6,8,10,12,14,15,13,11,9,7,5,3,1
	dc.b 0,2,4,6,8,10,12,14,15,13,11,9,7,5,3,1
#else
	dc.b 0,1,2,3,4,5,6,7,7,6,5,4,3,2,1,0
	dc.b 0,1,2,3,4,5,6,7,7,6,5,4,3,2,1,0
#endif

start
	;ldx #255
	;txs
	jsr setcolmem	; is overwritten by setvideomem
	jsr setvideomem

	lda #OCOL*17+8
	sta $900f

#if SAME_MUS
	sei
#endif
;synchronize with the screen

sync:	ldx #RASTER	; wait for this raster line (times 2)
0$:	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$:	ldx $9004
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
	bne 1$
	; now it is fully synchronized
	; 6 cycles have passed since last $9004 change
	; and we are on line 2(28+9)=74

;initialize the timers
timers:
	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126

#if SYSTEM & PAL
	nop		; make a little delay to get the raster effect to the
	nop		; right place
#endif
	bit $ea

	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts


#if SAME_MUS
	;jsr player_update
	jsr player_update
#endif
	lda $9124
	cli
#if 0
KEYWAIT	lda #0
	beq KEYWAIT

	;inc $900f
	jmp KEYWAIT
#else
	jmp main
#endif

setvideomem:
	ldx #COLUMNS-1
0$	txa
#if DOUBLEX
	and #7
#else
	and #3
#endif
#if DOUBLEY
	asl
#endif
	asl
	sta matrix0+0*COLUMNS,x
	sta matrix0+4*COLUMNS,x
	sta matrix0+8*COLUMNS,x
	sta matrix0+12*COLUMNS,x
#if ROWS > 16
	sta matrix0+16*COLUMNS,x
#endif
#if ROWS > 20
	sta matrix0+20*COLUMNS,x
#endif
#if ROWS > 24
	sta matrix0+24*COLUMNS,x
#endif
#if ROWS > 28
	sta matrix0+28*COLUMNS,x
#endif
	clc
	adc #1
	sta matrix0+1*COLUMNS,x
	sta matrix0+5*COLUMNS,x
	sta matrix0+9*COLUMNS,x
	sta matrix0+13*COLUMNS,x
#if ROWS > 17
	sta matrix0+17*COLUMNS,x
#endif
#if ROWS > 21
	sta matrix0+21*COLUMNS,x
#endif
#if ROWS > 25
	sta matrix0+25*COLUMNS,x
#endif
#if ROWS > 29
	sta matrix0+29*COLUMNS,x
#endif
	clc
#if DOUBLEY
	adc #1
#else
	sbc #0	;adc #1
#endif
	sta matrix0+2*COLUMNS,x
	sta matrix0+6*COLUMNS,x
	sta matrix0+10*COLUMNS,x
	sta matrix0+14*COLUMNS,x
#if ROWS > 18
	sta matrix0+18*COLUMNS,x
#endif
#if ROWS > 22
	sta matrix0+22*COLUMNS,x
#endif
#if ROWS > 26
	sta matrix0+26*COLUMNS,x
#endif
#if ROWS > 30
	sta matrix0+30*COLUMNS,x
#endif
	clc
	adc #1
	sta matrix0+3*COLUMNS,x
	sta matrix0+7*COLUMNS,x
	sta matrix0+11*COLUMNS,x
	sta matrix0+15*COLUMNS,x
#if ROWS > 19
	sta matrix0+19*COLUMNS,x
#endif
#if ROWS > 23
	sta matrix0+23*COLUMNS,x
#endif
#if ROWS > 27
	sta matrix0+27*COLUMNS,x
#endif
#if ROWS > 31
	sta matrix0+31*COLUMNS,x
#endif
	dex
	bpl 0$

	rts


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

#if SYSTEM & PAL
	samepage irq
#else
	nop
	bit $ea
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
#endif
	lda #ROWS*2+1	; rows & 16x8 chars
	sta $9003
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory

#if 1
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	dec fade+1
95$
#endif
	lda $9111
	and #$20
	beq button$
	jsr calc
button$

#if 0
	lda $9122
	tay
	and #$7f
	sta $9122	; sw3 to input
	lda $9120
	sty $9122	; restore DDR
	and #$80
	bne notright$
	;inc ampl
notright$
	lda $9111
	and #$10
	bne notleft$
	;dec ampl
notleft$
#endif

	lda $9111	; VIA#1 port A
	and #$08	; sw2? left
	bne notdown$
	jsr chcolor
notdown$
	lda $9111
	and #$04	; $10 left $08 down $04 up
	beq up$

	inc count
	lda count
	and #63
	bne up$
	inc cosoff+1
up$

	
2$	;jsr player_update
	jsr player_update

	jsr champ	; 96/8 = 12 frames to change totally

	jsr scroll

	;dec $900f
	;inc $900f

fade	lda #0
	beq 0$

	lda col2
	sta col1	
	asl
	asl
	asl
	asl
	ora #8
	ora col1
	sta $900f

	dec fade+1
	lda fade+1
	cmp #100
	bcs 0$
	inc loadnext+1	; todo: use FADE first
	inc fade+1
0$
	jmp $eb18	; return from IRQ


count	dc.b 0

calc:
AMP_SIZE = 16
SIZE = 16
	inc modi	;5

	lda modi	;3
	;lsr
	lsr		;2
	clc		;2
cosoff	adc #AMP_SIZE/4	;2
	and #AMP_SIZE-1	;2
	asl 		;2
	asl		;2
	asl		;2
	asl		;2
	;asl
	sta sinp+1	;4

	ldy #0		;2
	sty y1		;4
yloop:
	lda modi	;3
	;lsr
#if DOUBLEX
	and #(2*AMP_SIZE-1)	;2
#else
	and #(AMP_SIZE-1)*2
#endif
	asl		;2
	asl		;2
	asl		;2
	;asl
	tax		;2

	; Plot X--X or -XX-
	lda y1 	        ;3
	clc 		;2
	adc sini+0,x	;4
	asl 		;2
	lda #0		;2
	rol 		;2
	sta acc0	;3

	lda y1		;3
	adc sini+1,x	;4
	asl 		;3
	rol acc0	;5

	lda y1		;3
	adc sini+2,x	;4
	asl 		;2
	rol acc0	;5
	lda y1		;3
	adc sini+3,x	;4
	asl 		;2
	rol acc0	;5
	lda y1		;3
	adc sini+4,x	;4
	asl 		;2
	rol acc0	;5
	lda y1		;3
	adc sini+5,x	;4
	asl 		;2
	rol acc0	;5
	lda y1		;3
	adc sini+6,x	;4
	asl 		;2
	rol acc0	;5
	lda y1		;3
	adc sini+7,x	;4
	asl 		;2
	rol acc0	;5

	lda y1
	adc sini+7,x
	asl
	lda #0
	rol
	sta acc1
	lda y1
	adc sini+6,x
	asl
	rol acc1
	lda y1
	adc sini+5,x
	asl
	rol acc1
	lda y1
	adc sini+4,x
	asl
	rol acc1
	lda y1
	adc sini+3,x
	asl
	rol acc1
	lda y1
	adc sini+2,x
	asl
	rol acc1
	lda y1
	adc sini+1,x
	asl
	rol acc1
	lda y1
	adc sini+0,x
	asl
	rol acc1

	ldx sinmap,y	;4
sinp:	lda sini+0,x	;4 6D adc  ED sbc
	lsr 		;2
	lsr		;2
	lsr		;2
#if DOUBLEX
	cpy #SIZE	;2
	bcs ok$		;3/2

	eor #2*SIZE-1
	adc #1
	and #2*SIZE-1

ok$	asl
	asl
	and #127
	tax
	lda modi
	lsr
	bcc sides$

	lda acc0
	eor masks+1,x
	sta left,y
	lda acc1
	eor masks+2,x
	sta right,y
	jmp ov$

sides$	lda acc0
	eor masks+0,x
	sta left,y
	lda acc1
	eor masks+3,x
	sta right,y
ov$
#else
	lsr
	cpy #SIZE
	bcs ok$

	eor #SIZE-1
	adc #1
	and #SIZE-1
ok$	asl
	tax
	lda acc0
	eor masks+0,x
	sta left,y
	lda acc1
	eor masks+1,x
	sta right,y
#endif
	lda y1
	clc
#if QUADY
	adc #SIZE/2
#else
	adc #SIZE
#endif
	sta y1
	iny
	cpy #2*SIZE
	beq out
	jmp yloop
out:	; approx 9710 cyc = 137 lines (16(X)x32(Y) pixels)


#if DOUBLEX
	lda #<left
	sta l0$+1
	sta l1$+1
	lda #<right
	sta r0$+1
	sta r1$+1

#if DOUBLEY
	ldx #2*2*SIZE-2
#else
	ldx #2*SIZE-1
#endif
	ldy #0

	lda modi
	lsr
	bcc sides$
middle$
#if DOUBLEY
0$
l0$	lda left+0
	sta cs0+2*2*SIZE+0,x	; top left
	sta cs0+2*2*SIZE+1,x
	sta cvt0$+1
cvt0$	lda bitrev
	sta cs0+2*12*SIZE+0,y	; bottom right
	sta cs0+2*12*SIZE+1,y
r0$	lda right+0
	sta cs0+2*4*SIZE+0,x	; top right
	sta cs0+2*4*SIZE+1,x
	sta cvt1$+1
cvt1$	lda bitrev
	sta cs0+2*10*SIZE+0,y	; bottom left
	sta cs0+2*10*SIZE+1,y
	inc l0$+1
	inc r0$+1
	iny
	iny
	dex
	dex
	bpl 0$
	jmp 3$
#else
0$
l0$	lda left+0
	sta cs0+2*SIZE,x
	sta cvt0$+1
cvt0$	lda bitrev
	sta cs0+12*SIZE+0,y
r0$	lda right+0
	sta cs0+4*SIZE+0,x
	sta cvt1$+1
cvt1$	lda bitrev
	sta cs0+10*SIZE+0,y
	inc l0$+1
	inc r0$+1
	iny
	dex
	bpl 0$
	jmp 3$
#endif
sides$
#if DOUBLEY
1$
l1$	lda left+0
	sta cs0+2*0*SIZE,x
	sta cs0+2*0*SIZE+1,x
	sta cvt2$+1
cvt2$	lda bitrev
	sta cs0+2*14*SIZE+0,y
	sta cs0+2*14*SIZE+1,y
r1$	lda right+0
	sta cs0+2*6*SIZE+0,x
	sta cs0+2*6*SIZE+1,x
	sta cvt3$+1
cvt3$	lda bitrev
	sta cs0+2*8*SIZE+0,y
	sta cs0+2*8*SIZE+1,y
	inc l1$+1
	inc r1$+1
	iny
	iny
	dex
	dex
	bpl 1$
#else
1$
l1$	lda left+0
	sta cs0+0*SIZE,x
	sta cvt2$+1
cvt2$	lda bitrev
	sta cs0+14*SIZE+0,y
r1$	lda right+0
	sta cs0+6*SIZE+0,x
	sta cvt3$+1
cvt3$	lda bitrev
	sta cs0+8*SIZE+0,y
	inc l1$+1
	inc r1$+1
	iny
	dex
	bpl 1$
#endif
3$
#else
	lda #<left
	sta l$+1
	lda #<right
	sta r$+1
#if DOUBLEY
	ldx #2*2*SIZE-2
	ldy #0
0$
l$	lda left+0
	sta cs0+2*0*SIZE+0,x
	sta cs0+2*0*SIZE+1,x
	sta cvt0$+1
cvt0$	lda bitrev
	sta cs0+2*6*SIZE+0,y
	sta cs0+2*6*SIZE+1,y
r$	lda right+0
	sta cs0+2*2*SIZE+0,x
	sta cs0+2*2*SIZE+1,x
	sta cvt1$+1
cvt1$	lda bitrev
	sta cs0+2*4*SIZE+0,y
	sta cs0+2*4*SIZE+1,y
	inc l$+1
	inc r$+1
	iny
	iny
	dex
	dex
	bpl 0$
#else
	ldx #2*SIZE-1	;2*2*SIZE-2
	ldy #0
0$
l$	lda left+0
	sta cs0+0*SIZE+0,x	; sta cs0+2*0*SIZE+0,x
	;sta cs0+2*0*SIZE+1,x
	sta cvt0$+1
cvt0$	lda bitrev
	sta cs0+6*SIZE+0,y	; sta cs0+2*6*SIZE+0,y
	;sta cs0+2*6*SIZE+1,y
r$	lda right+0
	sta cs0+2*SIZE+0,x	; sta cs0+2*2*SIZE+0,x
	;sta cs0+2*2*SIZE+1,x
	sta cvt1$+1
cvt1$	lda bitrev
	sta cs0+4*SIZE+0,y	; sta cs0+2*4*SIZE+0,y
	;sta cs0+2*4*SIZE+1,y
	inc l$+1
	inc r$+1
	;iny
	iny
	;dex
	dex
	bpl 0$
#endif
#endif
	rts


ampl:	dc.b $01
ampcnt:	dc.b 0

champ:
	ldx #8-1	; change 8*2 of 96*2 at a time

loop$	lda ampl
	eor #$ff	; switch bcc->bcs
	sta y1
	ldy ampcnt
	lda ampsrc,y
	sta acc0
	; 3 x 5 -bit multiplier routine y1 * acc0
	lda #0
	lsr y1
	bcs 1$
	adc acc0
1$	asl acc0
	lsr y1
	bcs 2$
	adc acc0
2$	asl acc0
	lsr y1
	bcs 4$
	adc acc0
4$
;	asl acc0
;	lsr y1
;	bcs 8$
;	;clc
;	adc acc0
8$	sta sini,y
	lda #0
	sec
	sbc sini,y
	sta sini+128,y

	iny
	cpy #128-16
	bne 0$
	ldy #16
0$	sty ampcnt

	dex
	bpl loop$
	rts

scrolltext:
	dc.b "     GIVE A NEW WAVE PLEASE!", $a0
scrolltmp:
	ds.b 8

color	dc.b 0
; magenta + lt purple
; green + lt green
;               yel mgt cyn grn yel red yel
;	        grn pur blu lgr lorg pink lyel
;col1	dc.b 0, $7, $4, $3, $5, $7, $2, $7	; $2 $7
;col2	dc.b 0, $5, $c, $6, $d, $9, $a, $f	; $a $f

; 3x 4c 36 35

; 00- 01- 02+ 03- 04+ 05~ 06! 07- 08! 09- 0a- 0b- 0c- 0d- 0e+ 0f-
; 10- 11- 12- 13+ 14- 15+ 16- 17~ 18+ 19+ 1a~ 1b! 1c+ 1d+ 1e+ 1f~
; 20~ 21- 22- 23- 24~ 25~ 26+ 27- 28! 29+ 2a+ 2b- 2c~ 2d- 2e~ 2f- 
; 30- 31- 32- 33- 34- 35+ 36- 37- 38~ 39- 3a+ 3b+ 3c- 3d+ 3e+ 3f-
; 40+ 41- 42! 43- 44- 45- 46  47- 48- 49+ 4a+ 4b- 4c! 4d- 4e+ 4f-
; 50- 51- 52- 53+ 54- 55- 56~ 57- 58+ 59- 5a- 5b+ 5c- 5d! 5e- 5f~
; 60+ 61- 62- 63~ 64+ 65~ 66- 67- 68! 69+ 6a+ 6b~ 6c- 6d- 6e! 6f-
; 70- 71- 72~ 73- 74- 75~ 76- 77- 78~ 79~ 7a~ 7b- 7c- 7d- 7e~ 7f+
; Darker in col2, lighter in col1? because scroll is white, so closer to col2
; third entry has to have blue in col2
col1	dc.b 0, $1,   $2,$5,$3,$2,$4,$1,$0,$4,$2,$6,$3
col2	dc.b 0, OCOL, $6,$d,$6,$8,$c,$b,$8,$2,$a,$4,$5

chcolor	lda fade+1
	beq 0$
	rts
0$	lda color
	clc
	adc #1
	cmp #12
	bcc ok$
	lda #1	; start from the second..

ok$	cmp #4
	bne noauto$
	ldx AUTO
	bne noauto$
	dec fade+1
	rts
noauto$
	sta color
	tay
	lda col1+1,y
	sta col1
	lda col2+1,y
	sta col2
	asl
	asl
	asl
	asl
	ora #8
	sta $900f
	rts

scroll:			; scroll only every other frame :-(
#if SYSTEM == NTSC
	lda #0
	tay
	clc
	adc #COLUMNS
	sta scroll+1
	sbc #0
	sta end$+1

start$
	lda $9400+0*COLUMNS+1,y		; trade speed for space
	sta $9400+0*COLUMNS+0,y		; 2-line copy loop
	sta $9400+2*COLUMNS+0,y

	lda $9400+4*COLUMNS+1,y
	sta $9400+4*COLUMNS+0,y

	lda $9400+6*COLUMNS+1,y
	sta $9400+6*COLUMNS+0,y

	lda $9400+8*COLUMNS+1,y
	sta $9400+8*COLUMNS+0,y

	lda $9400+10*COLUMNS+1,y
	sta $9400+10*COLUMNS+0,y

	lda $9400+12*COLUMNS+1,y
	sta $9400+12*COLUMNS+0,y
#if ROWS > 14
	sta $9400+14*COLUMNS+0,y
#if ROWS > 16
	sta $9400+16*COLUMNS+0,y
#endif
#endif

	iny
end$	cpy #COLUMNS-1
	bne start$
	cpy #2*COLUMNS-1
	bne 1$
	ldy #0
	sty scroll+1
	rts
1$


#else
	lda #0
	eor #1
	sta scroll+1
	beq do$
	rts
do$

	ldy #0
0$
	lda $9400+0*COLUMNS+1,y		; trade speed for space
	sta $9400+0*COLUMNS+0,y		; 2-line copy loop
	;lda $9400+2*COLUMNS+1,y		; copies some unneeded bytes
	sta $9400+2*COLUMNS+0,y
	;lda $9400+4*COLUMNS+1,y
	sta $9400+4*COLUMNS+0,y

	lda $9400+6*COLUMNS+1,y
	sta $9400+6*COLUMNS+0,y

	lda $9400+8*COLUMNS+1,y
	sta $9400+8*COLUMNS+0,y

	lda $9400+10*COLUMNS+1,y
	sta $9400+10*COLUMNS+0,y

	lda $9400+12*COLUMNS+1,y
	sta $9400+12*COLUMNS+0,y

	lda $9400+14*COLUMNS+1,y
	sta $9400+14*COLUMNS+0,y
	;lda $9400+16*COLUMNS+1,y
	sta $9400+16*COLUMNS+0,y

	iny
	cpy #2*COLUMNS-1
	bne 0$
#endif
	; plot a new column

	ldx #0
	ldy #0
plot$	lda scrolltmp,y
col$	and #$80
	beq 11$
	lda fade+1
	bne 11$
	lda #1
	bne 12$		; jump always
11$	lda col1
#if SYSTEM == NTSC
12$	sta $9400+4*COLUMNS+COLUMNS-1,x	; 4,5,6,7,8,9,10
#else
12$	sta $9400+6*COLUMNS+COLUMNS-1,x	; 6,7,8,9,10,11,12
#endif
	txa
	clc
	adc #COLUMNS
	tax
	iny
	cpy #7
	bne plot$

	lda col1		; 'fade in' also the top and bottom rows
	sta $9400+0*COLUMNS+COLUMNS-1
	sta $9400+1*COLUMNS+COLUMNS-1
	sta $9400+2*COLUMNS+COLUMNS-1
	sta $9400+3*COLUMNS+COLUMNS-1
#if SYSTEM == NTSC
	sta $9400+11*COLUMNS+COLUMNS-1
	sta $9400+12*COLUMNS+COLUMNS-1
#else
	sta $9400+4*COLUMNS+COLUMNS-1
	sta $9400+5*COLUMNS+COLUMNS-1
#endif
	sta $9400+13*COLUMNS+COLUMNS-1
	sta $9400+14*COLUMNS+COLUMNS-1
	sta $9400+15*COLUMNS+COLUMNS-1
	sta $9400+16*COLUMNS+COLUMNS-1
	sta $9400+17*COLUMNS+COLUMNS-1

	lsr col$+1
	bne notnext$

ptr$	ldx #0
	inx
	lda scrolltext-1,x
	bpl asc$

	; Change amplitude
	lda ampl
	clc
	adc #1
	and #3
	bne jes$
	lda #1
jes$	sta ampl

	jsr chcolor

	ldx #0		; reset text pointer
	lda #32		; insert space

asc$	stx ptr$+1
	asl
	asl
	asl
	sta src$+1
	lda #$40
	rol
	sta src$+2

	lda #$80
	sta col$+1

	ldy #6
src$	lda $8000+8*32,y
	sta ora$+1
	lsr
ora$	ora #0
	sta scrolltmp,y
	dey
	bpl src$

notnext$

noupd$
	rts


#if SAME_MUS

MUSVEC = $1c00
player_update = MUSVEC+6

#else

	 .org $1c00
INCSONG equ 1
#include "../music/acplay2.a65"
;#include "../music/cutie.sng"
;#include "../music/canon3.sng"
#include "../music/mmix.sng"
;#include "../music/vicual.sng"
;#include "../music/voyage.sng"

#endif

	org $2000

