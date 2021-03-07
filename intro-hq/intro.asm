NTSC = 1
PAL  = 0
SAME_MUS = 1
END_MUS = 0

;SYSTEMSEL = $31a	; 0 for NTSC, 1 for PAL
AUTO      = $31b	; 0 for manual, 1 for automatic control
LOADER    = $31c	; ..$3e3
FADE = $3cf
FADENOT = $3ea

  processor 6502


tPtr  = $00	; 0..1
; codeorg	; $220..$2d8
; $1000-11e0	video matrix
; $11e0-1400	code, scrolltext
; $1400-1ca4	charfont
; $1ca4-2000	code, music

#if SYSTEM == NTSC

COLUMNS = 3*8	; must be even
ROWS = 12
TOPPOS = 26
RASTER = 20	;LINES/2-6

LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER = 26

#else

COLUMNS = 3*8	; 24 of 29/26	must be even
ROWS = 17
TOPPOS = 26
RASTER = 24	;LINES/2-6

LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER = 34

#endif

TIMER_VALUE = LINES * CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm



; The BASIC line - Note: it is overwritten by the video matrix quite quickly

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

	.org $1010
start
	lda #0
	sta AUTO

	; detect the system
	lda #0
	sta $9002

	sei

	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

	jsr setcolmem
	jsr inittext
#if SAME_MUS == 0
	jsr player_init
#endif

;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #0		; disable Timer A free run
	sta $912b
#if 0
sync:	ldx #RASTER		; wait for this raster line (times 2)
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
#else
sync:	ldx #RASTER-7	; wait for this raster line (times 2)
0$:	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$:	ldx $9004
	txa
	bit $24
	ldx #24
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne 1$
	; now it is fully synchronized
	; 6 cycles have passed since last $9004 change
#endif
	; No cycle-exact needed for this part..

	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	ldy #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sty $9126
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change

#if SYSTEM == NTSC
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
#else
	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts
#endif
	;lda #$82
	;sta $911e	; enable Restore key

	lda #0
	tax
	jmp continit


setcolmem:
	ldx #0
	lda #0
1$	sta $9400,x
	sta $9500,x
	dex
	bne 1$

	ldx #0
12$	lda codesrc,x
	sta codeorg,x
	inx
	cpx #codeend-codesrc
	bne 12$

	lda #TOPPOS
	sta $9001	; vertical centering

	ldx #SCRCENTER-COLUMNS	; centered
	stx $9000	; horizontal centering
	ldx #ROWS*2+1	; rows & 16x8 chars
	stx $9003
	lda #$cd	; matrix at $1000, chars at $1400..
	sta $9005
	lda #1*16+8+0
	sta $900f
	rts


codesrc
#rorg $200
codeorg
continit:
1$	sta $1000,x
	sta $1000+COLUMNS*ROWS/2,x
	inx
	cpx #COLUMNS*ROWS/2
	bne 1$

	;jmp mloop

mloop	cli
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory

KEYWAIT	lda #0
	beq KEYWAIT

	sei
	lda #<MUSVEC	;irq2
	sta $0314
	lda #>MUSVEC	;irq2
	sta $0315
	cli

	lda #3
	ldx #<nextFile
	ldy #>nextFile
	jsr LOADER
	bcs *

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


CR	lda (tPtr),y
	bne lrts

	ldx AUTO
	bne inittext
	inc KEYWAIT+1

inittext:
	lda #<text
	sta tPtr+0
	lda #>text
	sta tPtr+1
lrts	rts


puttextline:
	ldy #0		; calculate line length
	ldx #0
0$	lda (tPtr),y
	iny
	cmp #13
	beq 2$
	cmp #14
	beq 2$
	cmp #"I"
	beq 1$
	cmp #" "
	beq 1$
	inx
1$	inx
	inx
	bne 0$		; jump always
2$	stx 3$+1
	lda #COLUMNS
	sec
3$	sbc #0
	lsr
	clc
	tax		; start column

4$	ldy #0		; plot line
	lda (tPtr),y
	inc tPtr+0
	bne 5$
	inc tPtr+1
5$	cmp #13
	;bne 6$
	beq CR	;jmp CR
6$	cmp #14
	bne 8$
	jmp CR
8$	cmp #" "
	beq 7$		; space, no need to update anything
	and #31
	sta 66$+1
	asl
	sta adc$+1
	asl
adc$	adc #0
	tay
	lda map+0,y
	sta $1000+ROWS*COLUMNS-2*COLUMNS+0,x
	lda map+1,y
	sta $1000+ROWS*COLUMNS-1*COLUMNS+0,x
	lda map+2,y
	sta $1000+ROWS*COLUMNS-2*COLUMNS+1,x
	lda map+3,y
	sta $1000+ROWS*COLUMNS-1*COLUMNS+1,x
66$	lda #0
	cmp #9	; "I"
	beq 7$
	cmp #0	; " "
	beq 7$
	lda map+4,y
	sta $1000+ROWS*COLUMNS-2*COLUMNS+2,x
	lda map+5,y
	sta $1000+ROWS*COLUMNS-1*COLUMNS+2,x
	inx
7$	inx
	inx
	bne 4$		; jump always
#rend
codeend

#if SYSTEM == NTSC
	.org $1000 + 24*16	;COLUMNS*ROWS +160
#else
	.org $1000 + COLUMNS*ROWS
#endif
; unused chars: G, Q
text:
	dc.b "THE WAIT",13
	dc.b "IS NOW",13
	dc.b "COMPLETE",13
#if SYSTEM != NTSC
	dc.b 14
#endif
	dc.b "FOR A",13
	dc.b "NEW DEMO",13
#if SYSTEM != NTSC
	dc.b 14
#endif
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "VICUAL",13
	dc.b "MMIX",13
#if SYSTEM != NTSC
	dc.b 13
#endif
	dc.b "BY",13
	dc.b "ALBERT",13
	dc.b "OF PU[<]",13
#if SYSTEM != NTSC
	dc.b 14
#endif
	dc.b 128
#if 1
	dc.b 14
	dc.b 14
	dc.b 14,14
	dc.b "MUSIC BY",13
	dc.b "ZAPAC",13
	dc.b 14
#if SYSTEM != NTSC
	dc.b 14
#endif
	dc.b 14
	dc.b 128
#endif
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "X FOR",13
	dc.b "MANUAL",13
	dc.b "MODE",13
	dc.b 13
#if SYSTEM != NTSC
	dc.b 14
#endif
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b "RUNSTOP",13
#if SYSTEM != NTSC
	dc.b 13
#endif
	dc.b "TO SKIP",13
	dc.b "PARTS IN",13
#if SYSTEM != NTSC
	dc.b 13
#endif
	dc.b "MANUAL",13
	dc.b "MODE",13
#if SYSTEM != NTSC
	dc.b 14
#endif
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "ENJOY",13
;	dc.b "THE SHOW",13
	dc.b 14
	dc.b 14
	dc.b 14
;	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 13
	dc.b 0



plot
5$	lda #0
	and #1
	beq 0$

	ldy #0
	lda (tPtr),y
	bne 8$
	jmp CR

8$	bpl 9$
	sta swait+1
	bmi 10$
9$	cmp #13
	bne 6$
	; if an empty line, just one empty line
10$	inc tPtr+0
	bne 7$
	inc tPtr+1
7$	rts

6$	jsr puttextline
0$	inc 5$+1
	rts

nextFile:
#if SYSTEM == NTSC
	dc.b "SPIRAL.N"	; 8 significant chars
#else
	dc.b "SPIRAL.P"	; 8 significant chars
#endif

scroll:
	lda #TOPPOS
	sta $9001

	ldx #COLUMNS-1
1$	lda $1000+1*COLUMNS,x	;4
	sta $1000+0*COLUMNS,x	;5
	lda $1000+2*COLUMNS,x
	sta $1000+1*COLUMNS,x
	lda $1000+3*COLUMNS,x
	sta $1000+2*COLUMNS,x
	lda $1000+4*COLUMNS,x
	sta $1000+3*COLUMNS,x
	dex
	bpl 1$		;24*41 = 984 = ~14 lines

	ldx #COLUMNS-1
2$	lda $1000+5*COLUMNS,x
	sta $1000+4*COLUMNS,x
	lda $1000+6*COLUMNS,x
	sta $1000+5*COLUMNS,x
	lda $1000+7*COLUMNS,x
	sta $1000+6*COLUMNS,x
	lda $1000+8*COLUMNS,x
	sta $1000+7*COLUMNS,x
	lda $1000+9*COLUMNS,x
	sta $1000+8*COLUMNS,x
	dex
	bpl 2$

	ldx #COLUMNS-1
3$	lda $1000+10*COLUMNS,x
	sta $1000+9*COLUMNS,x
	lda $1000+11*COLUMNS,x
	sta $1000+10*COLUMNS,x
	lda $1000+12*COLUMNS,x
	sta $1000+11*COLUMNS,x
	lda $1000+13*COLUMNS,x
	sta $1000+12*COLUMNS,x
	lda $1000+14*COLUMNS,x
	sta $1000+13*COLUMNS,x
	dex
	bpl 3$

	ldx #COLUMNS-1
4$
	lda $1000+15*COLUMNS,x
	sta $1000+14*COLUMNS,x
#if ROWS > 16
	lda $1000+16*COLUMNS,x
	sta $1000+15*COLUMNS,x
#endif
#if ROWS > 17
	lda $1000+17*COLUMNS,x
	sta $1000+16*COLUMNS,x
#endif
#if ROWS > 18
	lda $1000+18*COLUMNS,x
	sta $1000+17*COLUMNS,x
#endif
#if ROWS > 19
	lda $1000+19*COLUMNS,x
	sta $1000+18*COLUMNS,x
#endif
	lda #0
	sta $1000+ROWS*COLUMNS-COLUMNS,x	; clear the last line

12$	dex
	bpl 4$

	rts


	.org $1380
back:
#if 0
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$88,$88,$28,$28
	dc.b $08,$28,$28,$88,$28,$88,$88,$88,$98,$98,$78,$78,$f8,$f8,$18,$18
	dc.b $18,$18,$f8,$f8,$f8,$78,$78,$98,$98,$88,$88,$88,$28,$88,$28,$28
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$88,$88,$28,$28

	dc.b $08,$6e,$2a,$4c,$4c,$3b,$3b,$19
	dc.b $08,$6e,$2a,$4c,$4c,$3b,$3b,$19,$19,$3b,$3b,$4c,$4c,$2a,$6e,$08
	dc.b $19,$3b,$3b,$4c,$4c,$2a,$6e,$08

	dc.b $08,$2a,$2a,$4c,$5d,$3b,$7f,$19
	dc.b $08,$2a,$2a,$4c,$5d,$3b,$7f,$19,$19,$7f,$3b,$5d,$4c,$2a,$2a,$08
	dc.b $19,$7f,$3b,$5d,$4c,$2a,$2a,$08
#else
	dc.b $08,$28,$28,$88,$28,$88,$88,$88,$98,$98,$78,$78,$f8,$f8,$18,$18
	dc.b $18,$18,$f8,$f8,$f8,$78,$78,$98,$98,$88,$88,$88,$28,$88,$28,$28

	dc.b $08,$68,$28,$48,$48,$38,$b8,$18
	dc.b $08,$68,$48,$48,$e8,$38,$b8,$18,$18,$b8,$38,$e8,$48,$48,$68,$08
	dc.b $18,$b8,$38,$e8,$48,$48,$68,$08

	;dc.b $08,$68,$28,$48,$48,$38,$38,$18
	;dc.b $08,$68,$28,$48,$48,$38,$38,$18,$18,$38,$38,$48,$48,$28,$68,$08
	;dc.b $18,$38,$38,$48,$48,$28,$68,$08

	dc.b $08,$28,$28,$88,$28,$88,$88,$88,$98,$98,$78,$78,$f8,$f8,$18,$18
	dc.b $18,$18,$f8,$f8,$f8,$78,$78,$98,$98,$88,$88,$88,$28,$88,$28,$28

	dc.b $08,$68,$28,$48,$48,$38,$b8,$18
	dc.b $08,$68,$48,$48,$e8,$38,$b8,$18,$18,$b8,$38,$e8,$48,$48,$68,$08
	dc.b $18,$b8,$38,$e8,$48,$48,$68,$08
	;dc.b $08,$68,$28,$48,$48,$38,$38,$18
	;dc.b $08,$68,$28,$48,$48,$38,$38,$18,$18,$38,$38,$48,$48,$28,$68,$08
	;dc.b $18,$38,$38,$48,$48,$28,$68,$08
#endif


	org $1400

	incbin "packed.bin"
map:
	incbin "map.bin"


irq:	; No stable raster needed in this part!

#if 1
cstart	ldy #0
	ldx #0
loop

#if SYSTEM == NTSC
	lda $9124	; ack interrupt

	nop
	nop
	nop
	nop
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla

	tya
	and #127
	tay
	lda back,y
autoeor	eor #0
	iny
	inx		; 2
	sta $900f	; 4
#else
	tya
	and #127
	tay
	lda back,y
autoeor	eor #0
	sta $900f	; 4

	lda $9124	; ack interrupt

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
	pha
	pla

	iny
	inx		; 2
#endif

#if SYSTEM == NTSC
	cpx #(ROWS-1)*16	;16
#else
	cpx #240
#endif
	bne loop	; 3

#endif

	lda #8
	sta $900f

swait	lda #0
	beq 0$
	dec swait+1

	dec colcnt
	lda colcnt
	lsr
	sta cstart+1
	jmp reti

0$	dec $9001
#if SYSTEM != NTSC
	dec $9001	; 2 lines/field for PAL
#endif

#if 1
	lda colcnt
	clc
#if SYSTEM == NTSC
	adc #4-1
#else
	adc #2*4-1
#endif
	sta colcnt
	lsr
	sta cstart+1
#else
	lda cstart+1
	clc
#if SYSTEM == NTSC
	adc #2
#else
	adc #4
#endif
	sta cstart+1
#endif

1$	lda $9001
	cmp #TOPPOS-2
	bne 2$
	jsr plot
	jmp reti
2$	cmp #TOPPOS-8
	bne reti
	jsr scroll
reti
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	inc KEYWAIT+1
95$	and #2
	bne 97$
	; x
	;ldx AUTO
	;bne 97$

	;inc AUTO	; disable automatic control
	lda #8
	sta autoeor+1
	sta AUTO
97$
	jmp MUSVEC	;irq2

colcnt	dc.b 0



#if SAME_MUS == 0

    org $1cc0
INCSONG equ 1
#include "../music/acplay2.a65"
#include "../music/folk2.sng"

#else
MUSVEC = $1cc0
player_update = MUSVEC+6
#endif

	org $2000


