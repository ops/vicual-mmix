SAME_MUS = 0
END_MUS = 0

;SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3
FADE = $3cf
FADENOT = $3ea


  processor 6502


; This part by Albert of Pu-239

	; 00 - background, 01 - border, 11 - aux, 10 - character

; $1000-	video matrix 0
; $1380-1400	charset 0


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
RASTER	= 16+4	;24		; effect at RASTER + 10 (+ 1)
#else
RASTER	= 8+4	;24		; effect at RASTER + 10 (+ 1)
#endif


#if SYSTEM & PAL
COLUMNS		EQU 30
ROWS		EQU 28	;30 (music is slow..)
#else
COLUMNS		EQU 26
ROWS		EQU 25
#endif

textptr = $f2	; $f2/f3, $f4/$f5, $f6/f7, $f8/f9, $fa/fb, $fc/fd, $fe/ff
zpcount = $f0
zp0 = $f1

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

	;lda #0
	sta $9002	; columns + 9th bit of video matrix/color memory

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315

	ldx #0
	lda #8+0	; multicolor, black
0$	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	dex
	bne 0$

#if SYSTEM == NTSC
	lda #RASTER+7
#else
	lda #RASTER+10
#endif
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #ROWS*2	; rows & 8x8 chars
	sta $9003
	lda #$cc
	sta $9005	; c=$1000
#if SAME_MUS
	lda #$37
#else
	lda #$30
#endif
	sta $900e
	lda #$8
	sta $900f

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
	ldx #0
	lda #128
0$	sta sin,x
	inx
	bne 0$

#if SAME_MUS
	;jsr player_update
	jsr player_update
#endif
	lda $9124
	cli

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
	dc.b "COLSCR.N"		; 8 significant chars
#else
	dc.b "COLSCR.P"		; 8 significant chars
#endif
#rend
mainend

	org $1000+ROWS*COLUMNS


	org $1400
	; charset	19*8 = 152 bytes used
cs0:		; 0 00 = back
		; 1 11 = aux
		; 2 10 = color
	org $1400+20*8
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

	org $1500
sin:
#if 0
;#include "sin.h"
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
	dc.b $80,$80,$80,$80,$80,$80,$80,$80, $80,$80,$80,$80,$80,$80,$80,$80
#else
setvideomem:
	ldx #COLUMNS-1
0$	txa
	and #15
	clc
	adc #$80
	sta matrix0+0*COLUMNS,x
	sta matrix0+1*COLUMNS,x
	sta matrix0+2*COLUMNS,x
	sta matrix0+3*COLUMNS,x
	sta matrix0+4*COLUMNS,x
	sta matrix0+5*COLUMNS,x
	sta matrix0+6*COLUMNS,x
	sta matrix0+7*COLUMNS,x
	sta matrix0+8*COLUMNS,x
	sta matrix0+9*COLUMNS,x
	sta matrix0+10*COLUMNS,x
	sta matrix0+11*COLUMNS,x
	sta matrix0+12*COLUMNS,x
	sta matrix0+13*COLUMNS,x
	sta matrix0+14*COLUMNS,x
	sta matrix0+15*COLUMNS,x
	sta matrix0+16*COLUMNS,x
	sta matrix0+17*COLUMNS,x
	sta matrix0+18*COLUMNS,x
	sta matrix0+19*COLUMNS,x
	sta matrix0+20*COLUMNS,x
#if SYSTEM == NTSC && ROWS == 22
	lda #$80+20
#endif
#if ROWS > 21
	sta matrix0+21*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 23
	lda #$80+20
#endif
#if ROWS > 22
	sta matrix0+22*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 24
	lda #$80+20
#endif
#if ROWS > 23
	sta matrix0+23*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 25
	lda #$80+20
#endif
#if ROWS > 24
	sta matrix0+24*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 26
	lda #$80+20
#endif
#if ROWS > 25
	sta matrix0+25*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 27
	lda #$80+20
#endif
#if ROWS > 26
	sta matrix0+26*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 28
	lda #$80+20
#endif
#if ROWS > 27
	sta matrix0+27*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 29
	lda #$80+20
#endif
#if ROWS > 28
	sta matrix0+28*COLUMNS,x
#endif
#if SYSTEM == NTSC && ROWS == 30
	lda #$80+20
#endif
#if ROWS > 29
	sta matrix0+29*COLUMNS,x
#endif
	dex
	bpl 0$


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

	rts
#endif

	org $1600
sinsrc:
#if 1
#include "sin.h"
#else
	dc.b $5f,$5f,$5f,$5f,$5f,$5f,$5f,$5f,$5e,$5e,$5e,$5e,$5d,$5d,$5d,$5c
        dc.b $5c,$5b,$5b,$5a,$5a,$59,$59,$58,$57,$57,$56,$55,$55,$54,$53,$52
        dc.b $51,$51,$50,$4f,$4e,$4d,$4c,$4b,$4a,$49,$48,$47,$46,$45,$44,$43
        dc.b $42,$41,$40,$3f,$3d,$3c,$3b,$3a,$39,$38,$37,$35,$34,$33,$32,$31
        dc.b $30,$2e,$2d,$2c,$2b,$2a,$28,$27,$26,$25,$24,$23,$22,$20,$1f,$1e
        dc.b $1d,$1c,$1b,$1a,$19,$18,$17,$16,$15,$14,$13,$12,$11,$10,$0f,$0e
        dc.b $0e,$0d,$0c,$0b,$0a,$0a,$09,$08,$08,$07,$06,$06,$05,$05,$04,$04
        dc.b $03,$03,$02,$02,$02,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00
        dc.b $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$03
        dc.b $03,$04,$04,$05,$05,$06,$06,$07,$08,$08,$09,$0a,$0a,$0b,$0c,$0d
        dc.b $0e,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c
        dc.b $1d,$1e,$1f,$20,$22,$23,$24,$25,$26,$27,$28,$2a,$2b,$2c,$2d,$2e
        dc.b $2f,$31,$32,$33,$34,$35,$37,$38,$39,$3a,$3b,$3c,$3d,$3f,$40,$41
        dc.b $42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51
        dc.b $51,$52,$53,$54,$55,$55,$56,$57,$57,$58,$59,$59,$5a,$5a,$5b,$5b
        dc.b $5c,$5c,$5d,$5d,$5d,$5e,$5e,$5e,$5e,$5f,$5f,$5f,$5f,$5f,$5f,$5f
#endif

back:
	dc.b $08, $28,$28,$48,$88,$98,$78,$18
	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
	dc.b $08, $18,$78,$98,$88,$48,$28,$08

	dc.b $08, $28,$28,$48,$58,$38,$78,$18
	dc.b $08, $28,$28,$48,$58,$38,$78,$18,$18,$18,$78,$38,$58,$48,$28,$28
	dc.b $08, $18,$78,$38,$58,$48,$28,$08

	dc.b $08, $28,$28,$88,$88,$98,$78,$18
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$88,$88,$28,$28
	dc.b $08, $18,$18,$78,$98,$88,$88,$28

	dc.b $08, $28,$28,$48,$88,$98,$78,$18
	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
	dc.b $08, $18,$78,$98,$88,$48,$28,$28

	dc.b $08, $28,$28,$48,$58,$38,$78,$18
	dc.b $08, $28,$28,$48,$58,$38,$78,$18,$18,$18,$78,$38,$58,$48,$28,$28
	dc.b $08, $18,$78,$38,$58,$48,$28,$28

	dc.b $08, $28,$28,$88,$88,$98,$78,$18
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$88,$88,$28,$28
	dc.b $08, $18,$18,$78,$98,$88,$88,$28

	dc.b $08, $28,$28,$48,$88,$98,$78,$18
	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
	dc.b $08, $18,$78,$98,$88,$48,$28,$28

	dc.b $08, $28,$28,$48,$58,$38,$78,$18
	dc.b $08, $28,$28,$48,$58,$38,$78,$18,$18,$18,$78,$38,$58,$48,$28,$28
	dc.b $08, $18,$78,$38,$58,$48,$28,$28
;border:
;	dc.b $00, $02,$04,$05,$07,$01,$03,$05,$04,$02
;	dc.b $00, $06,$02,$04,$04,$03,$03,$01,$01,$01,$03,$03,$04,$04,$02,$06

; 00 - background, 01 - border, 11 - aux, 10 - character
; 00 11 11 01  01 01 11 11  00 10 10 10 $3d $5f $2a
; 10 00 11 11  01 01 01 11  11 00 10 10 $8f $57 $ca
; 10 10 00 11  11 01 01 01  11 11 00 10 $a3 $d5 $f2
; 10 10 10 00  11 11 01 01  01 11 11 00 $a8 $f5 $7c

; 00 00 11 11  01 11 11 00  00 10 10 10 $0f $7c $2a
; 10 00 00 11  11 01 11 11  00 00 10 10 $83 $df $0a
; 10 10 00 00  11 11 01 11  11 00 00 10 $a0 $f7 $c2
; 10 10 10 00  00 11 11 01  11 11 00 00 $a8 $3d $f0

leftimg:
	dc.b $3d,$3d, $8f,$8f, $a3,$a3, $a8,$a8
	dc.b $0f,$0f, $83,$83, $a0,$a0, $a8,$a8
leftmask:
;	dc.b $00,$00, $00,$00, $00,$00, $00,$00
	dc.b $00,$00, $c0,$c0, $f0,$f0, $fc,$fc
midimg:
	dc.b $5f,$5f, $57,$57, $d5,$d5, $f5,$f5
	dc.b $7c,$7c, $df,$df, $f7,$f7, $3d,$3d
rightimg:
	dc.b $2a,$2a, $ca,$ca, $f2,$f2, $7c,$7c
	dc.b $2a,$2a, $0a,$0a, $c2,$c2, $f0,$f0
rightmask:
;	dc.b $00,$00, $00,$00, $00,$00, $00,$00
	dc.b $3f,$3f, $0f,$0f, $03,$03, $00,$00
	samepage leftimg


start
	jsr setcolmem	; is overwritten by setvideomem
	jsr setvideomem

	jmp main


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


#if SYSTEM == NTSC
	ldx #1
wa1	dex
	bne wa1	; 2+x*5+4 =
	samepage wa1

bgcol2	lda #8
	sta $900f
#else
	ldx #8
wa1	dex
	bne wa1	; 2+7*5+4 = 41
	samepage wa1
#endif


sinstart
	ldx #0
	stx zpcount
rloop
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$01,y	; 4
	and leftmask,x	; 4
	ora leftimg,x	; 4

	sta cs0+$01,y	; 5
	lda midimg,x	; 4
	sta cs0+$09,y	; 5

	lda cs0+$11,y	; 4
	and rightmask,x	; 4
	ora rightimg,x	; 4

	sta cs0+$11,y	; 5
	ldx zpcount	; 3
	inx		; 2
	stx zpcount	; 3
	; 65
#if SYSTEM == PAL
bgcol2	lda #8
	sta $900f
	; 71
#endif

	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$02,y	; 4
	and leftmask,x	; 4
	ora leftimg+8,x	; 4

	sta cs0+$02,y	; 5
	lda midimg+8,x	; 4
	sta cs0+$0a,y	; 5

	lda cs0+$12,y	; 4
	and rightmask,x	; 4
	ora rightimg+8,x	; 4

	sta cs0+$12,y	; 5
	ldx zpcount	; 3
	inx		; 2
	stx zpcount	; 3
	; 65
#if SYSTEM == PAL
	nop
	nop
	nop	; 71
#endif
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$03,y	; 4
	and leftmask,x	; 4
	ora leftimg,x	; 4

	sta cs0+$03,y	; 5
	lda midimg,x	; 4
	sta cs0+$0b,y	; 5

	lda cs0+$13,y	; 4
	and rightmask,x	; 4
	ora rightimg,x	; 4

	sta cs0+$13,y	; 5
	ldx zpcount	; 3
	inx		; 2
	stx zpcount	; 3
	; 65
#if SYSTEM == PAL
	nop
	nop
	nop	; 71
#endif
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$04,y	; 4
	and leftmask,x	; 4
	ora leftimg+8,x	; 4

	sta cs0+$04,y	; 5
	lda midimg+8,x	; 4
	sta cs0+$0c,y	; 5

	lda cs0+$14,y	; 4
	and rightmask,x	; 4
	ora rightimg+8,x	; 4

	sta cs0+$14,y	; 5
	ldx zpcount	; 3
	inx		; 2
	stx zpcount	; 3
	; 65
#if SYSTEM == PAL
	nop
	nop
	nop	; 71
#endif
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$05,y	; 4
	and leftmask,x	; 4
	ora leftimg,x	; 4

	sta cs0+$05,y	; 5
	lda midimg,x	; 4
	sta cs0+$0d,y	; 5

	lda cs0+$15,y	; 4
	and rightmask,x	; 4
	ora rightimg,x	; 4

	sta cs0+$15,y	; 5
	ldx zpcount	; 3
	inx		; 2
	stx zpcount	; 3
	; 65
#if SYSTEM == PAL
	nop
	nop
	nop	; 71
#endif
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$06,y	; 4
	and leftmask,x	; 4
	ora leftimg+8,x	; 4

	sta cs0+$06,y	; 5
	lda midimg+8,x	; 4
	sta cs0+$0e,y	; 5

	lda cs0+$16,y	; 4
	and rightmask,x	; 4
	ora rightimg+8,x	; 4

	sta cs0+$16,y	; 5
	ldx zpcount	; 3
	inx		; 2
	stx zpcount	; 3
	; 65
#if SYSTEM == PAL
	nop
	nop
	nop	; 71
#endif
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$07,y	; 4
	and leftmask,x	; 4
	ora leftimg,x	; 4

	sta cs0+$07,y	; 5
	lda midimg,x	; 4
	sta cs0+$0f,y	; 5

	lda cs0+$17,y	; 4
	and rightmask,x	; 4
	ora rightimg,x	; 4

	sta cs0+$17,y	; 5
	ldx zpcount	; 3
	inx		; 2
	;stx zpcount	; 3	borrow 3 cycles
	; 65
	;nop
#if SYSTEM == PAL
	nop
	nop	; 71
#endif
	lda sin,x	; 4
	and #$f8	; 2
	tay		; 2
	eor sin,x	; 4
	tax		; 2

	lda cs0+$00,y	; 4
	and leftmask,x	; 4
	ora leftimg+8,x	; 4

	sta cs0+$00,y	; 5
	lda midimg+8,x	; 4
	sta cs0+$08,y	; 5

	lda cs0+$10,y	; 4
	and rightmask,x	; 4
	ora rightimg+8,x	; 4

	sta cs0+$10,y	; 5
	ldx zpcount	; 3
	inx		; 2
	inx	; use 2 of borrowed 3 cycles
	stx zpcount	; 3
sinend	cpx #ROWS*8
	; 65
	beq outloop
#if SYSTEM == PAL
	nop
#endif			; 6 cycles too slow for NTSC!
	jmp rloop	; use 1 of borrowed 3 cycles

outloop:

9$	;pha
	;pla
	lda $9121
	lsr
	lda #8
	sta $900f
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory

bgcol	lda #$48+1
	sta bgcol2+1
#if 1
	; Check keyboard
	;lda $9121
	;lsr
	bcs 95$
	; run/stop
	dec fade+1	;inc KEYWAIT+1
95$
#endif
	jsr clearrow
	jsr fade

	;dec $900f
	;inc $900f
#if 0
	lda #$19
	sta $900f
#endif
	;jsr player_update
	;jmp $eb18	; return from IRQ
	jmp MUSVEC


sincnt	dc.b 0,160
sinp	dc.b 0
sint	dc.b 2, 3, 4, 5, 4, 3, 2, 1, 0

#if SYSTEM == NTSC
TIMES = 32
#else
TIMES = 48
#endif

dosin:
x$	ldx #0
	txa
	clc
times$	adc #1		; one line at a time until fadein complete
	sta cp$+1
	bne y$
	lda #TIMES
	sta times$+1	; fade in finished
y$	ldy #0
inner$
	lda sin2$+1
	clc
f2$	adc #3		; frequency
	sta sin2$+1
ph$	adc #0		; phase
	sta sin3$+1

sin0$	lda sinsrc,x
	;clc
sin1$	adc sinsrc,y
	iny
sin2$	adc sinsrc
sin3$	adc sinsrc
	ror
	sta sin,x
	inx
cp$	cpx #TIMES	; clears carry
	bne inner$

	stx x$+1
	sty y$+1

	lda times$+1
	cmp #TIMES
	bne 4$		; not while fading in..

	inc ph$+1
	inc ph$+1
	inc y$+1

	cpx #TIMES
	bcs 4$

	inc y$+1	; make it not exact 1/2

	dec sincnt+1
	bne 4$

	ldx sinp
	inx
	lda sint,x
	bne ok$
	ldx #0
ok$	stx sinp
	lda sint,x
	sta f2$+1

	lda #160
	sta sincnt+1

4$	rts



clearrow:
	lda #$aa
#if SYSTEM == NTSC
	ldy #40-1
0$	sta cs0+0*40,y
	sta cs0+1*40,y
	sta cs0+2*40,y
	sta cs0+3*40,y
	sta cs0+4*40,y
	sta cs0+5*40,y
	dey
	bpl 0$		; 35*40 = 1400/65 = 21.5 lines
#else
	ldy #2*40-1
0$	sta cs0+0*40,y
	sta cs0+2*40,y
	sta cs0+4*40,y
	dey
	bpl 0$	; 20*80 = 1600/65 = 24.6 lines
#endif

	jsr dosin

#if 1
#if 1
y$	ldy #0
	inc y$+1
	lda sinsrc,y
	lsr
	lsr
	lsr
	clc
	adc a$+1
	sec
	sbc #4
	sta a$+1
a$	lda #0
#else
	lda sinstart+1
	clc
	adc #2
#endif
	sta sinstart+1
	clc
#if SYSTEM == NTSC
	adc #ROWS*8-8	; last row masks a problem (extra line drawn)
#else
	adc #ROWS*8
#endif
	sta sinend+1
#endif
	dec colcnt
	bne 1$

	dec fadecnt
	bne 25$
	lda AUTO
	bne 25$
	dec fade+1
25$
	ldx colptr
	inx
	lda colbg,x
	bne 2$
	ldx #0
	lda colbg,x
2$	stx colptr
	sta bgcol+1
	lda $900e
	and #15
	ora colaux,x
	sta $900e
1$
	rts
fadecnt	dc.b 7
colcnt	dc.b 255
colptr	dc.b 0
colbg	dc.b $49, $89, $29, $59, $69, $49, $00
colaux	dc.b $30, $90, $a0, $d0, $e0, $c0

fade	ldx #0
	bne do$
	rts
do$	ldx #0
	lda #$48
	sta sinsrc+0,x
	sta sinsrc+1,x
	inc do$+1
	inc do$+1
	bne rts$
	inc loadnext+1
rts$	rts

	; 1bd2..1e51
#if SAME_MUS

MUSVEC = $1c00
player_update = MUSVEC+6

#else

	 .org $1c00
INCSONG equ 1
#include "../music/acplay2.a65"
#include "../music/cutie.sng"
;#include "../music/canon3.sng"
;#include "../music/mmix.sng"
;#include "../music/vicual.sng"
;#include "../music/voyage.sng"

#endif	 ;!SAME_MUS

	org $2000

