CO1 = $e6
CO2 = $06

SAME_MUS = 1
END_MUS = 1

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
RASTER	= 10	;24		; effect at RASTER + 10 (+ 1)
#else
RASTER	= 8	;24		; effect at RASTER + 10 (+ 1)
#endif


#if SYSTEM & PAL
COLUMNS		EQU 29
ROWS		EQU 32
#else
COLUMNS		EQU 25	; odd for a nicer wipe effect
ROWS		EQU 26
#endif

;textptr = $f2	; $f2/f3, $f4/$f5, $f6/f7, $f8/f9, $fa/fb, $fc/fd, $fe/ff
zpcount = $f0

shmask	= $f2
shbit	= $f3
shacc	= $f4

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

	lda #RASTER+10
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #0
	sta $9002
	sta $9003
	lda #$cd
	sta $9005	; c=$1000 d = $1400
#if SAME_MUS
	lda #$37
#else
	lda #$30
#endif
	sta $900e
	lda #$66+8
	sta $900f

	ldx #0
0$
	lda #CO2
	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	inx
	bne 0$


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
	dc.b "TWIRL.N",$a0		; 8 significant chars
#else
	dc.b "TWIRL.P",$a0		; 8 significant chars
#endif
#rend
mainend

	org $1000+ROWS*COLUMNS
	dc.b 0

	org $1400
sin:
#include "sin.h"

	org $1500
	; charset	19*8 = 152 bytes used
cs0:		; 0 00 = back
		; 1 11 = aux
		; 2 10 = color


	org $1600
back:
setvideomem:
	ldx #COLUMNS-1
0$	txa
	clc
	adc #$20
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
	sta matrix0+21*COLUMNS,x
	sta matrix0+22*COLUMNS,x
	sta matrix0+23*COLUMNS,x
	sta matrix0+24*COLUMNS,x
#if ROWS > 25
	sta matrix0+25*COLUMNS,x
#endif
#if ROWS > 26
	sta matrix0+26*COLUMNS,x
#endif
#if ROWS > 27
	sta matrix0+27*COLUMNS,x
#endif
#if ROWS > 28
	sta matrix0+28*COLUMNS,x
#endif
#if ROWS > 29
	sta matrix0+29*COLUMNS,x
#endif
#if ROWS > 30
	sta matrix0+30*COLUMNS,x
#endif
#if ROWS > 31
	sta matrix0+31*COLUMNS,x
#endif
	dex
	bpl 0$

	rts

	org $1700
backsrc:
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1

	dc.b $00, $20,$20,$80,$80,$90,$70,$10
	dc.b $00, $20,$20,$80,$80,$90,$70,$10,$10,$10,$70,$90,$80,$80,$20,$20

	dc.b $00,$20,$20,$80,$20,$80,$80,$80,$90,$90,$70,$70,$f0,$f0,$10,$10
	dc.b $10,$10,$f0,$f0,$f0,$70,$70,$90,$90,$80,$80,$80,$20,$80,$20,$20

	dc.b $00, $20,$20,$80,$80,$90,$70,$10,$10,$10,$70,$90,$80,$80,$20,$20
	dc.b $00, $10,$10,$70,$90,$80,$80,$20

	dc.b $00, CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1
	dc.b CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1,CO1

	org $1800
table:
#if COLUMNS > ROWS
	ds.b 8*COLUMNS
#else
	ds.b 8*ROWS
#endif

	;org $1920
start
	ldx #255
	txs
	jsr setcolmem	; is overwritten by setvideomem
	jsr setvideomem

	ldx #0
	lda #CO2*16
0$	sta back,x	; overwrites setvideomem
	inx
	bne 0$

#if SAME_MUS
	sei
#endif
;synchronize with the screen

#if SYSTEM & NTSC
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
#endif

sync:	ldx #RASTER	; wait for this raster line (times 2)
1$	cpx $9004
	beq 1$
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
outer:	ldx $9004
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
	bne outer
	samepage outer
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
#else
	ldy #5
w$	dey
	bne w$
#endif
	bit $ea

	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	lda #ROWS*2	; rows & 8x8 chars
	sta $9003
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory

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

	; wipe blue->black
	ldx #32-1
	lda #$ff
10$	sta $1400,x
	dex
	bpl 10$

	lda #$66
	sta $900f

	lda #ROWS*2+1	; rows & 8x8 chars
	sta $9003
	lda #10
	sta $9001	; vertical centering

	ldx #0
11$	lda #0
	sta $9400+0,x
	sta $9400+1*COLUMNS*ROWS/4,x
	sta $9400+2*COLUMNS*ROWS/4,x
	sta $9400+3*COLUMNS*ROWS/4,x
	txa
	and #1
	sta matrix0+0,x
	sta matrix0+1*COLUMNS*ROWS/4,x
	sta matrix0+2*COLUMNS*ROWS/4,x
	sta matrix0+3*COLUMNS*ROWS/4,x
	inx
	cpx #COLUMNS*ROWS/4
	bne 11$

13$	ldx #5
12$	cpx $9004
	beq 12$
14$	cpx $9004
	bne 14$

	ldx #15
pat1$	lda #$ff
	sta $1400,x
pat2$	lda #$ff
	sta $1410,x
	dex
	bpl pat1$

	lsr pat1$+1
	ror pat2$+1
	bne 13$

	lda #8
	sta $900f
	jmp main

#if SYSTEM == NTSC
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
	bit $ea
#endif

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

	lda #0
	sta tab0+1
	sta tab1+1
	sta shacc

colstart
	ldy #0

; shmask : init to 1
; shacc  : init to 0
sinstart
	ldx #0

z0	lda sin,x	;4
	clc 		;2
s0	adc #$d8	;2
	adc shacc	;3	(zp)
	sta shacc	;3	(zp)
	bcc 0$		;3/2
	; change the polarity
	jmp z1cont	;3
0$	nop		;2
z0cont	nop		;2
#if SYSTEM == PAL
	nop		;2
	nop		;2
	nop		;2
#endif
	lda back,y	;4
	ora #8		;2
	sta $900f	;4

	asl shmask	;5
	lda.a shmask	;4
tab0	sta table	;4
	inc tab0+1	;6
	inc tab1+1	;6

	iny		;2
	inx		;2
sinend0	cpx #<(ROWS*8)	;2
	; 65
	bne z0		;3
	samepage z0
	;jmp outloop0
;outloop0:
	pha
	pla
	pha
	pla
	bit $ea
	bit $ea
	ldy tab0+1
	lda #$66+8
	sta $900f
	;---------------------------------------------------
	; Then upto COLUMNS*8
#if COLUMNS > ROWS
z2	lda sin,x	;4
	inx		;2
	;clc
s2	adc #$d8	;2
	adc shacc	;3	(zp)
	sta shacc	;3	(zp)
	bcs z3cont	; change the polarity
z2cont	asl shmask	; 5
	lda shmask	; 3
	sta table,y
	iny
	cpy #COLUMNS*8
	bne z2
#endif
	jmp endloop


z1	lda sin,x	;4
	clc		;2
s1	adc #$D8	;2
	adc shacc	;3	(zp)
	sta shacc	;3	(zp)
	bcc 0$		;3
	; change the polarity
	jmp z0cont
0$	nop		;2
z1cont	nop		;2
#if SYSTEM == PAL
	nop		;2
	nop		;2
	nop		;2
#endif
	lda back,y
	sec		;2 
	sta $900f

	rol shmask	;5
	lda.a shmask	;4
tab1	sta table	;4
	inc tab0+1
	inc tab1+1

	iny		;2
	inx		;2
sinend1	cpx #<(ROWS*8)
	; 65
	bne z1
	samepage z1
	;jmp outloop1
;outloop1:
	pha
	pla
	pha
	pla
	bit $ea
	bit $ea
	ldy tab0+1
	lda #$66+8
	sta $900f
#if COLUMNS > ROWS
	;---------------------------------------------------
	; Then upto COLUMNS*8
z3	lda sin,x	;4
	inx		;2
	;clc		;2
s3	adc #$d8	;2
	adc shacc	;3	(zp)
	sta shacc	;3	(zp)
	bcs z2cont	; change the polarity
z3cont	sec		;2 
	rol shmask	;5
	lda shmask	;3
	sta table,y	;5
	iny		;2
	cpy #COLUMNS*8
	bne z3
#endif
endloop:
#if 1
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	ldx #255
	stx fade+0	;KEYWAIT+1
	inx
	stx fade+1
95$
#endif
	lda $9111
	and #$20
	beq button$
	jsr automatic
	jmp 2$
button$
	ldx s0+1
	lda $9111	; VIA#1 port A
	and #$08	; sw2? left
	bne 0$
	inx
0$	lda $9111
	and #$04	; $10 left $08 down $04 up
	bne 1$
	dex
1$	stx s0+1
	stx s1+1
#if COLUMNS > ROWS
	stx s2+1
	stx s3+1
#endif
2$

	;jsr player_update
	jsr player_update

	jsr clearrow

	;dec $900f
	;inc $900f

	jmp $eb18	; return from IRQ

sincnt	dc.b 0,0
sinptr	dc.b 0
sincyc	dc.b 0

clearrow:
	ldx #0
	clc
0$
	lda table+7,x
	sta cs0+0,x
	sta cs0+1,x
	sta cs0+2,x
	sta cs0+3,x
	sta cs0+4,x
	sta cs0+5,x
	sta cs0+6,x
	sta cs0+7,x

	txa
	adc #8
	tax
	cpx #COLUMNS*8
	bne 0$

c$	lda sin
	inc c$+1
	inc c$+1
	inc c2$+1
	asl
c2$	adc #76
	sta colstart+1

	lda sinstart+1
	;clc
	adc #2
	sta sinstart+1
	clc
	adc #<(ROWS*8)
	sta sinend0+1
	sta sinend1+1

	ldx fade+1
	lda fade+0
	beq out$
	bmi fade$
	lda backsrc,x
	ora #6
	sta back,x
	inc fade+1
	bne out$
	lda #0
	sta fade+0
	rts
fade$	lda #CO2*16
	sta back,x
	inc fade+1
	bne out$
	inc loadnext+1
out$	rts

fade	dc.b 1,0
autoidx	dc.b 0,4
autowait
	dc.b 1
autotarget
	dc.b $0e,$24,$0e,$60
automatic:
	lda autowait
	beq ok$
	dec autowait
	rts

ok$	ldy autoidx
	lda s0+1
	cmp autotarget,y
	bne ne$

	iny
	tya
	and #3
	sta autoidx
	dec autowait
	dec autoidx+1
	bne 0$
	lda AUTO
	bne 0$
	dec fade+0	;KEYWAIT+1
0$	rts

ne$	bcs decrease$
	clc
	adc #2
decrease$
	sec
	sbc #1
1$	sta s0+1
	sta s1+1
#if COLUMNS > ROWS
	sta s2+1
	sta s3+1
#endif
out$	rts

;irq2:
;	lda $9124
;	jsr player_update
;	jsr player_update
;	jmp $eb18	; return from IRQ

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

;	org $1d00
;	; charset	29*8 = 232 bytes used
;cs2:
;	ds.b COLUMNS*8

#endif


	org $2000

