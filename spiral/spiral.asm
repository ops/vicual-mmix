SAME_MUS = 1
END_MUS = 1


CHANGESPEED = 1	; 1 for slower
COL1 = $4   ; $5	; $5
COL2 = $c0  ; $d0	;$d0

;SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3ff
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

#if SYSTEM == NTSC
LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER = 26
#else
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

#if SYSTEM == NTSC
RASTER	= 70	;24		; effect at RASTER + 10 (+ 1)
COLUMNS		EQU 30		;31:every eigth crap + every other shifted
		    		;30:works in emulator the same as in real vic20
ROWS		EQU 32
#else
RASTER	= 102	;24		; effect at RASTER + 10 (+ 1)
COLUMNS		EQU 31
ROWS		EQU 32
#endif


; The BASIC line - Note: it is overwritten by the video matrix quite quickly

#if SYSTEM == NTSC
matrix = $1000 - 2	; move the screen few chars left to center the effect
#else
matrix = $1000
#endif

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
	sei	; to prevent keyboard scan to overwrite some data
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
main$	lda mainsrc,x
	sta main,x
	inx
	cpx #mainend-mainsrc
	bne main$

	ldx #0
0$	lda #COL1
	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	lda #0
	; do not overwrite us.. do not write to matrix+$000,x yet..
	sta matrix+$100,x
	sta matrix+$200,x
#if ROWS*COLUMNS > $200
	sta matrix+ROWS*COLUMNS-$100,x
#endif
	dex
	bne 0$

#if SYSTEM == NTSC
	lda #4
	sta $9001	; vertical centering
	lda #0	; not centered
#else
	lda #20
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
#endif
	sta $9000	; horizontal centering
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory
	lda #$cd
	sta $9005	; matrix=$1000 charset=$1400
	lda #$08+COL1
	sta $900f
#if SAME_MUS
	lda #$37
	sta $900e
#else
	lda #$30
	sta $900e
	jsr player_init
#endif
; synchronize with the screen, cycle-accurate not needed

	ldx #RASTER	; wait for this raster line (times 2)
11$	cpx $9004
	beq 11$
10$	cpx $9004
	bne 10$		; at this stage, the inaccuracy is 7 clock cycles

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

	lda $9124
	lda LOADER
	rts


mainsrc:
#rorg $100
main:
2$	lda loadnext+1
	beq 2$

	lda #$60
	sta $0364

	lda #COL1*16+COL1
	sta color+1
	sta $900f

	lda #$89	; no clear.. $9089
	ldx #<dataFile
	ldy #>dataFile
	jsr LOADER	; can no depend on Carry=1 for error condition!
	lda $035d
	cmp #$17
1$	bne 1$		; failed to load!

loadnext
	lda #0
	beq loadnext

	; more than one block to load, can fall through to load second block
again$
	lda #COL1*16+COL1
	sta color+1
	sta $900f

#if 0
	ldy #0
#endif
	jsr $0358	; y must be 0 when entered
	lda $035d
	cmp #>eos	;$19
	bne again$
	lda #>scr0	;$16	; back up for more data..
	sta $035d

	;ldy #0
	sty loadnext+1

	lda #COL2+COL1
	sta color+1

	dec dataCnt
	bne loadnext

	lda #$4c
	sta $0364
	jsr $0358	; read until EOF

10$	lda loadnext+1
	beq 10$

	sei
	lda #COL1*16+COL1
	sta $900f
	lda #<MUSVEC	;irq2
	sta $0314
	lda #>MUSVEC	;irq2
	sta $0315
	cli

	lda #3		; only clear $9000..$9003
	ldx #<nextFile
	ldy #>nextFile
	jsr LOADER
11$	bcs 11$		; failed to load!

	; then fade sound
#if END_MUS == 0
	ldx #<MUSVEC	;$1e00
	lda #>MUSVEC	;$1e00
	jmp FADENOT
#else
	lda $900e
	and #$0f
	ldx #RASTER
	jmp FADE
#endif

;irq2:	lda $9124
;	jsr player_update
;	jmp $eb18	; return from IRQ

#if SYSTEM == NTSC
dataFile:
	dc.b "SPIRAL.D"		; 8 significant chars
nextFile:
	dc.b "WAVE.N",$a0,$a0
#else
dataFile:
	dc.b "SPIRAL.D"		; 8 significant chars
nextFile:
	dc.b "WAVE.P",$a0,$a0
#endif
dataCnt:
	dc.b 5


	; cycle-exact IRQ not needed
irq
	lda #ROWS*2	; rows & 8x8 chars
	sta $9003

	;jsr player_update
	jsr player_update
	; Check keyboard
#if 0
	lda $9121
	lsr
#else
	lsr $9121
#endif
	bcs 95$
	; run/stop
	dec loadnext+1	;inc KEYWAIT+1

	lda #CHANGESPEED
	sta count+1
	lda #0
	sta count+0
95$
	; turn off plotting during loading
	lda color+1
	cmp #COL1*16+COL1
	beq nosh$
	jsr setvideomem
nosh$	jmp $eb15	; return from IRQ, ack int

#rend
mainend

#if SYSTEM == NTSC
	org $1000+31*32
#else
	org $1000+ROWS*COLUMNS
#endif
	; 32 bytes here..
selecth	dc.b >scr0,>scr1,>scr2,>scr3,>scr4,>scr5,>scr6,>scr7
selectl	dc.b <scr0,<scr1,<scr2,<scr3,<scr4,<scr5,<scr6,<scr7
count:	dc.b 0,CHANGESPEED ; 1 for slower changes..

start	jsr setcolmem	; is overwritten by setvideomem
	cli
	cmp #$8d
	bne *
	jmp main


	org $1400

cs:
	dc.b $00,$00,$00,$00,$00,$00,$00,$00
	dc.b $0f,$0f,$0f,$0f,$00,$00,$00,$00
	dc.b $f0,$f0,$f0,$f0,$00,$00,$00,$00
	dc.b $ff,$ff,$ff,$ff,$00,$00,$00,$00

	dc.b $00,$00,$00,$00,$0f,$0f,$0f,$0f
	dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	dc.b $f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f
	dc.b $ff,$ff,$ff,$ff,$0f,$0f,$0f,$0f

	dc.b $00,$00,$00,$00,$f0,$f0,$f0,$f0
	dc.b $0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0
	dc.b $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
	dc.b $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0

	dc.b $00,$00,$00,$00,$ff,$ff,$ff,$ff
	dc.b $0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff
	dc.b $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	; rotated
	; 10 -> 31
	; 32    20
	dc.b $00,$00,$00,$00,$00,$00,$00,$00	;0	abcd -> bdac
	dc.b $00,$00,$00,$00,$0f,$0f,$0f,$0f	;4	0001 -> 0100
	dc.b $0f,$0f,$0f,$0f,$00,$00,$00,$00	;1	0010 -> 0001
	dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f	;5	0011 -> 0101

	dc.b $00,$00,$00,$00,$f0,$f0,$f0,$f0	;8	0100 -> 1000
	dc.b $00,$00,$00,$00,$ff,$ff,$ff,$ff	;12	0101 -> 1100
	dc.b $0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0	;9	0110 -> 1001
	dc.b $0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff	;13

	dc.b $f0,$f0,$f0,$f0,$00,$00,$00,$00	;2
	dc.b $f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f	;6
	dc.b $ff,$ff,$ff,$ff,$00,$00,$00,$00	;3
	dc.b $ff,$ff,$ff,$ff,$0f,$0f,$0f,$0f	;7

	dc.b $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0	;10
	dc.b $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff	;14
	dc.b $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0	;11
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;15

; horizontal + vertical mirroring
; 10 -> 23
; 32    01
	dc.b $00,$00,$00,$00,$00,$00,$00,$00	;0	bit reverse!?
	dc.b $00,$00,$00,$00,$f0,$f0,$f0,$f0	;8
	dc.b $00,$00,$00,$00,$0f,$0f,$0f,$0f	;4
	dc.b $00,$00,$00,$00,$ff,$ff,$ff,$ff	;12
	dc.b $f0,$f0,$f0,$f0,$00,$00,$00,$00	;2
	dc.b $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0	;10
	dc.b $f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f	;6
	dc.b $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff	;14

	dc.b $0f,$0f,$0f,$0f,$00,$00,$00,$00	;1
	dc.b $0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0	;9
	dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f	;5
	dc.b $0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff	;13
	dc.b $ff,$ff,$ff,$ff,$00,$00,$00,$00	;3
	dc.b $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0	;11
	dc.b $ff,$ff,$ff,$ff,$0f,$0f,$0f,$0f	;7
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;15

; rotated
; 23 -> 02
; 01    13
	dc.b $00,$00,$00,$00,$00,$00,$00,$00	;0 compared to original
	dc.b $f0,$f0,$f0,$f0,$00,$00,$00,$00	;2
	dc.b $00,$00,$00,$00,$f0,$f0,$f0,$f0	;8
	dc.b $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0	;10
	dc.b $0f,$0f,$0f,$0f,$00,$00,$00,$00	;1
	dc.b $ff,$ff,$ff,$ff,$00,$00,$00,$00	;3
	dc.b $0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0	;9
	dc.b $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0	;11
	dc.b $00,$00,$00,$00,$0f,$0f,$0f,$0f	;4
	dc.b $f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f	;6
	dc.b $00,$00,$00,$00,$ff,$ff,$ff,$ff	;12
	dc.b $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff	;14
	dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f	;5
	dc.b $ff,$ff,$ff,$ff,$0f,$0f,$0f,$0f	;7
	dc.b $0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff	;13
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;15


#include "screen.h"
eos


setvideomem:
	inc selptr+1
selptr	lda #0
#if 1
	ldx dataCnt
	bne full$
	lsr ; half-speed for the logo part
full$
#endif
	pha
	and #7
	tay
	pla
	and #8
color	ora #COL2+COL1
	sta col$+1
	lda selecth,y	; is still faster than using zeropage:
	sta s0$+2	; only 72 cycles lost, 2*256=512 = 7 lines saved
	sta s1$+2	; and more, because (zp)+n,y is not possible

	lda selectl,y	; is still faster than using zeropage:
	sta s0$+1	; only 72 cycles lost, 2*256=512 = 7 lines saved
	sta s1$+1	; and more, because (zp)+n,y is not possible

#if 1
	ldx #0			; copy 2*128 -> 16*16, ~quarter of 32*31
	ldy #0
s0$	lda scr0+0*16,y		;4
	and #15
	sta matrix+0*COLUMNS,x	;5
s1$	lda scr0+0*16,y		;4
	lsr
	lsr
	lsr
	lsr
	sta matrix+8*COLUMNS,x	;5
	inx 			;2
	iny			;2
	tya 			;2
	and #15			;2
	bne s0$			;3/2	16*29-1
	txa 			;2
	clc			;2
	adc #COLUMNS-16		;2
	tax 			;2
	cpx #8*COLUMNS
	bne s0$
	;jmp s0$			;3
iu$	    			;=(16*29-1+9)*16=7552 = 106.4 (was 114) lines
#endif

#if SYSTEM == NTSC
	ldx #0
	ldy #0			; right top quarter
tt$	lda matrix+2*COLUMNS,x		; 4 plot chars rotated 90 degrees
	ora #$10		; 2 use font to rotate the 2x2 tiles
	sta matrix+1*COLUMNS-1,y

	lda matrix+2*COLUMNS+8,x	; 4 plot chars rotated 90 degrees
	ora #$10		; 2 use font to rotate the 2x2 tiles
	sta matrix+9*COLUMNS-1,y

	lda matrix+3*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-2,y

	lda matrix+3*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-2,y

	lda matrix+4*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-3,y

	lda matrix+4*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-3,y

	lda matrix+5*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-4,y

	lda matrix+5*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-4,y

	lda matrix+6*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-5,y

	lda matrix+6*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-5,y

	lda matrix+7*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-6,y

	lda matrix+7*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-6,y

	lda matrix+8*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-7,y

	lda matrix+8*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-7,y

	lda matrix+9*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-8,y

	lda matrix+9*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-8,y

	lda matrix+10*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-9,y

	lda matrix+10*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-9,y

	lda matrix+11*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-10,y

	lda matrix+11*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-10,y

	lda matrix+12*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-11,y

	lda matrix+12*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-11,y

	lda matrix+13*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-12,y

	lda matrix+13*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-12,y

	lda matrix+14*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-13,y

	lda matrix+14*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-13,y

	lda matrix+15*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-14,y

	lda matrix+15*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-14,y

	inx

	tya		;2
	clc		;2
	adc #COLUMNS	;2
	tay 		;2
	cpy #8*COLUMNS
	beq outt$
	jmp tt$		; X = 0..7
outt$
#else
	ldx #0
	ldy #0			; right top quarter
tt$	lda matrix+1*COLUMNS,x		; 4 plot chars rotated 90 degrees
	ora #$10		; 2 use font to rotate the 2x2 tiles
	sta matrix+1*COLUMNS-1,y

	lda matrix+1*COLUMNS+8,x	; 4 plot chars rotated 90 degrees
	ora #$10		; 2 use font to rotate the 2x2 tiles
	sta matrix+9*COLUMNS-1,y

	lda matrix+2*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-2,y

	lda matrix+2*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-2,y

	lda matrix+3*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-3,y

	lda matrix+3*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-3,y

	lda matrix+4*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-4,y

	lda matrix+4*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-4,y

	lda matrix+5*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-5,y

	lda matrix+5*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-5,y

	lda matrix+6*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-6,y

	lda matrix+6*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-6,y

	lda matrix+7*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-7,y

	lda matrix+7*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-7,y

	lda matrix+8*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-8,y

	lda matrix+8*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-8,y

	lda matrix+9*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-9,y

	lda matrix+9*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-9,y

	lda matrix+10*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-10,y

	lda matrix+10*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-10,y

	lda matrix+11*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-11,y

	lda matrix+11*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-11,y

	lda matrix+12*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-12,y

	lda matrix+12*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-12,y

	lda matrix+13*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-13,y

	lda matrix+13*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-13,y

	lda matrix+14*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-14,y

	lda matrix+14*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-14,y

	lda matrix+15*COLUMNS,x
	ora #$10
	sta matrix+1*COLUMNS-15,y

	lda matrix+15*COLUMNS+8,x
	ora #$10
	sta matrix+9*COLUMNS-15,y

	inx

	tya		;2
	clc		;2
	adc #COLUMNS	;2
	tay 		;2
	cpy #8*COLUMNS
	beq outt$
	jmp tt$		; X = 0..7
outt$
#endif

col$	lda #0
	sta $900f


#if SYSTEM == NTSC
	ldx #8*COLUMNS	; bottom is top mirrored in both directions
	ldy #0
mirror$
	lda matrix+2-1,x		; 4
	ora #$20		; 2 use font to mirror 2x2 tiles
	sta matrix+24*COLUMNS,y	; 5
	lda matrix+2-1+8*COLUMNS,x  ;4
	ora #$20		  ;2
	sta matrix+16*COLUMNS,y	  ;5
	iny 			  ;2
	dex			  ;2
	bne mirror$		  ;3/2
	    			  ;= 8*29* 31 = 7192 = 101.3 lines
				  ;total ~245.4 lines
#else
	ldx #8*COLUMNS	; bottom is top mirrored in both directions
	ldy #0
mirror$
	lda matrix+1-1,x		; 4
	ora #$20		; 2 use font to mirror 2x2 tiles
	sta matrix+24*COLUMNS,y	; 5
	lda matrix+1-1+8*COLUMNS,x  ;4
	ora #$20		  ;2
	sta matrix+16*COLUMNS,y	  ;5
	iny 			  ;2
	dex			  ;2
	bne mirror$		  ;3/2
	    			  ;= 8*29* 31 = 7192 = 101.3 lines
				  ;total ~245.4 lines
#endif

plotdone:
	dec count+0
	bne fuu$
	dec count+1
	bpl fuu$

	lda AUTO
	bne noauto$
	inc loadnext+1
noauto$
	lda #CHANGESPEED
	sta count+1
fuu$
	rts






#if SAME_MUS == 0

	 .org $1cc0
INCSONG equ 1
#include "../music/acplay2.a65"
#include "../music/folk2.sng"

#else
MUSVEC = $1cc0
player_update = MUSVEC+6
#endif

	org $2000

