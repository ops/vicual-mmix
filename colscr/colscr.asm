SAME_MUS = 1	; 1 = use same music than the previous part
END_MUS = 1	; 1 = next part does not use this music, so fade it

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
RASTER	= 18	;24		; effect at RASTER + 10 (+ 1)
#else
RASTER	= 18	;24		; effect at RASTER + 10 (+ 1)
#endif


#if SYSTEM & PAL
COLUMNS		EQU 31
ROWS		EQU 13	;14
SCRPOS		= 3
#else
COLUMNS		EQU 26
ROWS		EQU 10
SCRPOS		= 2
#endif

;textptr = $f2	; $f2/f3, $f4/$f5, $f6/f7, $f8/f9, $fa/fb, $fc/fd, $fe/ff
zpcount = $f0


; The BASIC line - Note: it is overwritten by the video matrix quite quickly

matrix0 = $1000
matrix1 = $1400
matrix2 =  $200 ; empty

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

	ldx #0
	lda #2
0$	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	dex
	bne 0$


	lda #RASTER+10
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory
	lda #ROWS*2+1	; rows & 16x8 chars
	sta $9003
	lda #$cd
	sta $9005	; c=$1000 d = $1400
#if SAME_MUS
	lda #$67
#else
	lda #$60
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
#rorg $000	;$100
main
	lda LOADER
	cmp #$8d
	bne *

loadnext
	lda #0
	beq loadnext

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

	; then fade sound
#if END_MUS == 0
	ldx #<irq2	;$1e00
	lda #>irq2	;$1e00
	jmp FADENOT
#else
	lda $900e
	and #$0f
	ldx #RASTER
	jmp FADE
#endif

nextFile:
#if SYSTEM == NTSC
	dc.b "7UP.N",$a0,$a0,$a0	; 8 significant chars
#else
	dc.b "7UP.P",$a0,$a0,$a0	; 8 significant chars
#endif

	; Note: is overwritten by the decompressor if at page 1!
irq2	;jsr player_update
	jsr player_update
	jmp $eb15	; return from IRQ
#rend
mainend

	org $1000+ROWS*COLUMNS
	dc.b 0


fade	ldx #255
	bmi fadein$
	lda #8
	sta back+0,x
#if 1
	cpx #64
	bcs 0$
	lda #0
	sta cs0,x
	sta cs1,x
	sta cs2,x
	sta cs3,x
0$
#endif
	dec fade+1
	bpl out$
	inc loadnext+1
fadein$	ldx #63
	bmi out$
	dec fadein$+1
	lda back+64,x
	ora #8
	sta back+0,x
out$	rts

videomap
	dc.b $cc, $dc, $cd, $dd
#if SYSTEM == NTSC
	dc.b $cc, $dc, $cd, $dd
	dc.b $cc, $dc, $cd, $dd
	dc.b $cc, $dc, $cd, $dd
	dc.b $cc, $dc, $cd, $dd
	dc.b $cc, $dc, $cd, $dd
	dc.b $cc, $dc, $cd, $dd
	dc.b $cc, $dc, $cd, $dd
#endif
	samepage videomap


scrolltext:
	dc.b "  ANOTHER FIRST FOR VIC-20 COMING UP... SIMPLE FOR PU-239 "
	dc.b $ff
scrolltmp:
	ds.b 8
scrollcnt
	dc.b 0

scroll:
#if 1
ptr$	lda sin+0
	inc ptr$+1
	inc ptr$+1
	lsr
	lsr
	clc
	adc #2
	sta sspeed+1
#endif
	lda xpos+1
	sec
sspeed:	sbc #2
	and #7
	sta xpos+1
	bcc do$
	rts
do$
#if 0
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	rts
95$
#endif
	ldy #0
0$	lda matrix0+SCRPOS*COLUMNS+1,y
	sta matrix0+SCRPOS*COLUMNS+0,y
	ora #4
	sta matrix1+SCRPOS*COLUMNS+0,y
	iny
	cpy #(10-3)*COLUMNS
	bne 0$

	; plot a new column

	ldx #0
	ldy #0
plot$	lda scrolltmp,y
col$	and #$80
	beq 11$
	lda #1
11$	sta a$+1
	lda matrix0+SCRPOS*COLUMNS+COLUMNS-2,x
	asl
a$	ora #1
	and #3
	ora #$38
	sta matrix0+SCRPOS*COLUMNS+COLUMNS-1,x
	ora #4
	sta matrix1+SCRPOS*COLUMNS+COLUMNS-1,x
	txa
	clc
	adc #COLUMNS
	tax
	iny
	cpy #7
	bne plot$

	lsr col$+1
	bne notnext$

ptr$	ldx #0
	inx
	lda scrolltext-1,x
	bpl asc$

	dec scrollcnt
	bpl not$
	lda AUTO
	bne not$
	lda #63	;inc KEYWAIT+1
	sta fade+1

not$	ldx #0
	lda #32
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


	;12d0..1300 free


	org $1300
back:	; $00
	dc.b 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
	dc.b 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
	dc.b 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
	dc.b 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8

#if SYSTEM == NTSC
	dc.b $00,$22,$22,$44,$84,$97,$77,$11,$00,$22,$22,$44,$84,$97,$77,$f1
	dc.b $11,$11,$f1,$77,$97,$84,$44,$22,$22,$00,$11,$77,$97,$84,$44,$22

	dc.b $00,$66,$22,$44,$44,$33,$b1,$11,$00,$66,$44,$44,$e3,$33,$b1,$11
	dc.b $11,$b1,$33,$e3,$44,$44,$66,$00,$11,$b1,$33,$e3,$44,$44,$66,$00
#else
	dc.b $08,$28,$28,$48,$88,$98,$78,$18,$08,$28,$28,$48,$88,$98,$78,$f8
	dc.b $18,$18,$f8,$78,$98,$88,$48,$28,$28,$08,$18,$78,$98,$88,$48,$28

	dc.b $08,$68,$28,$48,$48,$38,$b8,$18,$08,$68,$48,$48,$e8,$38,$b8,$18
	dc.b $18,$b8,$38,$e8,$48,$48,$68,$08,$18,$b8,$38,$e8,$48,$48,$68,$08
#endif


	org $1380
cs0:	; shift 0
	; 00 old blank + new blank
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	; 01 old blank + new set
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	; 10 old set + new blank
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	; 11 old set + new set
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

cs1:	; shift 1
	; 00 old blank + new blank
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	; 01 old blank + new set
	dc.b $7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$7f
	; 10 old set + new blank
	dc.b $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
	; 11 old set + new set
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

	org $1400
	org $1400+ROWS*COLUMNS
	;dc.b 0

start
	ldx #255
	txs
	jsr setcolmem	; is overwritten by setvideomem
	jsr setvideomem
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
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

#if SAME_MUS
	;jsr player_update
	jsr player_update
#else
	nop
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
	ldx #(ROWS*COLUMNS+1)/2
0$	lda #$38
	sta matrix0+0*COLUMNS-1,x
	sta matrix0+ROWS*COLUMNS/2-1,x
	lda #$3c
	sta matrix1+0*COLUMNS-1,x
	sta matrix1+ROWS*COLUMNS/2-1,x
	dex
	bne 0$

	rts

	ds.b 20

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

#if SYSTEM & PAL
#else
	nop
	bit $ea
	pha
	pla
	pha
	pla
	pha
	pla
#endif

wavestart
	ldy #0

#if SYSTEM == NTSC
loop
colstart
	lda #0
	clc
	adc tab,y
	and #$3f
	sta backp+1	;4

	lda sin,y	;0..12
xpos	adc #0		;0..7 -> 0..19
	sta char+1
	lsr
	lsr
	nop
	nop
	nop
	nop
	sta $9000	; shift by a/4 cycles
char	ldx #0		; shift by a%4 cycles
	lda videomap,x
	sta $9005
backp	lda back	; 4
	sta $900f	; 4

	iny
waveend	cpy #(ROWS*16&255)	;2
	bne loop	; 3
	samepage loop
outloop:

#else

loop
	lda sin,y
	clc
	adc #4
xpos	adc #0
	sta move+1
	and #3
	tax
	lda videomap,x
	sta char+1

colstart
	lda #0
	adc tab,y
	and #$3f
	sta backp+1	;4

move	lda #0		; shift by a/4 cycles
	lsr
	lsr
	sta $9000
backp	lda back	; 4
	sta $900f	; 4
char	lda #0		; shift by a%4 cycles
	sta $9005

	nop
	iny
waveend	cpy #(ROWS*16&255)	;2
	bne loop	; 3
	samepage loop
outloop:
#endif


#if SYSTEM == NTSC
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
	lda #0
	sta $900f
#endif
#if 1
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	lda #63	;inc KEYWAIT+1
	sta fade+1
95$
#endif
	lda $9111
	and #$20
	beq button$
	; not button
	inc wavestart+1
	inc wavestart+1
button$
	lda $9111
	and #$10
	beq left$
	; not left
	dec colstart+1
left$
	lda wavestart+1
	clc
	adc #(ROWS*16&255)
	sta waveend+1


	lda $9111	; VIA#1 port A
	and #$08	; sw2? left
	beq down$
	; not down
	jsr scroll
down$

	;jsr player_update
	jsr player_update

	jsr fade

	;dec $900f
	;inc $900f

	jmp $eb18	; return from IRQ




	; 1699..177f free

	org $1780

cs2:	; shift 2
	; 00 old blank + new blank
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	; 01 old blank + new set
	dc.b $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f
	; 10 old set + new blank
	dc.b $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
	; 11 old set + new set
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

cs3:	; shift 3
	; 00 old blank + new blank
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	; 01 old blank + new set
	dc.b $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f
	; 10 old set + new blank
	dc.b $e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
	; 11 old set + new set
	dc.b $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

	org $1800
tab:
#include "sin.h"




	; 1a00..1c7f
#if SAME_MUS

MUSVEC = $1c00
player_update = MUSVEC+6

#else

#if 0
MUS
;#include "../music/sng-newsong.dur"
;#include "../music/sng-tetris.dur"
#include "../music/dur-enjoy.sng"
;#include "../music/dur-pacster.sng"
;#include "../music/dur-action.sng"
;#include "../music/fssi_t5.dur"
#include "../music/player-dur.a65"
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

#endif

	;org $1fc0
	org $2000

; matrix0 = 1000..11b1 , matrix1 = 1400..15b1
; cs0 = 1000 (1380..13ff), cs1 = 1400 (1780..17ff)
; -------XXXXX m1 cs1 + 0 cyc	$dd
; ------XXXXXX m1 cs0 + 0 cyc	$dc
; -----XXXXXXX m0 cs1 + 0 cyc	$cd
; ----XXXXXXXX m0 cs0 + 0 cyc	$cc
