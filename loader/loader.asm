
;SYSTEMSEL = $31a
AUTO      = $31b
LOADER    = $31c	; ..$3ff
FADE = $3CF


  processor 6502


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

; =========================================================================
; Initializations and the disk drive's part of the fastloader.
; =========================================================================

; KERNAL definitions

secnd	= $ff93	; send secondary address for LISTEN
ciout	= $ffa8	; write serial data
unlsn	= $ffae	; send UNLISTEN command
listn	= $ffb1	; send LISTEN command
fa	= $ba	; Current Device Number

AMOUNT = $20	; amount of data bytes to transfer with one M-W command
ESCBYTE = $ef	; the escape char used in the transfers
RETRIES = 20	; amount of retries in reading a block


DEFAULT_DEVICE = 8	; Default device number

; the initialization code

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
	jsr initstuff

	ldx #0
cc$	lda $1e00,x
	sta matrix1+$000,x
	lda $1f00,x
	sta matrix1+$100,x
	lda $9600
	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	inx
	bne cc$

	; fill second video matrix
	ldx #(charset-$1400)/8
cc1$	stx matrix0
	inx
	cpx #(charset-$1400)/8+22
	bne 11$
	ldx #(charset-$1400)/8
11$	inc cc1$+1
	bne cc1$
	inc cc1$+2
	lda cc1$+2
	cmp #>(matrix0+$200)
	bne cc1$

	; relocate video matrix, do the switch before overwritten by music
	lda $9002
	ora #$80
	ldx #$e0	; matrix at $1800, charset at $8000
	stx $9005
	sta $9002

	ldx #0
0$	lda mussrc,x
	sta MUSVEC,x
	inx
#if musend-mussrc > 768
	bne 0$
1$	lda mussrc+256,x
	sta MUSVEC+256,x
	inx
	bne 1$
2$	lda mussrc+512,x
	sta MUSVEC+512,x
	inx
	bne 2$
3$	lda mussrc+768,x
	sta MUSVEC+768,x
	inx
	cpx #musend-mussrc-768
	bne 3$
#else
#if musend-mussrc > 512
	bne 0$
1$	lda mussrc+256,x
	sta MUSVEC+256,x
	inx
	bne 1$
2$	lda mussrc+512,x
	sta MUSVEC+512,x
	inx
	cpx #musend-mussrc-512
	bne 2$
#else
#if musend-mussrc > 256
	bne 0$
1$	lda mussrc+256,x
	sta MUSVEC+256,x
	inx
	cpx #musend-mussrc-256
	bne 1$
#else
	cpx #musend-mussrc
	bne 0$
#endif
#endif
#endif
	jsr player_init

NTSC	= 1
PAL	= 2

#if SYSTEM == NTSC

LINES = 261
CYCLES_PER_LINE = 65
RASTER = 20

#else

LINES = 312
CYCLES_PER_LINE = 71
RASTER = 155

#endif


TIMER_VALUE = LINES * CYCLES_PER_LINE - 2

;synchronize with the screen
	sei
	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315

	lda $9001
	sec
	sbc #10
	sta sync+1

sync	ldx #RASTER-10	; wait for this raster line (times 2)
10$	cpx $9004
	beq 10$
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$	ldx $9004
	txa

	bit $24
#if SYSTEM == NTSC
	bit $24
	ldx #21
#else
	ldx #24
#endif
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne 1$

	samepage sync

#if SYSTEM == NTSC
	pha
	pla
	pha
	pla
#endif

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
	cli

	; Do some fade effect

	jmp loadnext



line
#if SYSTEM == NTSC
	dc.b 184
#else
	dc.b 184
#endif

irq
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
#if 1
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
#else
	pha
	pla
	pha
	pla
#endif

	lda #22+128
	sta $9002
	lda #$e0	; matrix at $1a00, charset at $8000
	sta $9005

	ldx line
it$
	pha
	pla
	nop
	nop
	nop
	nop
	nop
	pha
	pla
	pha
	pla

	dex
	cpx #254
	beq out$

	txa
	ora #8
	lda $900f
#if SYSTEM == NTSC
	bit $ea
#else
	nop
	pha
	pla
#endif
	pha
	pla
	pha
	pla
	jmp it$

out$	lda $9002
	and #$7f
	sta $9002
	lda #$ed	; matrix at $1800, charset at $1400
	sta $9005

#if 0
odd$	lda #0
	inc odd$+1
	lsr
	bcc advance$
	jmp doing$
advance$
#endif

	lda line
	and #7
	tay
	bne next$	; copy next line..

	lda #117
wait$	cmp $9004
	bcs wait$

	ldy #0
	ldx #0
loop$	lda matrix1+22*22,x
	pha
	asl
	asl
	asl
	sta chr0$+1,y
	pla
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #$80
	sta chr0$+2,y
	tya
	clc
	adc #6
	tay
	inx
	cpx #22
	bne loop$
	lda loop$+1
	sec
	sbc #22
	sta loop$+1
	bcs hiok$
	dec loop$+2
hiok$

	ldy #7

next$	ldx #0
chr0$	lda $8000,y
	sta charset+0*8,x
chr1$	lda $8008,y
	sta charset+1*8,x
chr2$	lda $8010,y
	sta charset+2*8,x
chr3$	lda $8018,y
	sta charset+3*8,x
chr4$	lda $8020,y
	sta charset+4*8,x
chr5$	lda $8028,y
	sta charset+5*8,x
chr6$	lda $8030,y
	sta charset+6*8,x
chr7$	lda $8038,y
	sta charset+7*8,x
chr8$	lda $8038,y
	sta charset+8*8,x
chr9$	lda $8038,y
	sta charset+9*8,x
chr10$	lda $8038,y
	sta charset+10*8,x
chr11$	lda $8038,y
	sta charset+11*8,x
chr12$	lda $8038,y
	sta charset+12*8,x
chr13$	lda $8038,y
	sta charset+13*8,x
chr14$	lda $8038,y
	sta charset+14*8,x
chr15$	lda $8038,y
	sta charset+15*8,x
chr16$	lda $8038,y
	sta charset+16*8,x
chr17$	lda $8038,y
	sta charset+17*8,x
chr18$	lda $8038,y
	sta charset+18*8,x
chr19$	lda $8038,y
	sta charset+19*8,x
chr20$	lda $8038,y
	sta charset+20*8,x
chr21$	lda $8038,y
	sta charset+21*8,x
	inx
	cpx #8
	beq out1$
	jmp chr0$
out1$
	dec line
	lda line
	cmp #255
	bne doing$
#if 1
	inc loadnext+1
	lda #<MUSVEC	;irq2
	sta $314
	lda #>MUSVEC	;irq2
	sta $315
#endif
doing$	jmp MUSVEC	;irq2



mussrc
#rorg $1cc0
INCSONG equ 1
#include "../music/acplay2.a65"
;#include "../music/cutie.sng"
#include "../music/folk2.sng"
;#include "../music/canon3.sng"
;#include "../music/mmix.sng"
;#include "../music/vicual.sng"
;#include "../music/voyage.sng"
#rend

musend

	org $1800-22*8
charset:	; graphics of 'last row'
matrix0 = $1800	; filled with 'last row'
matrix1 = $1a00	; copy of original $1a00..$1bff

;irq2src
;#rorg $200
;irq2	jsr player_update
;	jsr player_update
;	jmp $eb15	; leave IRQ, ack timer interrupt
;#rend
;irq2end


initstuff:	; Fall through to initloader
	ldx #0
str$	lda string,x
	beq eol$
	jsr $ffd2
	inx
	jmp str$
eol$

;	ldx #irq2end-irq2src-1
;10$	lda irq2src,x
;	sta irq2,x
;	dex
;	bpl 10$


initloader:
	ldx #<loadersize
#if loadersize > 256
#echo "irq loader too long!"
#err
#endif
xferloop$:
	lda irqload - 1,x
	dex
	sta irqorig,x
	bne xferloop$

; send the m-w command to write the data
mwloop$:
	jsr inidev$
	ldx #lmwcmd$ - 1
smwcmd$:
	lda mwcmd$,x
	jsr ciwait
	dex
	bpl smwcmd$

; send the actual data bytes  
	ldx #0
mwbyte$:
	lda drvcode,x
	jsr ciwait
	inx
	cpx #AMOUNT
	bne mwbyte$

; complete the command
	jsr unlsn
	jsr serialwait

; update the addresses
	clc
	lda #AMOUNT
	adc mwbyte$ + 1
	sta mwbyte$ + 1
	bcc noupdhi1$
	clc
	inc mwbyte$ + 2
noupdhi1$:

	lda #AMOUNT
	adc mwcmd$ + 2
	sta mwcmd$ + 2
	tax
	lda #0
	adc mwcmd$ + 1
	sta mwcmd$ + 1
	cpx #<edrvcode
	sbc #>edrvcode
	bcc mwloop$

	jsr inidev$
	lda #"U"
	jsr ciwait

	lda #"3"
	jsr ciwait

	jsr unlsn
	jsr serialwait
	rts
	;jmp $100d

; subroutine: make the current drive listen

inidev$:
	lda fa	; get the device number
	bne nodef$	; if not set, then use the default device number
	lda #DEFAULT_DEVICE
nodef$:
	sta fa		; save the device number
	jsr listn
	jsr serialwait
	lda #$6f
	jsr secnd
	jmp serialwait


; the m-w command backwards

mwcmd$:
	dc.b AMOUNT,>drive,<drive,"W-M"
lmwcmd$ = . - mwcmd$


ciwait:	jsr ciout
serialwait:
	ldy #55
0$	dey
	bne 0$
	rts



LEDFLASH = 3	; LED flashing level:
		; 0 = normal (LED constantly on while loading a file)
		; 1 = LED glows on and off while waiting for a command
		; 2 = LED on only while reading sectors
		; 3 = 1 + 2


; =========================================================================
; the drive code
; =========================================================================

drvcode:

#rorg $500

;---------------------------------------
; The fastload routine for the drive
;---------------------------------------
; The 1581-code and some optimization by Pasi Ojala, albert@cs.tut.fi

PARANOID = 0	;1
		; The 1581 docs say that track 40, 0 contains
		; a pointer to the first directory block.
		; 1=use it, 0=make a guess (track 40, sector 3)
;---------------------------------------
; FOR 1581
;---------------------------------------
;acsbf8	= $03	; job for buffer 1 (not used directly)
trkbf8	= $0d	; track for job 1
sctbf8	= $0e	; sector for job 1

;ciapa	= $4000	; (not used directly)
ciapb	= $4001
ledon	= $cbcc	; activity led on: $cbcc, off: $cbc3
ledoff	= $cbc3	; or using the job queue:  on $94 / off $96

;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------
acsbf	= $01	; access to buffer 1
trkbf	= $08	; track of buffer 1
sctbf	= $09	; sector of buffer 1
iddrv0	= $12	; id of drive 0
id	= $16	; id

via1pb	= $1800
via2pb	= $1c00

;---------------------------------------
; For both 154x/7x and 1581
;---------------------------------------
buf	= $0400	; sector buffer (for job 1)
datbf	= $14	; databuffer - temporary (on 1581 sector for job 4)


drive	cld
	lda $ff54	; Execute Job -routine entry in the jump table
	cmp #$4c	; jmp abs	- probably exists
	beq drive1581
	cmp #$6c	; jmp (abs)	- probably exists
	beq drive1581
	jmp drive1571

name	ds.b 8


drive1581		; interrupts always enabled
#if !(LEDFLASH & 1)
	jsr ledoff
#endif
	ldy #0
nl$	jsr recv$	; get the file name, first char
	sta name,y
	iny
	cpy #8		; 8 chars
	bne nl$
#if !(LEDFLASH & 2)
	jsr ledon
#endif

#if PARANOID
	ldx #40		; get the root block
	ldy #0
	jsr readsect$	; read the sector
	bcs errquit$	; quit if it could not be read

	ldx buf		; read the disk directory (track 40, sector 3 (usually))
	ldy buf+1
#else
	ldx #40
	ldy #3
#endif
dirloop$
	jsr readsect$	; read the sector
	bcs errquit$	; quit if it could not be read

	ldy #2
nextfile$
	lda buf,y	; check file type
	and #$87
	cmp #$82	; must be PRG
	bne notfound$

	sty notfound1$+1
	ldx #0
cl$	lda buf+3,y	; check the first characters
	cmp name,x
	bne notfound1$
	iny
	inx
	cpx #8
	bne cl$

;found$
	; Error led flash off
	lda #$9a
	ldx #1
	jsr $ff54	; execute command for queue 1, return value in A also

	ldy notfound1$+1
	ldx buf+1,y	; get the track and sector numbers
	lda buf+2,y
	tay
nextsect$
	jsr readsect$
	bcs errquit$	; quit if the sector could not be read
	ldy #255
	lda buf
	bne notlast$	; if the track is nonzero, this wasn't the last sector

	ldy buf+1	; last sector: get the index of last valid byte
notlast$
	sty numlast$+1

	ldy #1		; skip the track and sector when sending the buffer
sendbuf$		; send the buffer contents to the computer
	ldx buf+1,y
	cpx #ESCBYTE
	bne noesc$

	jsr send$	; escape the escape character
	ldx #ESCBYTE
noesc$	jsr send$
	iny
numlast$
	cpy #255	; last valid byte in the sector?
	bne sendbuf$

	ldy buf+1	; the track and sector of next block
	ldx buf		; note the load order - buf[0] is zero for last block
	bne nextsect$	; loop until all sectors are loaded

finish$
	ldx #ESCBYTE	; send the escape byte followed by 0 to notify the computer
	jsr send$	; returns with X=0 and Z set
	;ldx #0
	jsr send$	; returns with X=0 and Z set
	jmp drive1581


notfound1$
	ldy #0
notfound$
	tya
	clc
	adc #$20
	tay
	bcc nextfile$

	ldy buf+1	; get next sector
	ldx buf		; and track
	bne dirloop$	; keep trying until the last directory block has been searched
	; file not found: fall through
errquit$
	ldx #ESCBYTE	; send the escape byte followed by 1 to notify the computer
	jsr send$	; returns with X=0 and Z set
	inx		;ldx #1
	jsr send$	; returns with X=0 and Z set

	; Error led flash on
	lda #$98
	inx		;ldx #1
	jsr $ff54	; execute command for queue 1, return value in A also
	jmp drive1581


;---------------------------------------
; readsect$: read a sector

readsect$
	stx trkbf8
	sty sctbf8
#if LEDFLASH & 2
	jsr ledon
#endif
	lda #$80	; $80 - read sector command
	ldx #1		; job queue #1
	jsr $ff54	; execute command A for queue X, return value in A too
	cmp #2		; 0 and 1 (0-2 and 1-2) clear, errors set carry
#if LEDFLASH & 2
	jmp ledoff	; led off (does not affect carry)
#else
	rts
#endif

; send$ sends the X register contents. datbf is used as temporary storage.
; returns with X=0 and Z set
send$	stx datbf
	ldx #8		; send 8 bits
sendb$	lsr datbf	; read next bit
	lda #2		; prepare for CLK=high, DATA=low
	bcs sskip$
	lda #8		; prepare for CLK=low, DATA=high
sskip$	sta ciapb	; send the data
sack$	lda ciapb	; wait for CLK==DATA==low
	and #5
	eor #5
	bne sack$
	sta ciapb	; set CLK=DATA=high (A=0)
	lda #5
swait$	bit ciapb
	bne swait$	; wait for CLK==DATA==high
	dex
	bne sendb$	; loop until all bits have been sent
	rts

;---------------------------------------
; recv$ receives a byte to A. datbf is used as temporary storage.

recv$	sty y$+1
#if LEDFLASH & 1
	ldy #0		; LED brightness (0=dim, 255=lit)
	tsx
fincr$	jsr doflash$
	ldy datbf
	iny
	bne fincr$
fdecr$	dey
	jsr doflash$
	ldy datbf
	bne fdecr$
	beq fincr$
doflash$
	sty datbf	; store the counter for LED flashing
	jsr ledoff
	jsr fdelay$	; perform the delay
	jsr ledon
	lda datbf
	eor #$ff
	tay		; fall through
fdelay$
	lda #$85
	and ciapb	; wait for any signal from the bus
	bne flashdone$
	iny
	bne fdelay$
	rts
flashdone$
	jsr ledoff
	txs		; discard the return address
#endif

	ldx #8		; counter: receive 8 bits
recvb$	lda #$85
	and ciapb	; wait for CLK==low || DATA==low
	beq recvb$
	bmi gotatn$	; quit if ATN was asserted
	lsr		; read the data bit
	lda #2		; prepare for CLK=high, DATA=low
	bcc rskip$
	lda #8		; prepare for CLK=low, DATA=high
rskip$	sta ciapb	; acknowledge the bit received
	ror datbf	; and store it
rwait$	lda ciapb	; wait for CLK==high || DATA==high
	and #5
	eor #5
	beq rwait$
	lda #0
	sta ciapb	; set CLK=DATA=high
	dex
	bne recvb$	; loop until all bits have been received
	lda datbf	; read the data to A
y$	ldy #0
	rts

gotatn$	pla		; If ATN gets asserted, exit to the operating system.
	pla		; Discard the return address.
	cli		; Enable the interrupts.
	rts


;edrvcode


;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------


drive1571
	;cld
	cli		; interrupts enabled until first sector read
#if !(LEDFLASH & 1)
	lda #$f7	; led off
	and via2pb
	sta via2pb
#endif
	ldy #0
nl$	jsr recv$	; get the file name, first char
	sta name,y
	iny
	cpy #8		; 8 chars
	bne nl$

#if !(LEDFLASH & 2)
	lda #8
	ora via2pb
	sta via2pb	; led on
#endif

	ldx #18
	ldy #1		; read the disk directory (track 18, sector 1)
dirloop$
	jsr readsect$	; read the sector
	bcs errquit$	; quit if it could not be read

	ldy #$02
nextfile$
	lda buf,y	; check file type
	and #$83
	cmp #$82	; must be PRG
	bne notfound$

	sty notfound1$+1
	ldx #0
cl$	lda buf+3,y	; check the first characters
	cmp name,x
	bne notfound1$
	iny
	inx
	cpx #8
	bne cl$

found$	ldy notfound1$+1
	ldx buf+1,y	; get the track and sector numbers
	lda buf+2,y
	tay

nextsect$
	jsr readsect$
	bcs errquit$	; quit if the sector could not be read
	ldy #255
	lda buf
	bne notlast$	; if the track is nonzero, this wasn't the last sector

	ldy buf+1	; last sector: get sector length
notlast$
	sty numlast$+1

	ldy #1		; skip the track and sector when sending the buffer
sendbuf$		; send the buffer contents to the computer
	ldx buf+1,y
	cpx #ESCBYTE
	bne noesc$

	jsr send$	; escape the escape character
	ldx #ESCBYTE

noesc$	jsr send$
	iny
numlast$
	cpy #255	; were all bytes of the block sent?
	bne sendbuf$

	ldy buf+1	; store the track and sector of next block
	ldx buf
	bne nextsect$	; loop until all sectors are loaded

finish$	ldx #ESCBYTE	; send the escape byte followed by 0 to notify the computer
	jsr send$	; returns with X=0 and Z set
	;ldx #0
	jsr send$	; returns with X=0 and Z set
	jmp drive1571

notfound1$
	ldy #0
notfound$
	tya
	clc
	adc #$20
	tay
	bcc nextfile$

	ldy buf+1	; get next sector
	ldx buf		; and track
	bne dirloop$	; keep trying until the last directory block has been searched
	; file not found: fall through
errquit$
	ldx #ESCBYTE	; send the escape byte followed by 1 to notify the computer
	jsr send$	; returns with X=0 and Z set
	inx		;ldx #1
	jsr send$	; returns with X=0 and Z set
	jmp drive1571


;---------------------------------------
; readsect$: read a sector

readsect$
	stx trkbf
	sty sctbf
#if LEDFLASH & 2
	lda #8
	ora via2pb
	sta via2pb	; turn the LED on
#endif
	ldy #RETRIES	; load the retry count
	cli		; enable interrupts, so that the command will be executed
retry$	lda #$80
	sta acsbf	; code for reading the sector
poll1$	lda acsbf	; wait for the command to complete
	bmi poll1$
	cmp #1
	bne noexit$
#if LEDFLASH & 2
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
#endif
	clc
	sei		; disable interrupts again to make the program faster
	rts		; success: exit the loop

noexit$	dey		; decrement the retry count
	bmi error$	; quit if there were too many retries

	cpy #RETRIES / 2
	bne skipcode$

	lda #$c0
	sta acsbf	; half the retries left: knock the head (seek track 1)
skipcode$
	lda id		; tolerate disk id changes
	sta iddrv0
	lda id+1
	sta iddrv0+1

poll2$	lda acsbf	; wait for the command to complete
	bmi poll2$
	bpl retry$	; branch always

error$
#if LEDFLASH & 2
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
#endif
	sec
	sei
	rts

; send$ sends the X register contents. datbf is used as temporary storage.
; returns with X=0 and Z set

send$	stx datbf
	ldx #8	; send 8 bits
; sendbit$ sends a bit
sendb$	lsr datbf	; read next bit
	lda #2		; prepare for CLK=high, DATA=low
	bcs sskip$
	lda #8		; prepare for CLK=low, DATA=high
sskip$	sta via1pb	; send the data
sack$	lda via1pb	; wait for CLK==DATA==low
	and #5
	eor #5
	bne sack$
	sta via1pb	; set CLK=DATA=high
	lda #5
swait$	bit via1pb
	bne swait$	; wait for CLK==DATA==high
	dex
	bne sendb$	; loop until all bits have been sent
	rts

;---------------------------------------
; recv$ receives a byte to A. datbf is used as temporary storage.

recv$	sty y$+1
#if LEDFLASH & 1
	ldy #0		; LED brightness (0=dim, 255=lit)
	tsx
fincr$	jsr doflash$
	ldy datbf
	iny
	bne fincr$
fdecr$	dey
	jsr doflash$
	ldy datbf
	bne fdecr$
	beq fincr$

doflash$
	sty datbf	; store the counter for LED flashing
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
	jsr fdelay$	; perform the delay
	lda #8
	ora via2pb
	sta via2pb	; turn the LED on
	lda datbf
	eor #$ff
	tay		; fall through

fdelay$	lda #$85
	and via1pb	; wait for any signal from the bus
	bne flashdone$
	iny
	bne fdelay$
	rts

flashdone$
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
	txs		; discard the return address
#endif

	ldx #8		; counter: receive 8 bits
recvbit$
	lda #$85
	and via1pb	; wait for CLK==low || DATA==low
	bmi gotatn$	; quit if ATN was asserted
	beq recvbit$
	lsr		; read the data bit
	lda #2		; prepare for CLK=high, DATA=low
	bcc rskip$
	lda #8		; prepare for CLK=low, DATA=high
rskip$	sta via1pb	; acknowledge the bit received
	ror datbf	; and store it
rwait$	lda via1pb	; wait for CLK==high || DATA==high
	and #5
	eor #5
	beq rwait$
	lda #0
	sta via1pb	; set CLK=DATA=high

	dex
	bne recvbit$	; loop until all bits have been received
	lda datbf	; read the data to A
y$	ldy #0
	rts

gotatn$	pla		; If ATN gets asserted, exit to the operating system.
	pla		; Discard the return address.
	cli		; Enable the interrupts.
	rts

edrvcode
;#endif

#rend
; =========================================================================


	; Code by Albert of Pu-239	http://www.cs.tut.fi/~albert/
	; Except part of the loader code originally by Marko Mäkelä


; =========================================================================
; The asynchronous 1540/1541/1570/1571 fast loader, computer's part
; =========================================================================

irqload
#rorg $31a
irqorig:

SYSTEMSEL	; $31a
	dc.b 0		; 0 for NTSC
AUTO		; $31b
	dc.b 0		; zero for automatic control

; I/O constants and the variables

iecport1 = $912c	;$dd00	;$912c
dato = 32		;32	;32
clko = 2		;16	;2
iecport2 = $911f	;$dd00	;$911f
atno = 128		;8	;128
clki = 1		;64	;1
dati = 2		;128	;2

LOADER			; $31c..3d1
	sta clrs$+1
	stx name$+1
	sty name$+2

	lda iecport1
	and #255 - dato - clko
	sta iec1d1a$	; CLK=1, DATA=1
	sta iec1d1b$
	eor #clko
	sta iec0d1a$	; CLK=0, DATA=1
	sta iec0d1b$

	lda #0
clrs$	ldx #3	;15	; $34b
clr$	sta $9000,x	; color, volume, rows, columns, etc.
	dex
	bpl clr$

	tay	; ldy #0
name$	lda $aaaa,y
	jsr putbyt$	; send the file name (8 chars)
	iny
	cpy #8
	bne name$

	jsr getbyt$	; get the start address
	tay
	jsr getbyt$
	sta adr$+2	; use the load address..

loadloop$:	; $358
	jsr getbyt$	; get next file byte, exit on completion
adr$	sta $100,y	; store it
	iny
	bne loadloop$
	inc adr$+2	; adr$+2 = $35d
	jmp loadloop$	; $364

;---------------------------------------
; getbyt$: get a byte, interpret the escape codes

getbyt$:
	jsr getbits$
	cmp #ESCBYTE
	bne getdone$
	jsr getbits$	; escape char fetched, get another byte
	cmp #ESCBYTE	; another escape char: it is a literal
	beq getdone$

	cmp #1		; Transfer finished. 0=ok, nonzero=error.
	pla		; Set the C flag accordingly.
	pla		; discard the return address
	rts


; getbits$: get a byte

getbits$:
	ldx #8	; counter: get 8 bits
getbit$:
	lda iecport2
	and #dati | clki
	eor #dati | clki
	beq getbit$	; wait for CLK==low || DATA==low

#if dati == 128
	asl		; Carry = DATA==low
#else
#if dati < clki
	and #dati
#endif
	cmp #dati
#endif

iec0d1a$ = . + 1
	lda #255 - dato
	bcs gskip$
	eor #dato | clko
gskip$:	sta iecport1	; acknowledge the bit
	ror store$	; store the data

#if 1
	lda #dati | clki
wgack$:	bit iecport2
	beq wgack$	; wait for CLK==high || DATA==high
#else
	lda #$ea
	lda #$ea
	lda #$ea
	nop
#endif
iec1d1a$ = . + 1
	lda #255 - clko - dato
	sta iecport1	; raise CLK and DATA
	dex
	bne getbit$	; loop until all bits are received
store$ = . + 1
	lda #0
getdone$:
	rts

; putbyt$ puts a byte

putbyt$:
	sta store$
	ldx #8	; counter: send all 8 bits
putbit$:
	lsr store$	; read a bit
iec0d1b$ = . + 1
	lda #255 - dato
	bcc pskip$
	eor #dato | clko
pskip$:	sta iecport1	; send the data

	lda #dati | clki
wputack1$:
	bit iecport2
	bne wputack1$	; wait for CLK==DATA==low

iec1d1b$ = . + 1
	lda #255 - clko - dato
	sta iecport1	; set DATA=CLK=high
wputack2$:
	lda iecport2
	and #dati | clki
	eor #dati | clki
	bne wputack2$	; wait for CLK==DATA==high
	dex
	bne putbit$	; loop until all bits are sent
	rts

FADE	; then fade sound
	;lda $900e
	;and #$0f
	;ldx #RASTER
4$	ldy #3		; 4 frames per volume step
2$	cpx $9004
	bne 2$
3$	cpx $9004
	beq 3$
	dey
	bpl 2$

	sec
	sbc #1
	sta $900e
	bne 4$

	ldx #<$eb15
	lda #>$eb15

;FADENOT
5$	sei
	stx $0314
	sta $0315
	cli

	lda $100d
	cmp #$78
	beq 0$
	jmp $100d	; no sei in short decompressor!
0$	jmp $100e

loaderend:
#rend
loadersize = . - irqload

	; initloader 1750..1b99

	org $1c00	;$1d54
	; color luminance order
	; 062485371

nextFile:
#if SYSTEM == NTSC
	dc.b "INTRO.N",$a0		; 8 significant chars
#else
	dc.b "INTRO.P",$a0		; 8 significant chars
#endif

	; This code needs to be high enough not to be overwrite by the
	; next part..
	; TODO: use the same code in the loader?
loadnext
	lda #0
	beq loadnext

	lda #3		; only clear $9000..$9003
	ldx #<nextFile
	ldy #>nextFile
	jsr LOADER
1$	bcs 1$		; failed to load!

	lda $900f
	ldy #0

w1$	ldx #4
w2$	cpx $9004
	bne w2$

	sta $900f

	sei
w3$
#if SYSTEM == NTSC
	ldx #129
#else
	ldx #153
#endif
w4$	cpx $9004
	bne w4$

	ldx #13
w5$	dex
	bne w5$

	sty $900f
	cli

	dec w3$+1
	dec w3$+1
	ldx w3$+1
	cpx #5
	bne w1$

#if 0
	lda #<$eb15
	sta $0314
	lda #>$eb15
	sta $0315
#endif

	lda $100d
	cmp #$78
	beq 0$
	jmp $100d	; no sei in short decompressor!
0$	jmp $100e



	; Can be overwritten by music!
string:
	dc.b "xxxxxxxxxxxxxxxxxxxxxx"
	dc.b "xVICUAL MMIX BY PU239x"
#if SYSTEM == NTSC
	dc.b "x NTSC-HACK 30.4.2009x"
#else
	dc.b "x RELEASED 26.4.2009 x"
#endif
	dc.b "xxxxxxxxxxxxxxxxxxxxxx"
	dc.b "   ",164,164,"  ",13
	dc.b "  ",110,"  ",109," ",13
	dc.b "  ",109,"  ",110," ",13
	dc.b "   ",109,110,"  ",13
	dc.b " ",164,164,117,105,164,164,13
	dc.b " ",165," ",106,107," ",167,13
	dc.b " ",165," ",110,109," ",167,13
	dc.b " ",109,110," "," ",109,110
	dc.b 0


