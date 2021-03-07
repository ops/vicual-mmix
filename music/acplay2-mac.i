; VIC-20 MUSIC PLAYER (vplay) version 6.1
; (c) 2002-2009 Anders Carlsson [Zapac] <anders.carlsson@sfks.se>
;
; See "readme.txt" for usage instructions etc.
;
; Track bytes
; -----------
; %111YYYYY, %1111XXXX => block# = YYYYY, transp = XXXX, end of track
; %111YYYYY, %ZZZZXXXX => block# = YYYYY, transp = XXXX, repeat = ZZZZ
; %XXXYYYYY            => block# = YYYYY, transp = XXX
; 
; Once the player has repeated the subsequent block enough times, the
; repeat counter remains zero and the 'rept' instruction would have to 
; be given again to repeat another block in the track list.
;
; IMPORTANT: Due to the structure of the tracks, the jump-to can not be
;            calculated on logical structure, but on the amount of bytes
;            required to represent the track entries prior to the jump point!
;
; Update 2008-05-25: By adding 128 to each jump_to, a track can be made
;                    to stop instead of loop when it reaches its end.

tend eqm 16  ; 15 + 1 due to repeat bug below

  mac block
  if ({2} > 6 || {3} > 1)
  .byte {1}+224,{2}+({3}-1)*16
  else
   .byte {1}+({2}*32)
  endif
  endm

; Block bytes             Note#   Dur   Vibrato  Arpeggio
; -----------             -----  -----  -------  --------
; %YYYXXXXX            => XXXXX, YYY    n/c      n/c
; %111XXXXX, %000YYYYY => XXXXX, YYYYY, n/c      n/c
; %111XXXXX, %100YYYYY => XXXXX, YYYYY, next     n/c
; %111XXXXX, %010YYYYY => XXXXX, YYYYY, n/c      next
; %111XXXXX, %110YYYYY => XXXXX, YYYYY, next     next
; %111XXXXX, %001YYYYY => XXXXX, YYYYY, off      off (block end)
;
; If vibrato byte is %11xxxxxx, it changes portamento instead of vibrato

; Total 31 playable notes (c1 - e3 plus high hat and silence)
;
; Maximum duration
; ----------------
; Immediate (one byte) = 1..6
; Extended (two bytes) = 7..32
;
; IMPORTANT! Playback duration depends on song speed:
;
; song speed =  8 => maximum duration 256/8  = 32
; song speed = 10 => maximum duration 256/10 = 25
; song speed = 12 => maximum duration 256/12 = 21 ... may cause confusion

; {3}

bend equ 1
arp equ 2
vib equ 4
por equ 4

  mac note
    if ({2} > 6 || {5} > 0)
      .byte {1}+224,({2}-1)+{5}*32
      if ({5} & vib)
      .byte {3}
      endif
      if ({5} & arp)
      .byte {4}
      endif
    else
      .byte {1}+(({2}-1)*32)
    endif
  endm

  mac c
    note 1+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac c#
    note  2+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac d
    note  3+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac d#
    note  4+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac e
    note  5+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac f
    note  6+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac f#
    note  7+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac g
    note  8+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac g#
    note  9+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac a
    note 10+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac a#
    note 11+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac h
    note 12+({1}-1)*12,{2},{3},{4},{5}
  endm
  mac b
    note 12+({1}-1)*12,{2},{3},{4},{5}
  endm


  mac quiet     
    note  0,{1},{2},{3},{4}
  endm

  mac bd
    note  13,{1},{2},{3},{4} ; 1,8,13,25
  endm
  mac sd
  note 25,{1},{2},{3},{4}  ; 20,25,29
    endm
  mac hh
    note 30,{1},{2},{3},{4}
  endm

