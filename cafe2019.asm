	device zxspectrum128
	org #8000
SnaStart:
	ld sp,#7fff

	ld hl,kartinko
	ld de,#4000
	ld bc,#1b00
	ldir

	ld hl,sinus
	ld de,sinus+32
	ld bc,#ff-32
	ldir

	ld hl,muzzzza
	call pluer+3
	call podGOT
	call podGOT2

	ld a,7
	out (#fe),a

zaLOOP:
	ei
	halt
	call pechatalko
	call podGOTdiff
	call diffekt
	call pluer+5
	call changer
	jr zaLOOP

podGOTdiff
.tb1
	ld a,00
	ld (tut0+1),a
.tb4
	ld a,00
	ld (tut1+1),a

	ld h,high sprite1tab
	ld b,high sinus
	ld de,tablerONE+2
	call tut0
	ld a,(.tb1+1)
	inc a
	and 31
	ld (.tb1+1),a
	ld a,(.tb4+1)
	inc a
	and 31
	ld (.tb4+1),a
	ret

tut0
	ld c,16
	ld a,(bc)
	ld (tut1.t1+1),a
tut1
	ld a,0
	ld xl,#40
.t2 push af
	ld c,a
	ld a,(bc)
.t1	add #00
	add a
	ld l,a
	ld a,h
	xor high sprite1tab ^ high sprite2tab
	ld h,a

	ld a,xl
	bit 0,a
	jr nz,.tt44
	ld a,(tut0+1)
	inc a
	and 31
	ld (tut0+1),a
	ld c,a
	ld a,(bc)
	ld (.t1+1),a
.tt44
	ld a,(hl)
	ld (de),a
	inc e
	inc l
	ld a,(hl)
	ld (de),a
	inc e
	inc e
	inc e
	pop af
	inc a
	and 31
	dec xl
	jr nz,.t2
	ret

diffekt
	ld hl,tablerONE
	ld (.syuda+1),hl
	ld (savesp+1),sp
.syuda
	ld sp,tablerONE
	ld a,64
.l0	pop de
	pop hl
	dup 24
	ldi
	edup
	dec a
	jr nz,.l0
savesp:
	ld sp,#babe
	ret

podGOT2:
	xor a
	ld bc,sprite1
	ld ix,sprite1tab
	call .l0
	xor a
	ld bc,sprite2
	ld ix,sprite2tab
.l0	ld h,0
	ld l,a
	add hl,hl ;2
	add hl,hl ;4
	add hl,hl ;8
	push hl
	add hl,hl ;16
	pop de
	add hl,de ;24
	add hl,bc
	ld (ix),l
	inc ix
	ld (ix),h
	inc ix
	inc a
	cp 32
	jr nz,.l0
	ret

podGOT:
	ld hl,tablerONE
	ld de,#4004
	call pg01
	ld de,#5004
pg01
	ld b,32
.l0	ld (hl),e
	inc l
	ld (hl),d
	inc l
	inc l
	inc l
	call downde
	call downde
	djnz .l0
	ret

downde
     INC d
     LD A,d
     AND 7
     JR NZ,.l2
     LD A,e
     SUB #E0
     LD e,A
     JR NC,.l2
     LD A,d
     SUB 8
     LD d,A
.l2:
	ret





pechatalko
	ld a,0
	or a
	jr z,pechat
	cp 2
	jr z,fadeoff
	cp 1
	jr z,pauzaza
pauzaza:
	ld a,0
	inc a
pauzmask:
	and %11111111
	ld (pauzaza+1),a
	or a
	ret nz
	ld a,2
	ld (pechatalko+1),a
	ret

fadeoff:
	ld a,0
	inc a
	and %11110000
	ld (fadeoff+1),a
	or a
	ret nz
;	jr $
foff
	ld a,0
	cp 24
	jr z,usyo
	ld hl,#5900+#20+#04
	add l
	ld l,a

	ld b,6
.l0
	ld (hl),%01001001
	ld a,#20
	add l
	ld l,a
	djnz .l0

	ld a,(foff+1)
	inc a
	ld (foff+1),a
	ret

usyo
	xor a
	ld (pechatalko+1),a
	ld (foff+1),a
	ret
pechat
	ld a,0
	or a
	jr nz,symbprint
txtaddr:
	ld hl,tekst
	ld a,(hl)
	or a
	jr nz,notnewline
	inc hl
	ld (txtaddr+1),hl
	ld hl,#4824
	ld (scrinaddr+1),hl
	ld a,1
	ld (pechatalko+1),a
	ret
notnewline
	cp #ff
	jr nz,notnewtekst
	ld hl,tekst
	ld (txtaddr+1),hl
	jr txtaddr
notnewtekst
	cp #01
	jr nz,notchangecolor
	inc hl
	ld a,(hl)
	ld (kkolor+1),a
	inc hl
	ld (txtaddr+1),hl
	jr txtaddr
notchangecolor
	cp 2
	jr nz,nottabulat
	inc hl
	ld a,(scrinaddr+1)
	add (hl)
	ld (scrinaddr+1),a
	inc hl
	ld (txtaddr+1),hl
	jr txtaddr
nottabulat
	cp 3
	jr nz,notsetpauza
	inc hl
	ld a,(hl)
	ld (pauzmask+1),a
	inc hl
	ld (txtaddr+1),hl
	jr txtaddr

notsetpauza
	inc hl
	ld (txtaddr+1),hl
	sub " "
	ld h,high FONT
	add a
	ld l,a
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld a,(hl)
	ld (pechat+1),a
	inc hl
	ld (bukvaddr+2),hl
	ret

symbprint:
	ld a,0
	inc a
	and %11110000
	ld (symbprint+1),a
	or a
	ret nz

bukvaddr:
	ld ix,#dead
	ld d,high fontile
scrinaddr:
	ld hl,#4824

	ld b,6
.l0 ld c,h
	ld a,(ix)
	inc ix
	add a,a
	add a,a
	add a,a
	ld e,a

	dup 8
	ld a,(de):ld (hl),a:inc e:inc h
	edup
	ld h,c
	ld a,#20
	add l
	ld l,a
	djnz .l0

	ld hl,(scrinaddr+1)
	ld a,h
	rra
	rra
	rra
	and #03
	or #58
	ld h,a

	ld b,6
loop1
kkolor
	ld (hl),#00
	ld a,#20
	add l
	ld l,a
	djnz loop1

	ld (bukvaddr+2),ix

	ld a,(scrinaddr+1)
	inc a
	ld (scrinaddr+1),a

	ld a,(pechat+1)
	dec a
	ld (pechat+1),a

	ret
;0 - end of line
;1 - color
;2 - tab
;3 - pause mask
;#ff - new text

tekst      ;123456789012
		db 03,%11111111
		db	02,01,01,%01001111,"HELLO  PEOPLE!",0
		db 02,01,01,%01001110,"Q-BONE ",01,%01001111," TEAM",0
		db 03,%00001111
		DB 02,07,"INVITE",0
		DB 2,9,"YOU",0
		db 2,10,"TO",0
		db 03,%11111111
		DB 02,04,01,%01001110,"CAFE 2019",0
	db #03,%00011111
	db 01,%01001111
	db 2,8,"TRUE",0
	db 2,2,"DEMOSCENE",0
	db 2,4,"WEEKEND",0
	db 2,10,"IN",0
	db 03,%00111111
	db "KAZAN . RUSSIA",0
	db "25-27 OCTOBER",0
	db 01,%01001110
	db 03,%00011111
	db 02,5
	db "NEWSKOOL",0
	db 2,9,"AND",0
	db 2,5,"OLDSKOOL",0
	db 01,%01001111
	db 03,%00111111
	db 2,1,"UP TO 400 PPL",0
	db 01,%01001110
	db 2,5
	db "BBQ ZONE",0
	db 01,%01001111
	db "RETROMUSEUM",0
	db 01,%01001110
	db "COOL SEMINARS",0
	db 01,%01001111
	db 2,1
	db "AMAZING DJ-S",0
	db 01,%01001110
	db 2,1
	db "GREAT PRIZES",0
	db 01,%01001110
	db 3,%11111111
	db "NON",01,%01001010,"STOP",01,%01001110,"PARTY",0
	db 3,%01111111
	db 2,5,"CREDITS",0
	db "CODE - RASMER",0
	db 2,1,"GFX - SAND",0
	db 2,1,"MFX - NIK-O",0
	db 03,%11111111
	db " ",0

	db #ff

	align #100
FONT:  DEFW       L_D000
       DEFW       L_D007
       DEFW       L_D00E
       DEFW       L_D015
       DEFW       L_D01C
       DEFW       L_D023
       DEFW       L_D02A
       DEFW       L_D031
       DEFW       L_D038
       DEFW       L_D03F
       DEFW       L_D046
       DEFW       L_D04D
       DEFW       L_D054
       DEFW       L_D05B
       DEFW       L_D062
       DEFW       L_D069
       DEFW       L_D070
       DEFW       L_D07D
       DEFW       L_D084
       DEFW       L_D091
       DEFW       L_D09E
       DEFW       L_D0AB
       DEFW       L_D0B8
       DEFW       L_D0C5
       DEFW       L_D0D2
       DEFW       L_D0DF
       DEFW       L_D0EC
       DEFW       L_D0F3
       DEFW       L_D0FA
       DEFW       L_D101
       DEFW       L_D108
       DEFW       L_D10F
       DEFW       L_D11C
       DEFW       L_D129
       DEFW       L_D136
       DEFW       L_D143
       DEFW       L_D150
       DEFW       L_D15D
       DEFW       L_D16A
       DEFW       L_D177
       DEFW       L_D184
       DEFW       L_D191
       DEFW       L_D198
       DEFW       L_D19F
       DEFW       L_D1AC
       DEFW       L_D1B3
       DEFW       L_D1C6
       DEFW       L_D1D3
       DEFW       L_D1E0
       DEFW       L_D1ED
       DEFW       L_D1FA
       DEFW       L_D207
       DEFW       L_D214
       DEFW       L_D221
       DEFW       L_D22E
       DEFW       L_D23B
       DEFW       L_D24E
       DEFW       L_D25B
       DEFW       L_D268
;
L_D000       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D007       DEFB       #01
       DEFB       #01,#05,#05,#08,#09,#00
L_D00E       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D015       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D01C       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D023       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D02A       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D031       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D038       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D03F       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D046       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D04D       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D054       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D05B       DEFB       #01
       DEFB       #00,#00,#04,#00,#00,#00
L_D062       DEFB       #01
       DEFB       #00,#00,#00,#00,#09,#00
L_D069       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D070       DEFB       #02
       DEFB       #02,#05,#05,#05,#06,#00
       DEFB       #03,#05,#05,#05,#07,#00
L_D07D       DEFB       #01
       DEFB       #02,#05,#05,#05,#08,#00
L_D084       DEFB       #02
       DEFB       #02,#07,#02,#05,#08,#00
       DEFB       #03,#05,#07,#02,#08,#00
L_D091       DEFB       #02
       DEFB       #02,#07,#00,#03,#06,#00
       DEFB       #03,#07,#03,#05,#07,#00
L_D09E       DEFB       #02
       DEFB       #01,#05,#06,#00,#00,#00
       DEFB       #01,#05,#05,#05,#08,#00
L_D0AB       DEFB       #02
       DEFB       #01,#06,#00,#03,#06,#00
       DEFB       #01,#06,#03,#05,#07,#00
L_D0B8       DEFB       #02
       DEFB       #02,#05,#05,#05,#06,#00
       DEFB       #03,#06,#03,#05,#07,#00
L_D0C5       DEFB       #02
       DEFB       #01,#07,#02,#05,#08,#00
       DEFB       #01,#07,#00,#00,#00,#00
L_D0D2       DEFB       #02
       DEFB       #02,#06,#02,#05,#06,#00
       DEFB       #03,#07,#03,#05,#07,#00
L_D0DF       DEFB       #02
       DEFB       #02,#05,#06,#03,#06,#00
       DEFB       #03,#05,#05,#05,#07,#00
L_D0EC       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D0F3       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D0FA       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D101       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D108       DEFB       #01
       DEFB       #00,#00,#00,#00,#00,#00
L_D10F       DEFB       #02
       DEFB       #02,#07,#02,#08,#09,#00
       DEFB       #03,#05,#07,#00,#00,#00
L_D11C       DEFB       #02
       DEFB       #00,#00,#00,#00,#00,#00
       DEFB       #00,#00,#00,#00,#00,#00
L_D129       DEFB       #02
       DEFB       #00,#02,#07,#02,#06,#00
       DEFB       #00,#03,#05,#05,#08,#00
L_D136       DEFB       #02
       DEFB       #01,#05,#05,#05,#08,#00
       DEFB       #00,#03,#05,#05,#07,#00
L_D143       DEFB       #02
       DEFB       #00,#02,#05,#05,#06,#00
       DEFB       #00,#03,#06,#02,#07,#00
L_D150       DEFB       #02
       DEFB       #00,#02,#05,#05,#06,#00
       DEFB       #01,#05,#05,#05,#08,#00
L_D15D       DEFB       #02
       DEFB       #00,#02,#05,#05,#06,#00
       DEFB       #00,#03,#07,#02,#07,#00
L_D16A       DEFB       #02
       DEFB       #02,#05,#05,#05,#08,#00
       DEFB       #03,#06,#07,#00,#00,#00
L_D177       DEFB       #02
       DEFB       #00,#02,#05,#05,#06,#00
       DEFB       #00,#01,#05,#05,#05,#07
L_D184       DEFB       #02
       DEFB       #01,#05,#05,#05,#08,#00
       DEFB       #00,#03,#05,#05,#08,#00
L_D191       DEFB       #01
       DEFB       #04,#01,#05,#05,#08,#00
L_D198       DEFB       #01
       DEFB       #04,#01,#05,#05,#05,#07
L_D19F       DEFB       #02
       DEFB       #01,#05,#05,#05,#08,#00
       DEFB       #00,#02,#07,#03,#06,#00
L_D1AC       DEFB       #01
       DEFB       #01,#05,#05,#05,#06,#00
L_D1B3       DEFB       #03
       DEFB       #00,#01,#05,#05,#08,#00
       DEFB       #00,#01,#05,#05,#08,#00
       DEFB       #00,#03,#05,#05,#08,#00
L_D1C6       DEFB       #02
       DEFB       #00,#01,#05,#05,#08,#00
       DEFB       #00,#03,#05,#05,#08,#00
L_D1D3       DEFB       #02
       DEFB       #00,#02,#05,#05,#06,#00
       DEFB       #00,#03,#05,#05,#07,#00
L_D1E0       DEFB       #02
       DEFB       #00,#01,#05,#05,#05,#08
       DEFB       #00,#03,#05,#05,#07,#00
L_D1ED       DEFB       #02
       DEFB       #00,#02,#05,#05,#06,#00
       DEFB       #00,#01,#05,#05,#05,#08
L_D1FA       DEFB       #02
       DEFB       #00,#01,#05,#05,#08,#00
       DEFB       #00,#03,#06,#00,#00,#00
L_D207       DEFB       #02
       DEFB       #00,#02,#06,#03,#06,#00
       DEFB       #00,#03,#06,#03,#07,#00
L_D214       DEFB       #02
       DEFB       #02,#05,#05,#05,#06,#00
       DEFB       #00,#07,#00,#02,#07,#00
L_D221       DEFB       #02
       DEFB       #00,#01,#05,#05,#06,#00
       DEFB       #00,#01,#05,#05,#08,#00
L_D22E       DEFB       #02
       DEFB       #00,#03,#05,#05,#06,#00
       DEFB       #00,#02,#05,#05,#07,#00
L_D23B       DEFB       #03
       DEFB       #00,#03,#05,#05,#06,#00
       DEFB       #00,#03,#05,#05,#08,#00
       DEFB       #00,#03,#05,#05,#07,#00
L_D24E       DEFB       #02
       DEFB       #00,#03,#06,#02,#07,#00
       DEFB       #00,#02,#07,#03,#06,#00
L_D25B       DEFB       #02
       DEFB       #00,#03,#05,#05,#06,#00
       DEFB       #00,#02,#05,#05,#05,#07
L_D268       DEFB       #02
       DEFB       #00,#01,#07,#02,#08,#00
       DEFB       #00,#01,#07,#02,#08,#00
       align #100
sinus
		include "sin.asm"

	align #100
fontile
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #00       ; ........
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #00       ; ........
       DEFB       #06       ; .....@@.
       DEFB       #1E       ; ...@@@@.
       DEFB       #3E       ; ..@@@@@.
       DEFB       #7E       ; .@@@@@@.
       DEFB       #7E       ; .@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #00       ; ........
       DEFB       #C0       ; @@......
       DEFB       #F0       ; @@@@....
       DEFB       #F8       ; @@@@@...
       DEFB       #FC       ; @@@@@@..
       DEFB       #FC       ; @@@@@@..
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #00       ; ........
       DEFB       #38       ; ..@@@...
       DEFB       #7C       ; .@@@@@..
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #7C       ; .@@@@@..
       DEFB       #38       ; ..@@@...
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #7E       ; .@@@@@@.
       DEFB       #7E       ; .@@@@@@.
       DEFB       #3E       ; ..@@@@@.
       DEFB       #1E       ; ...@@@@.
       DEFB       #06       ; .....@@.
       DEFB       #00       ; ........
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FC       ; @@@@@@..
       DEFB       #FC       ; @@@@@@..
       DEFB       #F8       ; @@@@@...
       DEFB       #F0       ; @@@@....
       DEFB       #C0       ; @@......
       DEFB       #00       ; ........
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #00       ; ........
       DEFB       #38       ; ..@@@...
       DEFB       #7C       ; .@@@@@..
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #FE       ; @@@@@@@.
       DEFB       #7C       ; .@@@@@..
       DEFB       #38       ; ..@@@...
       DEFB       #00       ; ........

		align #100
tablerONE
		ds 128*2

		align #100
sprite1tab
		ds 64
		align #100
sprite2tab
		ds 64

sprite1
		ds #300
sprite2
		ds #300
sprite
		incbin "sprite.bin"
pluer
	include "pt3xplayer.asm"
muzzzza
	incbin "nq-q-bone-10.pt3"


changer
	ld a,0
	or a
	jr nz,chachange
.l0	ld a,0
	inc a
	ld (.l0+1),a
	or a
	ret nz
.l1	ld a,0
	ld h,0
	ld l,a
	add hl,hl ;2
	push hl
	add hl,hl ;4
	pop de
	add hl,de ;6
	ld de,chtoto
	add hl,de
;	ex de,hl
	ld de,chachange.lll0
	ldi
	ldi
	ldi
	ld de,chachange.lll1
	ldi
	ldi
	ldi
	ld a,(.l1+1)
	inc a
	cp 14
	jr nz,.l2
	xor a
.l2	ld (.l1+1),a
	ld a,1
	ld (changer+1),a
	ret
chachange:
	ld a,0
	ld h,0
	ld l,a
	add hl,hl ;2
	add hl,hl ;4
	add hl,hl ;8
	push hl
	add hl,hl ;16
	pop de
	add hl,de ;24
	push hl
	push hl
	ld de,sprite
	add hl,de
	ld (.hhh0+1),hl
	ld (.hhh1+1),hl
	pop hl
	ld de,sprite1
	add hl,de
	ld (.ddd0+1),hl
	pop hl
	ld de,sprite2
	add hl,de
	ld (.ddd1+1),hl
.hhh0
	ld hl,#babe
.ddd0
	ld de,#face
	ld bc,#18
.rrr0
	ld a,(hl)
.lll0
	nop
	nop:nop
	ld (de),a
	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jr nz,.rrr0

.hhh1
	ld hl,#dead
.ddd1
	ld de,#face
	ld bc,#18
.rrr1
	ld a,(hl)
.lll1
	nop
	nop:nop
	ld (de),a
	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jr nz,.rrr1
	ld a,(chachange+1)
	inc a
	and 31
	ld (chachange+1),a
	or a
	ret nz
	xor a
	ld (changer+1),a
	ret

chtoto
	nop:nop:nop:nop:nop:nop
	cpl:or %00100010:cpl:or %10001000
	nop:nop:nop:nop:and %10001000
	cpl:and %00100010:cpl:or %10001000
	nop:nop:nop:nop:or %10001000
	cpl:and %00100010:cpl:and %10001000
	nop:nop:nop:cpl:and %10001000
	cpl:nop:nop:cpl:or %10001000
	nop:and %00100010:nop:and %10001000
	cpl:nop:nop:cpl:and %10001000
	nop:and %00100010:nop:or %10001000
	cpl:nop:nop:cpl:nop:nop
	nop:and %00100010:cpl:nop:nop
	nop:or %00100010:nop:or %10001000

kartinko
	incbin "sshot000007.scr"

	display $-SnaStart
	savesna "cafe2019.sna",SnaStart
	savebin "c2019inv.C",SnaStart,$-SnaStart
