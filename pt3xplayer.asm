  module p1
;universal pt2 and pt3 player for zx spectrum and msx
;(c)2004-2005 s.v.bulba <vorobey@mail.khstu.ru>
;http://bulba.at.kz
 
;release number
release equ "0"
 
;conditional assembly
;1) version of rout (zx or msx standards)
zx equ 1
msx equ 0
;2) current position counter at (start+11)
curposcounter equ 1
;3) allow channels allocation bits at (start+10)
acbbac equ 1
;4) allow loop checking and disabling
loopchecker equ 1
;5) insert official identificator
id equ 1
 
;features
;--------
;-can be compiled at any address (i.e. no need rounding org
; address).
;-variables (vars) can be located at any address (not only after
;code block).
;-init subprogram checks pt3-module version and rightly
; generates both note and volume tables outside of code block
; (in vars).
;-two portamento (spc. command 3xxx) algorithms (depending of
; pt3 module version).
;-any tempo value are accepted (including tempo=1 and tempo=2).
;-fully compatible with ay_emul pt3 and pt2 players codes.
;-see also notes at the end of this source code.
 
;limitations
;-----------
;-can run in ram only (self-modified code is used).
;-pt2 position list must be end by #ff marker only.
 
;warning!!! play subprogram can crash if no module are loaded
;into ram or init subprogram was not called before.
 
;call mute or init one more time to mute sound after stopping
;playing
 
 
;test codes (commented)
;       ld a,2 ;pt2,abc,looped
;       ld (start+10),a
;       call start
;       ei
;_lp    halt
;       call start+5
;       xor a
;       in a,(#fe)
;       cpl
;       and 15
;       jr z,_lp
 
tona    equ 0
tonb    equ 2
tonc    equ 4
noise   equ 6
mixer   equ 7
ampla   equ 8
amplb   equ 9
amplc   equ 10
env     equ 11
envtp   equ 13
 
;       struct  chp
;reset group
chp_psinor      equ 0
chp_psinsm      equ 1
chp_cramsl      equ 2
chp_crnssl      equ 3
chp_crensl      equ 4
chp_tslcnt      equ 5
chp_crtnsl      equ 6
chp_tnacc       equ 8
chp_conoff      equ 10
;reset group
 
chp_onoffd      equ 11
 
;ix for ptdecod here (+12)
chp_offond      equ 12
chp_ornptr      equ 13
chp_samptr      equ 15
chp_nntskp      equ 17
chp_note        equ 18
chp_sltont      equ 19
chp_env_en      equ 20
chp_flags       equ 21
 ;enabled - 0,simplegliss - 2
chp_tnsldl      equ 22
chp_tslstp      equ 23
chp_tndelt      equ 25
chp_ntskcn      equ 27
chp_volume      equ 28
;       ends
chp     equ 29
 
;entry and other points
;start initialize playing of module at mdladdr
;start+3 initialization with module address in hl
;start+5 play one quark
;start+8 mute
;start+10 setup and status flags
;start+11 current position value (byte) (optional)
;first 12 values of tone tables (packed)
 
@p1
start
        ld hl,0	; modstart
        jr init
        jp play
        jr mute
setup   db 0 ;set bit0, if you want to play without looping
             ;(optional);
             ;set bit1 for pt2 and reset for pt3 before
             ;calling init;
             ;bits2-3: %00-abc, %01 acb, %10 bac (optional);
             ;bits4-6 are not used
             ;bit7 is set each time, when loop point is passed
             ;(optional)
        if curposcounter
curpos  db 0 ;for visualization only (i.e. no need for playing)
        endif
 
;identifier
        if id
        db "=uni pt2 and pt3 player r.",release,"="
        endif
 
        if loopchecker
checklp ld hl,setup
        set 7,(hl)
        bit 0,(hl)
        ret z
        pop hl
        ld hl,delycnt
        inc (hl)
        ld hl,chana+chp_ntskcn
        inc (hl)
        endif
 
mute    xor a
        ld h,a
        ld l,a
        ld (ayregs+ampla),a
        ld (ayregs+amplb),hl
        jp rout
 
init
;hl - addressofmodule
        ld a,(start+10)
        and 2
        jr nz,initpt2
 
        call setmdad
        push hl
        ld de,100
        add hl,de
        ld a,(hl)
        ld (delay),a
        push hl
        pop ix
        add hl,de
        ld (crpsptr),hl
        ld e,(ix+102-100)
        inc hl
 
        if curposcounter
        ld a,l
        ld (possub+1),a
        endif
 
        add hl,de
        ld (lposptr),hl
        pop de
        ld l,(ix+103-100)
        ld h,(ix+104-100)
        add hl,de
        ld (patsptr),hl
        ld hl,169
        add hl,de
        ld (ornptrs),hl
        ld hl,105
        add hl,de
        ld (samptrs),hl
        ld a,(ix+13-100) ;extract version number
        sub #30
        jr c,l20
        cp 10
        jr c,l21
l20     ld a,6
l21     ld (version),a
        push af ;voltable version
        cp 4
        ld a,(ix+99-100) ;tone table number
        rla
        and 7
        push af ;notetable number
       ld hl,#1f18 ;(e_-samcnv-2)*256+#18
        ld (samcnv),hl
        ld a,#ba
        ld (orncp),a
        ld (samcp),a
        ld a,#7b
        ld (ornld),a
        ld (samld),a
        ld a,#87
        ld (samclc2),a
        ld bc,pt3pd
        ld hl,0
        ld de,pt3emptyorn
        jr initcommon
 
initpt2 ld a,(hl)
        ld (delay),a
        push hl
        push hl
        push hl
        inc hl
        inc hl
        ld a,(hl)
        inc hl
        ld (samptrs),hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        pop hl
        and a
        sbc hl,de
        call setmdad
        pop hl
        ld de,67
        add hl,de
        ld (ornptrs),hl
        ld e,32
        add hl,de
        ld c,(hl)
        inc hl
        ld b,(hl)
        ld e,30
        add hl,de
        ld (crpsptr),hl
        ld e,a
        inc hl
 
        if curposcounter
        ld a,l
        ld (possub+1),a
        endif
 
        add hl,de
        ld (lposptr),hl
        pop hl
        add hl,bc
        ld (patsptr),hl
        ld a,5
        ld (version),a
        push af
        ld a,2
        push af
        ld hl,#51cb
        ld (samcnv),hl
        ld a,#bb
        ld (orncp),a
        ld (samcp),a
        ld a,#7a
        ld (ornld),a
        ld (samld),a
        ld a,#80
        ld (samclc2),a
        ld bc,pt2pd
        ld hl,#8687
        ld de,pt2emptyorn
 
initcommon
 
        ld (ptdecod+1),bc
        ld (pscalc),hl
        push de
 
;note table data depacker
;(c) ivan roshin
        ld de,t_pack
        ld bc,t1_+(2*49)-1
tp_0    ld a,(de)
        inc de
        cp 15*2
        jr nc,tp_1
        ld h,a
        ld a,(de)
        ld l,a
        inc de
        jr tp_2
tp_1    push de
        ld d,0
        ld e,a
        add hl,de
        add hl,de
        pop de
tp_2    ld a,h
        ld (bc),a
        dec bc
        ld a,l
        ld (bc),a
        dec bc
        sub low (#f8*2)
        jr nz,tp_0
 
        ld hl,setup
        res 7,(hl)
 
        if curposcounter
        inc hl
        ld (hl),a
        endif
 
        ld hl,vars
        ld (hl),a
        ld de,vars+1
        ld bc,var0end-vars-1
        ldir
        ld (adinpta),hl ;ptr to zero
        inc a
        ld (delycnt),a
        ld hl,#f001 ;h - chp_volume, l - chp_ntskcn
        ld (chana+chp_ntskcn),hl
        ld (chanb+chp_ntskcn),hl
        ld (chanc+chp_ntskcn),hl
        pop hl
        ld (chana+chp_ornptr),hl
        ld (chanb+chp_ornptr),hl
        ld (chanc+chp_ornptr),hl
 
        pop af
 
;notetablecreator (c) ivan roshin
;a - notetablenumber*2+versionfornotetable
;(xx1b - 3.xx..3.4r, xx0b - 3.4x..3.6x..vtii1.0)
 
        ld hl,nt_data
        push de
        ld d,b
        add a,a
        ld e,a
        add hl,de
        ld e,(hl)
        inc hl
        srl e
        sbc a,a
        and #a7 ;#00 (nop) or #a7 (and a)
        ld (l3),a
        ex de,hl
        pop bc ;bc=t1_
        add hl,bc
 
        ld a,(de)
        add a,low (t_)
        ld c,a
        adc a,t_/256
        sub c
        ld b,a
        push bc
        ld de,nt_
        push de
 
        ld b,12
l1      push bc
        ld c,(hl)
        inc hl
        push hl
        ld b,(hl)
 
        push de
        ex de,hl
        ld de,23
        ld xh,8
 
l2      srl b
        rr c
l3      db #19  ;and a or nop
        ld a,c
        adc a,d ;=adc 0
        ld (hl),a
        inc hl
        ld a,b
        adc a,d
        ld (hl),a
        add hl,de
        dec xh
        jr nz,l2
 
        pop de
        inc de
        inc de
        pop hl
        inc hl
        pop bc
        djnz l1
 
        pop hl
        pop de
 
        ld a,e
        cp low (tcold_1)
        jr nz,corr_1
        ld a,#fd
        ld (nt_+#2e),a
 
corr_1  ld a,(de)
        and a
        jr z,tc_exit
        rra
        push af
        add a,a
        ld c,a
        add hl,bc
        pop af
        jr nc,corr_2
        dec (hl)
        dec (hl)
corr_2  inc (hl)
        and a
        sbc hl,bc
        inc de
        jr corr_1
 
tc_exit
 
        pop af
 
;voltablecreator (c) ivan roshin
;a - versionforvolumetable (0..4 - 3.xx..3.4x;
                           ;5.. - 2.x,3.5x..3.6x..vtii1.0)
 
        cp 5
        ld hl,#11
        ld d,h
        ld e,h
        ld a,#17
        jr nc,m1
        dec l
        ld e,l
        xor a
m1      ld (m2),a
 
        ld ix,vt_+16
 
        ld c,#f
initv2  push hl
 
        add hl,de
        ex de,hl
        sbc hl,hl
 
        ld b,#10
initv1  ld a,l
m2      db #7d
        ld a,h
        adc a,0
        ld (ix),a
        inc ix
        add hl,de
        djnz initv1
 
        pop hl
        ld a,e
        cp #77
        jr nz,m3
        inc e
m3      dec c
        jr nz,initv2
 
        jp rout
 
setmdad ld (modaddr),hl
        ld (mdaddr1),hl
        ld (mdaddr2),hl
        ret
 
ptdecod jp #c3c3
 
;pt2 pattern decoder
pd2_sam call setsam
        jr pd2_loop
 
pd2_eoff ld (ix-12+chp_env_en),a
        jr pd2_loop
 
pd2_env ld (ix-12+chp_env_en),16
        ld (ayregs+envtp),a
        ld a,(bc)
        inc bc
        ld l,a
        ld a,(bc)
        inc bc
        ld h,a
        ld (envbase),hl
        jr pd2_loop
 
pd2_orn call setorn
        jr pd2_loop
 
pd2_skip inc a
        ld (ix-12+chp_nntskp),a
        jr pd2_loop
 
pd2_vol rrca
        rrca
        rrca
        rrca
        ld (ix-12+chp_volume),a
        jr pd2_loop
 
pd2_del call c_delay
        jr pd2_loop
 
pd2_glis set 2,(ix-12+chp_flags)
        inc a
        ld (ix-12+chp_tnsldl),a
        ld (ix-12+chp_tslcnt),a
        ld a,(bc)
        inc bc
        ld (ix-12+chp_tslstp),a
        add a,a
        sbc a,a
        ld (ix-12+chp_tslstp+1),a
        scf
        jr pd2_lp2
 
pt2pd   and a
 
pd2_lp2 ex af,af'
 
pd2_loop ld a,(bc)
        inc bc
        add a,#20
        jr z,pd2_rel
        jr c,pd2_sam
        add a,96
        jr c,pd2_note
        inc a
        jr z,pd2_eoff
        add a,15
        jp z,pd_fin
        jr c,pd2_env
        add a,#10
        jr c,pd2_orn
        add a,#40
        jr c,pd2_skip
        add a,#10
        jr c,pd2_vol
        inc a
        jr z,pd2_del
        inc a
        jr z,pd2_glis
        inc a
        jr z,pd2_port
        inc a
        jr z,pd2_stop
        ld a,(bc)
        inc bc
        ld (ix-12+chp_crnssl),a
        jr pd2_loop
 
pd2_port res 2,(ix-12+chp_flags)
        ld a,(bc)
        inc bc
        inc bc ;ignoring precalc delta to right sound
        inc bc
        scf
        jr pd2_lp2
 
pd2_stop ld (ix-12+chp_tslcnt),a
        jr pd2_loop
 
pd2_rel ld (ix-12+chp_flags),a
        jr pd2_exit
 
pd2_note ld l,a
        ld a,(ix-12+chp_note)
        ld (prnote+1),a
        ld (ix-12+chp_note),l
        xor a
        ld (ix-12+chp_tslcnt),a
        set 0,(ix-12+chp_flags)
        ex af,af'
        jr nc,noglis2
        bit 2,(ix-12+chp_flags)
        jr nz,noport2
        ld (lostep),a
        add a,a
        sbc a,a
        ex af,af'
        ld h,a
        ld l,a
        inc a
        call setport
noport2 ld (ix-12+chp_tslcnt),1
noglis2 xor a
 
 
pd2_exit ld (ix-12+chp_psinsm),a
        ld (ix-12+chp_psinor),a
        ld (ix-12+chp_crtnsl),a
        ld (ix-12+chp_crtnsl+1),a
        jp pd_fin
 
;pt3 pattern decoder
pd_orsm ld (ix-12+chp_env_en),0
        call setorn
pd_sam_ ld a,(bc)
        inc bc
        rrca
 
pd_sam  call setsam
        jr pd_loop
 
pd_vol  rrca
        rrca
        rrca
        rrca
        ld (ix-12+chp_volume),a
        jr pd_lp2
 
pd_eoff ld (ix-12+chp_env_en),a
        ld (ix-12+chp_psinor),a
        jr pd_lp2
 
pd_sore dec a
        jr nz,pd_env
        ld a,(bc)
        inc bc
        ld (ix-12+chp_nntskp),a
        jr pd_lp2
 
pd_env  call setenv
        jr pd_lp2
 
pd_orn  call setorn
        jr pd_loop
 
pd_esam ld (ix-12+chp_env_en),a
        ld (ix-12+chp_psinor),a
        call nz,setenv
        jr pd_sam_
 
pt3pd   ld a,(ix-12+chp_note)
        ld (prnote+1),a
        ld l,(ix-12+chp_crtnsl)
        ld h,(ix-12+chp_crtnsl+1)
        ld (prslide+1),hl
 
pd_loop ld de,#2010
pd_lp2  ld a,(bc)
        inc bc
        add a,e
        jr c,pd_orsm
        add a,d
        jr z,pd_fin
        jr c,pd_sam
        add a,e
        jr z,pd_rel
        jr c,pd_vol
        add a,e
        jr z,pd_eoff
        jr c,pd_sore
        add a,96
        jr c,pd_note
        add a,e
        jr c,pd_orn
        add a,d
        jr c,pd_nois
        add a,e
        jr c,pd_esam
        add a,a
        ld e,a
        ld hl,high (spccoms-#2000+#ff20)*256+low (spccoms-#2000+#ff20)
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        push de
        jr pd_loop
 
pd_nois ld (ns_base),a
        jr pd_lp2
 
pd_rel  res 0,(ix-12+chp_flags)
        jr pd_res
 
pd_note ld (ix-12+chp_note),a
        set 0,(ix-12+chp_flags)
        xor a
 
pd_res  ld (pdsp_+1),sp
        ld sp,ix
        ld h,a
        ld l,a
        push hl
        push hl
        push hl
        push hl
        push hl
        push hl
pdsp_   ld sp,#3131
 
pd_fin  ld a,(ix-12+chp_nntskp)
        ld (ix-12+chp_ntskcn),a
        ret
 
c_portm ld a,(bc)
        inc bc
;skip precalculated tone delta (because
;cannot be right after pt3 compilation)
        inc bc
        inc bc
        ex af,af'
        ld a,(bc) ;signed tone step
        inc bc
        ld (lostep),a
        ld a,(bc)
        inc bc
        and a
        ex af,af'
        ld l,(ix-12+chp_crtnsl)
        ld h,(ix-12+chp_crtnsl+1)
 
;set portamento variables
;a - delay; a' - hi(step); zf' - (a'=0); hl - crtnsl
 
setport res 2,(ix-12+chp_flags)
        ld (ix-12+chp_tnsldl),a
        ld (ix-12+chp_tslcnt),a
        push hl
        ld de,nt_
        ld a,(ix-12+chp_note)
        ld (ix-12+chp_sltont),a
        add a,a
        ld l,a
        ld h,0
        add hl,de
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        push hl
prnote  ld a,#3e
        ld (ix-12+chp_note),a
        add a,a
        ld l,a
        ld h,0
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        pop hl
        sbc hl,de
        ld (ix-12+chp_tndelt),l
        ld (ix-12+chp_tndelt+1),h
        pop de
version equ $+1
        ld a,#3e
        cp 6
        jr c,oldprtm ;old 3xxx for pt v3.5-
prslide ld de,#1111
        ld (ix-12+chp_crtnsl),e
        ld (ix-12+chp_crtnsl+1),d
lostep  equ $+1
oldprtm ld a,#3e
        ex af,af'
        jr z,nosig
        ex de,hl
nosig   sbc hl,de
        jp p,set_stp
        cpl
        ex af,af'
        neg
        ex af,af'
set_stp ld (ix-12+chp_tslstp+1),a
        ex af,af'
        ld (ix-12+chp_tslstp),a
        ld (ix-12+chp_conoff),0
        ret
 
c_gliss set 2,(ix-12+chp_flags)
        ld a,(bc)
        inc bc
        ld (ix-12+chp_tnsldl),a
        ld (ix-12+chp_tslcnt),a
        ld a,(bc)
        inc bc
        ex af,af'
        ld a,(bc)
        inc bc
        jr set_stp
 
c_smpos ld a,(bc)
        inc bc
        ld (ix-12+chp_psinsm),a
        ret
 
c_orpos ld a,(bc)
        inc bc
        ld (ix-12+chp_psinor),a
        ret
 
c_vibrt ld a,(bc)
        inc bc
        ld (ix-12+chp_onoffd),a
        ld (ix-12+chp_conoff),a
        ld a,(bc)
        inc bc
        ld (ix-12+chp_offond),a
        xor a
        ld (ix-12+chp_tslcnt),a
        ld (ix-12+chp_crtnsl),a
        ld (ix-12+chp_crtnsl+1),a
        ret
 
c_engls ld a,(bc)
        inc bc
        ld (env_del),a
        ld (curedel),a
        ld a,(bc)
        inc bc
        ld l,a
        ld a,(bc)
        inc bc
        ld h,a
        ld (esldadd),hl
        ret
 
c_delay ld a,(bc)
        inc bc
        ld (delay),a
        ret
 
setenv  ld (ix-12+chp_env_en),e
        ld (ayregs+envtp),a
        ld a,(bc)
        inc bc
        ld h,a
        ld a,(bc)
        inc bc
        ld l,a
        ld (envbase),hl
        xor a
        ld (ix-12+chp_psinor),a
        ld (curedel),a
        ld h,a
        ld l,a
        ld (curesld),hl
c_nop   ret
 
setorn  add a,a
        ld e,a
        ld d,0
        ld (ix-12+chp_psinor),d
ornptrs equ $+1
        ld hl,#2121
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
mdaddr2 equ $+1
        ld hl,#2121
        add hl,de
        ld (ix-12+chp_ornptr),l
        ld (ix-12+chp_ornptr+1),h
        ret
 
setsam  add a,a
        ld e,a
        ld d,0
samptrs equ $+1
        ld hl,#2121
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
mdaddr1 equ $+1
        ld hl,#2121
        add hl,de
        ld (ix-12+chp_samptr),l
        ld (ix-12+chp_samptr+1),h
        ret
 
;all 16 addresses to protect from broken pt3 modules
spccoms dw c_nop
        dw c_gliss
        dw c_portm
        dw c_smpos
        dw c_orpos
        dw c_vibrt
        dw c_nop
        dw c_nop
        dw c_engls
        dw c_delay
        dw c_nop
        dw c_nop
        dw c_nop
        dw c_nop
        dw c_nop
        dw c_nop
 
chregs  xor a
        ld (ampl),a
        bit 0,(ix+chp_flags)
        push hl
        jp z,ch_exit
        ld (csp_+1),sp
        ld l,(ix+chp_ornptr)
        ld h,(ix+chp_ornptr+1)
        ld sp,hl
        pop de
        ld h,a
        ld a,(ix+chp_psinor)
        ld l,a
        add hl,sp
        inc a
                ;pt2    pt3
orncp   inc a   ;cp e   cp d
        jr c,ch_orps
ornld   db 1    ;ld a,d ld a,e
ch_orps ld (ix+chp_psinor),a
        ld a,(ix+chp_note)
        add a,(hl)
        jp p,ch_ntp
        xor a
ch_ntp  cp 96
        jr c,ch_nok
        ld a,95
ch_nok  add a,a
        ex af,af'
        ld l,(ix+chp_samptr)
        ld h,(ix+chp_samptr+1)
        ld sp,hl
        pop de
        ld h,0
        ld a,(ix+chp_psinsm)
        ld b,a
        add a,a
samclc2 add a,a ;or add a,b for pt2
        ld l,a
        add hl,sp
        ld sp,hl
        ld a,b
        inc a
                ;pt2    pt3
samcp   inc a   ;cp e   cp d
        jr c,ch_smps
samld   db 1    ;ld a,d ld a,e
ch_smps ld (ix+chp_psinsm),a
        pop bc
        pop hl
 
;convert pt2 sample to pt3
                ;pt2            pt3
samcnv  pop hl  ;bit 2,c        jr e_
        pop hl
        ld h,b
        jr nz,$+8
        ex de,hl
        and a
        sbc hl,hl
        sbc hl,de
        ld d,c
        rr c
        sbc a,a
        cpl
        and #3e
        rr c
        rr b
        and c
        ld c,a
        ld a,b
        rra
        rra
        rr d
        rra
        and #9f
        ld b,a
 
e_      ld e,(ix+chp_tnacc)
        ld d,(ix+chp_tnacc+1)
        add hl,de
        bit 6,b
        jr z,ch_noac
        ld (ix+chp_tnacc),l
        ld (ix+chp_tnacc+1),h
ch_noac ex de,hl
        ex af,af'
        add a,low (nt_)
        ld l,a
        adc a,nt_/256
        sub l
        ld h,a
        ld sp,hl
        pop hl
        add hl,de
        ld e,(ix+chp_crtnsl)
        ld d,(ix+chp_crtnsl+1)
        add hl,de
csp_    ld sp,#3131
        ex (sp),hl
        xor a
        or (ix+chp_tslcnt)
        jr z,ch_amp
        dec (ix+chp_tslcnt)
        jr nz,ch_amp
        ld a,(ix+chp_tnsldl)
        ld (ix+chp_tslcnt),a
        ld l,(ix+chp_tslstp)
        ld h,(ix+chp_tslstp+1)
        ld a,h
        add hl,de
        ld (ix+chp_crtnsl),l
        ld (ix+chp_crtnsl+1),h
        bit 2,(ix+chp_flags)
        jr nz,ch_amp
        ld e,(ix+chp_tndelt)
        ld d,(ix+chp_tndelt+1)
        and a
        jr z,ch_stpp
        ex de,hl
ch_stpp sbc hl,de
        jp m,ch_amp
        ld a,(ix+chp_sltont)
        ld (ix+chp_note),a
        xor a
        ld (ix+chp_tslcnt),a
        ld (ix+chp_crtnsl),a
        ld (ix+chp_crtnsl+1),a
ch_amp  ld a,(ix+chp_cramsl)
        bit 7,c
        jr z,ch_noam
        bit 6,c
        jr z,ch_amin
        cp 15
        jr z,ch_noam
        inc a
        jr ch_svam
ch_amin cp -15
        jr z,ch_noam
        dec a
ch_svam ld (ix+chp_cramsl),a
ch_noam ld l,a
        ld a,b
        and 15
        add a,l
        jp p,ch_apos
        xor a
ch_apos cp 16
        jr c,ch_vol
        ld a,15
ch_vol  or (ix+chp_volume)
        add a,low (vt_)
        ld l,a
        adc a,vt_/256
        sub l
        ld h,a
        ld a,(hl)
ch_env  bit 0,c
        jr nz,ch_noen
        or (ix+chp_env_en)
ch_noen ld (ampl),a
        bit 7,b
        ld a,c
        jr z,no_ensl
        rla
        rla
        sra a
        sra a
        sra a
        add a,(ix+chp_crensl) ;see comment below
        bit 5,b
        jr z,no_enac
        ld (ix+chp_crensl),a
no_enac ld hl,addtoen
        add a,(hl) ;bug in pt3 - need word here
        ld (hl),a
        jr ch_mix
no_ensl rra
        add a,(ix+chp_crnssl)
        ld (addtons),a
        bit 5,b
        jr z,ch_mix
        ld (ix+chp_crnssl),a
ch_mix  ld a,b
        rra
        and #48
ch_exit ld hl,ayregs+mixer
        or (hl)
        rrca
        ld (hl),a
        pop hl
        xor a
        or (ix+chp_conoff)
        ret z
        dec (ix+chp_conoff)
        ret nz
        xor (ix+chp_flags)
        ld (ix+chp_flags),a
        rra
        ld a,(ix+chp_onoffd)
        jr c,ch_ondl
        ld a,(ix+chp_offond)
ch_ondl ld (ix+chp_conoff),a
        ret
 
play    xor a
        ld (addtoen),a
        ld (ayregs+mixer),a
        dec a
        ld (ayregs+envtp),a
        ld hl,delycnt
        dec (hl)
        jp nz,pl2
        ld hl,chana+chp_ntskcn
        dec (hl)
        jr nz,pl1b
adinpta equ $+1
        ld bc,#0101
        ld a,(bc)
        and a
        jr nz,pl1a
jumptonextpos
        ld d,a
        ld (ns_base),a
crpsptr equ $+1
        ld hl,#2121
        inc hl
        ld a,(hl)
        inc a
        jr nz,plnlp
 
        if loopchecker
        call checklp
        endif
 
lposptr equ $+1
        ld hl,#2121
        ld a,(hl)
        inc a
plnlp   ld (crpsptr),hl
        dec a
                ;pt2            pt3
pscalc  dec a   ;add a,a        nop
        dec a   ;add a,(hl)     nop
        add a,a
        ld e,a
        rl d
 
        if curposcounter
        ld a,l
possub  sub #d6
        ld (curpos),a
        endif
 
patsptr equ $+1
        ld hl,#2121
        add hl,de
modaddr equ $+1
        ld de,#1111
        ld (psp_+1),sp
        ld sp,hl
        pop hl
        add hl,de
        ld b,h
        ld c,l
        pop hl
        add hl,de
        ld (adinptb),hl
        pop hl
        add hl,de
        ld (adinptc),hl
psp_    ld sp,#3131
pl1a    ld ix,chana+12
        call ptdecod
        ld (adinpta),bc
 
pl1b    ld hl,chanb+chp_ntskcn
        dec (hl)
        jr nz,pl1c
        ld ix,chanb+12
adinptb equ $+1
        ld bc,#0101
        call ptdecod
        ld (adinptb),bc
 
pl1c    ld hl,chanc+chp_ntskcn
        dec (hl)
        jr nz,pl1d
        ld ix,chanc+12
adinptc equ $+1
        ld bc,#0101
        call ptdecod
        ld (adinptc),bc
 
delay   equ $+1
pl1d    ld a,#3e
        ld (delycnt),a
 
pl2     ld ix,chana
        ld hl,(ayregs+tona)
        call chregs
        ld (ayregs+tona),hl
        ld a,(ampl)
        ld (ayregs+ampla),a
        ld ix,chanb
        ld hl,(ayregs+tonb)
        call chregs
        ld (ayregs+tonb),hl
        ld a,(ampl)
        ld (ayregs+amplb),a
        ld ix,chanc
        ld hl,(ayregs+tonc)
        call chregs
        ld (ayregs+tonc),hl
 
        ld hl,(ns_base_addtons)
        ld a,h
        add a,l
        ld (ayregs+noise),a
 
addtoen equ $+1
        ld a,#3e
        ld e,a
        add a,a
        sbc a,a
        ld d,a
        ld hl,(envbase)
        add hl,de
        ld de,(curesld)
        add hl,de
        ld (ayregs+env),hl
 
        xor a
        ld hl,curedel
        or (hl)
        jr z,rout
        dec (hl)
        jr nz,rout
env_del equ $+1
        ld a,#3e
        ld (hl),a
esldadd equ $+1
        ld hl,#2121
        add hl,de
        ld (curesld),hl
 
rout
        if acbbac
        ld a,(setup)
        and 12
        jr z,abc
        add a,low chtable
        ld e,a
        adc a,chtable/256
        sub e
        ld d,a
        ld b,0
        ld ix,ayregs
        ld hl,ayregs
        ld a,(de)
        inc de
        ld c,a
        add hl,bc
        ld a,(ix+tonb)
        ld c,(hl)
        ld (ix+tonb),c
        ld (hl),a
        inc hl
        ld a,(ix+tonb+1)
        ld c,(hl)
        ld (ix+tonb+1),c
        ld (hl),a
        ld a,(de)
        inc de
        ld c,a
        add hl,bc
        ld a,(ix+amplb)
        ld c,(hl)
        ld (ix+amplb),c
        ld (hl),a
        ld a,(de)
        inc de
        ld (rxca1),a
        xor 8
        ld (rxca2),a
        ld hl,ayregs+mixer
        ld a,(de)
        and (hl)
        ld e,a
        ld a,(hl)
rxca1   ld a,(hl)
        and %010010
        or e
        ld e,a
        ld a,(hl)
        and %010010
rxca2   or e
        or e
        ld (hl),a
abc
        endif
        if zx
noout	ld hl,ayregs
oregz	xor a
        ld de,#ffbf
        ld bc,#fffd
lout    out (c),a
        ld b,e
        outi
        ld b,d
        inc a
        cp 13
        jr nz,lout
        out (c),a
        ld a,(hl)
        and a
        ret m
        ld b,e
        out (c),a
        ret
        endif
        if msx
;msx version of rout (c)dioniso
        xor a
        ld c,#a0
        ld hl,ayregs
lout    out (c),a
        inc c
        outi
        dec c
        inc a
        cp 13
        jr nz,lout
        out (c),a
        ld a,(hl)
        and a
        ret m
        inc c
        out (c),a
        ret
        endif
 
        if acbbac
chtable equ $-4
        db 4,5,15,%001001,0,7,7,%100100
        endif
 
nt_data db (t_new_0-t1_)*2
        db tcnew_0-t_
        db (t_old_0-t1_)*2+1
        db tcold_0-t_
        db (t_new_1-t1_)*2+1
        db tcnew_1-t_
        db (t_old_1-t1_)*2+1
        db tcold_1-t_
        db (t_new_2-t1_)*2
        db tcnew_2-t_
        db (t_old_2-t1_)*2
        db tcold_2-t_
        db (t_new_3-t1_)*2
        db tcnew_3-t_
        db (t_old_3-t1_)*2
        db tcold_3-t_
 
t_
 
tcold_0 db #00+1,#04+1,#08+1,#0a+1,#0c+1,#0e+1,#12+1,#14+1
        db #18+1,#24+1,#3c+1,0
tcold_1 db #5c+1,0
tcold_2 db #30+1,#36+1,#4c+1,#52+1,#5e+1,#70+1,#82,#8c,#9c
        db #9e,#a0,#a6,#a8,#aa,#ac,#ae,#ae,0
tcnew_3 db #56+1
tcold_3 db #1e+1,#22+1,#24+1,#28+1,#2c+1,#2e+1,#32+1,#be+1,0
tcnew_0 db #1c+1,#20+1,#22+1,#26+1,#2a+1,#2c+1,#30+1,#54+1
        db #bc+1,#be+1,0
tcnew_1 equ tcold_1
tcnew_2 db #1a+1,#20+1,#24+1,#28+1,#2a+1,#3a+1,#4c+1,#5e+1
        db #ba+1,#bc+1,#be+1,0
 
pt3emptyorn equ $-1
        db 1,0
t_pack  db low (#06ec*2/256),low (#06ec*2)
        db #0755-#06ec
        db #07c5-#0755
        db #083b-#07c5
        db #08b8-#083b
        db #093d-#08b8
        db #09ca-#093d
        db #0a5f-#09ca
        db #0afc-#0a5f
        db #0ba4-#0afc
        db #0c55-#0ba4
        db #0d10-#0c55
        db #066d*2/256,low (#066d*2)
        db #06cf-#066d
        db #0737-#06cf
        db #07a4-#0737
        db #0819-#07a4
        db #0894-#0819
        db #0917-#0894
        db #09a1-#0917
        db #0a33-#09a1
        db #0acf-#0a33
        db #0b73-#0acf
        db #0c22-#0b73
        db #0cda-#0c22
        db #0704*2/256,low (#0704*2)
        db #076e-#0704
        db #07e0-#076e
        db #0858-#07e0
        db #08d6-#0858
        db #095c-#08d6
        db #09ec-#095c
        db #0a82-#09ec
        db #0b22-#0a82
        db #0bcc-#0b22
        db #0c80-#0bcc
        db #0d3e-#0c80
        db #07e0*2/256,low (#07e0*2)
        db #0858-#07e0
        db #08e0-#0858
        db #0960-#08e0
        db #09f0-#0960
        db #0a88-#09f0
        db #0b28-#0a88
        db #0bd8-#0b28
        db #0c80-#0bd8
        db #0d60-#0c80
        db #0e10-#0d60
        db #0ef8-#0e10
 
;vars from here can be stripped
;you can move vars to any other address
 
vars
 
;channelsvars
chana   ds chp
chanb   ds chp
chanc   ds chp
 
;globalvars
delycnt db 0
curesld dw 0
curedel db 0
ns_base_addtons
ns_base db 0
addtons db 0
 
ayregs
 
vt_     ds 256 ;createdvolumetableaddress
 
envbase equ vt_+14
 
t1_     equ vt_+16 ;tone tables data depacked here
 
t_old_1 equ t1_
t_old_2 equ t_old_1+24
t_old_3 equ t_old_2+24
t_old_0 equ t_old_3+2
t_new_0 equ t_old_0
t_new_1 equ t_old_1
t_new_2 equ t_new_0+24
t_new_3 equ t_old_3
 
pt2emptyorn equ vt_+31 ;1,0,0 sequence
 
	display "nt_ = ",nt_

nt_     ds 192 ;creatednotetableaddress
 
;local var
ampl    equ ayregs+amplc
 
var0end equ vt_+16 ;init zeroes from vars to var0end-1
 
varsend equ $
 
 
mdladdr equ $
 
;release 0 steps:
;02/27/2005
;merging pt2 and pt3 players; debug
;02/28/2005
;debug; optimization
;03/01/2005
;migration to sjasm; conditional assembly (zx, msx and
;visualization)
;03/03/2005
;setport subprogram (35 bytes shorter)
;03/05/2005
;fixed curposcounter error
;03/06/2005
;added acb and bac channels swapper (for spectre); more cond.
;assembly keys; optimization
 
;tests in immation tester v1.0 by andy man/pos
;(for minimal build)
;module name/author     min tacts       max tacts
;pt3 (a little slower than standalone player)
;spleen/nik-o           1720            9368
;chuta/miguel           1720            9656
;zhara/macros           4536            8792
;pt2 (more slower than standalone player)
;epilogue/nik-o         3928            10232
;ny themes/zhenya       3848            9208
;guest 4/alex job       2824            9352
;kickdb/fatal snipe     1720            9880
 
;size (minimal build for zx spectrum):
;code block #7b4 bytes
;variables #21d bytes (can be stripped)
;size in ram #7b4+#21d=#9d1 (2513) bytes
 
;notes:
;pro tracker 3.4r can not be detected by header, so pt3.4r tone
;tables realy used only for modules of 3.3 and older versions.
 

	display "player size = ",$-start
 
        endmodule

