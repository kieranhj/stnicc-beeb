\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB MUSIC MODULE
\ ******************************************************************

\ ******************************************************************
\ *	OS defines
\ ******************************************************************

osfile = &FFDD
oswrch = &FFEE
osasci = &FFE3
osbyte = &FFF4
osword = &FFF1
osfind = &FFCE
osgbpb = &FFD1
oscli  = &FFF7
osargs = &FFDA

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO PAGE_ALIGN
H%=P%
ALIGN &100
PRINT "Lost ", P%-H%, "bytes"
ENDMACRO

MACRO PAGE_ALIGN_FOR_SIZE size
IF HI(P%+size) <> HI(P%)
	PAGE_ALIGN
ENDIF
ENDMACRO

MACRO CHECK_SAME_PAGE_AS base
IF HI(P%-1) <> HI(base)
PRINT "WARNING! Table or branch base address",~base, "may cross page boundary at",~P%
ENDIF
ENDMACRO

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &80
GUARD &A0

.zp_start
INCLUDE "lib/vgcplayer.h.asm"
.zp_end

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &8000
GUARD &C000

.start
.main_start

\\ Jump table
{
    jmp init_intro_theme
    jmp init_main_theme
    jmp init_outro_theme
    jmp vgm_update
    jmp sn_reset
	jmp silent
	jmp loud
}

.init_intro_theme
{
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgc_data_intro_theme)
    ldy #hi(vgc_data_intro_theme)
    clc ; no loop
    jmp vgm_init
}

.init_main_theme
{
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgc_data_main_theme)
    ldy #hi(vgc_data_main_theme)
    clc ; no loop
    jmp vgm_init
}

.init_outro_theme
{
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgc_data_outro_theme)
    ldy #hi(vgc_data_outro_theme)
    sec ; loop
    jmp vgm_init
}

.silent
{
sei								; in case it's playing...
lda #$0f
jsr fiddle_vgm_register_headers
jsr sn_reset
cli
rts
}

.loud
{
lda #$00
jsr fiddle_vgm_register_headers
rts
}

.fiddle_vgm_register_headers
{
sta ora_bits+1
ldx #4
.loop
lda vgm_register_headers,x
and #$f0
.ora_bits:ora #$00
sta vgm_register_headers,x
inx
cpx #8
bne loop
rts
}

INCLUDE "lib/vgcplayer.asm"
.main_end

.data_start
PAGE_ALIGN
.vgc_data_intro_theme
INCBIN "build/intro_theme.vgc"
.vgc_data_main_theme
INCBIN "build/main_theme.vgc"
.vgc_data_outro_theme
INCBIN "build/outro_theme.vgc"
.data_end
.end

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

PAGE_ALIGN
.bss_start

PAGE_ALIGN
.vgm_buffer_start
; reserve space for the vgm decode buffers (8x256 = 2Kb)
.vgm_stream_buffers
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
.vgm_buffer_end

.bss_end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "build/MUSIC", start, end, start

\ ******************************************************************
\ *	Memory Info
\ ******************************************************************

PRINT "------"
PRINT "STNICC-BEEB MUSIC"
PRINT "------"
PRINT "ZP size =", ~zp_end-zp_start, "(",~&A0-zp_end,"free)"
PRINT "MAIN size =", ~main_end-main_start
PRINT "DATA size =",~data_end-data_start
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~&C000-P%
PRINT "------"
