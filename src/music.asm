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

\\ The VGI player (lib/vgiplayer.asm, from vgm-player-bbc): v3 files, unrolled
\\ decoder. Mean ~985 cycles a frame against the VGC player's ~1,790.
VGI_V3 = TRUE
VGI_UNROLL = TRUE

.zp_start
INCLUDE "lib/vgiplayer.h.asm"
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
    clc ; no loop
    jmp vgm_init
}

.silent
{
sei								; in case it's playing...
lda #$0f
jsr set_volume_bits
jsr sn_reset
cli
rts
}

.loud
{
lda #$00
jmp set_volume_bits
}

\\ A = &0F (every volume the player writes becomes 15, silent) or 0 (as played):
\\ the low nibble of the player's four volume ORs
MACRO SET_VOLUME_BITS ora_at
    lda ora_at+1
    and #$f0
    ora set_volume_bits_value
    sta ora_at+1
ENDMACRO

.set_volume_bits
{
sta set_volume_bits_value
SET_VOLUME_BITS vgi_vol0_ora
SET_VOLUME_BITS vgi_vol1_ora
SET_VOLUME_BITS vgi_vol2_ora
SET_VOLUME_BITS vgi_vol3_ora
rts
}
.set_volume_bits_value
equb 0

INCLUDE "lib/vgiplayer.asm"
.main_end

.data_start
PAGE_ALIGN
.vgc_data_intro_theme
INCBIN "build/intro_theme.vgi"
.vgc_data_main_theme
INCBIN "build/main_theme.vgi"
.vgc_data_outro_theme
INCBIN "build/outro_theme.vgi"
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

\\ The disc 1 layout (stnicc-build.asm) relies on MUSIC filling its slot
INCLUDE "src/exe_sizes.h.asm"
ASSERT ((end - start + 255) AND &FF00) = EXE_SIZE_MUSIC
INCLUDE "src/music.h.asm"
ASSERT vgm_buffer_end <= MUSIC_FREE_START     ; src/music.h.asm

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
