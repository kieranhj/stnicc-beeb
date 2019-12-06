\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB INTRO
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
osargs = &FFDA

IRQ1V = &204

\\ Palette values for ULA
PAL_black	= (0 EOR 7)
PAL_blue	= (4 EOR 7)
PAL_red		= (1 EOR 7)
PAL_magenta = (5 EOR 7)
PAL_green	= (2 EOR 7)
PAL_cyan	= (6 EOR 7)
PAL_yellow	= (3 EOR 7)
PAL_white	= (7 EOR 7)

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO SWRAM_SELECT bank
LDA #bank: sta &f4: sta &fe30
ENDMACRO

MACRO MODE5_PIXELS a,b,c,d
    EQUB (a AND 2) * &40 OR (a AND 1) * &08 OR (b AND 2) * &20 OR (b AND 1) * &04 OR (c AND 2) * &10 OR (c AND 1) * &02 OR (d AND 2) * &08 OR (d AND 1) * &01
ENDMACRO

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

; SCREEN constants
SCREEN_WIDTH_PIXELS = 320
SCREEN_HEIGHT_PIXELS = 256
SCREEN_ROW_BYTES = SCREEN_WIDTH_PIXELS * 8 / 4
SCREEN_SIZE_BYTES = (SCREEN_WIDTH_PIXELS * SCREEN_HEIGHT_PIXELS) / 4

screen_addr = &3000

MAX_GLIXELS = 64
LERP_FRAMES = 64

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD &A0

.zp_start

.writeptr       skip 2
.plot_x_offset  skip 1

.xstart         skip 2  ; pixel in 1:9:6
.ystart         skip 1
.xend           skip 1  ; column
.yend           skip 1

.loop_index     skip 1
.inner_index    skip 1

.count          skip 1
.text_index     skip 1

.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1100
GUARD screen_addr

.start
.main_start

\ ******************************************************************
\ *	Code entry
\ ******************************************************************

.main
{
    ldx #&ff
    txs

    \\ Init ZP
    lda #0
    ldx #0
    .zp_loop
    sta &00, x
    inx
    cpx #&A0
    bne zp_loop

	\\ Set interrupts and handler
	SEI							; disable interupts
	LDA #&7F					; A=01111111
	STA &FE4E					; R14=Interrupt Enable (disable all interrupts)
	STA &FE43					; R3=Data Direction Register "A" (set keyboard data direction)
	LDA #&C2					; A=11000010
	STA &FE4E					; R14=Interrupt Enable (enable main_vsync and timer interrupt)
    CLI

    \\ Set MODE 1

    lda #22
    jsr oswrch
    lda #1
    jsr oswrch

    lda #8:sta &fe00:lda #&C0:sta &fe01  ; cursor off

    lda #LO(160 << 6)
    sta xstart
    lda #HI(160 << 6)
    sta xstart+1
    lda #128
    sta ystart

    lda #0
    sta xend
    lda #0
    sta yend

    ldx #0
    lda #0
    .init_loop
    sta lerp_count, X
    inx
    cpx #MAX_GLIXELS
    bcc init_loop

    lda #19
    jsr osbyte

    .loop
    lda #19
    jsr osbyte

    jsr lerp_glixels

IF 0
    \\ Make a glixel
    \\ From (xstart,ystart) to (xend,yend)
    jsr make_lerp
    bcs table_full

    \\ Increment our destination
    ldx xend        ; columns
    inx
    cpx #80
    bcc xend_ok
    ldx #0
    clc
    lda yend
    adc #4
    sta yend
    .xend_ok
    stx xend
ENDIF

    lda count
    and #63
    bne continue

    ldy text_index
    lda string, y
    jsr lerp_char

    ldy text_index
    iny
    cpy #10
    bcc ok
    ldy #0
    sty xend
    sty yend
    .ok
    sty text_index

    .continue
    inc count
    jmp loop

    rts
}

.lerp_char
{
    sta char_def
    lda #10
    ldx #LO(char_def)
    ldy #HI(char_def)
    jsr osword

    ldx #0
    .loop
    stx loop_index

    lda #8
    sta inner_index

    .inner_loop
    ldx loop_index
    asl char_def+1, X
    bcc no_glixel

    jsr make_lerp

    .no_glixel
    inc xend

    dec inner_index
    bne inner_loop

    sec
    lda xend
    sbc #8
    sta xend

    clc
    lda yend
    adc #4
    sta yend

    ldx loop_index
    inx
    cpx #8
    bcc loop

    sec
    lda yend
    sbc #4*8
    sta yend

    clc
    lda xend
    adc #8
    sta xend

    rts
}

.lerp_glixels
{
    ldx #0
    .loop
    stx loop_index

    lda lerp_count, X
    beq next_lerp

    \\ Remove
    jsr plot_glixel_X
    ldx loop_index

    \\ Move
    clc
    lda xpos_LO, X
    adc xdelta_LO, X
    sta xpos_LO, X
    lda xpos_HI, X
    adc xdelta_HI, X
    sta xpos_HI, X

    clc
    lda ypos_LO, X
    adc ydelta_LO, X
    sta ypos_LO, X
    lda ypos_HI, X
    adc ydelta_HI, X
    sta ypos_HI, X

    dec lerp_count, X

    \\ Plot it
    jsr plot_glixel_X

    .next_lerp
    ldx loop_index
    inx
    cpx #MAX_GLIXELS
    bcc loop

    .return
    rts
}

.plot_glixel_X              ; X is trashed
{
    ldy ypos_HI, X
    lda xpos_HI, X
    sta load_col+1          ; column

    lda xpos_LO, X
    lsr a:lsr a:lsr a
    lsr a:lsr a:lsr a       ; top 2-bits

    .load_col
    ldx #0
    jsr plot_glixel_eor
    rts
}

\\ From xstart, ystart
\\ To xend, yend
.make_lerp
{
    jsr get_next_slot
    bcc found_slot
    rts

    .found_slot
    lda xstart
    sta xpos_LO, X
    lda xstart+1
    sta xpos_HI, X

    lda #0
    sta ypos_LO, X
    lda ystart
    sta ypos_HI, X

    \\ Calculate xend - xstart
    sec
    lda #0
    sbc xstart
    sta xdelta_LO, X
    lda xend
    sbc xstart+1
    sta xdelta_HI, X

    \\ Calculate yend - ystart
    sec
    lda #0          ; yend_LO
    sbc #0          ; ystart_LO
    sta ydelta_LO, X
    lda yend
    sbc ystart
    sta ydelta_HI, X

    \\ NEED EXTRA BIT FOR SIGN!
    \\ NEED TO KEEP SIGN!

    \\ Now divide by our number of frames.
    \\ Should do this entirely in ZP.
    lda xdelta_HI, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X

    lda ydelta_HI, X
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    

    lda #LERP_FRAMES
    sta lerp_count, X

    \\ Plot it to begin with
    jsr plot_glixel_X

    .return
    clc
    rts
}

.get_next_slot
{
    clc
    ldx #0
    .loop
    lda lerp_count, X
    beq return
    inx
    cpx #MAX_GLIXELS
    bcc loop
    .return
    rts
}

MACRO MOVE_ROW
{
    lda writeptr
    and #7
    cmp #7
    beq next_row
    inc writeptr
    bne cont 

    .next_row
    clc
    lda writeptr
    adc #LO(SCREEN_ROW_BYTES-7)
    sta writeptr
    lda writeptr+1
    adc #HI(SCREEN_ROW_BYTES-7)
    sta writeptr+1
    .cont
}
ENDMACRO

; X=column [0-79], A=pixel offset [0-3], Y=row [0-255]
.plot_glixel_eor
{
    sta plot_x_offset

    clc
    lda screen_row_LO, Y
    adc screen_col_LO, X
    sta writeptr
    lda screen_row_HI, Y
    adc screen_col_HI, X
    sta writeptr+1

    ldy #0
    ldx plot_x_offset
    cpx #2
    bcs two_bytes

    \\ One byte
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    MOVE_ROW
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    MOVE_ROW
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    rts

    \\ Two bytes
    .two_bytes
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    ldy #8
    lda (writeptr), Y
    eor glixel_byte1, X
    sta (writeptr), Y
    MOVE_ROW
    ldy #0
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    ldy #8
    lda (writeptr), Y
    eor glixel_byte1, X
    sta (writeptr), Y
    MOVE_ROW
    ldy #0
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    ldy #8
    lda (writeptr), Y
    eor glixel_byte1, X
    sta (writeptr), Y

    .done
    rts
}

.string
EQUS "HELLOWORLD",0

\\ Four fixed possibilities
\\ X=0 => ppp0
\\ X=1 => 0ppp
\\ X=2 => 00pp p000
\\ X=3 => 000p pp00
.glixel_byte0
EQUB %11101110, %01110111, %00110011, %00010001
.glixel_byte1
EQUB %00000000, %00000000, %11001100, %11101110

PAGE_ALIGN
.screen_row_LO
FOR y,0,255,1
row=y DIV 8:sl=y MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB LO(screen_addr + addr)
NEXT

.screen_row_HI
FOR y,0,255,1
row=y DIV 8:sl=y MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB HI(screen_addr + addr)
NEXT

.screen_col_LO
FOR c,0,79,1
EQUB LO(c * 8)
NEXT

.screen_col_HI
FOR c,0,79,1
EQUB HI(c * 8)
NEXT

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

.bss_start

PAGE_ALIGN
.xpos_LO
skip MAX_GLIXELS
.xpos_HI
skip MAX_GLIXELS
.ypos_LO
skip MAX_GLIXELS
.ypos_HI
skip MAX_GLIXELS

.xdelta_LO
skip MAX_GLIXELS
.xdelta_HI
skip MAX_GLIXELS
.ydelta_LO
skip MAX_GLIXELS
.ydelta_HI
skip MAX_GLIXELS

.lerp_count
skip MAX_GLIXELS

.char_def
skip 9

.xpos

.bss_end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "INTRO", start, end, main
