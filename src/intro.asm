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

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD &A0

.zp_start

.writeptr       skip 2
.plot_x_offset  skip 1

.test_x         skip 2
.test_y         skip 2

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

    lda #0
    sta test_x
    sta test_x+1
    sta test_y

    lda #19
    jsr osbyte

    .loop
    jsr plot_at_xy
    lda #19
    jsr osbyte
    jsr plot_at_xy

    clc
    lda test_x
    adc #128
    sta test_x
    lda test_x+1
    adc #0
    sta test_x+1

    lda test_x
    cmp #LO(318 << 7)
    bne not_right
    lda test_x+1
    cmp #HI(318 << 7)
    bne not_right

    inc test_y
    lda #0
    sta test_x
    sta test_x+1

    .not_right

    jmp loop
    rts
}

\\ X stored as 9:7 precision to reach [0-319] with 7 fractional bits [0-127].
\\ Y stored as 8:8 precision unless this proves to be annoying.
.plot_at_xy
{
    ldy test_y
    lda test_x+1
    lsr a
    tax     ; column

    lda test_x
    asl a                   ; top bit => C
    lda test_x+1
    rol a                   ; C => bottom bit
    and #3                  ; just these two bits
    
    jmp plot_glixel_eor
    \\ Could drop thru
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

; X=column [0-79], Y=row [0-255] (on char boundary)
.plot_glixel_sta
{
    clc
    lda screen_row_LO, Y
    adc screen_col_LO, X
    sta writeptr
    lda screen_row_HI, Y
    adc screen_col_HI, X
    sta writeptr+1

    \\ One byte only, column aligned
    ldy #0
    lda glixel_byte0
    sta (writeptr), Y
    iny
    sta (writeptr), Y
    iny
    sta (writeptr), Y
    rts 
}

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
\ *	Save the code
\ ******************************************************************

SAVE "INTRO", start, end, main
