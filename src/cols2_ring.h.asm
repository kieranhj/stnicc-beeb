\\ -*- mode:beebasm -*-
\\ ******************************************************************
\\ *	THE VERSION 2 COLUMN DECODER'S RUN RING (src/cols2_decode.asm)
\\ ******************************************************************

\\ The run ring (bin/colstream2.py's RING and COLUMN_MAX)
RING_SIZE = 640
COLUMN_MAX = 36
RING_VAL = &400                 ; value (pixel pair)
RING_END = &700                 ; the row after its last
RING_SLOPE = &A00               ; its end less the end of the run it came from
BG_POS = RING_SIZE              ; a column of background, past the ring's end

\\ In the ring arrays' spare tails (positions RING_SIZE onwards): the background
\\ column, zero slopes, each column's ring position and kind (two frames' worth,
\\ by cur_off and prev_off) and the colours
ZERO_SLOPES = RING_SLOPE + BG_POS + 1   ; COLUMN_MAX zeros: a reference without slopes
col_kind = ZERO_SLOPES + COLUMN_MAX     ; 64: 0 = coded in time or tslope mode, 1 = left
col_start_lo = RING_VAL + BG_POS + 1    ; 64
col_start_hi = RING_END + BG_POS + 1    ; 64

\\ The frame before the first: a run of background in each column, at ring
\\ positions 0-31 (cur_off = 0, so the first frame takes it as the frame
\\ before); the background column; zero slopes. (Zero page cleared first.)
MACRO RING_INIT
    ldx #31
    .ring_loop
    lda #0
    sta RING_VAL, X
    sta RING_SLOPE, X
    sta col_start_hi, X
    sta col_kind, X
    lda #ROWS
    sta RING_END, X
    txa
    sta col_start_lo, X
    dex
    bpl ring_loop
    lda #32
    sta next_lo
    lda #0
    sta RING_VAL + BG_POS
    sta RING_SLOPE + BG_POS
    ldx #COLUMN_MAX - 1
    .zero_loop
    sta ZERO_SLOPES, X
    dex
    bpl zero_loop
    lda #ROWS
    sta RING_END + BG_POS
ENDMACRO
