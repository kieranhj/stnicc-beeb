\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	MASKED SCREEN CLEAR (NULA AND CPC)
\ ******************************************************************

\\ NULA and CPC: clear only the rows set in the frame's clear mask (13 bytes, as
\\ LOW's screen_cls_masked; the encoder's --mask-mode both adds the rows the
\\ 64-wide raster leaves uncovered). Row y is scanline 2*(y AND 3) of character
\\ row y DIV 4 and the one below, each cleared by the unrolled span code for that
\\ character row (all 32 columns), which comes back through a JMP put over the
\\ start of return_here_from_unrolled_span_loop while the mask is walked.
mask_bits = accum
mask_row = span_start

.screen_cls_masked
{
    ldx #2
    .save
    lda return_here_from_unrolled_span_loop, X
    sta saved, X
    lda jmp_scanline_done, X
    sta return_here_from_unrolled_span_loop, X
    dex
    bpl save

    \\ The draw screen's tables, as swap_frame_buffers set them for the plotter
    lda plot_span_set_row_table_LO+1:sta table_LO+1
    lda plot_span_set_row_table_LO+2:sta table_LO+2
    lda plot_span_set_row_table_HI+1:sta table_HI+1
    lda plot_span_set_row_table_HI+2:sta table_HI+2

    ldx #0                      ; row
    .next_byte
    stx mask_row
    ldy #0
    GET_BYTE
    beq byte_done               ; no rows to clear in these eight
    .next_bit
    asl a
    bcs clear_row
    inx
    bne next_bit                ; always: A still holds a set bit

    .clear_row
    sta mask_bits
    txa
    lsr a:lsr a
    tay                         ; character row
    .table_LO
    lda &ffff, Y
    sta jump+1
    .table_HI
    lda &ffff, Y
    sta jump+2
    txa
    and #3
    asl a
    tay                         ; top scanline
    lda #0
    .jump
    jmp &ffff                   ; comes back to scanline_done, X and Y kept
    .scanline_done
    tya
    lsr a
    bcs row_done
    iny                         ; and the scanline below
    lda #0
    beq jump

    .row_done
    inx
    lda mask_bits
    bne next_bit                ; more rows in these eight
    .byte_done
    lda mask_row
    clc
    adc #8
    tax
    cmp #100
    bcc next_byte

    ldx #2
    .restore
    lda saved, X
    sta return_here_from_unrolled_span_loop, X
    dex
    bpl restore
    ldy #0                      ; parse_frame's GET_BYTE relies on it, as screen_cls leaves it
    rts

    .jmp_scanline_done
    jmp scanline_done
    .saved
    skip 3
}
