\ ******************************************************************
\ *	SCREEN FUNCTIONS
\ ******************************************************************

.screen_cls
{
    lda draw_buffer_HI
    cmp #HI(screen1_addr)
    bne screen2_cls
}
\\ Drop thru!
.screen1_cls
{
    lda #0
    tax
    .loop
    FOR page,0,SCREEN_SIZE_BYTES-1,&100
    sta screen1_addr + page, X
    NEXT
    inx
    IF _HALF_VERTICAL_RES AND NOT(_DOUBLE_PLOT_Y)
    inx
    ENDIF
    bne loop
    rts
}

.screen2_cls
{
    lda #0
    tax
    .loop
    FOR page,0,SCREEN_SIZE_BYTES-1,&100
    sta screen2_addr + page, X
    NEXT
    inx
    IF _HALF_VERTICAL_RES AND NOT(_DOUBLE_PLOT_Y)
    inx
    ENDIF
    bne loop
    rts
}

IF _FAST_POLY
\\ Clear only the rows set in the frame's clear mask (13 bytes from the stream,
\\ row y = bit 7 - (y AND 7) of byte y DIV 8): the rows the frame's polygons
\\ leave a pixel of uncovered. A row is cleared by the unrolled span code for its
\\ character row, entered at its first store (all 32 columns) with Y = the row's
\\ scanline. That code returns to the span plotter's next row, so the three bytes
\\ there are a JMP back here while the mask is walked.
mask_bits = accum

.screen_cls_masked
{
    ldx #2
    .save
    lda return_here_from_unrolled_span_loop, X
    sta saved, X
    lda jmp_row_done, X
    sta return_here_from_unrolled_span_loop, X
    dex
    bpl save

    \\ The draw screen's tables, as swap_frame_buffers set them for the plotter
    lda fps_row_table_LO+1:sta table_LO+1
    lda fps_row_table_LO+2:sta table_LO+2
    lda fps_row_table_HI+1:sta table_HI+1
    lda fps_row_table_HI+2:sta table_HI+2

    ldx #0                      ; row
    .next_byte
    ldy #0
    GET_BYTE
    sta mask_bits
    .next_bit
    asl mask_bits
    bcc row_done
    .table_LO
    lda &ffff, X
    sta jump+1
    .table_HI
    lda &ffff, X
    sta jump+2
    txa
    and #3
    asl a
    tay                         ; scanline 2*(y AND 3)
    lda #0
    .jump
    jmp &ffff                   ; comes back to row_done, X kept
    .row_done
    inx
    cpx #100
    beq done
    txa
    and #7
    bne next_bit
    beq next_byte

    .done
    ldx #2
    .restore
    lda saved, X
    sta return_here_from_unrolled_span_loop, X
    dex
    bpl restore
    ldy #0                      ; parse_frame's GET_BYTE relies on it, as screen_cls leaves it
    rts

    .jmp_row_done
    jmp row_done
    .saved
    skip 3
}
ENDIF
