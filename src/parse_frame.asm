\ ******************************************************************
\ *	PARSE A FRAME OF DATA FROM STNICCC STREAM
\ ******************************************************************

.parse_frame
{
    jsr get_byte
    sta frame_flags

    and #FLAG_CLEAR_SCREEN
IF _PLOT_WIREFRAME = FALSE
    beq no_clear_screen
ENDIF

    jsr screen_cls
    .no_clear_screen

    lda frame_flags
    and #FLAG_CONTAINS_PALETTE
    beq no_palette

    \\ Read 16-bit palette mask
    jsr get_byte
    sta frame_bitmask+1
    jsr get_byte
    sta frame_bitmask

    \\ Read palette words
    ldx #16
    .palette_loop
    lsr frame_bitmask+1
    ror frame_bitmask
    bcc not_this_bit

    \\ Discard our palette for now
    jsr get_byte
    jsr get_byte

    .not_this_bit
    dex
    bne palette_loop
    .no_palette

    \\ CHeck whether we have indexed data
    lda frame_flags
    and #FLAG_INDEXED_DATA
    beq read_poly_data

    \\ Read indexed data (most common)
    jsr get_byte
    sta indexed_num_verts

    ldx #0
    .read_verts_loop
    jsr get_byte
    lsr a
    sta vertices_x, X
    jsr get_byte
    sta vertices_y, X
    inx
    cpx indexed_num_verts
    bcc read_verts_loop

    \\ Read polygon data
    .read_poly_data

    jsr get_byte
    sta poly_descriptor
    cmp #POLY_DESC_END_OF_STREAM
    bcs end_of_frame

    and #&f
    sta poly_num_verts

    lda poly_descriptor
    lsr a:lsr a:lsr a:lsr a
    sta poly_colour

    lda frame_flags
    and #FLAG_INDEXED_DATA
    beq non_indexed_data

    ldx #0
    .read_poly_loop
    jsr get_byte
    tay

    lda vertices_x, Y
    sta poly_verts_x, X
    lda vertices_y, Y
    sta poly_verts_y, X

    inx
    cpx poly_num_verts
    bcc read_poly_loop
    jmp do_plot

    .non_indexed_data
    ldx #0
    .read_poly_ni_loop

    jsr get_byte
    lsr a
    sta poly_verts_x, X
    jsr get_byte
    sta poly_verts_y, X

    inx
    cpx poly_num_verts
    bcc read_poly_ni_loop

    .do_plot
IF _PLOT_WIREFRAME
    jsr plot_poly_line
ELSE
    jsr plot_poly_span
ENDIF
    jmp read_poly_data

    .end_of_frame

    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

    rts
}
