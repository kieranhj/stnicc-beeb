\ ******************************************************************
\ *	PARSE A FRAME OF DATA FROM STNICCC STREAM
\ ******************************************************************

MACRO GET_BYTE
{
    inc STREAM_ptr_LO
    bne no_carry
    inc STREAM_ptr_HI
    .no_carry
    lda (STREAM_ptr_LO), y
}
ENDMACRO

.parse_frame
\{
    ldy #0
    GET_BYTE
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
    GET_BYTE
    sta frame_bitmask+1
    GET_BYTE
    sta frame_bitmask

    \\ Read palette words
    ldx #16
    .parse_palette_loop
    lsr frame_bitmask+1
    ror frame_bitmask
    bcc not_this_bit

    \\ Discard our palette for now
    GET_BYTE
    GET_BYTE

    .not_this_bit
    dex
    bne parse_palette_loop
    .no_palette

    \\ Check whether we have indexed data
    lda frame_flags
    and #FLAG_INDEXED_DATA
    beq read_poly_data

    \\ Read indexed data (most common)
    GET_BYTE
    sta indexed_num_verts

IF _PREPROCESSED_VERTS
    GET_BYTE
    
    clc
    lda STREAM_ptr_LO
    sta read_verts_x+1
    adc indexed_num_verts
    sta STREAM_ptr_LO

    lda STREAM_ptr_HI
    sta read_verts_x+2
    adc #0
    sta STREAM_ptr_HI
ELSE
    ldx #0
    .read_verts_loop_x
    GET_BYTE
    lsr a
    sta vertices_x, X
    inx
    cpx indexed_num_verts
    bcc read_verts_loop_x
ENDIF

IF _PREPROCESSED_VERTS
    dec indexed_num_verts

    clc
    lda STREAM_ptr_LO
    sta read_verts_y+1
    adc indexed_num_verts
    sta STREAM_ptr_LO

    lda STREAM_ptr_HI
    sta read_verts_y+2
    adc #0
    sta STREAM_ptr_HI

    inc indexed_num_verts
ELSE
    ldx #0
    .read_verts_loop_y
    GET_BYTE
    IF _HALF_VERTICAL_RES
    lsr a
    ENDIF
    sta vertices_y, X
    inx
    cpx indexed_num_verts
    bcc read_verts_loop_y
ENDIF

    \\ Read polygon data
    .read_poly_data
    ldy #0
    GET_BYTE
    sta poly_descriptor
    cmp #POLY_DESC_END_OF_STREAM
    bcs parse_end_of_frame

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
    ldy #0
    GET_BYTE
    tay

IF _PREPROCESSED_VERTS
    \\ Read the vertices directly from the stream data
    .read_verts_x
    lda &ffff, Y
    sta poly_verts_x, X
    .read_verts_y
    lda &ffff, Y
    IF _HALF_VERTICAL_RES
    lsr a
    ENDIF
    sta poly_verts_y, X
ELSE
    lda vertices_x, Y
    sta poly_verts_x, X
    lda vertices_y, Y
    sta poly_verts_y, X
ENDIF

    \\ Next step would be to inline the poly loop here.

    inx
    cpx poly_num_verts
    bcc read_poly_loop
    bcs parse_do_plot

    .non_indexed_data
    ldx #0
    .read_poly_ni_loop

    \\ This can be changed to read the poly data directly.
    GET_BYTE
    lsr a
    sta poly_verts_x, X
    GET_BYTE
    IF _HALF_VERTICAL_RES
    lsr a
    ENDIF
    sta poly_verts_y, X

    \\ Next step would be to inline the poly loop here.

    inx
    cpx poly_num_verts
    bcc read_poly_ni_loop

    .parse_do_plot
IF _PLOT_WIREFRAME
    jsr plot_poly_line
ELSE
    jmp plot_poly_span      ; JSR/RTS => JMP/JMP
ENDIF
    .return_here_from_plot_poly

    jmp read_poly_data

    .parse_end_of_frame

    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

    rts
\}
