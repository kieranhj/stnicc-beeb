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

MACRO GET_PAL_BYTE
{
    inc pal_ptr_LO
    bne no_carry
    inc pal_ptr_HI
    .no_carry
    lda (pal_ptr_LO), y
}
ENDMACRO

.parse_frame
{
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
    ldx #15
    .parse_palette_loop
    asl frame_bitmask
    rol frame_bitmask+1
    bcc not_this_bit

    \\ Discard our palette for now
    GET_BYTE
    GET_BYTE

    .not_this_bit
    dex
    bpl parse_palette_loop

    GET_PAL_BYTE
    beq no_palette

    jmp handle_beeb_palette
    .^return_here_from_handle_beeb_palette
    .no_palette

    \\ Check whether we have indexed data
    lda frame_flags
    and #FLAG_INDEXED_DATA
    beq read_poly_data

    \\ Read indexed data (most common)
    GET_BYTE
    sta indexed_num_verts

    \\ Next comes our array of vertices_x
    GET_BYTE
    
    \\ calculate a ptr to the array
    clc
    lda STREAM_ptr_LO
    sta read_verts_x+1
    adc indexed_num_verts
    sta STREAM_ptr_LO

    lda STREAM_ptr_HI
    sta read_verts_x+2
    adc #0
    sta STREAM_ptr_HI

    \\ Next comes our array of vertices_y
    dec indexed_num_verts

    \\ calculate a ptr to the array
    clc
    lda STREAM_ptr_LO
    sta read_verts_y+1
    adc indexed_num_verts
    sta STREAM_ptr_LO

    lda STREAM_ptr_HI
    sta read_verts_y+2
    adc #0
    sta STREAM_ptr_HI

    ; note indexed_num_verts is actually one less than it should be but no longer used.

    \\ Read polygon data
    .read_poly_data
    .^return_here_from_plot_poly
    ldy #0

    \\ Reset our min/max tracking
    sty span_buffer_max_y
    lda #255
    sta span_buffer_min_y

    GET_BYTE
    tax ; poly_descriptor
    cmp #POLY_DESC_END_OF_STREAM
    bcs parse_end_of_frame

    and #&f
    sta poly_num_verts

    \\ Could be a table: lda poly_descriptor_to_palette_offset, X
    \\ To save 4c per poly at the expense of 1 page.
    txa ; poly_descriptor
    and #&f0:lsr a:lsr a
    sta load_palette+1      ; poly_colour * 4

    lda frame_flags
    and #FLAG_INDEXED_DATA
    beq non_indexed_data

    \\ Read first vertex from the array

    ldx #0
    .read_poly_loop
    ldy #0
    GET_BYTE
    tay

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

    \\ Next step would be to inline the poly loop here.

    inx
    cpx poly_num_verts
    bcc read_poly_loop

IF _PLOT_WIREFRAME
    jsr plot_poly_line
    jmp read_poly_data
ELSE
    jmp plot_poly_span      ; JSR/RTS => JMP/JMP
ENDIF

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

IF _PLOT_WIREFRAME
    jsr plot_poly_line
    jmp read_poly_data
ELSE
    jmp plot_poly_span      ; JSR/RTS => JMP/JMP
ENDIF

    .parse_end_of_frame

    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

    rts
}

.handle_beeb_palette
{
    sta pal_descriptor

    \\ Process the ULA palette changes first
    and #&0f
    beq skip_colours
    tax
    .colour_loop
    GET_PAL_BYTE
    sta &fe21
    GET_PAL_BYTE
    sta &fe21
    GET_PAL_BYTE
    sta &fe21
    GET_PAL_BYTE
    sta &fe21
    dex
    bne colour_loop
    .skip_colours

    \\ Process any poly_palette dither changes
    lda pal_descriptor
    and #&f0
    bne do_dithers
    jmp return_here_from_handle_beeb_palette

    .do_dithers
    lsr a:lsr a:lsr a:lsr a
    tax
    .dither_loop
    stx pal_dither_idx

    GET_PAL_BYTE
    sta pal_byte1

    GET_PAL_BYTE
    sta pal_byte2

    GET_PAL_BYTE
    tax     ; dither index

    GET_PAL_BYTE
    tay     ; poly_palette index

    lda dither_table, X
    and pal_byte2
    sta ora_bytes1+1

    lda dither_table, X
    eor #&ff
    and pal_byte1
    .ora_bytes1
    ora #0
    sta poly_palette, Y

    inx
    iny

    lda dither_table, X
    and pal_byte2
    sta ora_bytes2+1

    lda dither_table, X
    eor #&ff
    and pal_byte1
    .ora_bytes2
    ora #0
    sta poly_palette, Y

    inx
    iny

    lda dither_table, X
    and pal_byte2
    sta ora_bytes3+1

    lda dither_table, X
    eor #&ff
    and pal_byte1
    .ora_bytes3
    ora #0
    sta poly_palette, Y

    inx
    iny

    lda dither_table, X
    and pal_byte2
    sta ora_bytes4+1

    lda dither_table, X
    eor #&ff
    and pal_byte1
    .ora_bytes4
    ora #0
    sta poly_palette, Y

    ldy #0
    ldx pal_dither_idx
    dex
    beq done_dither_loop
    jmp dither_loop

    .done_dither_loop
    jmp return_here_from_handle_beeb_palette
}