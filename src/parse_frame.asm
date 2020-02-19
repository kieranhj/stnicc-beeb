\ -*- mode:beebasm -*-
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

if _NULA
.mode2_pixels
equb %00000000
equb %00000011
equb %00001100
equb %00001111
equb %00110000
equb %00110011
equb %00111100
equb %00111111
equb %11000000
equb %11000011
equb %11001100
equb %11001111
equb %11110000
equb %11110011
equb %11111100
equb %11111111

.ste_palette
for i,0,15,1
equb ((i>>3) or (i<<1)) and 15
next
endif

if NOT(_NULA)
MACRO GET_PAL_BYTE
{
    inc pal_ptr_LO
    bne no_carry
    inc pal_ptr_HI
    .no_carry
    lda (pal_ptr_LO), y
}
ENDMACRO
endif

.parse_frame
{
    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

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
if _NULA

	txa
	eor #$0f
	asl a
	asl a
	asl a
	asl a
	sta ora_index+1

; The colour palette values sometimes have bit 3 set - presumably
; these are STe-style palette values?
;
; The Beeb version just strips out this bit, and shifts the bottom 3
; bits left to get a 4-bit value for the NuLA palette. This isn't
; ideal, but it's not obviously noticeable.

    stx ldx_x+1

	GET_BYTE					; xxxxxrrr
	and #%00001111
	tax
	lda ste_palette,x
	.ora_index:ora #$ff
	sta $fe23

	GET_BYTE					; ggggbbbb

	sta lda_ggggbbbb+1

	lsr a:lsr a:lsr a:lsr a
	tax
	lda ste_palette,x
	asl a:asl a:asl a:asl a
	sta ora_gggg0000+1

	.lda_ggggbbbb:lda #$ff
	and #$0f
	tax
	lda ste_palette,x
	.ora_gggg0000:ora #$ff
	sta $fe23

	.ldx_x:ldx #$ff

else

    GET_BYTE
    GET_BYTE

endif

    .not_this_bit
    dex
    bpl parse_palette_loop

if NOT(_NULA)
    GET_PAL_BYTE
    beq no_palette

    jmp handle_beeb_palette
    .^return_here_from_handle_beeb_palette
endif
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

if _NULA

    lsr a:lsr a:lsr a:lsr a
	tax
	lda mode2_pixels,x
	sta span_colour

else
	
    and #&f0:lsr a:lsr a
    sta load_palette+1      ; poly_colour * 4

endif

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
    IF _SKIP_ODD_FRAMES
    lda frame_no
    lsr a
    bcs read_poly_data
    ENDIF
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
    IF _SKIP_ODD_FRAMES
    lda frame_no
    lsr a
    bcs read_poly_data
    ENDIF
    jmp plot_poly_span      ; JSR/RTS => JMP/JMP
ENDIF

    .parse_end_of_frame

    rts
}
