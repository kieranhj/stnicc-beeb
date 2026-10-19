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

if _NULA AND NOT(_CPC)
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
endif

IF _FAST_EDGES
\\ A = vertex y: reset its span buffer row and track the polygon's min/max y
\\ (the first step of plot_poly_span in plot_poly_fast.asm). Uses Y.
MACRO FAST_POLY_VERTEX_ROW
{
    tay
    lda #255:sta span_buffer_start, Y
    lda #0:sta span_buffer_end, Y
    cpy span_buffer_min_y:bcs not_min:sty span_buffer_min_y:.not_min
    cpy span_buffer_max_y:bcc not_max:sty span_buffer_max_y:.not_max
}
ENDMACRO
ENDIF

.parse_frame
{
    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

    ldy #0
    GET_BYTE
    sta frame_flags
    lda STREAM_ptr_HI           ; the loader leaves this frame's pages alone
    sta stream_frame_HI
    lda frame_flags

    and #FLAG_CLEAR_SCREEN
IF _TEST_PARSE_ONLY
    jmp no_clear_screen
ELIF _PLOT_WIREFRAME = FALSE
    beq no_clear_screen
ENDIF

IF _FAST_EDGES AND NOT(_TEST_PARSE_ONLY)
    \\ Clear only the rows the frame's polygons leave uncovered, if it says
    lda frame_flags
    and #FLAG_CLEAR_MASK
    beq full_clear
    jsr screen_cls_masked       ; reads the mask
    jmp cleared
    .full_clear
ENDIF

    jsr screen_cls
    .no_clear_screen

IF NOT(_FAST_EDGES) OR _TEST_PARSE_ONLY
    \\ Step over a clear mask: MEDIUM and HIGH clear the whole screen
    lda frame_flags
    and #FLAG_CLEAR_MASK
    beq cleared
    clc
    lda STREAM_ptr_LO
    adc #CLEAR_MASK_BYTES
    sta STREAM_ptr_LO
    bcc cleared
    inc STREAM_ptr_HI
ENDIF
    .cleared

    lda frame_flags
    and #FLAG_CONTAINS_PALETTE
    beq no_palette

	\\ Skip dummy value
	GET_BYTE

    \\ Read 8-bit palette entry count
	GET_BYTE
	tax

    \\ Read palette words
    .parse_palette_loop

	GET_BYTE

if _NULA AND NOT(_CPC)
   sta $fe23
endif

	GET_BYTE

if _NULA AND NOT(_CPC)
   sta $fe23
endif

    dex
    bne parse_palette_loop

if _CPC
    jsr cpc_palette_update
endif

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
    bcc not_end_of_frame
    rts                         ; end of frame (parse_end_of_frame is out of reach)
    .not_end_of_frame

    and #7
    sta poly_num_verts

    \\ A trim byte follows if the descriptor says so: rows LOW can leave out
    txa
    and #POLY_TRIM
    beq no_trim                 ; A = 0
    GET_BYTE
    .no_trim
IF _FAST_EDGES
    sta poly_trim
ENDIF

    \\ Could be a table: lda poly_descriptor_to_palette_offset, X
    \\ To save 4c per poly at the expense of 1 page.
    txa ; poly_descriptor

if _NULA

    lsr a:lsr a:lsr a:lsr a
	tax
if _CPC
	lda cpc_colour_top,x
	sta span_colour
	lda cpc_colour_bottom,x
	sta span_colour2
	lda cpc_colour_delta,x
	sta cpc_delta_eor+1         ; span_loop's EOR for a dithered colour
else
	lda mode2_pixels,x
	sta span_colour
endif

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
IF _NULA AND _FAST_EDGES
    lsr a                       ; NULA and CPC's 64 columns
ENDIF
    sta poly_verts_x, X
    .read_verts_y
    lda &ffff, Y
    if NOT(_HALF_VERTICAL_RES)
    asl a
    ENDIF
    sta poly_verts_y, X
IF _FAST_EDGES
    FAST_POLY_VERTEX_ROW
ENDIF

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
IF _NULA AND _FAST_EDGES
    lsr a
ENDIF
    sta poly_verts_x, X
    GET_BYTE
    IF NOT(_HALF_VERTICAL_RES)
    asl a
    ENDIF
    sta poly_verts_y, X
IF _FAST_EDGES
    FAST_POLY_VERTEX_ROW
    ldy #0                      ; for GET_BYTE
ENDIF

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
