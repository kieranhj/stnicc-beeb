\ ******************************************************************
\ *	SPAN BUFFER POLYGON FILL ROUTINES
\ ******************************************************************

\ ******************************************************************
\ *	SPAN PLOTTING FUNCTIONS
\ ******************************************************************

; Plot pixels from [span_start,span_end] on line span_y using span_colour
; Can optimise all of this later for poly fill, as shouldn't need to check
; start and end point, also can probably keep track of screen address without
; having to recalculate each time etc.
.plot_span
{
    ldx span_start
    \\ Calculate span width in pixels
	sec
	lda span_end
    sbc span_start

    \\ Check span_start < span_end - should always be true w/ span_buffer
;    bcs posdx
;    ldx span_end
;    eor #&ff
;    adc #1
;    .posdx
IF _POLY_PLOT_END_POINTS
    clc
    adc #1
ENDIF

	sta span_width
    beq return

    \\ Compute address of first screen byte
    ldy span_y
    clc
    lda screen_row_LO, Y
    adc screen_col_LO, X
    sta writeptr
    lda screen_row_HI, Y
    adc screen_col_HI, X
    clc
    adc draw_buffer_HI
    sta writeptr+1

    \\ Column index
    ldy #0

    \\ Check if the span is short (<4 pixels)
    lda span_width
    cmp #4
    bcs long_span
    jmp plot_short_span

    .long_span
    \\ First byte
    txa     ; span_start
    and #3
    beq skip_first_byte
    tax

    lda span_colour
    and colour_mask_starting_at_pixel, X
    sta ora_left_hand_byte+1

    \\ Read screen byte
    lda (writeptr), Y
    \\ Mask out pixels to be drawn
    and screen_mask_starting_at_pixel, X
    \\ Mask in our colour pixels
    .ora_left_hand_byte
    ora #0
    \\ Write to screen
    sta (writeptr), Y

    \\ Subtract pixels from width
    sec
    lda span_width
    sbc four_minus, X
    sta span_width

    \\ Increment column
    tya:clc:adc #8:tay
    .skip_first_byte

    \\ Main body of span
    lda span_width
    lsr a:lsr a
    tax
    beq end_loop

    lda span_colour         ; 3c
    sta load_span_colour+1  ; 4c
    clc                     ; 2c
    .loop

    \\ Need to unroll this really
    .load_span_colour
    lda #0                  ; 2c
    sta (writeptr), Y       ; 6c

    tya:adc #8:tay          ; 6c
    dex                     ; 2c
    bne loop                ; 3c
    .end_loop

    lda span_width
    and #3
    tax

    \\ Last byte?
    cpx #0
    beq skip_last_byte

    lda span_colour
    and screen_mask_starting_at_pixel, X
    sta ora_right_hand_byte+1

    \\ Read screen byte
    lda (writeptr), Y
    \\ Mask out pixels to be drawn
    and colour_mask_starting_at_pixel, X
    \\ Mask in our colour pixels
    .ora_right_hand_byte
    ora #0
    \\ Write to screen
    sta (writeptr), Y
    .skip_last_byte

    .return
    rts
}

.plot_short_span
{
    \\ X=span start pixel
    txa
    and #3
    tax

    .short_loop
    jsr plot_pixel_at

    \\ Could spill into next column
    inx
    cpx #4
    bcc same_column

    \\ Next column along
    tya:clc:adc #8:tay
    ldx #0

    .same_column
    dec span_width
    bne short_loop

    rts
}

; pixel X at writeptr column Y using colour_byte
.plot_pixel_at
{
    lda span_colour
    and colour_mask_pixel, X
    sta ora_pixel+1
    lda (writeptr), Y
    and screen_mask_pixel, X
    .ora_pixel
    ora #0
    sta (writeptr), Y
    rts
}

.plot_pixel
{
    \\ Compute address of first screen byte
    clc
    lda screen_row_LO, Y
    adc screen_col_LO, X
    sta writeptr
    lda screen_row_HI, Y
    adc screen_col_HI, X
    clc
    adc draw_buffer_HI
    sta writeptr+1

    txa:and #3:tax
    ldy #0
    jmp plot_pixel_at
}

\ ******************************************************************
\ *	POLYGON PLOT FUNCTIONS
\ ******************************************************************

IF _DEBUG
\\ Technically a debug feature!
.plot_poly_line
{
    \\ Duplicate first vertex to end
    ldx poly_num_verts
    lda poly_verts_x
    sta poly_verts_x, X
    lda poly_verts_y
    sta poly_verts_y, X

    ldx #0
    .loop
    stx poly_index

    lda poly_verts_x, X
    sta startx
    lda poly_verts_y, X
    sta starty

    lda poly_verts_x+1, X
    sta endx
    lda poly_verts_y+1, X
    sta endy

    jsr drawline

    ldx poly_index
    inx
    cpx poly_num_verts
    bcc loop

    rts
}
ENDIF

.plot_poly_span
{
    \\ Reset our min/max tracking
    lda #255
    sta span_buffer_min_y
    lda #0
    sta span_buffer_max_y

    \\ Duplicate first vertex to end
    ldx poly_num_verts
    lda poly_verts_x
    sta poly_verts_x, X
    lda poly_verts_y
    sta poly_verts_y, X

    \\ 'Draw' lines into our span buffer
    ldx #0
    .line_loop
    stx poly_index

    lda poly_verts_x, X
    sta startx
    lda poly_verts_y, X
    sta starty

    lda poly_verts_x+1, X
    sta endx
    lda poly_verts_y+1, X
    sta endy

    jsr drawline_into_span_buffer

    ldx poly_index
    inx
    cpx poly_num_verts
    bcc line_loop

    \\ Set our palette lookup
    lda poly_colour
    asl a:asl a
    sta load_palette+1

    \\ Plot the spans
    inc span_buffer_max_y

    ldy span_buffer_min_y
    .span_loop
    sty span_y

    tya:and #3:tax
    .load_palette
    lda poly_palette, X
    sta span_colour

    lda span_buffer_start, Y
    sta span_start

    lda span_buffer_end, Y
    sta span_end

    \\ Shouldn't have blank spans now we have min/max Y
    ;ora span_start
    ;beq skip_span

    jsr plot_span

    .skip_span
    ldy span_y

    \\ Reset this line of the span buffer since we're already here
    lda #255
    sta span_buffer_start, Y
    lda #0
    sta span_buffer_end, Y

    iny
    cpy span_buffer_max_y
    bcc span_loop

    rts
}

\ ******************************************************************
\ *	SPAN BUFFER FUNCTIONS
\ ******************************************************************

.init_span_buffer
{
    ldy #0
    .loop
    lda #255
    sta span_buffer_start, Y
    lda #0
    sta span_buffer_end, Y
    iny
    cpy #SCREEN_HEIGHT_PIXELS
    bne loop

    rts
}

\\ **

MACRO UPDATE_SPAN_BUFFER
{
    txa
    cmp span_buffer_start, Y
    bcs not_smaller
    sta span_buffer_start, Y

    .not_smaller
    cmp span_buffer_end, Y
    bcc not_larger
    sta span_buffer_end, Y

    .not_larger
}
ENDMACRO

.plot_pixel_into_span_buffer
{
    UPDATE_SPAN_BUFFER
    rts
}

.drawline_into_span_buffer
{
	; calc screen row of starty
	LDY starty
	; calc pixel within byte of startx
	LDX startx

	; calc dx = ABS(startx - endx)
	SEC
	LDA startx
	SBC endx
	BCS posdx
	EOR #255
	ADC #1
	.posdx
	STA dx
	
	; C=0 if dir of startx -> endx is positive, otherwise C=1
	PHP
	
	; calc dy = ABS(starty - endy)
	SEC
	LDA starty
	SBC endy
	BCS posdy
	EOR #255
	ADC #1
	.posdy
	STA dy
	
	; C=0 if dir of starty -> endy is positive, otherwise C=1
	PHP
	
	; Coincident start and end points exit early
	ORA dx
	BEQ exit_early

    \\ Track min/max y values as polys often quite small
    IF 0    ; get get away with not doing this as the polys are convex
    {
        ldy endy
        cpy span_buffer_min_y
        bcs not_smallest_y
        sty span_buffer_min_y
        .not_smallest_y
        cpy span_buffer_max_y
        bcc not_largest_y
        sty span_buffer_max_y
        .not_largest_y
    }
    ENDIF

    ldy starty
    cpy span_buffer_min_y
    bcs not_smallest_y
    sty span_buffer_min_y
    .not_smallest_y
    cpy span_buffer_max_y
    bcc not_largest_y
    sty span_buffer_max_y
    .not_largest_y

	; determine which type of line it is
	LDA dy
	CMP dx
	BCC shallowline
		
.steepline

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	lda #&c8		; INY	\\ going down
	BCC P%+4
	lda #&88		; DEY	\\ going up
	STA branchupdown
	
	PLP
	lda #&e8		; INX	\\ going right
	BCC P%+4
	lda #&ca		; DEX	\\ going left
	sta branchleftright

	; initialise accumulator for 'steep' line
	LDA dy
	STA count
	LSR A

IF _POLY_PLOT_END_POINTS
    INC count
ENDIF

.steeplineloop

	STA accum
	
	; 'plot' pixel
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER

	; check if done
	DEC count
    beq exitline

	.branchupdown
	; move up to next line
	; move down to next line
	nop                     ; self-mod to goingup or goingdown

	; check move to next pixel column
	.movetonextcolumn
	SEC
	LDA accum
	SBC dx
	BCS steeplineloop
	ADC dy
	
	.branchleftright
	nop					    ; self-modified to goingright or goingleft
	JMP steeplineloop		; always taken
	
    .exit_early
    PLP:PLP

	.exitline
    RTS

.shallowline

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	lda #&c8		; INY	\\ going down
	BCC P%+4
	lda #&88		; DEY	\\ going up
	sta branchupdown2
	
	PLP
	lda #&e8		; INX	\\ going right
	BCC P%+4
	lda #&ca		; DEX	\\ going left
	sta branchleftright2

	; initialise accumulator for 'steep' line
	LDA dx
	STA count
	LSR A
	STA accum

IF _POLY_PLOT_END_POINTS
    INC count
ENDIF

    \\ Plot first 'pixel' into span buffer
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER

.shallowlineloop

	; cache byte from destination screen address
	; doesn't mean anything in our context

	; plot pixel in cached byte
	; doesn't mean anything in our context
	
	; check if done
	DEC count
    beq exitline2

	.branchleftright2
	nop 					; self-modified to goingleft2 or goingright2

	; check whether we move to the next line
	SEC
	LDA accum
	SBC dy
	BCC movetonextline

	STA accum
	BCS shallowlineloop 	; always taken
	
	; move down to next line
	.movetonextline
	.goingdown2
	ADC dx
	STA accum				; store new accumulator

    ; Plot 'pixel' for end of span on current line
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER

    .branchupdown2
	nop		                ; self-modified to goingdown2 or goingup2

    ; Plot 'pixel' for start of span on next line
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER

	JMP shallowlineloop		; always taken

	.exitline2
    ; Plot last 'pixel' into span buffer
    jmp plot_pixel_into_span_buffer
}
