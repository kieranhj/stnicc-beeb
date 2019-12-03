\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	SPAN BUFFER POLYGON FILL ROUTINES
\ ******************************************************************

\\ 1 pixel = 1 byte max
\\ 5 pixels = 2 bytes max
\\ 9 pixels = 3 bytes max
\\ 13 pixels = 4 bytes max
_SHORT_SPAN_MAX_BYTES=4

if _NULA
_PIXELS_PER_BYTE=2
else
_PIXELS_PER_BYTE=4
endif

\ ******************************************************************
\ *	SPAN PLOTTING FUNCTIONS
\ ******************************************************************

; Plot pixels from [span_start,span_end] on line span_y using span_colour
; Can optimise all of this later for poly fill, as shouldn't need to check
; start and end point, also can probably keep track of screen address without
; having to recalculate each time etc.

; This fn is called 3 million times across the entire sequence so every cycle counts!
.plot_long_span
{
    ; X=span_start
    ; Y=span_y / poly_y
    ; span_width already computed

    \\ Compute address screen row for writeptr
    \\ NB. writeptr_LO = 0 then indexed by Y
    .^plot_long_span_set_screen
    lda screen1_row_HI, Y                   ; 4c
    sta writeptr+1                          ; 3c

    \\ Calculate offset into screen row for Y
    clc                                     ; 2c
    lda screen_row_LO, Y                    ; 4c        ; scanline
    adc screen_col_LO, X                    ; 4c        ; column * 8
    tay                                     ; 2c

    \\ X=span_start
if _NULA

txa:and #1:beq skip_first_byte

lda span_colour:and #%01010101:sta ora_left_hand_byte+1
lda (writeptr),y:and #%10101010:.ora_left_hand_byte:ora #$ff:sta (writeptr),y
if _DOUBLE_PLOT_Y:iny:sta (writeptr),y:dey:endif

dec span_width

clc\\todo - what's the actual state of carry here?

tya:adc #8:tay
\\C=0

else

    txa                                     ; 2c
    and #3                                  ; 2c
    beq skip_first_byte                     ; 2/3c

    \\ First byte starts X pixels in
    tax                                     ; 2c
    lda span_colour                         ; 3c
    and colour_mask_starting_at_pixel, X    ; 4c
    sta ora_left_hand_byte+1                ; 4c

    \\ Read screen byte
    lda (writeptr), Y                       ; 5c
    \\ Mask out pixels to be drawn
    and screen_mask_starting_at_pixel, X    ; 4c
    \\ Mask in our colour pixels
    .ora_left_hand_byte
    ora #0                                  ; 2c
    \\ Write to screen
    sta (writeptr), Y                       ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y:dey               ; 10c
    ENDIF

    \\ Subtract pixels from width
    sec                                     ; 2c
    lda span_width                          ; 3c
    sbc four_minus, X                       ; 4c
    sta span_width                          ; 3c
    \\ C=1

    \\ Increment column - can't overflow row
    tya:adc #7:tay                          ; 6c
	\\ C=0

endif

    .skip_first_byte
    \\ Main body of span; bytes to plot = span_width DIV 4
    lda span_width                          ; 3c
if _NULA
    and #%01111110
else
	and #%11111100							; 2c
endif

    \\ Can't skip the inner span loop if we have a separate fn for short spans
    IF _SHORT_SPAN_MAX_BYTES < 3
    .branch_to_skip_span_loop
    beq skip_span_loop                      ; 2/3c
    ENDIF

    \\ Y=writeptr_LO
	sty load_y_offset+1                     ; 4c

    \\ Calculate row from span_y DIV 8
	ldx span_y                              ; 3c
	ldy y_to_row,x                          ; 4c

    ; A=(span_width DIV 4) << 2
	tax                                     ; 2c

    \\ Select row fn
    \\ Add offset into fn based on #columns to plot
    .^plot_span_set_row_table_LO
    lda span_row_table_screen1_LO, Y        ; 4c
    adc long_span_tables+0,X				; 4c
    sta jump_to_unrolled_span_row+1         ; 4c
    .^plot_span_set_row_table_HI
    lda span_row_table_screen1_HI, Y        ; 4c
    adc #0                                  ; 2c
    sta jump_to_unrolled_span_row+2         ; 4c
	\\ C=0
	
    \\ Y=column offset from start of row + scanline = writeptr_LO
    .load_y_offset
    ldy #0                                  ; 2c

    \\ A=screen byte
    .do_unrolled_span
    lda span_colour                         ; 3c
    .jump_to_unrolled_span_row
    jmp &ffff                               ; _DOUBLE_PLOT_Y ?
    .^return_here_from_unrolled_span_loop
    IF _DOUBLE_PLOT_Y
    tya:and #1                              ; 4c
    .branch_to_done_double_plot
    bne done_double_plot                    ; 6c

    iny:bne do_unrolled_span                ; 5c
    CHECK_SAME_PAGE_AS do_unrolled_span

    .done_double_plot
    CHECK_SAME_PAGE_AS branch_to_done_double_plot
    dey                                     ; 2c
	ENDIF

    \\ Increment to last column
    tya:adc long_span_tables+1,X:tay        ; 8c  

    IF _SHORT_SPAN_MAX_BYTES < 3
    .skip_span_loop
    CHECK_SAME_PAGE_AS branch_to_skip_span_loop
    ENDIF

if _NULA

lda span_width:and #1:.branch_to_skip_last_byte:beq return_here_from_plot_span

lda span_colour:and #%10101010:sta ora_right_hand_byte+1

lda (writeptr),y:and #%01010101:.ora_right_hand_byte:ora #$ff:sta (writeptr),y

else

    \\ Last byte?
    lda span_width                          ; 3c
    and #3                                  ; 2c
    .branch_to_skip_last_byte
    beq return_here_from_plot_span          ; 2/3c
    tax                                     ; 2c

    lda span_colour                         ; 3c
    and screen_mask_starting_at_pixel, x    ; 4c
    sta ora_right_hand_byte+1               ; 4c

    \\ Read screen byte
    lda (writeptr), Y                       ; 5c
    \\ Mask out pixels to be drawn
    and colour_mask_starting_at_pixel, X    ; 4c
    \\ Mask in our colour pixels
    .ora_right_hand_byte
    ora #0                                  ; 2c
    \\ Write to screen
    sta (writeptr), Y                       ; 6c

endif

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y                   ; 8c
    ENDIF
    .skip_last_byte
    CHECK_SAME_PAGE_AS branch_to_skip_last_byte

    jmp return_here_from_plot_span          ; 3c
}

\ ******************************************************************
\ *	POLYGON PLOT FUNCTIONS
\ ******************************************************************

.plot_poly_span
{
    ; X=poly_num_verts

    \\ Duplicate first vertex to end
    lda poly_verts_x
    sta poly_verts_x, X
    lda poly_verts_y
    sta poly_verts_y, X

    \\ 'Draw' lines into our span buffer
    dex
    .line_loop
    stx poly_index              ; 3c

    lda poly_verts_x+1, X       ; 4c
if _NULA
	lsr a
endif
    sta endx                    ; 3c
    lda poly_verts_y+1, X       ; 4c
    sta endy                    ; 4c

    ; starty
    ldy poly_verts_y, X         ; 4c
    ; startx
    lda poly_verts_x, X         ; 4c
if _NULA
	lsr a
endif
    tax                         ; 2c

    jmp drawline_into_span_buffer       ; JSR/RTS => JMP/JMP
    .^return_here_from_drawline

    ldx poly_index              ; 3c
    dex                         ; 2c
    bpl line_loop               ; 3c
    CHECK_SAME_PAGE_AS line_loop

    \\ Palette lookup set during frame_parse

    \\ Plot the spans
    ldy span_buffer_max_y
    iny

    IF _WIDESCREEN
    {
        \\ max_y < top of screen
        cpy #WIDESCREEN_TOP+1
        bcc skip_span_loop
        \\ clamp max_y to bottom of screen
        cpy #WIDESCREEN_BOTTOM
        bcc bottom_ok
        ldy #WIDESCREEN_BOTTOM
        .bottom_ok
        sty span_loop_max_y+1

        ldy span_buffer_min_y
        \\ min_y > bottom of screen
        cpy #WIDESCREEN_BOTTOM+1
        bcs skip_span_loop
        cpy #WIDESCREEN_TOP
        bcs top_ok
        ldy #WIDESCREEN_TOP
        .top_ok
    }
    ELSE
    sty span_loop_max_y+1
    ldy span_buffer_min_y
    ENDIF

    .span_loop
    sty poly_y                  ; 3c

    \\ v= on Master would happily burn a PAGE per palette to save 6c!
if not(_NULA)
    tya:and #3:tax              ; 6c
    .^load_palette
    lda poly_palette, X         ; 4c
    sta span_colour             ; 3c    <= where is this used?
endif

    lda span_buffer_start, Y    ; 4c
    tax                         ; 2c    X=span_start

    \\ Calculate span width in pixels
    sec                         ; 2c
    sbc span_buffer_end, Y      ; 4c
    eor #&ff                    ; 2c
    clc                         ; 2c
    adc #2                      ; 2c    _POLY_PLOT_END_POINTS
    sta span_width              ; 3c
    \\ 21c

    \\ Shouldn't have blank spans now we have min/max Y

    \\ Check if the span is short...
    ; cmp #(_SHORT_SPAN_MAX_BYTES+1)     ; 2c
	cmp #_SHORT_SPAN_MAX_BYTES*_PIXELS_PER_BYTE-(_PIXELS_PER_BYTE-1)+1
    bcc plot_short_span     ; [1-5] ; 2/3c
    .^branch_to_short_span

    IF _HALF_VERTICAL_RES
    tya:asl a:tay:sty span_y    ; 6c
    ENDIF

    jmp plot_long_span          ; eventually inline entire fn to save 6c
    .^return_here_from_plot_span

    .skip_span
    ldy poly_y                  ; 3c

    \\ Reset this line of the span buffer since we're already here
    lda #255                    ; 2c
    sta span_buffer_start, Y    ; 5c
    lda #0                      ; 2c
    sta span_buffer_end, Y      ; 5c

    iny                         ; 2c
    .span_loop_max_y
    cpy #0                      ; 2c
    bcc span_loop               ; 3c
    CHECK_SAME_PAGE_AS span_loop

    .skip_span_loop
    jmp return_here_from_plot_poly
}

.plot_short_span
{
    CHECK_SAME_PAGE_AS branch_to_short_span

    IF _HALF_VERTICAL_RES
    tya:asl a:tay                   ; 6c
    ENDIF

    \\ A=span_width
    \\ X=span_start (pixel column)
    \\ Y=span_y

    clc                             ; 2c
    lda screen_row_LO, Y            ; 4c
    adc screen_col_LO, X            ; 4c        ; column * 8
    sta shortptr
    .^plot_short_span_set_screen
    lda screen1_row_HI, Y           ; 4c
    ; no carry!
    sta shortptr+1                  ; 3c

if _NULA

    txa
	and #1

	ldx span_width
	adc minus_1_times_2,x
	tax

else

    \\ w = [1,N] x = [0,3]
    txa                             ; 2c
    and #3                          ; 2c

    ldx span_width                  ; 3c
    adc minus_1_times_4, X          ; 4c
    tax                             ; 2c
	
endif

    \\ Byte 1
    lda colour_mask_short_0, X      ; 2c
    and span_colour                 ; 3c
    sta ora_byte1+1                 ; 4c

    ldy #0                          ; 2c
    lda (shortptr), Y               ; 5c
    and screen_mask_short_0, X      ; 4c
    .ora_byte1
    ora #0                          ; 2c
    sta (shortptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (shortptr), Y           ; 8c
    ENDIF

    \\ Byte 2
    lda colour_mask_short_1, X      ; 4c
    beq return_here_from_plot_span                        ; 2/3c
    CHECK_SAME_PAGE_AS return_here_from_plot_span

    and span_colour                 ; 3c
    sta ora_byte2+1                 ; 4c

    ldy #8                          ; 2c
    lda (shortptr), Y               ; 5c
    and screen_mask_short_1, X      ; 4c
    .ora_byte2
    ora #0                          ; 2c
    sta (shortptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (shortptr), Y           ; 8c
    ENDIF

IF _SHORT_SPAN_MAX_BYTES >= 3
    \\ Byte 3
    lda colour_mask_short_2, X      ; 4c
    beq return_here_from_plot_span                        ; 2/3c
    CHECK_SAME_PAGE_AS return_here_from_plot_span

    and span_colour                 ; 3c
    sta ora_byte3+1                 ; 4c

    ldy #16                         ; 2c
    lda (shortptr), Y               ; 5c
    and screen_mask_short_2, X       ; 4c
    .ora_byte3
    ora #0                          ; 2c
    sta (shortptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (shortptr), Y           ; 8c
    ENDIF
ENDIF

IF _SHORT_SPAN_MAX_BYTES >= 4
    \\ Byte 4
    lda colour_mask_short_3, X      ; 4c
    beq return_here_from_plot_span                        ; 2/3c
    CHECK_SAME_PAGE_AS return_here_from_plot_span

    and span_colour                 ; 3c
    sta ora_byte4+1                 ; 4c

    ldy #24                         ; 2c
    lda (shortptr), Y               ; 5c
    and screen_mask_short_3, X      ; 4c
    .ora_byte4
    ora #0                          ; 2c
    sta (shortptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (shortptr), Y           ; 8c
    ENDIF
ENDIF

    .done
    jmp return_here_from_plot_span
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
    CHECK_SAME_PAGE_AS loop

    rts
}

\\ **

MACRO UPDATE_SPAN_BUFFER_WITH_A must_set_carry
{
    cmp span_buffer_end, Y
    bcc not_larger
    sta span_buffer_end, Y

    .not_larger
	
    cmp span_buffer_start, Y
    bcs not_smaller
    sta span_buffer_start, Y

IF must_set_carry
    sec
ENDIF
	
    .not_smaller
}
ENDMACRO

MACRO UPDATE_SPAN_BUFFER_WITH_X must_set_carry
{
    txa
	UPDATE_SPAN_BUFFER_WITH_A must_set_carry
}
ENDMACRO

.drawline_into_span_buffer
{
	; calc screen row of starty
    ; Y=starty

    \\ Keep track of our min/max Y for span plot
    {
        cpy span_buffer_min_y
        bcs not_smallest_y
        sty span_buffer_min_y
        .not_smallest_y
        cpy span_buffer_max_y
        bcc not_largest_y
        sty span_buffer_max_y
        .not_largest_y
    }
    \\ Don't need to do this for endy as we know tris are convex

	; calc pixel within byte of startx
    ; X=startx

	; calc dx = ABS(startx - endx)
	SEC
    ; actually already in A as well.
    ;txa
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
    tya
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

	; determine which type of line it is
	LDA dy
	CMP dx
	BCC shallowline
		
.steepline

; count = X counter
; Y = Y counter
; X = count

    stx count

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	lda #&c8		; INY	\\ going down
	BCC P%+4
	lda #&88		; DEY	\\ going up
	STA branchupdown
	
	PLP
	lda #&e6		; INC zp	\\ going right
	BCC P%+4
	lda #&c6		; DEC zp	\\ going left
	sta branchleftright

	; initialise accumulator for 'steep' line
	LDA dy
	tax
	LSR A

    \\ _POLY_PLOT_END_POINTS
    INX

.steeplineloop

	STA accum
	
	; 'plot' pixel
    ;jsr plot_pixel_into_span_buffer
	lda count
	UPDATE_SPAN_BUFFER_WITH_A TRUE

	; check if done
	dex
    .branch_to_exitline
    beq exitline        ; <= can this branch directly to return_here_from_drawline?

	.branchupdown
	; move up to next line
	; move down to next line
	nop                     ; iny/dey - self-mod to goingup or
							; goingdown

	; check move to next pixel column
	.movetonextcolumn
	LDA accum
	SBC dx
	BCS steeplineloop
    CHECK_SAME_PAGE_AS steeplineloop
	ADC dy
	
	.branchleftright
	inc count			    ; inc/dec - self-modified to goingright or
						    ; goingleft
	JMP steeplineloop	    ; always taken
    CHECK_SAME_PAGE_AS steeplineloop
	
    .exit_early
    PLP:PLP

    .exitline
    CHECK_SAME_PAGE_AS branch_to_exitline
    jmp return_here_from_drawline


.shallowline

; X = X counter
; count = Y counter
; Y = count

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

    \\ _POLY_PLOT_END_POINTS
    INC count

    \\ Plot first 'pixel' into span buffer
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER_WITH_X FALSE

.shallowlineloop_1

	sec
	lda accum

.shallowlineloop_2

	; cache byte from destination screen address
	; doesn't mean anything in our context

	; plot pixel in cached byte
	; doesn't mean anything in our context
	
	; check if done
	DEC count
    .branch_to_exitline2
    beq exitline2       ; <= can this branch directly to return_here_from_drawline?

	.branchleftright2
	nop 					; inx/dex - self-modified to goingleft2 or
							; goingright2

	; check whether we move to the next line
	SBC dy
	BCS shallowlineloop_2
    CHECK_SAME_PAGE_AS shallowlineloop_2

	; move down to next line
	.movetonextline
	.goingdown2
	ADC dx
	STA accum				; store new accumulator

    ; Plot 'pixel' for end of span on current line
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER_WITH_X FALSE

    .branchupdown2
	nop		                ; iny/dey - self-modified to goingdown2 or
							; goingup2

    ; Plot 'pixel' for start of span on next line
    ;jsr plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER_WITH_X FALSE

	JMP shallowlineloop_1		; always taken

	.exitline2
    CHECK_SAME_PAGE_AS branch_to_exitline2
    ; Plot last 'pixel' into span buffer
    ;jmp plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER_WITH_X FALSE
    jmp return_here_from_drawline
}

\ ******************************************************************
\ *	UPDATE PALETTE
\ ******************************************************************

if not(_NULA)
.handle_beeb_palette
{
    sta pal_descriptor

    \\ Process the ULA palette changes first
    and #&f0
    beq skip_colours
    lsr a:lsr a:lsr a:lsr a
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
    and #&0f
    beq done_dither_loop

    tax
    .dither_loop
    stx pal_dither_idx

    GET_PAL_BYTE
    tax

    GET_PAL_BYTE                ; best case 5c+5c+3c = 13c
    sta poly_palette+0, X

    GET_PAL_BYTE
    sta poly_palette+1, X

    GET_PAL_BYTE
    sta poly_palette+2, X

    GET_PAL_BYTE                ; total=52c
    sta poly_palette+3, X

    ldx pal_dither_idx
    dex
    bne dither_loop

    .done_dither_loop
    jmp return_here_from_handle_beeb_palette
}
endif

\ ******************************************************************
\ *	UNROLLED SPAN PLOT ROUTINES
\ ******************************************************************

MACRO UNROLL_SPAN_ROW screen, row
    IF screen = 1
    row_addr = screen1_addr + row * SCREEN_ROW_BYTES
    ELSE
    row_addr = screen2_addr + row * SCREEN_ROW_BYTES
    ENDIF

    FOR col,31,0,-1
    sta row_addr + col*8, Y
    NEXT
    JMP return_here_from_unrolled_span_loop
ENDMACRO

UNROLLED_ROW_SIZE = 33 * 3

PAGE_ALIGN_FOR_SIZE 25
.span_row_table_screen1_LO
FOR row,0,24,1    ;SCREEN_HEIGHT_PIXELS-1,1
;row = y DIV 8
EQUB LO(span_screen1_row0_unrolled + row * UNROLLED_ROW_SIZE)
NEXT
CHECK_SAME_PAGE_AS span_row_table_screen1_LO

PAGE_ALIGN_FOR_SIZE 25
.span_row_table_screen1_HI
FOR row,0,24,1    ;SCREEN_HEIGHT_PIXELS-1,1
;row = y DIV 8
EQUB HI(span_screen1_row0_unrolled + row * UNROLLED_ROW_SIZE)
NEXT
CHECK_SAME_PAGE_AS span_row_table_screen1_HI

PAGE_ALIGN_FOR_SIZE 25
.span_row_table_screen2_LO
FOR row,0,24,1    ;SCREEN_HEIGHT_PIXELS-1,1
;row = y DIV 8
EQUB LO(span_screen2_row0_unrolled + row * UNROLLED_ROW_SIZE)
NEXT
CHECK_SAME_PAGE_AS span_row_table_screen2_LO

PAGE_ALIGN_FOR_SIZE 25
.span_row_table_screen2_HI
FOR row,0,24,1    ;SCREEN_HEIGHT_PIXELS-1,1
;row = y DIV 8
EQUB HI(span_screen2_row0_unrolled + row * UNROLLED_ROW_SIZE)
NEXT
CHECK_SAME_PAGE_AS span_row_table_screen2_HI

.span_screen1_row0_unrolled
UNROLL_SPAN_ROW 1, 0
.span_screen1_row1_unrolled
UNROLL_SPAN_ROW 1, 1
.span_screen1_row2_unrolled
UNROLL_SPAN_ROW 1, 2
.span_screen1_row3_unrolled
UNROLL_SPAN_ROW 1, 3
.span_screen1_row4_unrolled
UNROLL_SPAN_ROW 1, 4
.span_screen1_row5_unrolled
UNROLL_SPAN_ROW 1, 5
.span_screen1_row6_unrolled
UNROLL_SPAN_ROW 1, 6
.span_screen1_row7_unrolled
UNROLL_SPAN_ROW 1, 7
.span_screen1_row8_unrolled
UNROLL_SPAN_ROW 1, 8
.span_screen1_row9_unrolled
UNROLL_SPAN_ROW 1, 9
.span_screen1_row10_unrolled
UNROLL_SPAN_ROW 1, 10
.span_screen1_row11_unrolled
UNROLL_SPAN_ROW 1, 11
.span_screen1_row12_unrolled
UNROLL_SPAN_ROW 1, 12
.span_screen1_row13_unrolled
UNROLL_SPAN_ROW 1, 13
.span_screen1_row14_unrolled
UNROLL_SPAN_ROW 1, 14
.span_screen1_row15_unrolled
UNROLL_SPAN_ROW 1, 15
.span_screen1_row16_unrolled
UNROLL_SPAN_ROW 1, 16
.span_screen1_row17_unrolled
UNROLL_SPAN_ROW 1, 17
.span_screen1_row18_unrolled
UNROLL_SPAN_ROW 1, 18
.span_screen1_row19_unrolled
UNROLL_SPAN_ROW 1, 19
.span_screen1_row20_unrolled
UNROLL_SPAN_ROW 1, 20
.span_screen1_row21_unrolled
UNROLL_SPAN_ROW 1, 21
.span_screen1_row22_unrolled
UNROLL_SPAN_ROW 1, 22
.span_screen1_row23_unrolled
UNROLL_SPAN_ROW 1, 23
.span_screen1_row24_unrolled
UNROLL_SPAN_ROW 1, 24

.span_screen2_row0_unrolled
UNROLL_SPAN_ROW 2, 0
.span_screen2_row1_unrolled
UNROLL_SPAN_ROW 2, 1
.span_screen2_row2_unrolled
UNROLL_SPAN_ROW 2, 2
.span_screen2_row3_unrolled
UNROLL_SPAN_ROW 2, 3
.span_screen2_row4_unrolled
UNROLL_SPAN_ROW 2, 4
.span_screen2_row5_unrolled
UNROLL_SPAN_ROW 2, 5
.span_screen2_row6_unrolled
UNROLL_SPAN_ROW 2, 6
.span_screen2_row7_unrolled
UNROLL_SPAN_ROW 2, 7
.span_screen2_row8_unrolled
UNROLL_SPAN_ROW 2, 8
.span_screen2_row9_unrolled
UNROLL_SPAN_ROW 2, 9
.span_screen2_row10_unrolled
UNROLL_SPAN_ROW 2, 10
.span_screen2_row11_unrolled
UNROLL_SPAN_ROW 2, 11
.span_screen2_row12_unrolled
UNROLL_SPAN_ROW 2, 12
.span_screen2_row13_unrolled
UNROLL_SPAN_ROW 2, 13
.span_screen2_row14_unrolled
UNROLL_SPAN_ROW 2, 14
.span_screen2_row15_unrolled
UNROLL_SPAN_ROW 2, 15
.span_screen2_row16_unrolled
UNROLL_SPAN_ROW 2, 16
.span_screen2_row17_unrolled
UNROLL_SPAN_ROW 2, 17
.span_screen2_row18_unrolled
UNROLL_SPAN_ROW 2, 18
.span_screen2_row19_unrolled
UNROLL_SPAN_ROW 2, 19
.span_screen2_row20_unrolled
UNROLL_SPAN_ROW 2, 20
.span_screen2_row21_unrolled
UNROLL_SPAN_ROW 2, 21
.span_screen2_row22_unrolled
UNROLL_SPAN_ROW 2, 22
.span_screen2_row23_unrolled
UNROLL_SPAN_ROW 2, 23
.span_screen2_row24_unrolled
UNROLL_SPAN_ROW 2, 24
