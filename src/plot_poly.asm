\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	SPAN BUFFER POLYGON FILL ROUTINES
\ ******************************************************************

_UNROLL_SPAN_LOOP = TRUE
_USE_MEDIUM_SPAN_PLOT = FALSE

\ ******************************************************************
\ *	SPAN PLOTTING FUNCTIONS
\ ******************************************************************

\\ Can only be a maximum of 2 bytes plotted for short (<=5 pixel) spans
\\ X = [0,3] W = [1,5]

\\ p000 0000  pp00 0000  ppp0 0000  pppp 0000  pppp p000
\\ 0p00 0000  0pp0 0000  0ppp 0000  0ppp p000  0ppp pp00
\\ 00p0 0000  00pp 0000  00pp p000  00pp pp00  00pp ppp0
\\ 000p 0000  000p p000  000p pp00  000p ppp0  000p pppp

MACRO SHORT_MASK_SHIFTS p, e, table_index
FOR x,0,3,1
s=p>>x
h=(s AND &f0) >> 4
l=(s AND &0f)
IF table_index==0
EQUB (h OR h<<4) EOR e
ELIF table_index==1
EQUB (l OR l<<4) EOR e
ELSE
error "no"
ENDIF
NEXT
ENDMACRO

MACRO SHORT_MASK_TABLE e,table_index
SHORT_MASK_SHIFTS &80, e, table_index
SHORT_MASK_SHIFTS &c0, e, table_index
SHORT_MASK_SHIFTS &e0, e, table_index
SHORT_MASK_SHIFTS &f0, e, table_index
SHORT_MASK_SHIFTS &f8, e, table_index
ENDMACRO

.color_mask_short_0:SHORT_MASK_TABLE 0,0
.color_mask_short_1:SHORT_MASK_TABLE 0,1

.screen_mask_short_0:SHORT_MASK_TABLE $ff,0
.screen_mask_short_1:SHORT_MASK_TABLE $ff,1

.plot_short_span
{
    \\ A=span_width
    \\ X=span_start (pixel column)
    \\ Y=0

    \\ w = [1,5] x = [0,3]
    \\ index= ((w-1)*4+(xAND3))*2
    txa                             ; 2c
    and #3                          ; 2c

    ldx span_width
    adc minus_1_times_4, X
    tax

    lda color_mask_short_0, X
    and span_colour
    sta ora_byte1+1

    lda (writeptr), Y               ; 5c
    and screen_mask_short_0, X        ; 4c
    .ora_byte1
    ora #0                          ; 2c
    sta (writeptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y           ; 8c
    ENDIF

    lda color_mask_short_1, X
    beq done
    and span_colour
    sta ora_byte2+1

    ldy #8                          ; 2c
    lda (writeptr), Y               ; 5c
    and screen_mask_short_1, X        ; 4c
    .ora_byte2
    ora #0                          ; 2c
    sta (writeptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y               ; 6c
    ENDIF

    .done

    jmp return_here_from_plot_span
    ;rts
}

; Plot pixels from [span_start,span_end] on line span_y using span_colour
; Can optimise all of this later for poly fill, as shouldn't need to check
; start and end point, also can probably keep track of screen address without
; having to recalculate each time etc.

; This fn is called 3 million times across the entire sequence so every cycle counts!
.plot_span
\{
    ; X=span_start
    ; Y=span_y
    ; span_width already computed

    \\ Compute address of first screen byte
    clc                             ; 2c
    lda screen_row_LO, Y            ; 4c
    adc screen_col_LO, X            ; 4c
    sta writeptr                    ; 3c
    .plot_span_set_screen
    lda screen1_row_HI, Y           ; 4c
    adc screen_col_HI, X            ; 4c
    sta writeptr+1                  ; 3c

    ldy #0                          ; 2c

    \\ Check if the span is short...
    lda span_width                  ; 3c
    cmp #6                          ; 2c
    bcc plot_short_span     ; [1-5] ; 2/3c

IF _USE_MEDIUM_SPAN_PLOT
    \\ Long...
    cmp #10                         ; 2c
    bcs plot_long_span              ; 2/3c
    
    \\ Or medium...
    jmp plot_medium_span   ; [6-9]
ENDIF

    .plot_long_span
    \\ First byte
    \\ X=span_start
    txa                                     ; 2c
    and #3                                  ; 2c
    beq skip_first_byte                     ; 2/3c
    tax

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
    iny:sta (writeptr), Y:dey               ; 8c
    ENDIF

    \\ Subtract pixels from width
    sec                                     ; 2c
    lda span_width                          ; 3c
    sbc four_minus, X                       ; 4c
    sta span_width                          ; 3c

    \\ Increment column - can't overflow
    IF _UNROLL_SPAN_LOOP
    lda writeptr:adc #7:sta writeptr
    ELSE
    tya:clc:adc #8:tay
    ENDIF

    .skip_first_byte

    \\ Main body of span
    lda span_width                      ; 3c
    lsr a:lsr a                         ; 4c
    beq skip_span_loop                  ; 2/3c
    tax                                 ; 2c

IF _UNROLL_SPAN_LOOP = FALSE
    lda span_colour                     ; 3c
    sta load_span_colour+1              ; 4c
    clc                                 ; 2c
    .loop

    \\ Need to unroll this really
    .load_span_colour
    lda #0                              ; 2c
    sta (writeptr), Y                   ; 6c

    tya:adc #8:tay                      ; 6c
    dex                                 ; 2c
    bne loop                            ; 3c
    \\ 19c per byte
ELSE
    lda plot_span_unrolled_LO, X        ; 4c
    sta jmp_to_unrolled_span_loop+1     ; 4c
    lda plot_span_unrolled_HI, X        ; 4c
    sta jmp_to_unrolled_span_loop+2     ; 4c
    lda span_colour                     ; 3c
    .jmp_to_unrolled_span_loop
    jmp &ffff                           ; 3c + 3c
    .return_here_from_unrolled_span_loop
    \\ 22c overhead + 8c per byte

	lda next_line_offset,y:tay		; 6c
ENDIF

    .skip_span_loop
    \\ Last byte?
    lda span_width                      ; 3c
    and #3                              ; 2c
    beq skip_last_byte                  ; 2/3c
    tax                                 ; 2c

    lda span_colour                             ; 3c
    and screen_mask_starting_at_pixel, x        ; 4c
    sta ora_right_hand_byte+1                   ; 4c

    \\ Read screen byte
    lda (writeptr), Y                           ; 5c
    \\ Mask out pixels to be drawn
    and colour_mask_starting_at_pixel, X        ; 4c
    \\ Mask in our colour pixels
    .ora_right_hand_byte
    ora #0                                      ; 2c
    \\ Write to screen
    sta (writeptr), Y                           ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y                       ; 8c
    ENDIF
    .skip_last_byte

    .plot_span_return
    jmp return_here_from_plot_span
    ;rts
\}

IF _USE_MEDIUM_SPAN_PLOT
\\ Can only be a maximum of 3 bytes plotted for medium spans..
\\ X = [0,3] W = [6,9]

\\ pppp pp00 0000  pppp ppp0 0000  pppp pppp 0000  pppp pppp p000
\\ 0ppp ppp0 0000  0ppp pppp 0000  0ppp pppp p000  0ppp pppp pp00
\\ 00pp pppp 0000  00pp pppp p000  00pp pppp pp00  00pp pppp ppp0
\\ 000p pppp p000  000p pppp pp00  000p pppp ppp0  000p pppp pppp

MACRO MEDIUM_MASK_SHIFTS p, e, table_index
FOR x,0,3,1
s=p>>x
h=(s AND &f00) >> 8
m=(s AND &0f0) >> 4
l=(s AND &00f)
IF table_index==0
EQUB (h OR h<<4) EOR e
ELIF table_index==1
EQUB (m OR m<<4) EOR e
ELIF table_index==2
EQUB (l OR l<<4) EOR e
ELSE
ERROR "no"
ENDIF
NEXT
ENDMACRO

MACRO MEDIUM_MASK_TABLE e,table_index
MEDIUM_MASK_SHIFTS &FC0, e, table_index
MEDIUM_MASK_SHIFTS &FE0, e, table_index
MEDIUM_MASK_SHIFTS &FF0, e, table_index
MEDIUM_MASK_SHIFTS &FF8, e, table_index
ENDMACRO

.colour_mask_medium_0:MEDIUM_MASK_TABLE 0,0
.colour_mask_medium_1:MEDIUM_MASK_TABLE 0,1
.colour_mask_medium_2:MEDIUM_MASK_TABLE 0,2

.screen_mask_medium_0:MEDIUM_MASK_TABLE $ff,0
.screen_mask_medium_1:MEDIUM_MASK_TABLE $ff,1
.screen_mask_medium_2:MEDIUM_MASK_TABLE $ff,2

.plot_medium_span
{
    \\ w = [6,9] x = [0,3]
    \\ X= ((w-6)*4+(xAND3))*4
    txa
    and #3

    ldx span_width
    adc minus_6_times_4, X
    tax

    \\ Byte 1
    lda colour_mask_medium_0, X
    and span_colour
    sta ora_byte1+1

    lda (writeptr), Y               ; 5c
    and screen_mask_medium_0, X        ; 4c
    .ora_byte1
    ora #0                          ; 2c
    sta (writeptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y
    ENDIF

    \\ Byte 2
    lda colour_mask_medium_1, X
    and span_colour
    sta ora_byte2+1

    ldy #8
    lda (writeptr), Y               ; 5c
    and screen_mask_medium_1, X        ; 4c
    .ora_byte2
    ora #0                          ; 2c
    sta (writeptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y
    ENDIF

    \\ Byte 3
    lda colour_mask_medium_2, X
    beq done
    and span_colour
    sta ora_byte3+1

    ldy #16
    lda (writeptr), Y               ; 5c
    and screen_mask_medium_2, X        ; 4c
    .ora_byte3
    ora #0                          ; 2c
    sta (writeptr), Y               ; 6c

    IF _DOUBLE_PLOT_Y
    iny:sta (writeptr), Y
    ENDIF

    .done

    jmp return_here_from_plot_span
    ;rts
}
ENDIF

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
\{
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
    dex
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

    jmp drawline_into_span_buffer       ; JSR/RTS => JMP/JMP
    .return_here_from_drawline

    ldx poly_index
    dex
    bpl line_loop

    \\ Set our palette lookup
    lda poly_colour
    asl a:asl a
    sta load_palette+1

    \\ Plot the spans
    ldy span_buffer_max_y
    iny
    sty span_loop_max_y+1

    ldy span_buffer_min_y
    .span_loop
    sty poly_y                  ; 3c

    \\ v= on Master would happily burn a PAGE per palette to save 6c!
    tya:and #3:tax              ; 6c
    .load_palette
    lda poly_palette, X         ; 4c
    sta span_colour             ; 3c    <= where is this used?

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

    IF _HALF_VERTICAL_RES
    tya:asl a:tay               ; 6c
    ENDIF

    ;jsr plot_span
    jmp plot_span               ; eventually inline entire fn to save 6c
    .return_here_from_plot_span

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

    jmp return_here_from_plot_poly
\}

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
	LDY starty

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
    beq exitline

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
	ADC dy
	
	.branchleftright
	inc count			   ; inc/dec - self-modified to goingright or
						   ; goingleft
	JMP steeplineloop		; always taken
	
    .exit_early
    PLP:PLP

	.exitline
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
    beq exitline2

	.branchleftright2
	nop 					; inx/dex - self-modified to goingleft2 or
							; goingright2

	; check whether we move to the next line
	SBC dy
	BCS shallowlineloop_2

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
    ; Plot last 'pixel' into span buffer
    ;jmp plot_pixel_into_span_buffer
    UPDATE_SPAN_BUFFER_WITH_X FALSE
    jmp return_here_from_drawline
}

MACRO ONE_OR_MANY v     ; haven't decided yet! may x4
    EQUB v
ENDMACRO

IF _UNROLL_SPAN_LOOP
.plot_span_unrolled_LO
ONE_OR_MANY LO(span_loop_unrolled_0)
ONE_OR_MANY LO(span_loop_unrolled_1)
ONE_OR_MANY LO(span_loop_unrolled_2)
ONE_OR_MANY LO(span_loop_unrolled_3)
ONE_OR_MANY LO(span_loop_unrolled_4)
ONE_OR_MANY LO(span_loop_unrolled_5)
ONE_OR_MANY LO(span_loop_unrolled_6)
ONE_OR_MANY LO(span_loop_unrolled_7)
ONE_OR_MANY LO(span_loop_unrolled_8)
ONE_OR_MANY LO(span_loop_unrolled_9)
ONE_OR_MANY LO(span_loop_unrolled_10)
ONE_OR_MANY LO(span_loop_unrolled_11)
ONE_OR_MANY LO(span_loop_unrolled_12)
ONE_OR_MANY LO(span_loop_unrolled_13)
ONE_OR_MANY LO(span_loop_unrolled_14)
ONE_OR_MANY LO(span_loop_unrolled_15)
ONE_OR_MANY LO(span_loop_unrolled_16)
ONE_OR_MANY LO(span_loop_unrolled_17)
ONE_OR_MANY LO(span_loop_unrolled_18)
ONE_OR_MANY LO(span_loop_unrolled_19)
ONE_OR_MANY LO(span_loop_unrolled_20)
ONE_OR_MANY LO(span_loop_unrolled_21)
ONE_OR_MANY LO(span_loop_unrolled_22)
ONE_OR_MANY LO(span_loop_unrolled_23)
ONE_OR_MANY LO(span_loop_unrolled_24)
ONE_OR_MANY LO(span_loop_unrolled_25)
ONE_OR_MANY LO(span_loop_unrolled_26)
ONE_OR_MANY LO(span_loop_unrolled_27)
ONE_OR_MANY LO(span_loop_unrolled_28)
ONE_OR_MANY LO(span_loop_unrolled_29)
ONE_OR_MANY LO(span_loop_unrolled_30)
ONE_OR_MANY LO(span_loop_unrolled_31)
ONE_OR_MANY LO(span_loop_unrolled_32)

.plot_span_unrolled_HI
ONE_OR_MANY HI(span_loop_unrolled_0)
ONE_OR_MANY HI(span_loop_unrolled_1)
ONE_OR_MANY HI(span_loop_unrolled_2)
ONE_OR_MANY HI(span_loop_unrolled_3)
ONE_OR_MANY HI(span_loop_unrolled_4)
ONE_OR_MANY HI(span_loop_unrolled_5)
ONE_OR_MANY HI(span_loop_unrolled_6)
ONE_OR_MANY HI(span_loop_unrolled_7)
ONE_OR_MANY HI(span_loop_unrolled_8)
ONE_OR_MANY HI(span_loop_unrolled_9)
ONE_OR_MANY HI(span_loop_unrolled_10)
ONE_OR_MANY HI(span_loop_unrolled_11)
ONE_OR_MANY HI(span_loop_unrolled_12)
ONE_OR_MANY HI(span_loop_unrolled_13)
ONE_OR_MANY HI(span_loop_unrolled_14)
ONE_OR_MANY HI(span_loop_unrolled_15)
ONE_OR_MANY HI(span_loop_unrolled_16)
ONE_OR_MANY HI(span_loop_unrolled_17)
ONE_OR_MANY HI(span_loop_unrolled_18)
ONE_OR_MANY HI(span_loop_unrolled_19)
ONE_OR_MANY HI(span_loop_unrolled_20)
ONE_OR_MANY HI(span_loop_unrolled_21)
ONE_OR_MANY HI(span_loop_unrolled_22)
ONE_OR_MANY HI(span_loop_unrolled_23)
ONE_OR_MANY HI(span_loop_unrolled_24)
ONE_OR_MANY HI(span_loop_unrolled_25)
ONE_OR_MANY HI(span_loop_unrolled_26)
ONE_OR_MANY HI(span_loop_unrolled_27)
ONE_OR_MANY HI(span_loop_unrolled_28)
ONE_OR_MANY HI(span_loop_unrolled_29)
ONE_OR_MANY HI(span_loop_unrolled_30)
ONE_OR_MANY HI(span_loop_unrolled_31)
ONE_OR_MANY HI(span_loop_unrolled_32)

MACRO UNROLL_SPAN_LOOP n
    FOR i,0,n-1,1
        ldy #(i*8)
        sta (writeptr), y
        IF _DOUBLE_PLOT_Y
        iny:sta (writeptr), y
        ENDIF
    NEXT
    jmp return_here_from_unrolled_span_loop
ENDMACRO

.span_loop_unrolled_0
brk     ; shouldn't happen!
.span_loop_unrolled_1
UNROLL_SPAN_LOOP 1
.span_loop_unrolled_2
UNROLL_SPAN_LOOP 2
.span_loop_unrolled_3
UNROLL_SPAN_LOOP 3
.span_loop_unrolled_4
UNROLL_SPAN_LOOP 4
.span_loop_unrolled_5
UNROLL_SPAN_LOOP 5
.span_loop_unrolled_6
UNROLL_SPAN_LOOP 6
.span_loop_unrolled_7
UNROLL_SPAN_LOOP 7
.span_loop_unrolled_8
UNROLL_SPAN_LOOP 8
.span_loop_unrolled_9
UNROLL_SPAN_LOOP 9
.span_loop_unrolled_10
UNROLL_SPAN_LOOP 10
.span_loop_unrolled_11
UNROLL_SPAN_LOOP 11
.span_loop_unrolled_12
UNROLL_SPAN_LOOP 12
.span_loop_unrolled_13
UNROLL_SPAN_LOOP 13
.span_loop_unrolled_14
UNROLL_SPAN_LOOP 14
.span_loop_unrolled_15
UNROLL_SPAN_LOOP 15
.span_loop_unrolled_16
UNROLL_SPAN_LOOP 16
.span_loop_unrolled_17
UNROLL_SPAN_LOOP 17
.span_loop_unrolled_18
UNROLL_SPAN_LOOP 18
.span_loop_unrolled_19
UNROLL_SPAN_LOOP 19
.span_loop_unrolled_20
UNROLL_SPAN_LOOP 20
.span_loop_unrolled_21
UNROLL_SPAN_LOOP 21
.span_loop_unrolled_22
UNROLL_SPAN_LOOP 22
.span_loop_unrolled_23
UNROLL_SPAN_LOOP 23
.span_loop_unrolled_24
UNROLL_SPAN_LOOP 24
.span_loop_unrolled_25
UNROLL_SPAN_LOOP 25
.span_loop_unrolled_26
UNROLL_SPAN_LOOP 26
.span_loop_unrolled_27
UNROLL_SPAN_LOOP 27
.span_loop_unrolled_28
UNROLL_SPAN_LOOP 28
.span_loop_unrolled_29
UNROLL_SPAN_LOOP 29
.span_loop_unrolled_30
UNROLL_SPAN_LOOP 30
.span_loop_unrolled_31
UNROLL_SPAN_LOOP 31
.span_loop_unrolled_32
UNROLL_SPAN_LOOP 32
ENDIF
