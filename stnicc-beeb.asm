\ ******************************************************************
\ *	STNICC BEEB
\ ******************************************************************

_POLY_PLOT_END_POINTS = TRUE
_DOUBLE_BUFFER = TRUE

\ ******************************************************************
\ *	OS defines
\ ******************************************************************

osfile = &FFDD
oswrch = &FFEE
osasci = &FFE3
osbyte = &FFF4
osword = &FFF1
osfind = &FFCE
osgbpb = &FFD1
osargs = &FFDA

\\ Palette values for ULA
PAL_black	= (0 EOR 7)
PAL_blue	= (4 EOR 7)
PAL_red		= (1 EOR 7)
PAL_magenta = (5 EOR 7)
PAL_green	= (2 EOR 7)
PAL_cyan	= (6 EOR 7)
PAL_yellow	= (3 EOR 7)
PAL_white	= (7 EOR 7)

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO SWRAM_SELECT bank
{
    LDA #bank: sta &f4: sta &fe30
}
ENDMACRO

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

MAX_VERTS_PER_POLY = 7

SCREEN_ROW_BYTES = 256
SCREEN_WIDTH_PIXELS = 128
SCREEN_HEIGHT_PIXELS = 200
SCREEN_SIZE_BYTES = (SCREEN_WIDTH_PIXELS * SCREEN_HEIGHT_PIXELS) / 4

screen1_addr = &8000 - SCREEN_SIZE_BYTES
screen2_addr = screen1_addr - SCREEN_SIZE_BYTES

FLAG_CLEAR_SCREEN = 1
FLAG_CONTAINS_PALETTE = 2
FLAG_INDEXED_DATA = 4

POLY_DESC_END_OF_FRAME = &FD
POLY_DESC_SKIP_TO_64K = &FE
POLY_DESC_END_OF_DATA = &FF

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD &9F

; vars for plot_span
.writeptr           skip 2
.span_start         skip 1
.span_end           skip 1
.span_width         skip 1
.span_y             skip 1
.span_colour        skip 1

; vars for drawline
.startx             skip 1
.starty             skip 1
.endx               skip 1
.endy               skip 1
.count              skip 1
.accum              skip 1
.scrn               skip 2
.dx                 skip 1
.dy                 skip 1
.temp               skip 1

; vars for plot_poly
.poly_num_verts     skip 1
.poly_colour        skip 1
.poly_index         skip 1
.poly_verts_x       skip MAX_VERTS_PER_POLY+1
.poly_verts_y       skip MAX_VERTS_PER_POLY+1

; vars for span_buffer
.span_buffer_min_y  skip 1
.span_buffer_max_y  skip 1
.temp_x             skip 1
.temp_y             skip 1

; frame parser
.frame_no           skip 2
.frame_flags        skip 1
.frame_bitmask      skip 2
.indexed_num_verts  skip 1
.poly_descriptor    skip 1

; system vars
.rom_bank           skip 1
.vsyncs             skip 2
.draw_buffer_HI     skip 1
.disp_buffer        skip 2

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1100
GUARD &4000   			; ensure code size doesn't hit start of screen memory

.start

.main_start

\ ******************************************************************
\ *	Code entry
\ ******************************************************************

.main
{
	\\ Set interrupts

	SEI							; disable interupts
	LDA #&7F					; A=01111111
	STA &FE4E					; R14=Interrupt Enable (disable all interrupts)
	STA &FE43					; R3=Data Direction Register "A" (set keyboard data direction)
	LDA #&C2					; A=11000010
	STA &FE4E					; R14=Interrupt Enable (enable main_vsync and timer interrupt)
	CLI							; enable interupts

    \\ Init ZP
    lda #0
    ldx #0
    .zp_loop
    sta &00, x
    inx
    cpx #&A0
    bne zp_loop

    \\ Set MODE 5

    lda #22
    jsr oswrch
    lda #5
    jsr oswrch

    \\ Resolution 256x200 => 128x200
    lda #1:sta &fe00:lda #32:sta &fe01
    lda #6:sta &fe00:lda #24:sta &fe01
    lda #8:sta &fe00:lda #&C0:sta &fe01  ; cursor off

    \\ Load SWRAM data
    SWRAM_SELECT 4
    lda #HI(&8000)
    ldx #LO(filename0)
    ldy #HI(filename0)
    jsr disksys_load_file

    \\ Load SWRAM data
    SWRAM_SELECT 5
    lda #HI(&8000)
    ldx #LO(filename1)
    ldy #HI(filename1)
    jsr disksys_load_file

    \\ Load SWRAM data
    SWRAM_SELECT 6
    lda #HI(&8000)
    ldx #LO(filename2)
    ldy #HI(filename2)
    jsr disksys_load_file

    \\ Load SWRAM data
    SWRAM_SELECT 7
    lda #HI(&8000)
    ldx #LO(filename3)
    ldy #HI(filename3)
    jsr disksys_load_file

    SWRAM_SELECT 4
    sta rom_bank

    \\ Init system
    lda #HI(screen1_addr)
    sta draw_buffer_HI

    \\ Clear screen
    jsr screen_cls

;    jsr test_plot_span
;    jsr test_drawline
;    jsr test_plot_poly

    .loop
    \\ Wait vsync
    lda #19
    jsr osbyte

    \\ Set display buffer
    lda #0:sta disp_buffer
    lda draw_buffer_HI
    IF _DOUBLE_BUFFER
    eor #HI(screen1_addr EOR screen2_addr)
    ENDIF
    sta disp_buffer+1

    clc
    lsr disp_buffer+1:ror disp_buffer
    lsr disp_buffer+1:ror disp_buffer
    lsr disp_buffer+1:ror disp_buffer

    lda #12:sta &fe00
    lda disp_buffer+1:sta &fe01
    lda #13:sta &fe00
    lda disp_buffer:sta &fe01

    jsr parse_frame

    \\ Toggle draw buffer
    lda draw_buffer_HI
    IF _DOUBLE_BUFFER
    eor #HI(screen1_addr EOR screen2_addr)
    sta draw_buffer_HI
    ENDIF

    jmp loop

	\\ Re-enable useful interupts

	LDA #&D3					; A=11010011
	STA &FE4E					; R14=Interrupt Enable
    CLI

	\\ Exit gracefully (in theory)

	RTS
}

.main_end

.fx_start

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
    \\ Check span_start < span_end
    bcs posdx
    ldx span_end
    eor #&ff
    adc #1
    .posdx
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

    \\ Check if the span is 
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
    ldx span_width
    .loop
    cpx #4
    bcc end_loop

    lda span_colour
    sta (writeptr), Y

    tya:clc:adc #8:tay
    dex:dex:dex:dex
    jmp loop
    .end_loop

    \\ Last byte
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

.test_plot_span
{
    lda #7: sta span_start
    lda #64: sta span_end
    lda #0: sta span_y
    lda #&ff: sta span_colour

    .loop
    jsr plot_span

    ldx span_end
    inx
    txa
    and #127
    sta span_end
    
    inc span_y
    bne loop
    rts
}

.drawline
{
	; calc screen address of (startx, starty)
	LDA startx
	AND #&FC        ; MODE4=&F8
    ASL A           ; not MODE4
	STA scrn
	LDA starty
	LSR A
	LSR A
	LSR A
	CLC
	ADC draw_buffer_HI
	STA scrn+1
	
	; calc screen row of starty
	LDA starty
	AND #7
	TAY
	
	; calc pixel within byte of startx
	LDA startx
	AND #3          ; MODE4=7
	TAX
	
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

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	LDA #goingdown-branchupdown-2
	BCC P%+4
	LDA #goingup-branchupdown-2
	STA branchupdown+1
	
	PLP
	LDA #goingright-branchleftright-2
	BCC P%+4
	LDA #goingleft-branchleftright-2
	STA branchleftright+1

	; initialise accumulator for 'steep' line
	LDA dy
	STA count
	LSR A

.steeplineloop

	STA accum
	
	; plot pixel
	LDA (scrn),Y
	EOR pixels_mode5,X      ; MODE4
	STA (scrn),Y
	
	; check if done
	DEC count
	.branchupdown
	BNE P%			; self-modified to goingdown or goingup
	.exitline
	RTS

    .exit_early
    PLP:PLP:RTS
	
	; move up to next line
	.goingup
	DEY
	BPL movetonextcolumn
	DEC scrn+1
	LDY #7
	BNE movetonextcolumn	; always taken
	
	; move down to next line
	.goingdown
	INY
	CPY #8
	BCC movetonextcolumn
	INC scrn+1
	LDY #0
	
	; check move to next pixel column
	.movetonextcolumn
	SEC
	LDA accum
	SBC dx
	BCS steeplineloop
	ADC dy
	
	.branchleftright
	BCS P%					; self-modified to goingright or goingleft
	
	; move left to next pixel column
	.goingleft
	DEX
	BPL steeplineloop
	STA accum
	LDA scrn
	SBC #8					; C set
	STA scrn
	LDX #3                  ; MODE4=7
	BNE steeplineloop+2		; always taken
	
	; move right to next pixel column
	.goingright
	INX
	CPX #4                  ; MODE4=8
	BCC steeplineloop
	STA accum
	LDA scrn
	ADC #7
	STA scrn
	LDX #0
	BEQ steeplineloop+2		; always taken
	
.shallowline

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	LDA #goingdown2-branchupdown2-2
	BCC P%+4
	LDA #goingup2-branchupdown2-2
	STA branchupdown2+1
	
	PLP
	LDA #goingright2-branchleftright2-2
	BCC P%+4
	LDA #goingleft2-branchleftright2-2
	STA branchleftright2+1

	; initialise accumulator for 'steep' line
	LDA dx
	STA count
	LSR A
	STA accum	

.shallowlineloop

	; cache byte from destination screen address
	LDA (scrn),Y
	
.shallowlineloop2

	; plot pixel in cached byte
	EOR pixels_mode5,X      ; MODE4
	
	; check if done
	DEC count
	.branchleftright2
	BNE P%					; self-modified to goingleft2 or goingright2
	STA (scrn),Y
	.exitline2
	RTS
	
	; move left to next pixel column
	.goingleft2
	DEX
	BPL movetonextline
	STA (scrn),Y			; store cached byte, advance screen address, and cache new byte
	LDA scrn
	SEC
	SBC #8
	STA scrn
	LDA (scrn),Y
	LDX #3                  ; MODE4=7
	BNE movetonextline		; always taken
	
	; move right to next pixel column
	.goingright2
	INX
	CPX #4                  ; MODE4=8
	BCC movetonextline
	STA (scrn),Y
	LDA scrn
	ADC #7
	STA scrn
	LDA (scrn),Y
	LDX #0
	
	; check whether we move to the next line
	.movetonextline
	STA temp
	SEC
	LDA accum
	SBC dy
	.branchupdown2
	BCC P%					; self-modified to goingdown2 or goingup2
	STA accum
	LDA temp
	BCS shallowlineloop2	; always taken
	
	; move down to next line
	.goingdown2
	ADC dx
	STA accum				; store new accumulator
	LDA temp
	STA (scrn),Y			; and store cached screen byte before moving to a new one
	INY
	CPY #8
	BCC shallowlineloop
	INC scrn+1
	LDY #0
	BEQ shallowlineloop		; always taken
	
	; move up to next line
	.goingup2
	ADC dx
	STA accum
	LDA temp
	STA (scrn),Y
	DEY
	BPL shallowlineloop
	DEC scrn+1
	LDY #7
	BNE shallowlineloop		; always taken
			
.pixels_mode4
	EQUB 128, 64, 32, 16, 8, 4, 2, 1
.pixels_mode5
    EQUB &88,&44,&22,&11
}

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

.test_drawline
{
    lda #0
    sta startx
    lda #119
    sta starty

    lda #127
    sta endx
    lda #146
    sta endy

    jsr drawline
    rts
}

.test_plot_poly
{
    ldx num_verts
    stx poly_num_verts

    ldy #0
    ldx #0
    .loop
    lda vertex_data, Y
    lsr a
    sta poly_verts_x, X
    iny
    lda vertex_data, Y
    sta poly_verts_y, X
    iny
    inx
    cpx num_verts
    bne loop

;    jsr plot_poly_line
    lda #15
    sta poly_colour
    jsr plot_poly_span

    rts

    .num_verts
    EQUB 4
    .vertex_data
    EQUB 128,7
    EQUB 255,64
    EQUB 128,188
    EQUB 0,128
}

.plot_poly_span
{
    jsr init_span_buffer

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

    \\ Check whether we only have a single pixel (plot anyway?)
    lda span_buffer_HI, Y
    cmp #HI(span_buffer_end)
    bne do_span

    \\ Plot pixel
    ldx span_start
    jsr plot_pixel
    jmp skip_span

    .do_span
    lda span_buffer_end, Y
    sta span_end

    \\ Shouldn't have blank spans now we have min/max Y
    ;ora span_start
    ;beq skip_span

    jsr plot_span

    .skip_span
    ldy span_y
    iny
    cpy span_buffer_max_y
    bcc span_loop

    rts
}

.init_span_buffer
{
    lda #255
    sta span_buffer_min_y
    lda #0
    sta span_buffer_max_y
IF 0
    ldy #0
    .loop
    lda #HI(span_buffer_start)
    sta span_buffer_HI, Y
    iny
    bne loop        ; actullay only needs to be 200
ELSE
    ldy #0
    .loop
    lda #255
    sta span_buffer_start, Y
    lda #0
    sta span_buffer_end, Y
    iny
    bne loop
ENDIF

    rts
}

.plot_pixel_into_span_buffer
{
IF 0
    ; in this context means plotting the X value into our current span buffer for this Y
    lda span_buffer_HI, Y
    sta writeptr+1
    txa
    sta (writeptr), Y
    \\ SUPER ANNOYING!
    sty temp_y:stx temp_x:ldx temp_y
    inc span_buffer_HI, X
    ldx temp_x
    rts
ELSE
    txa
    cmp span_buffer_start, Y
    bcs not_smaller
    sta span_buffer_start, Y

    .not_smaller
    cmp span_buffer_end, Y
    bcc not_larger
    sta span_buffer_end, Y

    .not_larger
    rts
ENDIF
}

.drawline_into_span_buffer
{
	; calc screen row of starty
	LDY starty
	; calc pixel within byte of startx
	LDX startx

    lda #0
    sta writeptr
	
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
	LDA #goingdown-branchupdown-2
	BCC P%+4
	LDA #goingup-branchupdown-2
	STA branchupdown+1
	
	PLP
	LDA #goingright-branchleftright-2
	BCC P%+4
	LDA #goingleft-branchleftright-2
	STA branchleftright+1

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
    jsr plot_pixel_into_span_buffer

	; check if done
	DEC count
	.branchupdown
	BNE P%			; self-modified to goingdown or goingup
	.exitline
	RTS
	
    .exit_early
    PLP:PLP
    RTS

	; move up to next line
	.goingup
	DEY
	JMP movetonextcolumn	; always taken
	
	; move down to next line
	.goingdown
	INY
	; check move to next pixel column
	.movetonextcolumn
	SEC
	LDA accum
	SBC dx
	BCS steeplineloop
	ADC dy
	
	.branchleftright
	BCS P%					; self-modified to goingright or goingleft
	
	; move left to next pixel column
	.goingleft
	DEX
	JMP steeplineloop
	
	; move right to next pixel column
	.goingright
	INX
	JMP steeplineloop		; always taken
	
.shallowline

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	LDA #goingdown2-branchupdown2-2
	BCC P%+4
	LDA #goingup2-branchupdown2-2
	STA branchupdown2+1
	
	PLP
	LDA #goingright2-branchleftright2-2
	BCC P%+4
	LDA #goingleft2-branchleftright2-2
	STA branchleftright2+1

	; initialise accumulator for 'steep' line
	LDA dx
	STA count
	LSR A
	STA accum

IF _POLY_PLOT_END_POINTS
    INC count
ENDIF

.shallowlineloop

	; cache byte from destination screen address
	; doesn't mean anything in our context
	
.shallowlineloop2

	; plot pixel in cached byte
	; doesn't mean anything in our context
	
	; check if done
	DEC count
	.branchleftright2
	BNE P%					; self-modified to goingleft2 or goingright2

    ; in this context means plotting the X value into our current span buffer for this Y
    jsr plot_pixel_into_span_buffer

	.exitline2
	RTS
	
	; move left to next pixel column
	.goingleft2
	DEX
	JMP movetonextline		; always taken
	
	; move right to next pixel column
	.goingright2
	INX
	
	; check whether we move to the next line
	.movetonextline
	SEC
	LDA accum
	SBC dy
	.branchupdown2
	BCC P%					; self-modified to goingdown2 or goingup2
	STA accum
	BCS shallowlineloop2	; always taken
	
	; move down to next line
	.goingdown2
	ADC dx
	STA accum				; store new accumulator

    ; in this context means plotting the X value into our current span buffer for this Y
    jsr plot_pixel_into_span_buffer

	INY
	JMP shallowlineloop		; always taken
	
	; move up to next line
	.goingup2
	ADC dx
	STA accum

    ; in this context means plotting the X value into our current span buffer for this Y
    jsr plot_pixel_into_span_buffer

	DEY
	JMP shallowlineloop		; always taken
}

.screen_cls
{
    lda draw_buffer_HI
    sta loop+2
    ldx #HI(SCREEN_SIZE_BYTES)
    lda #0
    ldy #0
    .loop
    sta &FF00, y
    iny
    bne loop
    inc loop+2
    dex
    bne loop
    rts
}

.get_byte
    lda &8000
    inc get_byte+1
    bne get_byte_return
    inc get_byte+2

    \\ Optimise me!
    pha
    lda get_byte+2
    cmp #&C0
    bcc same_bank

    inc rom_bank
    lda rom_bank
    sta &f4:sta &fe30 

    lda #&80
    sta get_byte+2

    .same_bank
    pla

    .get_byte_return
    rts

.parse_frame
{
    jsr get_byte
    sta frame_flags

    and #FLAG_CLEAR_SCREEN
    beq no_clear_screen

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
    cmp #POLY_DESC_END_OF_FRAME
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
    jsr plot_poly_span
    jmp read_poly_data

    .end_of_frame
    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

    rts
}

.fx_end

\ ******************************************************************
\ *	LIBRARIES
\ ******************************************************************

include "lib/disksys.asm"

\ ******************************************************************
\ *	DATA
\ ******************************************************************

.data_start

IF 0
.crtc_regs_default
{
	EQUB 127				; R0  horizontal total
	EQUB 80					; R1  horizontal displayed
	EQUB 98					; R2  horizontal position
	EQUB &28				; R3  sync width 40 = &28
	EQUB 38					; R4  vertical total
	EQUB 0					; R5  vertical total adjust
	EQUB 32					; R6  vertical displayed
	EQUB 35					; R7  vertical position; 35=top of screen
	EQUB &0					; R8  interlace; &30 = HIDE SCREEN
	EQUB 7					; R9  scanlines per row
	EQUB 32					; R10 cursor start
	EQUB 8					; R11 cursor end
	EQUB HI(screen_addr/8)	; R12 screen start address, high
	EQUB LO(screen_addr/8)	; R13 screen start address, low
}

.palette
{
	EQUB &00 + PAL_black
	EQUB &10 + PAL_black
	EQUB &20 + PAL_red
	EQUB &30 + PAL_red
	EQUB &40 + PAL_black
	EQUB &50 + PAL_black
	EQUB &60 + PAL_red
	EQUB &70 + PAL_red
	EQUB &80 + PAL_yellow
	EQUB &90 + PAL_yellow
	EQUB &A0 + PAL_white
	EQUB &B0 + PAL_white
	EQUB &C0 + PAL_yellow
	EQUB &D0 + PAL_yellow
	EQUB &E0 + PAL_white
	EQUB &F0 + PAL_white
}
ENDIF

ALIGN &100  ; lazy
.poly_palette
{
    EQUB &00,&00,&00,&00        ; black
    EQUB &0F,&0F,&0F,&0F        ; colour 1
    EQUB &F0,&F0,&F0,&F0        ; colour 2
    EQUB &FF,&FF,&FF,&FF        ; colour 3
    EQUB &05,&00,&0A,&00        ; colour 1.1
    EQUB &05,&0A,&05,&0A        ; colour 1.2
    EQUB &05,&0F,&0A,&0F        ; colour 1.3
    EQUB &0F,&00,&0F,&00        ; stripe 1
    EQUB &50,&00,&A0,&00        ; colour 2.1
    EQUB &50,&A0,&50,&A0        ; colour 2.2
    EQUB &50,&F0,&A0,&F0        ; colour 2.3
    EQUB &F0,&00,&F0,&00        ; stripe 2
    EQUB &55,&00,&AA,&00        ; colour 3.1
    EQUB &55,&AA,&55,&AA        ; colour 3.2
    EQUB &55,&FF,&AA,&FF        ; colour 3.3
    EQUB &FF,&00,&FF,&00        ; stripe 3
}

.screen_mask_starting_at_pixel
{
    EQUB %00000000
    EQUB %10001000
    EQUB %11001100
    EQUB %11101110
}

.colour_mask_starting_at_pixel
{
    EQUB %11111111
    EQUB %01110111
    EQUB %00110011
    EQUB %00010001
}

.screen_mask_pixel
{
    EQUB %01110111
    EQUB %10111011
    EQUB %11011101
    EQUB %11101110
}

.colour_mask_pixel
{
    EQUB %10001000
    EQUB %01000100
    EQUB %00100010
    EQUB %00010001
}

.four_minus
{
    EQUB 4,3,2,1
}

.filename0
EQUS "00", 13
.filename1
EQUS "01", 13
.filename2
EQUS "02", 13
.filename3
EQUS "03", 13

ALIGN &100
.screen_row_LO
FOR n,0,255,1
row=n DIV 8:sl=n MOD 8
addr=row * SCREEN_ROW_BYTES + sl
EQUB LO(addr)
NEXT

.screen_row_HI
FOR n,0,255,1
row=n DIV 8:sl=n MOD 8
addr=row * SCREEN_ROW_BYTES + sl
EQUB HI(addr)
NEXT

.screen_col_LO
FOR n,0,255,1
col=n DIV 4
EQUB LO(col*8)
NEXT

.screen_col_HI
FOR n,0,255,1
col=n DIV 4
EQUB HI(col*8)
NEXT

.data_end

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "STNICC", start, end

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

.bss_start

.vertices_x
skip &100
.vertices_y
skip &100

ALIGN &100
.span_buffer_HI
skip &100
.span_buffer_start
skip &100
.span_buffer_end
skip &100

.bss_end

\ ******************************************************************
\ *	Memory Info
\ ******************************************************************

PRINT "------"
PRINT "RASTER FX"
PRINT "------"
PRINT "MAIN size =", ~main_end-main_start
PRINT "FX size = ", ~fx_end-fx_start
PRINT "DATA size =",~data_end-data_start
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~screen2_addr-P%
PRINT "------"

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************

PUTFILE "data/scene1_beeb.00.bin", "00", &8000
PUTFILE "data/scene1_beeb.01.bin", "01", &8000
PUTFILE "data/scene1_beeb.02.bin", "02", &8000
PUTFILE "data/scene1_beeb.03.bin", "03", &8000
PUTFILE "data/scene1_beeb.04.bin", "04", &8000
PUTFILE "data/scene1_beeb.05.bin", "05", &8000
PUTFILE "data/scene1_beeb.06.bin", "06", &8000
PUTFILE "data/scene1_beeb.07.bin", "07", &8000
