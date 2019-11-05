\ ******************************************************************
\ *	STNICC BEEB
\ ******************************************************************

_DEBUG = TRUE
_POLY_PLOT_END_POINTS = TRUE
_DOUBLE_BUFFER = TRUE
_CHECK_LOAD_BUFFER = FALSE
_LOAD_TO_SWRAM = FALSE
_PLOT_WIREFRAME = FALSE

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

IRQ1V = &204

\\ Palette values for ULA
PAL_black	= (0 EOR 7)
PAL_blue	= (4 EOR 7)
PAL_red		= (1 EOR 7)
PAL_magenta = (5 EOR 7)
PAL_green	= (2 EOR 7)
PAL_cyan	= (6 EOR 7)
PAL_yellow	= (3 EOR 7)
PAL_white	= (7 EOR 7)

DFS_sector_size = 256
DFS_sectors_per_track = 10
DFS_sectors_to_load = 10
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

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

DISK1_drive_no = 0			; for loop!
DISK1_first_track = 2		; the track at which the video file is located on side 0; all tracks prior to this are reserved for code
DISK1_last_track = 80		; could potentially deduce these from DFS catalog

DISK2_drive_no = 2			; should be 2
DISK2_first_track = 1
DISK2_last_track = 79		; doesn't actually matter as data stream should indicate end of buffer

STREAM_buffer_size = (3 * DFS_track_size)

FLAG_CLEAR_SCREEN = 1
FLAG_CONTAINS_PALETTE = 2
FLAG_INDEXED_DATA = 4

POLY_DESC_END_OF_STREAM = &FD
POLY_DESC_SKIP_TO_64K = &FE
POLY_DESC_END_OF_FRAME = &FF

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

; frame parser
.frame_no           skip 2
.frame_flags        skip 1
.frame_bitmask      skip 2
.indexed_num_verts  skip 1
.poly_descriptor    skip 1

; system vars
.rom_bank           skip 1
.vsync_counter      skip 2
.last_vsync         skip 1
.draw_buffer_HI     skip 1
.disp_buffer        skip 2
.sector_no          skip 1
.track_no			skip 1
.load_to_HI			skip 1
.error_flag			skip 1

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

ORG &300
GUARD &800
.vertices_x
skip &100
.vertices_y
skip &100
.span_buffer_start
skip &100
.span_buffer_end
skip &100

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

    LDA IRQ1V:STA old_irqv
    LDA IRQ1V+1:STA old_irqv+1

    LDA #LO(irq_handler):STA IRQ1V
    LDA #HI(irq_handler):STA IRQ1V+1		; set interrupt handler
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

IF _LOAD_TO_SWRAM
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

ELSE

	\\ Load our entire stream buffer from first track

	LDA #DISK1_first_track
	STA track_no

	LDA #HI(STREAM_buffer_start)
	STA load_to_HI

	\\ Fill entire buffer
	{
		.loop
		JSR load_next_track
		
		LDA error_flag
		BNE read_error
	
		LDA load_to_HI
		CMP #HI(STREAM_buffer_start)			; wrapped means buffer filled
		BNE loop

        .read_error
	}
ENDIF

    \\ Init system
    lda #HI(screen1_addr)
    sta draw_buffer_HI

    \\ Clear screen
    jsr screen_cls
    jsr init_span_buffer

    IF 0
    {
        \\ Set test screen
        lda #0:sta disp_buffer
        lda draw_buffer_HI
        sta disp_buffer+1

        clc
        lsr disp_buffer+1:ror disp_buffer
        lsr disp_buffer+1:ror disp_buffer
        lsr disp_buffer+1:ror disp_buffer

        lda #12:sta &fe00
        lda disp_buffer+1:sta &fe01
        lda #13:sta &fe00
        lda disp_buffer:sta &fe01

    ;    jsr test_drawline
        jsr test_plot_poly
    ;    jsr test_plot_span
        rts
    }
    ENDIF

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

    cmp #POLY_DESC_SKIP_TO_64K
    bne stream_ok
    
    \\ Align to next page
    lda #&ff:sta STREAM_ptr_LO

    .stream_ok
    cmp #POLY_DESC_END_OF_STREAM
    beq track_load_error

IF _DEBUG
    sec
    lda vsync_counter
    sbc last_vsync
    jsr debug_write_A

    lda vsync_counter
    sta last_vsync
ENDIF

    \\ Toggle draw buffer
    lda draw_buffer_HI
    IF _DOUBLE_BUFFER
    eor #HI(screen1_addr EOR screen2_addr)
    sta draw_buffer_HI
    ENDIF

	\\ Which page are we reading crunched data from?
	sec
	lda STREAM_ptr_HI

	\\ Is it more than a "track" away?
	SBC load_to_HI
	.sectors_to_load_1
	CMP #DFS_sectors_to_load
	BCC not_ready_to_load

	\\ If so, load a track's worth of data into our buffer
	JSR load_next_track

	.not_ready_to_load
	\\ Check for errors
	LDA error_flag
	BNE track_load_error

    jmp loop
    .track_load_error

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

.test_plot_span
{
    lda #123: sta span_start
    lda #64: sta span_end
    lda #0: sta span_y
    lda #&0f: sta span_colour

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
	ORA pixels_mode5,X      ; MODE4 EOR
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
	ORA pixels_mode5,X      ; MODE4 EOR
	
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
    lda #1
    sta poly_colour
    jsr plot_poly_span

    rts

    .num_verts
    EQUB 4
    .vertex_data
    EQUB 31,189
    EQUB 120,190
    EQUB 127,64
    EQUB 32,7
}

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

.plot_pixel_into_span_buffer
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
    rts
}

.exit_early
    PLP:PLP
    RTS

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
    jsr plot_pixel_into_span_buffer

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
    jsr plot_pixel_into_span_buffer

.shallowlineloop

	; cache byte from destination screen address
	; doesn't mean anything in our context
	
.shallowlineloop2

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
	BCS shallowlineloop2	; always taken
	
	; move down to next line
	.movetonextline
	.goingdown2
	ADC dx
	STA accum				; store new accumulator

    ; Plot 'pixel' for end of span on current line
    jsr plot_pixel_into_span_buffer

    .branchupdown2
	nop		                ; self-modified to goingdown2 or goingup2

    ; Plot 'pixel' for start of span on next line
    jsr plot_pixel_into_span_buffer

	JMP shallowlineloop		; always taken

	.exitline2
    ; Plot last 'pixel' into span buffer
    jmp plot_pixel_into_span_buffer
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

\\ Move me to ZP!
.get_byte
    inc STREAM_ptr_LO
    bne get_byte_from_stream
    inc STREAM_ptr_HI

IF _CHECK_LOAD_BUFFER
	\\ Are we reading from the same page we intend to load at next?
    lda STREAM_ptr_HI
	cmp load_to_HI
	bne not_caught_up

	\\ If so then we have caught up with the disk load and run out of data
	\\ So bomb out with an error:
	inc error_flag
	lda #0
	rts

	.not_caught_up
ENDIF

	\\ Have we gone over the end of our stream buffer?
    lda STREAM_ptr_HI
	cmp #HI(STREAM_buffer_end)
	bcc get_byte_from_stream

	\\ If so then wrap around to the beginning
	lda #HI(STREAM_buffer_start)
    sta STREAM_ptr_HI

    .get_byte_from_stream
    lda STREAM_buffer_start-1

STREAM_ptr_LO = get_byte_from_stream+1
STREAM_ptr_HI = get_byte_from_stream+2

    .get_byte_return
    rts

IF 0
    \\ SWRAM reader
    lda get_byte+2
    cmp #&C0
    bcc same_bank

    inc rom_bank
    lda rom_bank
    sta &f4:sta &fe30 

    lda #&80
    sta get_byte+2
    .same_bank
ENDIF

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

.fx_end

\ ******************************************************************
\ *	LIBRARIES
\ ******************************************************************

include "lib/disksys.asm"

\ ******************************************************************
\ *	File loading routines
\ ******************************************************************

.load_next_track
\\{
	\\ Track &FF indicates no more reading
	LDA track_no
	BMI TRACK_LOAD_return

	\\ Store track no in params block
	STA osword_params_track

	LDA sector_no
	STA osword_params_sector

	\\ Update load address in params block
	LDA load_to_HI
	STA osword_params_address+1
	
	\\ Make DFS read multi-sector call
	LDX #LO(osword_params)
	LDY #HI(osword_params)
	LDA #&7F
	JSR osword

	\\ Error value returned at end of params block
	LDA osword_params_return
	STA error_flag

	\\ Next track
	CLC
	LDA sector_no
	.sectors_to_load_2
	ADC #DFS_sectors_to_load
	CMP #DFS_sectors_per_track
	BCC same_track
	SBC #DFS_sectors_per_track
	INC track_no
	.same_track
	STA sector_no

	\\ Which disk?
	LDA osword_params_drive
	BEQ TRACK_LOAD_disk_1							; assumes we start on drive 0

	\\ Disk N
	LDA track_no
	CMP #DISK2_last_track
	BNE TRACK_LOAD_no_swap_disk

	\\ Reached end of disk N
;	LDA #&FF
;	STA track_no
;	BNE TRACK_LOAD_no_wrap				; and store &FF in load_to_HI
    JMP TRACK_LOAD_disk_N

	\\ Disk 1
	.TRACK_LOAD_disk_1
	LDA track_no
	CMP #DISK1_last_track
	BNE TRACK_LOAD_no_swap_disk

	\\ Reached end of disk 1 so swap drives
	.TRACK_LOAD_disk_N
    LDX osword_params_drive
	LDA drive_order, X
	STA osword_params_drive

	\\ Reset track to start of disk 2
	LDA #DISK2_first_track
	STA track_no

	.TRACK_LOAD_no_swap_disk

	\\ Increment our load ptr
	CLC
	LDA load_to_HI
	.sectors_to_load_3
	ADC #DFS_sectors_to_load

	\\ Have we fallen off the end of the buffer?
	CMP #HI(STREAM_buffer_end)
	BNE TRACK_LOAD_no_wrap

	\\ If so then reset to start
	LDA #HI(STREAM_buffer_start)

	.TRACK_LOAD_no_wrap
	STA load_to_HI

	.TRACK_LOAD_return
	RTS
\\}

IF _DEBUG
.debug_plot_glyph
{
    asl a:asl a:asl a
    clc
    adc #LO(debug_font)
    sta read_glyph_data+1
    lda #HI(debug_font)
    adc #0
    sta read_glyph_data+2

    ldy #7
    .loop

    .read_glyph_data
    lda &ffff,y
    .write_glyph_data
    sta (writeptr),y

    dey
    bpl loop

    rts
}

.debug_write_A
{
    pha:pha
    lda draw_buffer_HI
    sta writeptr+1
    lda #0
    sta writeptr

    pla
    lsr a:lsr a:lsr a:lsr a
    jsr debug_plot_glyph

    lda writeptr
    clc
    adc #8
    sta writeptr

    pla
    and #&f
    jmp debug_plot_glyph
}
ENDIF

.irq_handler
{
	LDA &FC
	PHA

	\\ Which interrupt?
;	LDA &FE4D
;	AND #&40			; timer 1
;	BNE is_timer1

	LDA &FE4D
	AND #2
	BEQ return_to_os

    INC vsync_counter
    BNE no_carry
    INC vsync_counter+1
    .no_carry

	\\ Pass on to OS IRQ handler
	.return_to_os
	PLA
	STA &FC
	JMP &FFFF
}
old_irqv = P%-2

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

IF _LOAD_TO_SWRAM
.filename0
EQUS "00", 13
.filename1
EQUS "01", 13
.filename2
EQUS "02", 13
.filename3
EQUS "03", 13
ELSE
.osword_params
.osword_params_drive
EQUB 0				; drive
.osword_params_address
EQUD 0				; address
EQUB &03			; number params
EQUB &53			; command = read data multi-sector
.osword_params_track
EQUB 0				; logical track
.osword_params_sector
EQUB 0				; logical sector
.osword_params_size_sectors
EQUB &20 + DFS_sectors_to_load		; sector size / number sectors = 256 / 10
.osword_params_return
EQUB 0				; returned error value

.drive_order
EQUB 2,3,1,0
ENDIF

MACRO MODE5_PIXELS a,b,c,d
    EQUB (a AND 2) * &40 OR (a AND 1) * &08 OR (b AND 2) * &20 OR (b AND 1) * &04 OR (c AND 2) * &10 OR (c AND 1) * &02 OR (d AND 2) * &08 OR (d AND 1) * &01
ENDMACRO

IF _DEBUG
.debug_font
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,3,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 0,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,0,3,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 0,0,0,0

MODE5_PIXELS 3,3,3,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,3,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 3,0,0,0
MODE5_PIXELS 0,0,0,0
ENDIF

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

ALIGN &100
.STREAM_buffer_start
SKIP STREAM_buffer_size
.STREAM_buffer_end

.bss_end

\ ******************************************************************
\ *	Memory Info
\ ******************************************************************

PRINT "------"
PRINT "STNICC-BEEB"
PRINT "------"
PRINT "MAIN size =", ~main_end-main_start
PRINT "FX size = ", ~fx_end-fx_start
PRINT "DATA size =",~data_end-data_start
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~screen2_addr-P%
PRINT "------"

exe_size=(end-start+&ff)AND&FF00
PRINT "EXE size = ",~exe_size
; We know that Catalog + !Boot = &300
; Need to make a dummy file so 00 is at sector 20=track 2
dummy_size=2*DFS_track_size-exe_size-&300

CLEAR &0000,&FFFF
ORG &0000
.dummy
skip dummy_size
SAVE "dummy", dummy, P%

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************

IF _LOAD_TO_SWRAM
PUTFILE "data/scene1_beeb.00.bin", "00", &8000
PUTFILE "data/scene1_beeb.01.bin", "01", &8000
PUTFILE "data/scene1_beeb.02.bin", "02", &8000
PUTFILE "data/scene1_beeb.03.bin", "03", &8000
PUTFILE "data/scene1_beeb.04.bin", "04", &8000
PUTFILE "data/scene1_beeb.05.bin", "05", &8000
PUTFILE "data/scene1_beeb.06.bin", "06", &8000
PUTFILE "data/scene1_beeb.07.bin", "07", &8000
PUTFILE "data/scene1_beeb.07.bin", "08", &8000
PUTFILE "data/scene1_beeb.07.bin", "09", &8000
PUTFILE "data/scene1_beeb.07.bin", "10", &8000
PUTFILE "data/scene1_beeb.07.bin", "11", &8000
ELSE
PUTFILE "data/scene1_disk.00.bin", "00", 0
ENDIF
