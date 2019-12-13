\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB INTRO
\ ******************************************************************

_DEBUG_RASTERS = FALSE
_ENABLE_MUSIC = TRUE

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

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO SWRAM_SELECT bank
LDA #bank: sta &f4: sta &fe30
ENDMACRO

MACRO MODE5_PIXELS a,b,c,d
    EQUB (a AND 2) * &40 OR (a AND 1) * &08 OR (b AND 2) * &20 OR (b AND 1) * &04 OR (c AND 2) * &10 OR (c AND 1) * &02 OR (d AND 2) * &08 OR (d AND 1) * &01
ENDMACRO

MACRO PAGE_ALIGN
H%=P%
ALIGN &100
PRINT "Lost ", P%-H%, "bytes"
ENDMACRO

MACRO PAGE_ALIGN_FOR_SIZE size
IF HI(P%+size) <> HI(P%)
	PAGE_ALIGN
ENDIF
ENDMACRO

MACRO CHECK_SAME_PAGE_AS base
IF HI(P%-1) <> HI(base)
PRINT "WARNING! Table or branch base address",~base, "may cross page boundary at",~P%
ENDIF
ENDMACRO

MACRO SET_BGCOL c
IF _DEBUG_RASTERS
{
    LDA #&00+c:STA &FE21
    LDA #&10+c:STA &FE21
    LDA #&40+c:STA &FE21
    LDA #&50+c:STA &FE21
}
ENDIF
ENDMACRO

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

; SCREEN constants
SCREEN_WIDTH_PIXELS = 320
SCREEN_HEIGHT_PIXELS = 256
SCREEN_ROW_BYTES = SCREEN_WIDTH_PIXELS * 8 / 4
SCREEN_SIZE_BYTES = (SCREEN_WIDTH_PIXELS * SCREEN_HEIGHT_PIXELS) / 4

screen_addr = &3000

MAX_GLIXELS = 64
LERP_FRAMES = 64

; GLIXEL BUFFER
GLIXEL_WIDTH = 80
GLIXEL_HEIGHT = 64
GLIXEL_STRIDE = GLIXEL_WIDTH / 8

MAX_VISITS_PER_FRAME = 32

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD &A0

.zp_start

.writeptr       skip 2
.plot_x_offset  skip 1

.xstart         skip 2  ; pixel in 1:9:6
.ystart         skip 2  ; pixel in 1:8:7
.xend           skip 1  ; column
.yend           skip 1  ; row

.plot_y         skip 1
.loop_index     skip 1

.count          skip 1
.text_index     skip 1
.start_index    skip 1

.char_row       skip 1
.char_col       skip 1

.char_left      skip 1
.char_top       skip 1
.plotted        skip 1

.lerps_active   skip 1
.cls_active     skip 1

.visit_count    skip 1
.temp_x         skip 1
.temp_y         skip 1
.seed           skip 1
.seed2          skip 1
.seed3          skip 1
.direction      skip 1

.old_irqv       skip 2
.vsync_counter  skip 1
.music_on       skip 1

.last_index     skip 1
.last_vsync     skip 1
.rounds         skip 1

.char_def
skip 9

include "src/vgmplayer.h.asm"

.zp_end


\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

ORG &400
GUARD &D00
.vgm_buffer_start
; reserve space for the vgm decode buffers (8x256 = 2Kb)
PAGE_ALIGN
.vgm_stream_buffers
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
.vgm_buffer_end

PAGE_ALIGN
.xpos_LO
skip MAX_GLIXELS
.xpos_HI
skip MAX_GLIXELS
.ypos_LO
skip MAX_GLIXELS
.ypos_HI
skip MAX_GLIXELS

ORG &E00
GUARD screen_addr

.lerp_count
skip MAX_GLIXELS
.lerp_direction
skip MAX_GLIXELS

.glixel_buffer
skip GLIXEL_HEIGHT * GLIXEL_STRIDE
.glixel_buffer_end
PRINT ~P%

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1100
GUARD screen_addr

.start
.main_start

\ ******************************************************************
\ *	Code entry
\ ******************************************************************

.main
{
    ldx #&ff
    txs

    \\ Init ZP
    lda #0
    ldx #0
    .zp_loop
    sta &00, x
    inx
    cpx #&A0
    bne zp_loop

	\\ Set interrupts and handler
	SEI							; disable interupts
	LDA #&7F					; A=01111111
	STA &FE4E					; R14=Interrupt Enable (disable all interrupts)
	STA &FE43					; R3=Data Direction Register "A" (set keyboard data direction)
	LDA #&82					; A=11000010
	STA &FE4E					; R14=Interrupt Enable (enable main_vsync and timer interrupt)

    LDA IRQ1V:STA old_irqv
    LDA IRQ1V+1:STA old_irqv+1

    LDA #LO(irq_handler):STA IRQ1V
    LDA #HI(irq_handler):STA IRQ1V+1		; set interrupt handler
    CLI

    \\ Set MODE 1

    lda #22
    jsr oswrch
    lda #1
    jsr oswrch

    lda #8:sta &fe00:lda #&C0:sta &fe01  ; cursor off

    \\ Set pal

    ldx #15
    .pal_loop
    lda palette, X
    sta &fe21
    dex
    bpl pal_loop

    ; initialize the vgm player with a vgc data stream
    IF _ENABLE_MUSIC
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgm_data)
    ldy #hi(vgm_data)
    sec
    jsr vgm_init
    ENDIF

;    lda startx_table_LO
    lda #LO(160 << 6)
    sta xstart
;    lda startx_table_HI
    lda #HI(160 << 6)
    sta xstart+1
;    lda starty_table_LO
    lda #LO(128 << 6)
    sta ystart
;    lda starty_table_HI
    lda #HI(128 << 6)
    sta ystart+1

    lda #0
    sta char_top
    sta char_col
    sta char_def+8
    sta char_def+1
    lda #0
    sta char_row
    lda #LO(-8)
    sta char_left

    ldx #0
    lda #0
    .init_loop
    sta lerp_count, X
    inx
    cpx #MAX_GLIXELS
    bcc init_loop

    \\ Char defs
    IF 0
    lda #128+'$'
    ldx #LO(flux_def)
    ldy #HI(flux_def)
    jsr def_char

    lda #128+'@'
    ldx #LO(smiley_def)
    ldy #HI(smiley_def)
    jsr def_char

    lda #128+'%'
    ldx #LO(quarter_def)
    ldy #HI(quarter_def)
    jsr def_char
    ENDIF

    jsr cls
    lda #25         ; can use this as a lazy timer
    sta cls_active

    ; init visitor
    lda #0
    sta xend
    lda #GLIXEL_HEIGHT-1
    sta yend

    ; set up the glb
    jsr write_string_to_glb

    lda #&ff
    sta music_on

    .loop

    ; don't wait for vsync if we missed it!
    lda last_vsync
    .wait_for_vsync
    cmp vsync_counter
    beq wait_for_vsync

    lda vsync_counter
    sta last_vsync

    SET_BGCOL PAL_red
    jsr lerp_glixels
    SET_BGCOL PAL_black

    ldy cls_active
    beq do_text

    \\ Wait until everything has finished moving
    lda lerps_active
    bne continue

; Don't actually wipe, we're just a delay now
;   jsr wipe_line_Y
    dec cls_active
    bne continue

    .do_text
    ; jsr make_lerp_from_string

    lda direction
    beq do_enter
    
    \\ Exit
    jsr make_exit_from_glb
    bcc continue

    ; update glb
    jsr write_string_to_glb

    ; reset visitor here
    lda #GLIXEL_HEIGHT-1
    sta yend

    ; start enter
    lda #0
    sta direction

    ; we did a round of enter & exit
    inc rounds

    lda rounds
    and #1
    bne continue

    ; every other round just wait a few frames for timing
    lda #75
    sta cls_active
    bne continue

    .do_enter
    jsr make_lerp_from_glb
    bcc continue

    ; reset visitor here
    lda #0
    sta xstart
    sta ystart
    sta ystart+1

    ; start exit
    lda #1
    sta direction

    .continue
    inc count
    jmp loop

    .return
    rts
}

.get_char_def
{
    sta char_def
    bmi local_chars

    lda #10
    ldx #LO(char_def)
    ldy #HI(char_def)
    jsr osword
    rts

    .local_chars
    and #&7f
    tax
    ldy #0
    .loop
    lda local_char_defs, X
    sta char_def+1, Y
    inx
    iny
    cpy #8
    bcc loop

    rts
}

IF 0
.make_lerp_from_string
{
    lda #0
    sta plotted

    ldx char_row
    .loop
    lda char_def+1, X
    beq next_char_row

    \\ Pop the top bit
    asl a
    sta char_def+1, X
    bcc no_glixel

    \\ Make a glixel to lerp
    jsr make_lerp
    lda #&ff
    sta plotted
    ldx char_row

    .no_glixel
    \\ Next x-coord
    inc xend

    \\ Column count
    dec char_col
    bne next_char_col

    \\ Next char row
    .next_char_row
    lda #8
    sta char_col

    \\ Reset to lhs of char
    lda char_left
    sta xend

    \\ Next y down
    dec yend

    \\ Have we done all rows?
    dex
    cpx #0
    bpl next_char_col

    \\ Move x across one char
    clc
    lda char_left
    adc #8
    sta char_left

    \\ Next character from string
    ldy text_index
    .string_loop
    lda string, y

    \\ Handle special chars
    bne not_eos
    ldy #0
    beq string_loop

    .not_eos
    cmp #31     ; VDU 31 = tab cursor
    bne not_vdu31

    \\ VDU 31,x,y
    iny
    lda string, y
    sta char_left
    iny
    lda string, y
    sta char_top
    iny
    bne string_loop
    .not_vdu31
    cmp #12     ; VDU 12 = cls
    bne not_vdu12

    lda #&ff
    sta cls_active
    sta plotted
    iny
    bne string_loop

    .not_vdu12
    iny
    sty text_index
    jsr get_char_def

    \\ Put x,y to start of char plot
    lda char_left
    sta xend
    lda char_top
    sta yend

    ldx #7

    \\ LOOP UNTIL THERE IS A GLIXEL
    .next_char_col
    stx char_row

    lda plotted
    beq loop

    rts
}
ENDIF

.lerp_glixels
{
    ldx #0
    stx lerps_active

    .loop
    stx loop_index

    lda lerp_count, X
    beq next_lerp

    inc lerps_active

    \\ Remove
    jsr plot_glixel_X
    ldx loop_index

    \\ Move
    clc
    lda xpos_LO, X
    adc xdelta_LO, X
    sta xpos_LO, X
    lda xpos_HI, X
    adc xdelta_HI, X
    sta xpos_HI, X

    clc
    lda ypos_LO, X
    adc ydelta_LO, X
    sta ypos_LO, X
    lda ypos_HI, X
    adc ydelta_HI, X
    sta ypos_HI, X

    dec lerp_count, X
    bne plot_it

    lda lerp_direction, X
    bne next_lerp

    \\ Plot it
    .plot_it
    jsr plot_glixel_X

    .next_lerp
    ldx loop_index
    inx
    cpx #MAX_GLIXELS
    bcc loop

    .return
    rts
}

.plot_glixel_X              ; X is trashed
{
    lda ypos_HI, X
    sta plot_y

    lda ypos_LO, X
    asl a                   ; top 2-bits
    rol plot_y
    asl a
    rol plot_y

    ldy plot_y

    lda xpos_HI, X
    sta load_col+1          ; column

    lda xpos_LO, X
    lsr a:lsr a:lsr a
    lsr a:lsr a:lsr a       ; top 2-bits

    .load_col
    ldx #0
    jsr plot_glixel_eor
    rts
}

\\ From xstart, ystart
\\ To xend, yend
.make_lerp
{
    jsr get_next_slot
    bcc found_slot
    rts

    .found_slot
    lda xstart
    sta xpos_LO, X
    lda xstart+1
    sta xpos_HI, X

    lda ystart
    sta ypos_LO, X
    lda ystart+1
    sta ypos_HI, X

    \\ Calculate xend - xstart
    sec
    lda #0          ; xend_LO
    sbc xstart
    sta xdelta_LO, X
    lda xend
    sbc xstart+1
    sta xdelta_HI, X

    \\ Calculate yend - ystart
    sec
    lda #0          ; yend_LO
    sbc ystart
    sta ydelta_LO, X
    lda yend
    sbc ystart+1
    sta ydelta_HI, X

    \\ NEED EXTRA BIT FOR SIGN!
    \\ NEED TO KEEP SIGN!

    \\ Now divide by our number of frames.
    \\ Should do this entirely in ZP.
    lda xdelta_HI, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X
    cmp #&80
    ror xdelta_HI, X
    ror xdelta_LO, X

    lda ydelta_HI, X
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    
    cmp #&80
    ror ydelta_HI, X
    ror ydelta_LO, X    

    lda #LERP_FRAMES
    sta lerp_count, X

    lda direction
    sta lerp_direction, X
    bne return

    \\ Plot it to begin with
    jsr plot_glixel_X

IF 0
    inc start_index
    ldx start_index
    lda startx_table_LO, X
    sta xstart
    lda startx_table_HI, X
    sta xstart+1
    lda starty_table_LO, X
    sta ystart
    lda starty_table_HI, X
    sta ystart+1
ENDIF

    .return
    clc
    rts
}

.get_next_slot
{
    clc
    lda #MAX_GLIXELS
    sta visit_count
    ldx last_index
    inx
    txa:and #MAX_GLIXELS-1:tax
    .loop
    lda lerp_count, X
    beq return
    inx
    txa:and #MAX_GLIXELS-1:tax
    dec visit_count
    bne loop
    .return
    stx last_index
    rts
}

MACRO MOVE_ROW
{
    lda writeptr
    and #7
    cmp #7
    beq next_row
    inc writeptr
    bne cont 

    .next_row
    clc
    lda writeptr
    adc #LO(SCREEN_ROW_BYTES-7)
    sta writeptr
    lda writeptr+1
    adc #HI(SCREEN_ROW_BYTES-7)
    sta writeptr+1
    .cont
}
ENDMACRO

; X=column [0-79], A=pixel offset [0-3], Y=row [0-255]
.plot_glixel_eor
{
    sta plot_x_offset

    clc
    lda screen_row_LO, Y
    adc screen_col_LO, X
    sta writeptr
    lda screen_row_HI, Y
    adc screen_col_HI, X
    sta writeptr+1

    ldy #0
    ldx plot_x_offset
    cpx #2
    bcs two_bytes

    \\ One byte
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    MOVE_ROW
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    MOVE_ROW
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    rts

    \\ Two bytes
    .two_bytes
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    ldy #8
    lda (writeptr), Y
    eor glixel_byte1, X
    sta (writeptr), Y
    MOVE_ROW
    ldy #0
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    ldy #8
    lda (writeptr), Y
    eor glixel_byte1, X
    sta (writeptr), Y
    MOVE_ROW
    ldy #0
    lda (writeptr), Y
    eor glixel_byte0, X
    sta (writeptr), Y
    ldy #8
    lda (writeptr), Y
    eor glixel_byte1, X
    sta (writeptr), Y

    .done
    rts
}

.cls
{
    ldX #0
    .loop
    txa:tay
    jsr wipe_line_Y
    dex
    bne loop
    rts
}

.wipe_line_Y
{
    lda screen_row_LO, Y
    sta writeptr
    lda screen_row_HI, Y
    sta writeptr+1

    tya
    and #1
    tay
    lda stipple, Y
    FOR n,0,SCREEN_ROW_BYTES,8
    ldy #LO(n)
    sta (writeptr), Y
    IF LO(n) = &F8
    inc writeptr+1
    ENDIF
    NEXT

    rts

    .stipple
    EQUB &0A, &05
}

IF 0
.def_char
{
    stx loop+1
    sty loop+2
    pha
    lda #23
    jsr oswrch
    pla
    jsr oswrch
    ldx #0
    .loop
    lda &ffff, X
    jsr oswrch
    inx
    cpx #8
    bcc loop
    rts
}
ENDIF

MACRO CALCULATE_GLB_PTR
{
    txa
    lsr a:lsr a:lsr a       ; x DIV 8
    clc
    adc glixel_row_LO, Y
    sta writeptr
    lda glixel_row_HI, Y
    adc #0
    sta writeptr+1
    txa
    and #7
    tax
}
ENDMACRO

.set_glb_XY
{
    CALCULATE_GLB_PTR
    ldy #0
    lda (writeptr), Y
    ora glixel_bit, X
    sta (writeptr), Y
    rts
}

.get_glb_XY
{
    CALCULATE_GLB_PTR
    ldy #0
    lda (writeptr), Y
    and glixel_bit, X
    rts
}

.make_lerp_from_glb
{
    lda #MAX_VISITS_PER_FRAME+1
    sta visit_count

    .loop
    dec visit_count         ; maximum visits per frame
    beq visited_all

    .^glb_visit_fn
    jsr visit_fn_random_up     ; returns X,Y of next position

    stx xend                ; finishing position xend
    sty yend                ; yend

    bcs visited_all         ; or Carry set if finished

    jsr get_glb_XY          ; is there a bit in the buffer?
    beq loop                ; loop until we get a bit

    .^glb_start_fn
    jsr start_fn_random_on_top ; sets startx, starty
;    jsr start_fn_from_table
    jmp make_lerp           ; make a lerp
                            ; what to do if this fails?

    .visited_all
    rts
}

.make_exit_from_glb
{
    lda #MAX_VISITS_PER_FRAME+1
    sta visit_count

    .loop
    dec visit_count         ; maximum visits per frame
    beq visited_all

    .^glb_visit_fn2
    jsr visit_fn_random_down     ; returns X,Y of next position

    stx xstart+1            ; now starting position xstart
    sty ystart+1            ; yend

    bcs visited_all         ; or Carry set if finished

    jsr get_glb_XY          ; is there a bit in the buffer?
    beq loop                ; loop until we get a bit

    .^glb_end_fn
    jsr end_fn_random_on_right       ; sets endx, endy
    jmp make_lerp           ; make a lerp
                            ; what to do if this fails?

    .visited_all
    rts
}

IF 0
.visit_fn_by_row_from_bottom
{
    ldx xend
    ldy yend

    inx
    cpx #GLIXEL_WIDTH
    bcc same_row

    ldx #0
    dey
    bpl same_row

    ldy #GLIXEL_HEIGHT-1
    sec
    \\ Set Carry if we've reached max height of glb
    .same_row
    rts
}
ENDIF

.visit_fn_random_up
{
    ldy yend

    .loop
    inc local_count
    bne cont

    dey

    .cont
    jsr rand
    tax
    cpx #GLIXEL_WIDTH
    bcs loop
    
    cpy #&80
    bcc return

    ldy #GLIXEL_HEIGHT-1

    .return
    rts

    .local_count EQUB 0
}

.visit_fn_random_down
{
    ldy ystart+1

    .loop
    inc local_count
    bne cont

    iny

    .cont
    jsr rand
    tax
    cpx #GLIXEL_WIDTH
    bcs loop
    
    cpy #GLIXEL_HEIGHT
    bcc return

    ldy #0

    .return
    rts

    .local_count EQUB 0
}

IF 0
.start_fn_static
{
    \\ do nothing!
    \\ startx,starty static
    rts
}
ENDIF

.end_fn_random_on_right
{
    lda #79
    sta xend

    .loop
    jsr rand2
    cmp #GLIXEL_HEIGHT
    bcs loop
    sta yend    

    rts
}

.start_fn_random_on_top
{
    lda #0
    sta ystart+1

    .loop
    jsr rand3
    cmp #GLIXEL_WIDTH
    bcs loop
    sta xstart+1

    rts
}

IF 0
.start_fn_from_table
{
    ldx start_index
    inx
    stx start_index

    \\ Get new startx, starty from our table
    lda startx_table_LO, X
    sta xstart
    lda startx_table_HI, X
    sta xstart+1
    lda starty_table_LO, X
    sta ystart
    lda starty_table_HI, X
    sta ystart+1

    rts
}
ENDIF

.write_char_to_glb
{
    pha
    CALCULATE_GLB_PTR
    pla
    jsr get_char_def

    ldx #0
    ldy #0
    .loop
    lda char_def+1, X
    sta (writeptr), Y
    clc
    lda writeptr
    adc #GLIXEL_STRIDE
    sta writeptr
    bcc no_carry
    inc writeptr+1
    .no_carry
    inx
    cpx #8
    bcc loop

    rts
}

.write_string_to_glb
{
    jsr clear_glb

    ldy #0
    sty temp_x
    sty temp_y

    ldy text_index

    .loop
    sty text_index

    lda string, Y
    beq restart

    cmp #12
    bne not_vdu12
    iny
    jmp return

    .not_vdu12
    cmp #31
    bne not_vdu31

    iny
    lda string, Y
    sta temp_x
    iny
    lda string, Y
    sta temp_y
    iny
    jmp loop

    .not_vdu31
    ldx temp_x
    ldy temp_y
    jsr write_char_to_glb

    clc
    lda temp_x
    adc #8
    sta temp_x

    ldy text_index
    iny
    jmp loop

    .restart
    ldy #0

    .return
    sty text_index
    rts
}

.rand
{
    lda seed
    asl a
    asl a
    clc
    adc seed
    clc
    adc #&45
    sta seed
    rts
}

.rand2
{
    lda seed2
    asl a
    asl a
    clc
    adc seed2
    clc
    adc #&45
    sta seed2
    rts
}

.rand3
{
    lda seed3
    asl a
    asl a
    clc
    adc seed3
    clc
    adc #&45
    sta seed3
    rts
}

.clear_glb
{
    lda #HI(glixel_buffer)
    sta loop+2
    ldy #2
    lda #0
    ldx #0
    .loop
    sta glixel_buffer, X
    inx
    bne loop
    inc loop+2
    dey
    bne loop
    .loop2
    sta glixel_buffer+&200,X
    inx
    cpx #&80
    bne loop2
    rts
}

.irq_handler
{
	lda &FE4D
	and #2
	beq return

    .handle_vsync
    sta &FE4D       ; ack vsync

    inc vsync_counter

    IF _ENABLE_MUSIC
    lda music_on
    beq return

    txa:pha:tya:pha
    jsr vgm_update
    pla:tay:pla:tax
    ENDIF

    .return
    lda &fc
    rti
}

include "src/vgmplayer.asm"

.main_end

.data_start

.string
EQUS 31,0,24, "BIT ",128+0  ; flux
EQUS 31,0,32, "SHIFTERS"
EQUS 31,0,40, "WISH YOU"
EQUS 31,0,48, "A MERRY"
EQUS 31,0,56, "CHRISTMAS!"
EQUS 12
EQUS 31,0,24, "AND ",128+0  ; flux
EQUS 31,0,32, "A HAPPY"
EQUS 31,0,40, "NEW YEAR!"
EQUS 31,0,48, "SEE YOU"
EQUS 31,0,56, "IN 2020 ",128+8,128+8    ; smiley
EQUS 0

\\ Four fixed possibilities
\\ X=0 => ppp0
\\ X=1 => 0ppp
\\ X=2 => 00pp p000
\\ X=3 => 000p pp00
.glixel_byte0
EQUB %11101110, %01110111, %00110011, %00010001
.glixel_byte1
EQUB %00000000, %00000000, %11001100, %11101110

.palette
{
    EQUB &00 + PAL_black
    EQUB &10 + PAL_black
    EQUB &20 + PAL_blue
    EQUB &30 + PAL_blue
    EQUB &40 + PAL_black
    EQUB &50 + PAL_black
    EQUB &60 + PAL_blue
    EQUB &70 + PAL_blue
    EQUB &80 + PAL_cyan
    EQUB &90 + PAL_cyan
    EQUB &A0 + PAL_white
    EQUB &B0 + PAL_white
    EQUB &C0 + PAL_cyan
    EQUB &D0 + PAL_cyan
    EQUB &E0 + PAL_white
    EQUB &F0 + PAL_white
}

.local_char_defs
.flux_def
EQUB %00001110
EQUB %00001110
EQUB %01111110
EQUB %01111110
EQUB %01111110
EQUB %01110000
EQUB %01110000
EQUB %00000000

.smiley_def
EQUB %01111100
EQUB %11111110
EQUB %10111010
EQUB %11111110
EQUB %10111010
EQUB %11000110
EQUB %01111100
EQUB %00000000

.quarter_def
EQUB %00100000
EQUB %00100110
EQUB %00101100
EQUB %00011000
EQUB %00110101
EQUB %01100111
EQUB %00000001
EQUB %00000000

.glixel_bit
EQUB 128,64,32,16,8,4,2,1

.glixel_row_LO
FOR r,0,GLIXEL_HEIGHT-1,1
addr = r * GLIXEL_STRIDE
EQUB LO(glixel_buffer + addr)
NEXT

.glixel_row_HI
FOR r,0,GLIXEL_HEIGHT-1,1
addr = r * GLIXEL_STRIDE
EQUB HI(glixel_buffer + addr)
NEXT

PAGE_ALIGN
.screen_row_LO
FOR y,0,255,1
row=y DIV 8:sl=y MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB LO(screen_addr + addr)
NEXT

.screen_row_HI
FOR y,0,255,1
row=y DIV 8:sl=y MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB HI(screen_addr + addr)
NEXT

.screen_col_LO
FOR c,0,79,1
EQUB LO(c * 8)
NEXT

.screen_col_HI
FOR c,0,79,1
EQUB HI(c * 8)
NEXT

IF 0
k = 3

PAGE_ALIGN
.startx_table_LO
FOR n,0,255,1
;a = 160 + 48 * SIN(n * 2 * PI / 256)
;EQUB LO(a << 6)

\\ Rose: x = cos(ka) * cos(a)
;a = n *  4 * PI / 256
;x = 160 + 100 * COS(k * a) * COS(a)
;a = n *  2 * PI / 256
;x = 160 + 150 * COS(a)
x = RND(320)
EQUB LO(x << 6)
NEXT

.startx_table_HI
FOR n,0,255,1
;a = 160 + 48 * SIN(n * 2 * PI / 256)
;EQUB HI(a << 6)

\\ Rose: x = cos(ka) * cos(a)
;a = n *  4 * PI / 256
;x = 160 + 100 * COS(k * a) * COS(a)
;a = n *  2 * PI / 256
;x = 160 + 150 * COS(a)
x = RND(320)
EQUB HI(x << 6)
NEXT

.starty_table_LO
FOR n,0,255,1
;a = 128 + 48 * COS(n * 2 * PI / 256)
;EQUB LO(a << 6)

\\ Rose: y = cos(ka) * sin(a)
a = n *  4 * PI / 256
x = 128 + 100 * COS(k * a) * SIN(a)
y = 0
EQUB LO(y << 6)
NEXT

.starty_table_HI
FOR n,0,255,1
;a = 128 + 48 * COS(n * 2 * PI / 256)
;EQUB HI(a << 6)

\\ Rose: y = cos(ka) * sin(a)
a = n *  4 * PI / 256
x = 128 + 100 * COS(k * a) * SIN(a)
y = 0
EQUB HI(y << 6)
NEXT
ENDIF

.vgm_data
INCBIN "music/CRIMBO.vgc"

.data_end

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

.bss_start

PAGE_ALIGN
.xdelta_LO
skip MAX_GLIXELS
.xdelta_HI
skip MAX_GLIXELS
.ydelta_LO
skip MAX_GLIXELS
.ydelta_HI
skip MAX_GLIXELS


.bss_end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "INTRO", start, end, main

\ ******************************************************************
\ *	Memory Info
\ ******************************************************************

PRINT "------"
PRINT "XMAS-CARD"
PRINT "------"
PRINT "ZP size =", ~zp_end-zp_start, "(",~&C0-zp_end,"free)"
PRINT "CODE size =", ~main_end-main_start
PRINT "DATA size =",~data_end-data_start
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~screen_addr-P%
PRINT "------"
