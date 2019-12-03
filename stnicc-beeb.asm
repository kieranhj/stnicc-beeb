\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB
\ ******************************************************************

; 0 = LOW quality (128 x 100 scanline skipped)
; 1 = MEDIUM quality (128 x 100 scanline doubled)
; 2 = HIGH quality (128 x 200)
_QUALITY = 2

_DEBUG = TRUE
_TESTS = FALSE

; Display <drive no | sector no> <track no> <load to HI> <stream ptr HI>
_SHOW_STREAMING_INFO = FALSE

; If set, show total vsync count, rather than just the count for the
; last frame. Intended for use in conjunction with _STOP_AT_FRAME.
_SHOW_TOTAL_VSYNC_COUNTER = TRUE
_STOP_AT_FRAME = -1
; Debug defines
_DOUBLE_BUFFER = TRUE
_PLOT_WIREFRAME = FALSE
; Rendering defines
_HALF_VERTICAL_RES = (_QUALITY < 2)
_DOUBLE_PLOT_Y = (_QUALITY = 1)
_WIDESCREEN = (_QUALITY = 2) AND FALSE

_NULA=TRUE

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
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO SWRAM_SELECT bank
LDA #bank: sta &f4: sta &fe30
ENDMACRO

if _NULA
MACRO MODE2_PIXELS a,b
equb ((-(a!=0))*%10101010) or ((-(b!=0))*%01010101)
ENDMACRO
endif

MACRO MODE5_PIXELS a,b,c,d
if _NULA
    MODE2_PIXELS a,b
	MODE2_PIXELS c,d
else
    EQUB (a AND 2) * &40 OR (a AND 1) * &08 OR (b AND 2) * &20 OR (b AND 1) * &04 OR (c AND 2) * &10 OR (c AND 1) * &02 OR (d AND 2) * &08 OR (d AND 1) * &01
endif
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

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

; SCREEN constants
SCREEN_ROW_BYTES = 256
SCREEN_WIDTH_PIXELS = 128
SCREEN_HEIGHT_PIXELS = 200
SCREEN_SIZE_BYTES = (SCREEN_WIDTH_PIXELS * SCREEN_HEIGHT_PIXELS) / 4

WIDESCREEN_HEIGHT = 2 * SCREEN_WIDTH_PIXELS * 9 / 16
WIDESCREEN_TOP = (SCREEN_HEIGHT_PIXELS/2) - (WIDESCREEN_HEIGHT/2)
WIDESCREEN_BOTTOM = WIDESCREEN_TOP + WIDESCREEN_HEIGHT - 1

screen1_addr = &8000 - SCREEN_SIZE_BYTES
screen2_addr = screen1_addr - SCREEN_SIZE_BYTES

; STREAMING constants
STREAMING_tracks_per_disk = 75
STREAMING_sectors_to_load = 10

DISK1_drive_no = 0
DISK1_first_track = 5
DISK1_last_track = DISK1_first_track + STREAMING_tracks_per_disk

DISK2_first_track = 1
DISK2_last_track = DISK2_first_track + STREAMING_tracks_per_disk

STREAM_buffer_size = 3 * DFS_track_size

; STNICCC scene1.bin constants
MAX_VERTS_PER_POLY = 7

FLAG_CLEAR_SCREEN = 1
FLAG_CONTAINS_PALETTE = 2
FLAG_INDEXED_DATA = 4

POLY_DESC_END_OF_STREAM = &FD
POLY_DESC_SKIP_TO_64K = &FE
POLY_DESC_END_OF_FRAME = &FF

; Exact time for a 50Hz frame less latch load time
FramePeriod = 312*64-2

; This is when we trigger the next frame draw during the frame
; Essentially how much time we give the main loop to stream the next track
TimerValue = (12)*64 - 2*64

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD &A0

.zp_start
.STREAM_ptr_LO      skip 1
.STREAM_ptr_HI      skip 1

; vars for plot_span
.writeptr           skip 2
.span_start         skip 1
;.span_end           skip 1
.span_width         skip 1
.span_colour        skip 1
.shortptr			skip 2

; vars for drawline
; loaded directly into X&Y
IF _DEBUG
.startx             skip 1
.starty             skip 1
ENDIF
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
;.poly_colour        skip 1
.poly_index         skip 1
IF _HALF_VERTICAL_RES
.span_y             skip 1
ELSE
.span_y		\\ alias for poly_y
ENDIF
.poly_y             skip 1

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
;.poly_descriptor    skip 1
.eof_flag			skip 1

; palette vars
.pal_ptr_LO			skip 1
.pal_ptr_HI			skip 1
.pal_descriptor		skip 1
.pal_byte1			skip 1
.pal_byte2			skip 1
.pal_dither_idx		skip 1

; system vars
.rom_bank           skip 1
.vsync_counter      skip 2
.draw_buffer_HI     skip 1
.sector_no          skip 1
.track_no			skip 1
.load_to_HI			skip 1
.error_flag			skip 1
.decode_lock		skip 1
.screen_lock		skip 1
.buffer_lock		skip 1

; debug vars
IF _DEBUG
.pause_lock			skip 1
.last_vsync         skip 1
.debug_writeptr		skip 2
ENDIF
.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

ORG &300
GUARD &800
.reloc_to_start
.screen_row_LO
skip &100
.screen1_row_HI
skip &100
.screen2_row_HI
skip &100
.y_to_row
skip &100
PAGE_ALIGN  ; lazy
if not(_NULA)
.poly_palette
skip &40
.dither_table
skip &40
endif
.screen_col_LO
skip &80
.reloc_to_end

ORG &A00
GUARD &D00
.palette_stream_buffer ; or use &E00?
skip &300

ORG &E00
GUARD &1000
.span_buffer_start
skip &100
.span_buffer_end
skip &100

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1100
GUARD screen2_addr

.start

.main_start

\ ******************************************************************
\ *	Code entry
\ ******************************************************************

.main
{
	\\ Set interrupts

	SEI							; disable interupts

    \\ Init ZP
    lda #0
    ldx #0
    .zp_loop
    sta &00, x
    inx
    cpx #&A0
    bne zp_loop

	\\ Don't let the IRQ do anything yet!
	INC decode_lock

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

    \\ Set MODE 5

    lda #22
    jsr oswrch
    lda #5
    jsr oswrch

if _NULA

    lda #9:ldx #0:jsr osbyte
	lda #10:ldx #0:jsr osbyte
	lda #154:ldx #0:jsr osbyte

	lda #$00
	clc
	.reset_palette_loop
	pha
	eor #$07:sta $fe21
	pla
	adc #$11
	bcc reset_palette_loop

; disable NuLA auxiliary palette
	lda #$10
	sta $fe22

; set colour 0 to black

    lda #$00
	sta $fe23
	sta $fe23

endif

	\\ Relocate data to lower RAM
	\\ Might want to do this before clearing the screen if data overlaps!
	lda #HI(reloc_from_start)
	ldy #HI(reloc_to_start)
	ldx #HI(reloc_to_end - reloc_to_start + &ff)
	jsr copy_pages

if not(_NULA)
	lda #HI(palette_stream_start)
	ldy #HI(palette_stream_buffer)
	ldx #HI(palette_stream_end - palette_stream_start + &ff)
	jsr copy_pages

	ldx #15
	.pal_loop
	lda mode5_palette, X
	sta &fe21
	dex
	bpl pal_loop
endif

	\\ Clear the extra bit!
	jsr screen2_cls

	{
		lda #2
		.vsync1
		bit &FE4D
		beq vsync1 \ wait for vsync
	}

	\\ Close enough for our purposes
	; Write T1 low now (the timer will not be written until you write the high byte)
    LDA #LO(TimerValue):STA &FE44
    ; Get high byte ready so we can write it as quickly as possible at the right moment
    LDX #HI(TimerValue):STX &FE45             		; start T1 counting		; 4c +1/2c 

  	; Latch T1 to interupt exactly every 50Hz frame
	LDA #LO(FramePeriod):STA &FE46
	LDA #HI(FramePeriod):STA &FE47

    \\ Resolution 256x200 => 128x200
    lda #1:sta &fe00:lda #32:sta &fe01
    lda #6:sta &fe00:lda #25:sta &fe01
    lda #8:sta &fe00:lda #&C0:sta &fe01  ; cursor off
	lda #2:sta $fe00:lda #45:sta $fe01
	lda #7:sta $fe00:lda #31:sta $fe01

    lda #12:sta &fe00
    lda #HI(screen2_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen2_addr/8):sta &fe01

    \\ Load SWRAM data
;    SWRAM_SELECT 4
;    lda #HI(&8000)
;    ldx #LO(filename0)
;    ldy #HI(filename0)
;    jsr disksys_load_file

    \\ Set stream pointer
    lda #LO(STREAM_buffer_start-1)
    sta STREAM_ptr_LO
    lda #HI(STREAM_buffer_start-1)
    sta STREAM_ptr_HI

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

    \\ Init system
    lda #HI(screen1_addr)
    sta draw_buffer_HI

	lda #LO(palette_stream_buffer-1)
	sta pal_ptr_LO
	lda #HI(palette_stream_buffer-1)
	sta pal_ptr_HI

    \\ Clear screen
    jsr screen_cls
    jsr init_span_buffer

    \\ jmp do_tests

	DEC decode_lock

    .loop
    \\ Debug
    IF _DEBUG
    {
		lda pause_lock
		beq continue
        .wait_for_Key
        lda #&79:ldx #&10:jsr osbyte:cpx #&ff:beq wait_for_Key
		lda #0:sta pause_lock
        .continue
    }
    ENDIF

	lda eof_flag
	cmp #POLY_DESC_END_OF_STREAM
    beq track_load_error

	\\ Which page are we reading crunched data from?
	sec
	lda STREAM_ptr_HI

	\\ Is it more than a "track" away?
	SBC load_to_HI
	BCS sectors_to_load_1
	EOR #255
	ADC #1
	.sectors_to_load_1
	CMP #STREAMING_sectors_to_load
	BCC not_ready_to_load

	\\ If so, load a track's worth of data into our buffer
	JSR load_next_track

	\\ Unlock the buffer - there is some data!
	lda #0:sta buffer_lock

	.not_ready_to_load

	\\ Check for errors
	LDA error_flag
	BNE track_load_error

	jmp loop

    .track_load_error

	\\ Re-enable useful interupts
	SEI
	LDA #&D3					; A=11010011
	STA &FE4E					; R14=Interrupt Enable

    LDA old_irqv:STA IRQ1V
    LDA old_irqv+1:STA IRQ1V+1	; set interrupt handler
	CLI

	\\ Exit gracefully (in theory)
    \\ But not back to BASIC as we trashed all its workspace :D
	RTS
}

IF _DEBUG
.show_vsync_counter
{
	jsr debug_reset_writeptr

	\\ Frame no.
	lda frame_no+1
	jsr debug_write_A
	lda frame_no
	jsr debug_write_A_spc

IF _SHOW_TOTAL_VSYNC_COUNTER

    lda vsync_counter+1
	jsr debug_write_A

	lda vsync_counter+0
	jsr debug_write_A_spc

ELSE

    sec
    lda vsync_counter
    sbc last_vsync
    jsr debug_write_A_spc

    lda vsync_counter
    sta last_vsync

ENDIF

IF _SHOW_STREAMING_INFO

	lda osword_params_drive
	asl a:asl a:asl a:asl a
	ora osword_params_sector

	jsr debug_write_A_spc

	lda osword_params_track
	jsr debug_write_A_spc

	lda load_to_HI
	jsr debug_write_A_spc

	lda STREAM_ptr_HI
	jsr debug_write_A_spc

ENDIF

    {
        lda frame_no+1
		IF _STOP_AT_FRAME > -1
        cmp #HI(_STOP_AT_FRAME)+1
		bcs lock_me
		ENDIF
        cmp #HI(_STOP_AT_FRAME)
        bcc continue
        lda frame_no
        cmp #LO(_STOP_AT_FRAME)
        bcc continue
		.lock_me
		lda #&ff:sta pause_lock
		.continue
    }

	rts
}
ENDIF

\ ******************************************************************
\ *	Streaming tracks from disk
\ ******************************************************************

.load_next_track
{
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
	ADC #STREAMING_sectors_to_load
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
	ADC #STREAMING_sectors_to_load

	\\ Have we fallen off the end of the buffer?
	CMP #HI(STREAM_buffer_end)
	BNE TRACK_LOAD_no_wrap

	\\ If so then reset to start
	LDA #HI(STREAM_buffer_start)

	.TRACK_LOAD_no_wrap
	STA load_to_HI

	.TRACK_LOAD_return
	RTS
}

.irq_handler
{
	LDA &FC
	PHA

	LDA &FE4D
	AND #2
	BEQ not_vsync

	lda #0
	sta screen_lock

	\\ Vsync
    INC vsync_counter
    BNE no_carry
    INC vsync_counter+1
    .no_carry
	JMP return_to_os

	.not_vsync
	\\ Which interrupt?
	LDA &FE4D
	AND #&40			; timer 1
	BEQ return_to_os

	\\ Acknowledge timer 1 interrupt
	STA &FE4D

	\\ If we're already busy just exit function
	LDA decode_lock
	\\ Can't start rendering as our frame buffer hasn't flipped
	\\ Could start parse then block before touching the screen buffer
	ora screen_lock
	\\ Waiting for buffer fill
	ora buffer_lock
	IF _DEBUG
	\\ Waiting for keypress
	ora pause_lock
	ENDIF
	BNE return_to_os

	\\ Set a lock on our decode function
	INC decode_lock

	\\ Store registers in case of interupts
	TXA:PHA:TYA:PHA

	\\ Do the slow bit!
	{
		\\ Decode the frame with interrupts off!
		CLI

		\\ Parse and draw the next frame
		jsr parse_frame
		sta eof_flag

		cmp #POLY_DESC_SKIP_TO_64K
		bne stream_ok
		
		\\ Align to start of streaming buffer (track size)
		lda #LO(STREAM_buffer_start-1)
		sta STREAM_ptr_LO
		lda #HI(STREAM_buffer_start-1)
		sta STREAM_ptr_HI

		\\ Check whether we're still loading into the start of the streaming buffer
		lda load_to_HI
		cmp #HI(STREAM_buffer_start)
		bne stream_ok
		\\ We've caught our tail...
		lda #&ff:sta buffer_lock

		.stream_ok
		IF _PLOT_WIREFRAME
		{
			lda STREAM_ptr_HI
			cmp load_to_HI
			bne wire_ok
			lda #&ff:sta buffer_lock
			.wire_ok
		}
		ENDIF

		IF _DEBUG
		jsr show_vsync_counter
		ENDIF

		\\ Disable interrupts again!
		SEI
	}

	\\ Restore registers
	PLA:TAY:PLA:TAX

	\\ Remove our work lock
	DEC decode_lock

	\\ Set our screen lock until frame swap
	lda #&ff:sta screen_lock

	\\ Swap frame buffers
	jmp swap_frame_buffers
	.^return_here_from_swap_frame_buffers

	\\ Pass on to OS IRQ handler
	.return_to_os
	PLA
	STA &FC
	JMP &FFFF
}
old_irqv = P%-2

; A=from page, Y=to page, X=number of pages
.copy_pages
{
	sta read_from+2
	sty write_to+2

	ldy #0
	.page_loop
	.read_from
	lda &ff00, Y
	.write_to
	sta &ff00, Y
	iny
	bne page_loop
	inc read_from+2
	inc write_to+2
	dex
	bne page_loop

	rts
}

.swap_frame_buffers
{
    \\ Toggle draw buffer
    lda draw_buffer_HI
    eor #HI(screen1_addr EOR screen2_addr)
    sta draw_buffer_HI

	\\ Set screen buffer address in CRTC - not read until vsync
	cmp #HI(screen1_addr)
    IF _DOUBLE_BUFFER
	beq show_screen2
	ELSE
	bne show_screen2
    ENDIF

	\\ Show screen 1
    lda #12:sta &fe00
    lda #HI(screen1_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen1_addr/8):sta &fe01

	\\ Draw screen 2
	lda #HI(screen2_row_HI)
	sta plot_long_span_set_screen+2
	sta plot_short_span_set_screen+2

		lda #LO(span_row_table_screen2_LO)
		sta plot_span_set_row_table_LO+1
		lda #HI(span_row_table_screen2_LO)
		sta plot_span_set_row_table_LO+2
		lda #LO(span_row_table_screen2_HI)
		sta plot_span_set_row_table_HI+1
		lda #HI(span_row_table_screen2_HI)
		sta plot_span_set_row_table_HI+2

	\\ Continue
	bne continue

	.show_screen2
	\\ Show screen 2
    lda #12:sta &fe00
    lda #HI(screen2_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen2_addr/8):sta &fe01

	\\ Draw screen 1
	lda #HI(screen1_row_HI)
	sta plot_long_span_set_screen+2
	sta plot_short_span_set_screen+2
		
		lda #LO(span_row_table_screen1_LO)
		sta plot_span_set_row_table_LO+1
		lda #HI(span_row_table_screen1_LO)
		sta plot_span_set_row_table_LO+2
		lda #LO(span_row_table_screen1_HI)
		sta plot_span_set_row_table_HI+1
		lda #HI(span_row_table_screen1_HI)
		sta plot_span_set_row_table_HI+2

	\\ Continue
	.continue
	jmp return_here_from_swap_frame_buffers
}

.main_end

\ ******************************************************************
\ *	FX CODE
\ ******************************************************************

.fx_start

;INCLUDE "lib/disksys.asm"
INCLUDE "src/parse_frame.asm"
INCLUDE "src/plot_poly.asm"
INCLUDE "src/screen.asm"

.fx_end

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
ENDIF

.mode5_palette
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

;.filename0
;EQUS "00", 13

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
EQUB &20 + STREAMING_sectors_to_load		; sector size / number sectors = 256 / 10
.osword_params_return
EQUB 0				; returned error value

.drive_order
EQUB 2,3,1,0

.colour_table
EQUB &00, &0F, &F0, &FF

include "src/plot_data.asm"

PAGE_ALIGN_FOR_SIZE 33*4
.long_span_tables
FOR col,0,32,1
EQUB (32-col)*3					; +0,x for span_column_offset
EQUB (col*8) AND 255			; +1,x for mult_8
if not(_NULA)
EQUB 0							; +2,x spare
EQUB 0							; +3,x spare
endif
NEXT
CHECK_SAME_PAGE_AS long_span_tables

.data_end

\ ******************************************************************
\ *	ADDITIONAL CODE MODULES
\ ******************************************************************

.additional_start

INCLUDE "src/debug.asm"
INCLUDE "src/tests.asm"

.additional_end

\ ******************************************************************
\ *	RELOCATABLE DATA OVERLAYING BSS DATA
\ ******************************************************************

PAGE_ALIGN
.reloc_from_start
.reloc_screen_row_LO
FOR n,0,255,1
row=n DIV 8:sl=n MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB LO(screen1_addr + addr)
NEXT
CHECK_SAME_PAGE_AS reloc_screen_row_LO

.reloc_screen1_row_HI
FOR n,0,255,1
row=n DIV 8:sl=n MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB HI(screen1_addr + addr)
NEXT
CHECK_SAME_PAGE_AS reloc_screen1_row_HI

.reloc_screen2_row_HI
FOR n,0,255,1
row=n DIV 8:sl=n MOD 8
addr = row * SCREEN_ROW_BYTES + sl
EQUB HI(screen2_addr + addr)
NEXT
CHECK_SAME_PAGE_AS reloc_screen2_row_HI

.reloc_y_to_row		; div_8
FOR n,0,255,1
row=n DIV 8
EQUB row
NEXT
CHECK_SAME_PAGE_AS reloc_y_to_row

\ ******************************************************************
\ *	PALETTE DATA - MUST BE PAGE ALIGNED DUE TO SMC
\ ******************************************************************

PAGE_ALIGN  ; lazy
if not(_NULA)
include "src/palette.asm"
endif

.reloc_screen_col_LO
FOR n,0,127,1
if _NULA
col=n DIV 2
else
col=n DIV 4
endif
EQUB LO(col*8)
NEXT
CHECK_SAME_PAGE_AS reloc_screen_col_LO

.reloc_from_end

if not(_NULA)
PAGE_ALIGN
GUARD P%+&300
include "src/palette_stream.asm"
endif

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "STNICC", start, end, main

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

.bss_start

CLEAR reloc_from_start, screen2_addr
ORG reloc_from_start
GUARD screen2_addr

PAGE_ALIGN
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
PRINT "ZP size =", ~zp_end-zp_start, "(",~&C0-zp_end,"free)"
PRINT "MAIN size =", ~main_end-main_start
PRINT "FX size = ", ~fx_end-fx_start
PRINT "DATA size =",~data_end-data_start
PRINT "ADDITIONAL size =",~additional_end-additional_start
PRINT "RELOC size =",~reloc_from_end-reloc_from_start
if not(_NULA)
PRINT "PALETTE STREAM size =",~palette_stream_end-palette_stream_start
endif
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~screen2_addr-P%
PRINT "------"

\ ******************************************************************
\ *	SWRAM
\ ******************************************************************

CLEAR 0, &FFFF
ORG &8000
GUARD &C000
.bank0_start

.bank0_end

PRINT "------"
PRINT "BANK 0"
PRINT "------"
PRINT "TOTAL size =", ~bank0_end-bank0_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~&C000-P%
PRINT "------"

;SAVE "BANK0", bank0_start, bank0_end

\ ******************************************************************
\ *	DISC LAYOUT
\ ******************************************************************

exe_size=(end-start+&ff)AND&FF00
PRINT "EXE size = ",~exe_size
; We know that Catalog + !Boot = &300
; Need to make a dummy file so 00 is at sector 20=track 2
dummy_size = (DISK1_first_track * DFS_track_size) - exe_size - &300

CLEAR &0000,&FFFF
ORG &0000
.dummy
skip dummy_size
SAVE "dummy", dummy, P%

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************

PUTFILE "data/scene1_disk.00.bin", "00", 0
