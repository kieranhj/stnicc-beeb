\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB OUTRO
\ ******************************************************************

_DEBUG = TRUE
_DEBUG_RASTERS = FALSE

; Display <drive no | sector no> <track no> <load to HI> <stream ptr HI>
_SHOW_STREAMING_INFO = TRUE

; If set, show total vsync count, rather than just the count for the
; last frame. Intended for use in conjunction with _STOP_AT_FRAME.
_SHOW_TOTAL_VSYNC_COUNTER = TRUE
_STOP_AT_FRAME = -1
; Debug defines
_DOUBLE_BUFFER = TRUE
_PLOT_WIREFRAME = TRUE
_HALF_VERTICAL_RES = TRUE
_NULA = FALSE
_ENABLE_MUSIC = TRUE

PRINT "------"
PRINT "STNICC-BEEB OUTRO"
PRINT "------"
PRINT "_DEBUG =",_DEBUG
PRINT "_SHOW_STREAMING_INFO =",_SHOW_STREAMING_INFO
PRINT "_SHOW_TOTAL_VSYNC_COUNTER =",_SHOW_TOTAL_VSYNC_COUNTER
PRINT "_STOP_AT_FRAME =",_STOP_AT_FRAME
PRINT "_DOUBLE_BUFFER =",_DOUBLE_BUFFER
PRINT "_PLOT_WIREFRAME =",_PLOT_WIREFRAME
PRINT "------"

include "src/zp.h.asm"
include "src/music.h.asm"

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
oscli  = &FFF7
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

ULA_Mode4 	= &88

DFS_sector_size = 256
DFS_sectors_per_track = 10
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

\ ******************************************************************
\ *	MACROS
\ ******************************************************************

MACRO MODE4_PIXELS a,b,c,d
    EQUB (a AND 1) * &08 OR (b AND 1) * &04 OR (c AND 1) * &02 OR (d AND 1) * &01
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

MACRO SETBGCOL col
IF _DEBUG_RASTERS
{
	LDA #&00 + col:STA &FE21
	LDA #&10 + col:STA &FE21
	LDA #&20 + col:STA &FE21
	LDA #&30 + col:STA &FE21
	LDA #&40 + col:STA &FE21
	LDA #&50 + col:STA &FE21
	LDA #&60 + col:STA &FE21
	LDA #&70 + col:STA &FE21
}
ENDIF
ENDMACRO

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

; SCREEN constants
SCREEN_WIDTH_PIXELS = 320
SCREEN_HEIGHT_PIXELS = 14*8
SCREEN_SIZE_BYTES = (SCREEN_WIDTH_PIXELS * SCREEN_HEIGHT_PIXELS) / 8

TOP_SCREEN_ROWS = 14

MODE4_ROW_BYTES = 320
TEXT_BOX_COLS = 20
TEXT_BOX_ROWS = 8
CURSOR_SPEED = 25
CURSOR_CODE = 128
QUARTER_CODE = 129

WIREFRAME_CORNER_X = 96		; centred
WIREFRAME_CORNER_Y = 1

; NB. top screen from main part is &6700
stnicc_screen1_addr = &6700

screen1_addr = &6E00
screen2_addr = &5C00
screen3_addr = &4500

end_screen_addr = &5800

; STREAMING constants
STREAMING_tracks_per_disk = 79
STREAMING_sectors_to_load = 10

DISK1_drive_no = 0
DISK1_first_track = 30
DISK1_last_track = DISK1_first_track + 50

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
TimerValue = (40 + 14*8)*64 - 2*64

Timer2Value = (40)*64 - 2*64
Timer2Period = (48)*64

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD zp_top

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
IF _PLOT_WIREFRAME OR _DEBUG
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

.poly_verts_x       skip MAX_VERTS_PER_POLY+1
.poly_verts_y       skip MAX_VERTS_PER_POLY+1

; frame parser
.frame_no           skip 2
.frame_flags        skip 1
.frame_bitmask      skip 2
.indexed_num_verts  skip 1
;.poly_descriptor    skip 1
.eof_flag			skip 1

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

.timer2_cycle		skip 1

; credits vars
.char_def			skip 9
.glyphptr			skip 2
.glyphptr_copy		skip 2
.text_ptr			skip 2
.text_index			skip 1
.text_wait			skip 1
.text_lock			skip 1
.cursor_x			skip 1
.cursor_y			skip 1
.text_cls			skip 1
.cursor_timer		skip 1
.cursor_state		skip 1

; debug vars
IF _DEBUG
.pause_lock			skip 1
.last_vsync         skip 1
.debug_writeptr		skip 2
ENDIF

.music_enabled		skip 1
.music_lock			skip 1

.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

; Can't use &300 until we remove any actual VDU calls
ORG &400
GUARD &B00
.reloc_to_start
.screen_row_LO		skip 16
.screen_row_HI		skip 16
.screen_col_LO		skip 80
.screen_col_HI		skip 80
.credits_text		skip &600
.credits_end
.reloc_to_end

ORG &E00
GUARD &1000

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1100
GUARD screen3_addr

.start

.main_start

\ ******************************************************************
\ *	Code entry
\ ******************************************************************

.main
{
    \\ Init ZP
    lda #0
    ldx #0
    .zp_loop
    sta &00, x
    inx
    cpx #zp_top
    bne zp_loop

    ; fix up the NuLA message, if appropriate
	bit NULA_FLAG_ZP
	bpl nula_done
	ldx #nula_message_end-nula_message_begin-1
.nula_message_loop
	lda nula_message_begin,x
	sta just_3bit_colour_begin,x
	dex
	bpl nula_message_loop
.nula_done

	SEI
	LDA #&7F					; A=01111111
	STA &FE4E					; R14=Interrupt Enable (disable all interrupts)
	CLI

	\\ Relocate data to lower RAM
	lda #HI(reloc_from_start)
	ldy #HI(reloc_to_start)
	ldx #HI(reloc_to_end - reloc_to_start + &ff)
	jsr copy_pages

	\\ Clear invisible screens here!
	ldy #HI(screen3_addr)						; from page
	ldx #HI(stnicc_screen1_addr - screen3_addr)	; number pages
	jsr clear_pages

	IF 0
    lda #8:sta &fe00:lda #&C0:sta &fe01  ; cursor off
	lda #7:sta &fe00:lda #34:sta &fe01	 ; vsync pos
	ENDIF

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
IF 0
	\\ Init text system
	lda #CURSOR_CODE
	ldx #LO(cursor_char_def)
	ldy #HI(cursor_char_def)
	jsr def_char

    lda #QUARTER_CODE
    ldx #LO(quarter_def)
    ldy #HI(quarter_def)
    jsr def_char
ENDIF

	ldx #0:ldy #0:jsr set_cursor_XY
	lda #CURSOR_SPEED:sta cursor_timer
	ldx #LO(credits_text):ldy #HI(credits_text)
	stx text_ptr:sty text_ptr+1

	\\ Init music
	{
		lda MUSIC_SLOT_ZP
		bmi no_music
		sta &f4:sta &fe30
		jsr MUSIC_JUMP_INIT_OUTRO
		.no_music
	}

	\\ Setup video
	jsr wait_for_vsync

	\\ Set CRTC to MODE 4
	jsr reset_crtc_regs

	\\ Set ULA to MODE 4
	lda #ULA_Mode4
	sta &248			; OS copy
	sta &fe20

	\\ Set palette
	ldx #15
	.pal_loop
	lda mode4_palette, X
	sta &fe21
	dex
	bpl pal_loop

	\\ Clear visible screen here!
	ldy #HI(stnicc_screen1_addr)			; from page
	ldx #HI(&8000 - stnicc_screen1_addr)	; number pages
	jsr clear_pages

	\\ Set interrupts and handler
	SEI							; disable interupts
	jsr wait_for_vsync

	\\ Close enough for our purposes
	; Write T1 low now (the timer will not be written until you write the high byte)
    LDA #LO(TimerValue):STA &FE44
    ; Get high byte ready so we can write it as quickly as possible at the right moment
    LDX #HI(TimerValue):STX &FE45            ; start T1 counting		; 4c +1/2c 

  	; Latch T1 to interupt exactly every 50Hz frame
	LDA #LO(FramePeriod):STA &FE46
	LDA #HI(FramePeriod):STA &FE47

	STA &FE43					; R3=Data Direction Register "A" (set keyboard data direction)
	lda #&40					; A=01000000
	sta &fe4b					; R11=ACR (Timer 1 continuous; Timer 2 one-shot)
	LDA #&E2					; A=11000010
	STA &FE4E					; R14=Interrupt Enable (enable main_vsync and timer interrupt)

    LDA IRQ1V:STA old_irqv
    LDA IRQ1V+1:STA old_irqv+1

    LDA #LO(irq_handler):STA IRQ1V
    LDA #HI(irq_handler):STA IRQ1V+1		; set interrupt handler
	CLI							; enable interupts

	\\ Need one frame to settle rupture
	jsr wait_for_vsync
	jsr wait_for_vsync

	\\ GO!
	{
		lda MUSIC_SLOT_ZP
		bmi no_music
		inc music_enabled
		.no_music
	}

	lda #8:sta &fe00:lda #&c0:sta &fe01		; show the screen!

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
	BEQ loop

	.track_load_error
	\\ Wait for vsync
	{
		ldx #100
		.loop
		jsr wait_for_vsync
		dex
		bne loop
	}

	\\ Disable interrupts but replace old handler
	SEI
	LDA #&7F					; A=11010011
	STA &FE4E					; R14=Interrupt Enable

    LDA old_irqv:STA IRQ1V
    LDA old_irqv+1:STA IRQ1V+1	; set interrupt handler
	CLI

	{
		lda MUSIC_SLOT_ZP
		bmi no_music
		sta &f4:sta &fe30
		jsr MUSIC_JUMP_SN_RESET
		.no_music
	}

	jsr reset_crtc_regs
	lda #8:sta &fe00:lda #&f0:sta &fe01		; hide screen

	IF 0
	{
	    .wait_for_Key
	    lda #&79:ldx #&10:jsr osbyte:cpx #&ff:beq wait_for_Key
	}
	ENDIF

    \\ Load next part
    ldx #LO(next_part_cmd)
    ldy #HI(next_part_cmd)
    jmp oscli
}

.wait_for_vsync
{
	lda #2
	.vsync1
	bit &FE4D
	beq vsync1
	sta $fe4d
	rts
}

.reset_crtc_regs
{
	\\ Reset CRTC after rupture
	ldx #13
	.crtc_loop
	stx &fe00
	lda crtc_mode4_regs, X
	sta &fe01
	dex
	bpl crtc_loop

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

	lda osword_params_sector
	asl a:asl a:asl a:asl a
	ora osword_params_drive
	jsr debug_write_A

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

	BNE TRACK_LOAD_continue

	\\ End of last disk
	\\ Reached end of disk N
	LDA #&FF
	STA track_no
	BNE TRACK_LOAD_return

	.TRACK_LOAD_continue

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

	\\ Acknowledge vsync interrupt
	STA &FE4D

	lda #0
	sta screen_lock

 	; Set T2 to fire in a bit...
	LDA #LO(Timer2Value):STA &FE48
	LDA #HI(Timer2Value):STA &FE49

	\\ Vsync
    INC vsync_counter
    BNE no_carry
    INC vsync_counter+1
    .no_carry

	\\ Swap frame buffers
	jmp swap_frame_buffers
	.^return_here_from_swap_frame_buffers

    lda music_enabled
    beq no_music

	lda music_lock
	bne no_music

	inc music_lock
	lda &f4:pha
	lda MUSIC_SLOT_ZP
	sta &f4:sta &fe30
    txa:pha:tya:pha
    jsr MUSIC_JUMP_VGM_UPDATE
    pla:tay:pla:tax
	pla:sta &f4:sta &fe30
	dec music_lock
	.no_music

	JMP return_to_os

	.not_vsync
	\\ Which interrupt?
	LDA &FE4D
	AND #&40			; timer 1
	BEQ not_timer1

	jmp is_timer1

	.not_timer1
	\\ Which interrupt?
	LDA &FE4D
	AND #&20			; timer 2
	BNE is_timer2
	jmp not_timer2		; return_to_os

	\\ Acknowledge timer 2 interrupt
	.is_timer2
	STA &FE4D

	lda timer2_cycle
	eor #1
	sta timer2_cycle
	bne first_cycle
	jmp do_plot

	\\ First cycle
	.first_cycle
 	; Set T2 to fire in a bit...
	LDA #LO(Timer2Period):STA &FE48
	LDA #HI(Timer2Period):STA &FE49

    lda #4:sta &fe00:lda #13:sta &fe01		; Vertical total
	lda #7:sta &fe00:lda #20:sta &fe01		; Vsync > Vertical total
    lda #6:sta &fe00:lda #14:sta &fe01		; Vertical displayed

	; Fixed screen address for *next* cycle
    lda #12:sta &fe00
    lda #HI(screen3_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen3_addr/8):sta &fe01

	SETBGCOL PAL_red

	{
		LDA text_lock
		BNE not_timer2

		\\ Don't re-enter this section
		INC text_lock
		\\ Store registers in case of interupts
		TXA:PHA:TYA:PHA

		\\ Do the slow bit!
		CLI
		jsr process_text
		SEI

		\\ Restore registers
		PLA:TAY:PLA:TAX
		\\ Remove our work lock
		DEC text_lock
	}

	SETBGCOL PAL_black

	.not_timer2
	jmp return_to_os

	.second_cycle
	\\ Set up Vsync cycle
;	lda #1:sta &fe00:lda #80:sta &fe01		; Horizontal displayed
	; could centre screen here?	
	; lda #2:sta &fe00:lda #98:sta &fe01	; Horizontal sync - TBD
    lda #4:sta &fe00:lda #24:sta &fe01		; Vertical total
    lda #6:sta &fe00:lda #17:sta &fe01		; Vertical displayed

	; TEST
	SETBGCOL PAL_blue
	jmp return_to_os

	.is_timer1
	\\ Acknowledge timer 1 interrupt
	STA &FE4D
	jmp second_cycle

	.do_plot
	SETBGCOL PAL_green

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

		cmp #POLY_DESC_END_OF_STREAM
		bne not_eof
		inc decode_lock		; lock us out of decoding the stream.

		.not_eof
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
			lda track_no
			bmi enough_data_for_next_frame
			
			lda STREAM_ptr_HI
			cmp load_to_HI
			bcs stream_ptr_gt_load_to

			\\ Stream Ptr < Load To
			sec
			lda load_to_HI
			sbc STREAM_ptr_HI
			cmp #4
			bcs enough_data_for_next_frame

			\\ Otherwise lock our buffer
			lda #&ff:sta buffer_lock
			bne enough_data_for_next_frame

			.stream_ptr_gt_load_to
			\\ Stream Ptr > Load To
			clc
			lda load_to_HI
			adc #HI(STREAM_buffer_size)
			sec
			sbc STREAM_ptr_HI
			cmp #4
			bcs enough_data_for_next_frame

			\\ Otherwise lock our buffer
			lda #&ff:sta buffer_lock

			.enough_data_for_next_frame
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

    \\ Toggle draw buffer but not screen buffer (do that in vsync)
    lda draw_buffer_HI
    eor #HI(screen1_addr EOR screen2_addr)
    sta draw_buffer_HI

	\\ Pass on to OS IRQ handler
	.return_to_os
	PLA
	STA &FC
	RTI
}
.old_irqv EQUW &FFFF

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

; Y=to page, X=number of pages
.clear_pages
{
	sty write_to+2

	ldy #0
	lda #0
	.page_loop
	.write_to
	sta &ff00, Y
	iny
	bne page_loop
	inc write_to+2
	dex
	bne page_loop

	rts
}

.swap_frame_buffers
{
	\\ Set screen buffer address in CRTC - not read until vsync
    lda draw_buffer_HI
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

	\\ Continue
	jmp continue

	.show_screen2
	\\ Show screen 2
    lda #12:sta &fe00
    lda #HI(screen2_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen2_addr/8):sta &fe01

	\\ Draw screen 1

	\\ Continue
	.continue
	jmp return_here_from_swap_frame_buffers
}

.main_end

\ ******************************************************************
\ *	FX CODE
\ ******************************************************************

.fx_start

\ ******************************************************************
\ *	PARSE A FRAME OF DATA FROM STNICCC STREAM
\ ******************************************************************

IF 0		; this also makes no sense!
.deliberate_pause
{
	lda #2
	sta several_times
	lda #0
	sta pause_me
	.loop
	dec pause_me
	bne loop
	dec several_times
	bne loop
	rts
	.pause_me equb 0
	.several_times equb 0
}
ENDIF

MACRO GET_BYTE
{
    inc STREAM_ptr_LO
    bne no_carry
    inc STREAM_ptr_HI

	\\ This is an attempt to stop us running out of data
	\\ but doesn't always work yet. I guess when the track
	\\ hasn't even been requested yet; we can't yield back
	\\ to the main loop. Hmmmm.
;	lda STREAM_ptr_HI
;	cmp load_to_HI
;	bne no_carry

; This makes no sense!
;	jsr deliberate_pause

    .no_carry
    lda (STREAM_ptr_LO), y
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

    GET_BYTE
    tax ; poly_descriptor
    cmp #POLY_DESC_END_OF_STREAM
    bcs parse_end_of_frame

    and #&f
    sta poly_num_verts

	\\ We don't care about the colour of the poly

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
    ; _HALF_VERTICAL_RES
    sta poly_verts_y, X

    \\ Next step would be to inline the poly loop here.

    inx
    cpx poly_num_verts
    bcc read_poly_loop

    jsr plot_poly_line
    jmp read_poly_data

    .non_indexed_data
    ldx #0
    .read_poly_ni_loop

    \\ This can be changed to read the poly data directly.
    GET_BYTE
    sta poly_verts_x, X
    GET_BYTE
    ; _HALF_VERTICAL_RES
    sta poly_verts_y, X

    \\ Next step would be to inline the poly loop here.

    inx
    cpx poly_num_verts
    bcc read_poly_ni_loop

    jsr plot_poly_line
    jmp read_poly_data

    .parse_end_of_frame

    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

    rts
}

INCLUDE "src/screen4.asm"

\ ******************************************************************
\ *	CREDITS BIT
\ ******************************************************************

.get_char_def
{
    sta char_def
	bmi local_char_def

    lda #10
    ldx #LO(char_def)
    ldy #HI(char_def)
    jmp osword

	.local_char_def
	sec
	sbc #128
	asl a:asl a:asl a
	tax
	ldy #0
	.loop
	lda local_char_defs, X
	sta char_def+1, Y
	inx
	iny
	cpy #8
	bne loop
	rts
}

.plot_glyph_at_ptr
{
	lda glyphptr
	sta glyphptr_copy
	lda glyphptr+1
	sta glyphptr_copy+1

	ldx #0
	.loop
	lda char_def+1, X
	pha
	lsr a:lsr a
	lsr a:lsr a
	tay
	lda four_bits_to_four_pixels, y
	ldy #0:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y

	pla:and #&f:tay
	lda four_bits_to_four_pixels, y
	ldy #8:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y

	inc glyphptr_copy
	inc glyphptr_copy

	inx
	cpx #4
	bne ok

	clc
	lda glyphptr_copy
	adc #LO(MODE4_ROW_BYTES-8)
	sta glyphptr_copy
	lda glyphptr_copy+1
	adc #HI(MODE4_ROW_BYTES-8)
	sta glyphptr_copy+1
	.ok

	cpx #8
	bcc loop

	rts
}

.plot_char_at_cursor
{
	pha
	jsr cursor_remove
	pla
	jsr get_char_def
	jsr plot_glyph_at_ptr
}
\\ fall through
.step_removed_cursor
{
	; Step cursor but check window extents
	ldx cursor_x
	inx
	cpx #TEXT_BOX_COLS
	bcc x_ok
	ldx #0

	ldy cursor_y
	iny
	cpy #TEXT_BOX_ROWS
	bcc y_ok
	ldy #0
	.y_ok
	sty cursor_y

	.x_ok
	stx cursor_x

	jsr calc_cursor_XY
	jmp cursor_redraw
}

.wipe_char_at_cursor
{
	jsr cursor_remove
	jsr plot_blank_at_ptr
	jmp step_removed_cursor
}

.plot_blank_at_ptr
{
	lda glyphptr
	sta glyphptr_copy
	lda glyphptr+1
	sta glyphptr_copy+1

	ldx #1
	.loop
	lda #0
	tay:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y
	dex
	bmi done_loop

	clc
	lda glyphptr_copy
	adc #LO(MODE4_ROW_BYTES)
	sta glyphptr_copy
	lda glyphptr_copy+1
	adc #HI(MODE4_ROW_BYTES)
	sta glyphptr_copy+1
	jmp loop

	.done_loop
	rts
}

.plot_block_at_ptr
{
	lda glyphptr
	sta glyphptr_copy
	lda glyphptr+1
	sta glyphptr_copy+1

	ldy #0
	.loop1
	lda block_mode4_data, Y
	sta (glyphptr_copy), Y
	iny
	cpy #16
	bcc loop1

	clc
	lda glyphptr_copy
	adc #LO(MODE4_ROW_BYTES)
	sta glyphptr_copy
	lda glyphptr_copy+1
	adc #HI(MODE4_ROW_BYTES)
	sta glyphptr_copy+1

	ldy #0
	.loop2
	lda block_mode4_data+16, Y
	sta (glyphptr_copy), Y
	iny
	cpy #16
	bcc loop2

	rts
}

.backspace_at_cursor
{
	jsr plot_blank_at_ptr

	; step cursor backwards
	ldx cursor_x
	dex
	bpl x_ok
	ldx #TEXT_BOX_COLS-1

	ldy cursor_y
	dey
	bpl y_ok
	ldy #TEXT_BOX_ROWS-1
	.y_ok
	sty cursor_y

	.x_ok
	stx cursor_x

	jsr calc_cursor_XY
	jmp cursor_redraw
}

.calc_cursor_XY
{
	lda cursor_x
	asl a
	tax
	lda cursor_y
	asl a
	tay
	iny
	; calc_glyphptr_XY
	clc
	lda screen_row_LO, Y
	adc screen_col_LO, X
	sta glyphptr
	lda screen_row_HI, Y
	adc screen_col_HI, X
	sta glyphptr+1
	rts
}

IF 0
.plot_string_at_ptr
{
	stx text_ptr
	sty text_ptr+1

	ldy #0
	.loop
	sty text_index
	lda (text_ptr), Y
	beq done_loop
	jsr plot_char_at_ptr
	ldy text_index
	iny
	bne loop
	.done_loop
	rts
}

.plot_char_at_ptr
{
	jsr get_char_def
	jmp plot_glyph_at_ptr
}
ENDIF

MACRO TEXT_PTR_INC
{
	inc text_ptr
	bne no_carry
	inc text_ptr+1
	.no_carry
}
ENDMACRO

MACRO TEXT_PTR_ADC add
{
	clc
	lda text_ptr
	adc #add
	sta text_ptr
	bcc no_carry
	inc text_ptr+1
	.no_carry
}
ENDMACRO

.set_cursor_XY
{
	txa:pha:tya:pha
	jsr cursor_remove
	pla:sta cursor_y
	pla:sta cursor_x
	jsr calc_cursor_XY
	jmp cursor_redraw
}

.process_text
{
	jsr cursor_update
	
	\\ Are we in a wait state?
	lda text_wait
	beq not_waiting
	dec text_wait
	rts

	.not_waiting
	lda text_cls
	beq not_in_cls

	jsr wipe_char_at_cursor
	jsr wipe_char_at_cursor

	lda cursor_x
	ora cursor_y
	bne still_cls		; stop when cursor wraps to 0,0
	sta text_cls
	.still_cls
	rts

	.not_in_cls
	.read_next_char
	ldy #0
	lda (text_ptr), Y
	beq eos

	cmp #32
	bcc vdu_code

	\\ ASCII
	jsr plot_char_at_cursor

	.return
	TEXT_PTR_INC
	rts

	.eos
	lda #LO(credits_text)
	sta text_ptr
	lda #HI(credits_text)
	sta text_ptr+1
	rts

	.vdu_code
	cmp #31
	bne not_vdu31

	\\ VDU 31, x, y
	iny:lda (text_ptr), Y
	tax
	iny:lda (text_ptr), Y
	tay
	jsr set_cursor_XY

	TEXT_PTR_ADC 3
	bne read_next_char

	.not_vdu31
	cmp #12
	bne not_cls

	\\ CLS
	ldx #0:ldy #0:jsr set_cursor_XY
	lda #1:sta text_cls
	bne return

	.not_cls
	cmp #1		; actually send next char to printer!
	bne not_wait

	iny:lda (text_ptr), Y
	sta text_wait
	TEXT_PTR_ADC 2
	rts

	.not_wait
	cmp #13
	bne not_cr
	\\ Carriage return
	{
		ldx #0
		ldy cursor_y
		iny
		cpy #TEXT_BOX_ROWS
		bcc y_ok
		ldy #0
		.y_ok
		sty cursor_y
		jsr set_cursor_XY
	}
	jmp return

	.not_cr
	cmp #8
	bne not_delete

	jsr backspace_at_cursor
	jmp return

	.not_delete
	\\ Unknown VDU code!
	TEXT_PTR_INC
	jmp read_next_char
}

.cursor_update
{
	dec cursor_timer
	bne same_state

	jsr cursor_remove

	\\ Toggle state
	lda cursor_state
	eor #1
	sta cursor_state

	jsr cursor_redraw

	lda #CURSOR_SPEED
	sta cursor_timer

	.same_state
	rts
}

.cursor_remove
{
	lda cursor_state
	beq return
	jsr plot_blank_at_ptr
	.return
	rts
}

.cursor_redraw
{
	lda cursor_state
	beq return
	jsr plot_block_at_ptr
	.return
	rts
}

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

.fx_end

\ ******************************************************************
\ *	DATA
\ ******************************************************************

.data_start

.crtc_mode4_regs
{
	EQUB 63					; R0  horizontal total
	EQUB 40					; R1  horizontal displayed
	EQUB 49					; R2  horizontal position
	EQUB &24				; R3  sync width
	EQUB 38					; R4  vertical total
	EQUB 0					; R5  vertical total adjust
	EQUB 32					; R6  vertical displayed
	EQUB 34					; R7  vertical position; 35=top of screen
	EQUB &F0				; R8  no interlace, no cursor, hide screen
	EQUB 7					; R9  scanlines per row
	EQUB 32					; R10 cursor start
	EQUB 8					; R11 cursor end
	EQUB HI(screen2_addr/8)	; R12 screen start address, high
	EQUB LO(screen2_addr/8)	; R13 screen start address, low
}

.mode4_palette
{
	EQUB &00 + PAL_black
	EQUB &10 + PAL_black
	EQUB &20 + PAL_black
	EQUB &30 + PAL_black
	EQUB &40 + PAL_black
	EQUB &50 + PAL_black
	EQUB &60 + PAL_black
	EQUB &70 + PAL_black
	EQUB &80 + PAL_white
	EQUB &90 + PAL_white
	EQUB &A0 + PAL_white
	EQUB &B0 + PAL_white
	EQUB &C0 + PAL_white
	EQUB &D0 + PAL_white
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
EQUB 2,0			; only two discs now. WAS 3,1,0

.two_bits_to_two_pixels
EQUB %00000000, %00110011, %11001100, %11111111

.four_bits_to_four_pixels
FOR b, 0, 15, 1
b0 = b AND 1
b1 = (b AND 2) >> 1
b2 = (b AND 4) >> 2
b3 = (b AND 8) >> 3
EQUB b3 * &C0 + b2 * &30 + b1 * &0C + b0 * &03
NEXT

.local_char_defs
{
.cursor_char_def
;EQUB &7F, &7F, &7F, &7F
;EQUB &7F, &7F, &7F, &00
EQUB &FF, &FF, &FF, &FF
EQUB &FF, &FF, &FF, &FF

.quarter_def
EQUB %00100000
EQUB %00100110
EQUB %00101100
EQUB %00011000
EQUB %00110101
EQUB %01100111
EQUB %00000001
EQUB %00000000

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
EQUB %10111010
EQUB %11000110
EQUB %01111100
}

.block_mode4_data
EQUB &3F, &3F, &3F, &3F, &3F, &3F, &3F, &3F
EQUB &FF, &FF, &FF, &FF, &FF, &FF, &FF, &FF
EQUB &3F, &3F, &3F, &3F, &3F, &3F, &00, &00
EQUB &FF, &FF, &FF, &FF, &FF, &FF, &00, &00

.next_part_cmd
EQUS "/INTRO", 13

.nula_message_begin
EQUS "80 x 100 pixels   "
.nula_message_end

.data_end

\ ******************************************************************
\ *	ADDITIONAL CODE MODULES
\ ******************************************************************

.additional_start

INCLUDE "src/plot_line4.asm"
INCLUDE "src/debug4.asm"

.additional_end

\ ******************************************************************
\ *	RELOCATABLE DATA OVERLAYING BSS DATA
\ ******************************************************************

PAGE_ALIGN
.reloc_from_start
.reloc_screen_row_LO
FOR y,0,15,1
row=y:sl=0
addr = row * MODE4_ROW_BYTES + sl
EQUB LO(screen3_addr + addr)
NEXT

.reloc_screen_row_HI
FOR y,0,15,1
row=y:sl=0
addr = row * MODE4_ROW_BYTES + sl
EQUB HI(screen3_addr + addr)
NEXT

.reloc_screen_col_LO
FOR c,0,79,1
EQUB LO(c * 8)
NEXT

.reloc_screen_col_HI
FOR c,0,79,1
EQUB HI(c * 8)
NEXT

.reloc_credits_text
; Page 0
;    |---------------------|
EQUS 31, 0, 0, 1, 150
EQUS "BBC Computer 32K"
EQUS 13, 13
EQUS "Acorn DFS"
EQUS 13, 13
EQUS "BASIC"
EQUS 13, 13
EQUS ">", 1, 100
EQUS 31, 5, 4, 8, 8, 8, 8, 8, 1, 50
EQUS "BITSHIFTERS"
EQUS 31, 1, 6, 1, 150, 12

; Page 1
;    |--------------------|
EQUS "ST-NICCC 2000", 1, 50
EQUS 8, 8, 8, 8, "BEEB", 1, 50, 13, 13
EQUS ">", 1, 25
EQUS "2MHz 6502 CPU", 1, 50, 13
EQUS ">", 1, 25
EQUS "32K RAM + 16K SWRAM"
EQUS ">", 1, 50
EQUS "5", QUARTER_CODE, '"'," 400K floppy", 1, 50, 13
EQUS ">", 1, 25
EQUS "No VIC, no Blitter", 1, 50, 13
EQUS ">", 1, 25
.just_3bit_colour_begin:EQUS "Just 3-bit colour!":.just_3bit_colour_end:EQUS 1, 50, 13
EQUS ">", 1, 25
EQUS "SN76489 sound chip", 1, 150, 12

; Page 2
;    |--------------------|
EQUS "Sorry we are a year", 13
EQUS "late to the STNICCC", 13
EQUS "patarty", 1, 25
EQUS 8, 8, 8, 8, 8, "rty.", 1, 50
EQUS 13, 13
EQUS "Squeezing a 16-bit", 13
EQUS "1 Mb demo into", 13
EQUS "8-bits and 48K took", 13
EQUS "us some time...", 1, 50
EQUS "!", 1, 50, 12

; Page 2
;    |--------------------|
EQUS "Original ST polygon", 13
EQUS "stream data repacked"
EQUS "with half the bits.", 13
EQUS ">> 320K vs 625K <<", 1, 100, 13
EQUS 13
EQUS "2MHz 6502", 13
EQUS 31,5,6,"vs 8MHz 68000", 1, 50
EQUS "!", 1, 50, 12

; Page 3
;    |--------------------|
EQUS "CREDITS", 1, 50, 13, 13
EQUS "Code:       kieranhj", 1, 50
EQUS "Code:     Tom Seddon", 1, 50
EQUS "Code:         Henley", 1, 50
EQUS "Logo by:    Dethmunk"
EQUS 1, 100, 12

; Page 4
;    |--------------------|
EQUS "MUSIC", 1, 50, 13, 13
EQUS "All music composed",13
EQUS "in Arkos Tracker 2",13
EQUS "by Rhino / Torment", 1, 75
EQUS 13, 13
EQUS "Checknobankh by"
EQUS 31,2,7,"Laxity / Kefrens"
EQUS 1, 100, 12

; Page 5
;    |--------------------|
EQUS "BITSHIFTERS THANKS", 1, 50, 13, 13
EQUS "Matt Godbolt.....RTW"
EQUS "scarybeasts....StewB"
EQUS "The Master....Tricky"
EQUS "0xC0DE....VectorEyes"
EQUS "HEx.....HorsenBurger"
EQUS 1, 100, 12

; Page 6
;    |--------------------|
EQUS "BITSHIFTERS GREETZ", 1, 50, 13, 13
EQUS ">", 1, 25
EQUS "Ate Bit", 1, 25, 13
EQUS ">", 1, 25
EQUS "CRTC", 1, 25, 13
EQUS ">", 1, 25
EQUS "DESiRE", 1, 25, 13
EQUS ">", 1, 25
EQUS "HOOY-PROGRAM", 1, 25, 13
EQUS ">", 1, 25
EQUS "Inverse Phase", 1, 25, 13
EQUS ">", 1, 25
EQUS "Logicoma", 1, 25
EQUS 12

; Page 7
; Greets continued
;    |--------------------|
EQUS "BITSHIFTERS GREETZ", 1, 50, 13, 13
EQUS ">", 1, 25
EQUS "Oxygene", 1, 25, 13
EQUS ">", 1, 25
EQUS "Polarity", 1, 25, 13
EQUS ">", 1, 25
EQUS "RiFT", 1, 25, 13
EQUS ">", 1, 25
EQUS "Slipstream", 1, 25, 13
EQUS ">", 1, 25
EQUS "Torment", 1, 25, 13
EQUS ">", 1, 25
EQUS "YM Rockerz", 1, 25
EQUS 12

; Page 8
; BASIC
;    |--------------------|
EQUS 1, 100
EQUS "BASIC", 13, 13
EQUS ">", 1, 75
EQUS "10 CLS", 13
EQUS ">", 1, 25
EQUS "20 PRINT ", '"', "THANKS FOR WATCHING!", '"', 13
EQUS ">", 1, 25
EQUS "30 GOTO 20", 13
EQUS ">", 1, 50
EQUS "RUN", 13
EQUS 1, 50, 12

; Page 9
; BASIC
;    |--------------------|
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING"
EQUS 1, 50, 12

; Page 10
; Bitshifters Flux
;    |--------------------|
EQUS 1, 50
EQUS 31,7,0,32,32,32,128,128,128,1,50
EQUS 31,7,1,32,32,32,128,128,128,1,50
EQUS 31,7,2,128,128,128,128,128,128,1,50
EQUS 31,7,3,128,128,128,128,128,128,1,50
EQUS 31,7,4,128,128,128,128,128,128,1,50
EQUS 31,7,5,128,128,128,32,32,32,1,50
EQUS 31,7,6,128,128,128,32,32,32
EQUS 31,19,7
EQUS 1, 255, 1, 255, 12

; Time for 9 pages max!
EQUS 0
.reloc_credits_end

IF (P%-reloc_credits_text) > (credits_end-credits_text)
	ERROR "Need more space for credits_text."
ENDIF

IF nula_message_end-nula_message_begin<>just_3bit_colour_end-just_3bit_colour_begin
    error "NuLA message is the wrong size"
endif

.reloc_from_end

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "build/OUTRO", start, end, main

\ ******************************************************************
\ *	Space reserved for runtime buffers not preinitialised
\ ******************************************************************

.bss_start

CLEAR reloc_from_start, screen3_addr
ORG reloc_from_start
GUARD screen3_addr

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
PRINT "CREDITS TEXT size =", (reloc_credits_end-reloc_credits_text), " allocated =",(credits_end-credits_text)
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~screen3_addr-P%
PRINT "------"
