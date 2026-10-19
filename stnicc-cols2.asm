\\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	BEEB-NICCC: THE 1800-FRAME COLUMN PLAYER (CPC PALETTE)
\ ******************************************************************

\\ Plays the version 2 column stream (bin/colstream2.py): every frame of the
\\ original, each one's final picture as 32 byte columns of runs, each column
\\ coded against the column to its left or the same column in the frame before
\\ (with or without each run's motion), in variable-length codes. The picture
\\ and the fill are the 900-frame column player's (stnicc-cols.asm): MODE 2
\\ packing at 64 x 100, each row on both its scanlines, every screen byte written
\\ once; short runs through a pointer, long ones through unrolled stores.
\\
\\ The runs of the frame before are kept until each column has used them, in a
\\ ring of RING_SIZE runs: a column's runs go where the last column's ended, or
\\ at the start of the ring if fewer than COLUMN_MAX places are left. The ring
\\ is three arrays (value, end row, slope) three pages apart, so a run's three
\\ bytes share an address's low byte. bin/colstream2.py checks that a column
\\ never overwrites a run still wanted.
\\
\\ Built by make.bat onto its own disc (stnicc-cols2-build.asm); run.mjs
\\ --variant cols2 plays it.

_DEBUG = FALSE
_NULA = TRUE                    ; for the shared modules: as the CPC build
_CPC = TRUE

include "src/zp.h.asm"
include "src/music.h.asm"
include "src/cols2_disc.h.asm"

\ ******************************************************************
\ *	OS defines
\ ******************************************************************

osbyte = &FFF4
osword = &FFF1
oscli  = &FFF7

IRQ1V = &204

PAL_black	= (0 EOR 7)
PAL_white	= (7 EOR 7)
PAL_red		= (1 EOR 7)
PAL_yellow	= (3 EOR 7)

ULA_Mode5 	= &C4
ULA_Mode4 	= &88

DFS_sector_size = 256
DFS_sectors_per_track = 10
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

OPCODE_RTS = &60
OPCODE_STA_ABSX = &9D
OPCODE_DEC_zp = &C6             ; load_next_track's track step on side 2

\ ******************************************************************
\ *	GLOBAL constants
\ ******************************************************************

SCREEN_ROW_BYTES = 256
SCREEN_SIZE_BYTES = &1900
screen1_addr = &8000 - SCREEN_SIZE_BYTES
screen2_addr = screen1_addr - SCREEN_SIZE_BYTES

STREAMING_sectors_to_load = 10
DISK1_drive_no = 0
DISK1_first_track = COLS2_FIRST_TRACK
DISK1_last_track = 80
DISK2_first_track = 79          ; side 2 runs backwards (bin/split.py)
DISK2_last_track = 0

STREAM_buffer_size = 3 * DFS_track_size
STREAM_buffer_start = screen2_addr - STREAM_buffer_size
STREAM_buffer_end = screen2_addr
\\ Pages that must be loaded before a frame starts (stream_check_ahead): the
\\ largest frame (bin/colstream2.py prints it) must fit in (N-1)*256+1 bytes
STREAM_pages_ahead = 3

POLY_DESC_END_OF_STREAM = &FD   ; parse_frame's return: the stream ends
POLY_DESC_SKIP_TO_64K = &FE     ; the next frame starts the next block

FramePeriod = 312*64-2
TimerValue = (12)*64 - 2*64

ROWS = 100
SHORT_RUN = 4                   ; runs of up to this many rows are stored directly

INCLUDE "src/cols2_ring.h.asm"

\ ******************************************************************
\ *	ZERO PAGE
\ ******************************************************************

ORG &00
GUARD zp_top

.zp_start
.STREAM_ptr_LO      skip 1
.STREAM_ptr_HI      skip 1
.stream_frame_HI	skip 1		; page of the frame being decoded (the loader leaves it alone)
.frame_no           skip 2
.eof_flag			skip 1
.cpc_count          skip 1

.vsync_counter      skip 2
.draw_buffer_HI     skip 1
.code_or            skip 1      ; 0 when drawing screen 1, 4 for screen 2's column code
.sector_no          skip 1
.track_no			skip 1
.load_to_HI			skip 1
.error_flag			skip 1
.decode_lock		skip 1
.screen_lock		skip 1
.buffer_lock		skip 1
.vsync_final		skip 2
.clock_minutes		skip 1
.clock_seconds		skip 1
.clock_flashes		skip 1
.char_def			skip 9
.glyphptr			skip 2
.glyphptr_copy		skip 2
.music_enabled		skip 1
.music_lock			skip 1

INCLUDE "src/cols2_decode_zp.asm"

\\ the fill
.colour             skip 1      ; rebuild_colour's
.run_n              skip 1      ; rows left to store (short runs)
.save_x             skip 1      ; the decoder's X and Y, while a run is stored
.save_y             skip 1
.vB                 skip 2      ; and the store it is stopped at
.dptr               skip 2      ; screen pointer (short runs)
.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

\\ The colours, in the ring arrays' spare tails after the column tables

cpc_colour_top = col_start_lo + 64      ; each colour's top line byte (cpc_palette_update)
cpc_colour_bottom = cpc_colour_top + 16 ; and its bottom line byte on even rows
cpc_colour_delta = cpc_colour_bottom + 16 ; what to EOR with it on odd rows
shadow_top = col_start_hi + 64          ; the colours the pair tables were built from
shadow_bottom = shadow_top + 16
shadow_delta = shadow_bottom + 16

\\ Each pixel pair's bytes, from the palette (build_pair_tables); pair_bottom_odd is in main RAM
pair_top = &E00                 ; top line
pair_bottom_even = &F00         ; bottom line, even rows

\ ******************************************************************
\ *	CODE START
\ ******************************************************************

ORG &1100
GUARD STREAM_buffer_start

.start
.main_start

.main
{
    \\ Init ZP
    lda #0
    tax
    .zp_loop
    sta &00, x
    inx
    cpx #zp_top
    bne zp_loop

    \\ No colours yet: the first palette builds every pair (update_pair_tables)
    lda #&FF
    ldx #&2F
    .shadow_loop
    sta shadow_top, X
    dex
    bpl shadow_loop

    RING_INIT

    \\ Set stream pointer
    lda #LO(STREAM_buffer_start-1)
    sta STREAM_ptr_LO
    lda #HI(STREAM_buffer_start-1)
    sta STREAM_ptr_HI
    lda #HI(STREAM_buffer_start)
    sta stream_frame_HI

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

    \\ Init early system
    lda #HI(screen1_addr)
    sta draw_buffer_HI

	\\ Init music
	{
		lda MUSIC_SLOT_ZP
		bmi no_music
		sta &f4:sta &fe30
		jsr MUSIC_JUMP_INIT_MAIN
		.no_music
	}

	\\ Setup video
	jsr hide_screen

	\\ The CPC build's ULA and CRTC set-up: 128 x 200 of MODE 5's clock, MODE 2 packing
	lda #ULA_Mode5
	sta &248			; OS copy - OS interupt might be running..
	sta &fe20

	ldx #0
	.crtc_loop
	stx &fe00
	lda mode5_crtc_regs, X
	sta &fe01
	inx
	cpx #14
	bcc crtc_loop

	\\ The BBC's eight colours, logical = physical
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

	\\ Clear all screens
	jsr screen_clear_full

	\\ Set interrupts and handler
	SEI										; disable interupts

	\\ Wait for vsync
	jsr wait_for_vsync

	\\ Not stable but close enough for our purposes
    LDA #LO(TimerValue):STA &FE44
    LDX #HI(TimerValue):STX &FE45            ; start T1 counting

  	; Latch T1 to interupt exactly every 50Hz frame
	LDA #LO(FramePeriod):STA &FE46
	LDA #HI(FramePeriod):STA &FE47

	LDA #&7F								; A=01111111
	STA &FE4E								; R14=Interrupt Enable (disable all interrupts)
	sta &fe6e								; User via
	STA &FE43								; R3=Data Direction Register "A" (set keyboard data direction)
	LDA #&C2								; A=11000010
	STA &FE4E								; R14=Interrupt Enable (enable main_vsync and timer interrupt)

    LDA IRQ1V:STA old_irqv
    LDA IRQ1V+1:STA old_irqv+1

    LDA #LO(irq_handler):STA IRQ1V
    LDA #HI(irq_handler):STA IRQ1V+1		; set interrupt handler
	CLI										; enable interupts

	\\ GO!
	{
		lda MUSIC_SLOT_ZP
		bmi no_music
		inc music_enabled
		.no_music
	}

	jsr show_screen

    .loop
	lda eof_flag
	cmp #POLY_DESC_END_OF_STREAM
    beq track_load_error

	\\ The track at load_to can be refilled once the frame being decoded starts a
	\\ track or more past it round the ring (as in stnicc-beeb.asm)
	lda stream_frame_HI
	sec
	sbc load_to_HI
	bcs sectors_to_load_1
	adc #HI(STREAM_buffer_size)
	.sectors_to_load_1
	cmp #STREAMING_sectors_to_load
	bcs ready_to_load
	lda buffer_lock
	beq not_ready_to_load
	.ready_to_load

	JSR load_next_track
	lda #0:sta buffer_lock

	.not_ready_to_load
	LDA error_flag
	BEQ loop

    .track_load_error
	lda vsync_counter:sta vsync_final
	lda vsync_counter+1:sta vsync_final+1

	ldx #10:jsr wait_X_frames
	jsr hide_screen

	inc decode_lock				; stop decoder from running
	jsr show_final_screen

	\\ Turn off all interrupts
	SEI
	LDA #&7F
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

    \\ Load next part
    ldx #LO(next_part_cmd)
    ldy #HI(next_part_cmd)
    jmp oscli
}

.set_palette
	\\ Set ULA palete (the clock patches the address for MODE 4's)
	ldx #15
	.pal_loop
	lda mode5_palette, X
	sta &fe21
	dex
	bpl pal_loop
	rts

.show_screen
	lda #&c0
	equb &2c		; BIT abs
.hide_screen
	lda #&f0
{
	ldx #8:stx &fe00:sta &fe01		; show the screen!
	rts
}

INCLUDE "src/load_next_track.asm"

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

	\\ Vsync
    INC vsync_counter
    BNE no_carry
    INC vsync_counter+1
    .no_carry

    lda music_enabled
    beq vsync_done

	lda music_lock
	bne vsync_done

	inc music_lock
	lda &f4:pha
    lda MUSIC_SLOT_ZP
	sta &f4:sta &fe30
    txa:pha:tya:pha
    jsr MUSIC_JUMP_VGM_UPDATE
    pla:tay:pla:tax
	pla:sta &f4:sta &fe30
	dec music_lock

	.vsync_done
	JMP return_to_os

	.not_vsync
	LDA &FE4D
	AND #&40			; timer 1
	BEQ return_to_os

	\\ Acknowledge timer 1 interrupt
	STA &FE4D

	\\ If we're already busy just exit function
	LDA decode_lock
	ora screen_lock
	ora buffer_lock
	BNE return_to_os

	\\ Set a lock on our decode function
	INC decode_lock

	\\ Store registers in case of interupts
	TXA:PHA:TYA:PHA

	{
		CLI

		\\ Decode and draw the next frame
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
		\\ Lock the decoder unless enough of the stream is loaded
		jsr stream_check_ahead

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

	\\ Don't pass on to OS IRQ handler :)
	.return_to_os
	PLA
	STA &FC
	RTI
}
.old_irqv
	EQUW &FFFF

\\ (Straight after irq_handler: the harness leaves out the cycles between the two)
.swap_frame_buffers
{
    \\ Toggle draw buffer
    lda draw_buffer_HI
    eor #HI(screen1_addr EOR screen2_addr)
    sta draw_buffer_HI

	\\ Set screen buffer address in CRTC - not read until vsync
	cmp #HI(screen1_addr)
	beq show_screen2

	\\ Show screen 1, draw screen 2
    lda #12:sta &fe00
    lda #HI(screen1_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen1_addr/8):sta &fe01
	lda #HI(column_code2 - column_code1)
	sta code_or
	bne continue

	.show_screen2
    lda #12:sta &fe00
    lda #HI(screen2_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen2_addr/8):sta &fe01
	lda #0
	sta code_or

	.continue
	jmp return_here_from_swap_frame_buffers
}

.main_end

\ ******************************************************************
\ *	THE FRAME DECODER
\ ******************************************************************

.fx_start

\\ cpc_palette_update reads the palette change from the stream
MACRO GET_PAL_BYTE
	jsr get_byte
ENDMACRO

MACRO PALETTE_CHANGE
	jsr cpc_palette_update
	jsr update_pair_tables
ENDMACRO

MACRO COLUMN_START
ENDMACRO

COLS_KEEP = TRUE

INCLUDE "src/cols2_decode.asm"

\\ A pass through one of the column code's runs of stores: from row run_s's
\\ store to the one before row run_e's, with A = the pass's byte
MACRO FILL_PASS lo, hi, bytes
	ldy run_e
	lda lo, Y
	sta vB
	lda hi, Y
	sta vB+1
	ldy run_s
	lda lo, Y
	sta pass+1
	lda hi, Y
	sta pass+2
	ldy #0
	lda #OPCODE_RTS
	sta (vB), Y
	ldy run_p
	lda bytes, Y
	.pass
	jsr &ffff
	ldy #0
	lda #OPCODE_STA_ABSX
	sta (vB), Y
ENDMACRO

\\ A long run's passes, Y = its value: two for a chequer, three when its bottom line
\\ alternates (the even rows, then the odd)
MACRO LONG_PASSES top_hi, bottom_hi, even_hi, odd_hi
	lda pair_bottom_even, Y
	cmp pair_bottom_odd, Y
	beq chequer
	jmp dithered
	.chequer
	FILL_PASS seq_top_lo, top_hi, pair_top
	FILL_PASS seq_bottom_lo, bottom_hi, pair_bottom_even
	jmp done
	.dithered
	FILL_PASS seq_top_lo, top_hi, pair_top
	FILL_PASS seq_even_lo, even_hi, pair_bottom_even
	FILL_PASS seq_odd_lo, odd_hi, pair_bottom_odd
	jmp done
ENDMACRO

\\ The run sink (src/cols2_decode.asm): store the run just kept, from row last
\\ to run_e - 1, value run_p, in the draw buffer, and move last on to run_e;
\\ then the next op, or the next column once a run reaches the last row. X and
\\ Y kept.
.run_sink
{
	stx save_x
	sty save_y
	lda run_e
	sec
	sbc run_s                   ; run_s is last
	ldx col_x
	ldy run_p
	cmp #SHORT_RUN+1
	bcs long

	\\ A short run (C = 0): through dptr at its first row's character cell, by
	\\ a routine for that row's place in the cell and the run's length
	ldy run_s
	adc short_index, Y           ; (row AND 3) * 4 + length
	tax
	lda short_lo - 1, X
	sta short_jump+1
	lda short_hi - 1, X
	sta short_jump+2
	lda row_page, Y             ; row DIV 4
	adc draw_buffer_HI          ; (C = 0)
	sta dptr+1
	lda col_x
	sta dptr
	ldx run_p
	.short_jump
	jmp &FFFF

	.long
	lda code_or
	beq long1
	jmp long2
	.long1
	LONG_PASSES seq_top_hi1, seq_bottom_hi1, seq_even_hi1, seq_odd_hi1
	.long2
	LONG_PASSES seq_top_hi2, seq_bottom_hi2, seq_even_hi2, seq_odd_hi2

	.done
	.^fill_done
	ldx save_x
	ldy save_y
	lda run_e
	sta last
	cmp #ROWS
	bcs column_done
}
INCLUDE "src/cols2_decode_ops.asm"

\\ Each pixel pair's three bytes from the colours' (colour 0 is the background):
\\ rebuild the pairs with a colour that changed since the last time
.update_pair_tables
{
	lda #0
	sta cpc_colour_top
	sta cpc_colour_bottom
	sta cpc_colour_delta
	ldx #15
	.check
	lda cpc_colour_top, X
	cmp shadow_top, X
	bne changed
	lda cpc_colour_bottom, X
	cmp shadow_bottom, X
	bne changed
	lda cpc_colour_delta, X
	cmp shadow_delta, X
	beq same
	.changed
	jsr rebuild_colour
	.same
	dex
	bpl check
	rts
}

\\ The 32 pairs with colour X on the left or the right; X kept
.rebuild_colour
{
	stx colour
	lda cpc_colour_top, X
	sta shadow_top, X
	and #&AA
	sta row_top+1
	lda cpc_colour_top, X
	and #&55
	sta col_top+1
	lda cpc_colour_bottom, X
	sta shadow_bottom, X
	and #&AA
	sta row_even+1
	lda cpc_colour_bottom, X
	and #&55
	sta col_even+1
	lda cpc_colour_delta, X
	sta shadow_delta, X
	eor cpc_colour_bottom, X
	pha
	and #&AA
	sta row_odd+1
	pla
	and #&55
	sta col_odd+1

	txa                         ; colour X on the left: pairs X*16 to X*16+15
	asl a:asl a:asl a:asl a
	tax
	ldy #0
	.row
	lda cpc_colour_top, Y
	and #&55
	.row_top
	ora #0
	sta pair_top, X
	lda cpc_colour_bottom, Y
	and #&55
	.row_even
	ora #0
	sta pair_bottom_even, X
	lda cpc_colour_bottom, Y
	eor cpc_colour_delta, Y
	and #&55
	.row_odd
	ora #0
	sta pair_bottom_odd, X
	inx
	iny
	cpy #16
	bne row

	ldx colour                  ; on the right: pairs X, X+16, ...
	ldy #0
	.col
	lda cpc_colour_top, Y
	and #&AA
	.col_top
	ora #0
	sta pair_top, X
	lda cpc_colour_bottom, Y
	and #&AA
	.col_even
	ora #0
	sta pair_bottom_even, X
	lda cpc_colour_bottom, Y
	eor cpc_colour_delta, Y
	and #&AA
	.col_odd
	ora #0
	sta pair_bottom_odd, X
	txa
	clc
	adc #16
	tax
	iny
	cpy #16
	bne col
	ldx colour
	rts
}

INCLUDE "src/cpc_palette_update.asm"
INCLUDE "src/stream_helpers.asm"

.fx_end

\ ******************************************************************
\ *	DATA
\ ******************************************************************

.data_start

\\ Where row y starts within its character row, and which character row it is in
.row_offset
FOR y,0,ROWS-1
EQUB (y MOD 4) * 2
NEXT
.row_page
FOR y,0,ROWS-1
EQUB y DIV 4
NEXT

\\ Resolution 256x200 => 128x200
.mode5_crtc_regs
{
	EQUB 63					; R0  horizontal total
	EQUB 32					; R1  horizontal displayed
	EQUB 45					; R2  horizontal position
	EQUB &24				; R3  sync width
	EQUB 38					; R4  vertical total
	EQUB 0					; R5  vertical total adjust
	EQUB 25					; R6  vertical displayed
	EQUB 31					; R7  vertical position
	EQUB &F0				; R8  no interlace; cursor off; display off
	EQUB 7					; R9  scanlines per row
	EQUB 32					; R10 cursor start
	EQUB 8					; R11 cursor end
	EQUB HI(screen2_addr/8)	; R12 screen start address, high
	EQUB LO(screen2_addr/8)	; R13 screen start address, low
}

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
EQUB 2,&FF,0

.next_part_cmd
EQUS "/OUTRO", 13

.four_bits_to_four_pixels
FOR b, 0, 15, 1
b0 = b AND 1
b1 = (b AND 2) >> 1
b2 = (b AND 4) >> 2
b3 = (b AND 8) >> 3
EQUB b3 * &C0 + b2 * &30 + b1 * &0C + b0 * &03
NEXT

.data_end

\ ******************************************************************
\ *	ADDITIONAL CODE MODULES
\ ******************************************************************

.additional_start

INCLUDE "src/clock.asm"

.screen1_cls
{
	lda #HI(screen1_addr)
	sta loop+2
	lda #0
	tax
	.loop
	sta screen1_addr, X
	inx
	bne loop
	inc loop+2
	ldy loop+2
	bpl loop                    ; up to &8000
	rts
}

.screen_clear_full
{
	lda #0
	tax
	.loop
	sta screen2_addr, X
	inx
	bne loop
	ldy loop+2
	iny
	sty loop+2
	bpl loop
	rts
}

.additional_end

\ ******************************************************************
\ *	THE COLUMN CODE
\ ******************************************************************

\\ For each screen: a store per row, byte column in X, in four runs that each
\\ end with an RTS: every row's top line, every row's bottom line, the even
\\ rows' bottom lines and the odd rows'. The two screens' copies are
\\ COLUMN_CODE_PAGES pages apart, so the entry tables' low bytes serve both.

SEQ_TOP = 0
SEQ_BOTTOM = SEQ_TOP + ROWS * 3 + 1
SEQ_EVEN = SEQ_BOTTOM + ROWS * 3 + 1
SEQ_ODD = SEQ_EVEN + ROWS / 2 * 3 + 1
SEQ_DUMMY = SEQ_ODD + ROWS / 2 * 3 + 1   ; the stop for a pass that runs to the end
COLUMN_CODE_PAGES = 4

MACRO COLUMN_CODE scr
	FOR y,0,ROWS-1
	sta scr + (y DIV 4) * SCREEN_ROW_BYTES + (y MOD 4) * 2, X
	NEXT
	rts
	FOR y,0,ROWS-1
	sta scr + (y DIV 4) * SCREEN_ROW_BYTES + (y MOD 4) * 2 + 1, X
	NEXT
	rts
	FOR y,0,ROWS-1,2
	sta scr + (y DIV 4) * SCREEN_ROW_BYTES + (y MOD 4) * 2 + 1, X
	NEXT
	rts
	FOR y,1,ROWS-1,2
	sta scr + (y DIV 4) * SCREEN_ROW_BYTES + (y MOD 4) * 2 + 1, X
	NEXT
	rts
	EQUB 0                      ; SEQ_DUMMY
ENDMACRO

\\ Each run of stores' entry by row: the store for the first row at or after it
\\ that the run has, or SEQ_DUMMY past the last. The low bytes serve both
\\ screens' copies; each has its high bytes.
MACRO SEQ_TABLE code, seq, first, step, count, part
FOR r,0,ROWS
s = (r + first) DIV step
IF s < count
addr = code + seq + s * 3
ELSE
addr = code + SEQ_DUMMY
ENDIF
IF part = 0
EQUB LO(addr)
ELSE
EQUB HI(addr)
ENDIF
NEXT
ENDMACRO

ALIGN &100
.column_code1                   ; screen 1
COLUMN_CODE screen1_addr
\\ in the rest of the slot: some of the entry tables
.seq_top_lo
SEQ_TABLE column_code1, SEQ_TOP, 0, 1, ROWS, 0
ASSERT P% <= column_code1 + COLUMN_CODE_PAGES * &100
ORG column_code1 + COLUMN_CODE_PAGES * &100
.column_code2                   ; screen 2
COLUMN_CODE screen2_addr
.seq_bottom_lo
SEQ_TABLE column_code1, SEQ_BOTTOM, 0, 1, ROWS, 0
ASSERT P% <= column_code2 + COLUMN_CODE_PAGES * &100
ORG column_code2 + COLUMN_CODE_PAGES * &100

.seq_top_hi1
SEQ_TABLE column_code1, SEQ_TOP, 0, 1, ROWS, 1
.seq_top_hi2
SEQ_TABLE column_code2, SEQ_TOP, 0, 1, ROWS, 1
.seq_bottom_hi1
SEQ_TABLE column_code1, SEQ_BOTTOM, 0, 1, ROWS, 1
.seq_bottom_hi2
SEQ_TABLE column_code2, SEQ_BOTTOM, 0, 1, ROWS, 1
.seq_even_lo
SEQ_TABLE column_code1, SEQ_EVEN, 1, 2, ROWS/2, 0
.seq_even_hi1
SEQ_TABLE column_code1, SEQ_EVEN, 1, 2, ROWS/2, 1
.seq_even_hi2
SEQ_TABLE column_code2, SEQ_EVEN, 1, 2, ROWS/2, 1
.seq_odd_lo
SEQ_TABLE column_code1, SEQ_ODD, 0, 2, ROWS/2, 0
.seq_odd_hi1
SEQ_TABLE column_code1, SEQ_ODD, 0, 2, ROWS/2, 1
.seq_odd_hi2
SEQ_TABLE column_code2, SEQ_ODD, 0, 2, ROWS/2, 1

\\ The short runs' routines, by first row's place in its character cell (p)
\\ and length (n): a row's two bytes at 2 * its place, X = the value
MACRO SHORT_RUN_CODE p, n
FOR k, 0, n - 1
q = p + k
IF q = 4
	inc dptr+1                  ; on to the next character row
ENDIF
	ldy #(q AND 3) * 2
	lda pair_top, X
	sta (dptr), Y
	iny
IF (q AND 1) = 0
	lda pair_bottom_even, X
ELSE
	lda pair_bottom_odd, X
ENDIF
	sta (dptr), Y
NEXT
	jmp fill_done
ENDMACRO

ASSERT SHORT_RUN = 4
.short_code
FOR p, 0, 3
FOR n, 1, SHORT_RUN
	SHORT_RUN_CODE p, n
NEXT
NEXT
ASSERT P% - short_code = 4 * 142 + 12

\\ Each routine's address: 13 bytes a row, 3 for the JMP and 2 more for a run
\\ into the next character row (so 142 + 2p for each place p's four)
.short_index
FOR y, 0, ROWS - 1
EQUB (y AND 3) * 4
NEXT
.short_lo
FOR p, 0, 3
FOR n, 1, 4
EQUB LO(short_code + 142 * p + p * (p - 1) + 13 * (n - 1) * n / 2 + 3 * (n - 1) + (n - 5 + p + ABS(n - 5 + p)))
NEXT
NEXT
.short_hi
FOR p, 0, 3
FOR n, 1, 4
EQUB HI(short_code + 142 * p + p * (p - 1) + 13 * (n - 1) * n / 2 + 3 * (n - 1) + (n - 5 + p + ABS(n - 5 + p)))
NEXT
NEXT


.end

\\ Bottom line, odd rows (the other two pair tables are in low RAM), past the
\\ end of the executable: the first frame's palette builds every pair
.pair_bottom_odd                ; (not page aligned: a few reads a frame cross a page)
SKIP 256
ASSERT P% <= STREAM_buffer_start

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

exe_sectors_size = (end - start + &FF) AND &FF00
PRINT "Executable size =", ~end - start, "(", ~exe_sectors_size, "in sectors )"
ASSERT exe_sectors_size = EXE_SIZE_COLS2
SAVE "build/CPCCOL2", start, end, main

PRINT "------"
PRINT "ZP size =", ~zp_end-zp_start, "(",~zp_top-zp_end,"free)"
PRINT "CODE end =", ~additional_end, " column code at", ~column_code1, "and", ~column_code2
PRINT "STREAM buffer =", ~STREAM_buffer_start, "-", ~STREAM_buffer_end
PRINT "------"
