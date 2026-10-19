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

\\ The run ring (bin/colstream2.py's RING and COLUMN_MAX)
RING_SIZE = 640
COLUMN_MAX = 36
RING_VAL = &400                 ; value (pixel pair)
RING_END = &700                 ; the row after its last
RING_SLOPE = &A00               ; its end less the end of the run it came from
BG_POS = RING_SIZE              ; a column of background, past the ring's end

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

\\ the column decoder
.ctl                skip 1      ; control byte bits not yet taken, above a sentinel bit
.frame_end          skip 1      ; parse_frame's return
.cur_off            skip 1      ; this frame's half of the column tables (0 or 32)
.prev_off           skip 1      ; and the frame before's
.col_no             skip 1
.next_lo            skip 1      ; ring position of this column's runs, then of the next column's
.next_hi            skip 1
.tmp                skip 1
.cnt                skip 1
.tmp_e              skip 1      ; a run's end row
.sl                 skip 1      ; and its slope
.last               skip 1      ; end row of the run before (the next run's first row)
run_s = last                    ; the run being stored's first row
.col_x              skip 1      ; byte column * 8
.walkers_set        skip 1      ; set_walkers has run for this column
.colour             skip 1      ; rebuild_colour's

\\ a new run's neighbours (read_value): a walker each through the column to the
\\ left (this frame), and the same column and the one to its right (the frame before)
.lc_val             skip 2
.lc_end             skip 2
.lc_w               skip 1
.pc_val             skip 2
.pc_end             skip 2
.pc_w               skip 1
.pr_val             skip 2
.pr_end             skip 2
.pr_w               skip 1
.n_lc               skip 1      ; the pixel pairs at the run's first row
.n_pc               skip 1
.n_pr               skip 1
.cand               skip 7      ; a pixel's candidates, in order
.d1                 skip 1
.new_l              skip 1
.vsave_x            skip 1
.vsave_y            skip 1

\\ the fill
.run_e              skip 1      ; the run's row after its last
.run_p              skip 1      ; its value (pixel pair); the value above, for a new one
.run_n              skip 1      ; rows left to store (short runs)
.save_x             skip 1      ; the decoder's X and Y, while a run is stored
.save_y             skip 1
.vB                 skip 2      ; and the store it is stopped at
.dptr               skip 2      ; screen pointer (short runs)
.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

\\ In the ring arrays' spare tails (positions RING_SIZE onwards): the background
\\ column, zero slopes, each column's ring position and kind (two frames' worth,
\\ by cur_off and prev_off) and the colours
ZERO_SLOPES = RING_SLOPE + BG_POS + 1   ; COLUMN_MAX zeros: a reference without slopes
col_kind = ZERO_SLOPES + COLUMN_MAX     ; 64: 0 = coded in time or tslope mode, 1 = left
col_start_lo = RING_VAL + BG_POS + 1    ; 64
col_start_hi = RING_END + BG_POS + 1    ; 64

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

    \\ The frame before the first: a run of background in each column, at ring
    \\ positions 0-31 (cur_off = 0, so the first frame takes it as the frame
    \\ before); the background column; zero slopes
    ldx #31
    .ring_loop
    lda #0
    sta RING_VAL, X
    sta RING_SLOPE, X
    sta col_start_hi, X
    sta col_kind, X
    lda #ROWS
    sta RING_END, X
    txa
    sta col_start_lo, X
    dex
    bpl ring_loop
    lda #32
    sta next_lo
    lda #0
    sta RING_VAL + BG_POS
    sta RING_SLOPE + BG_POS
    ldx #COLUMN_MAX - 1
    .zero_loop
    sta ZERO_SLOPES, X
    dex
    bpl zero_loop
    lda #ROWS
    sta RING_END + BG_POS

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

\\ C = the next bit of the stream. ctl holds the control byte's bits not yet
\\ taken, high first, above a sentinel bit: when shifting out a bit leaves it
\\ empty, that bit was the sentinel and the next control byte is due. A is
\\ lost when a control byte is read.
MACRO GET_BIT
	asl ctl
	bne have
	jsr next_control
	.have
ENDMACRO

\\ cpc_palette_update reads the palette change from the stream
MACRO GET_PAL_BYTE
	jsr get_byte
ENDMACRO

\\ Decode and draw a frame. Returns &FF, &FE (the next frame starts the next
\\ block) or &FD (the last frame).
.parse_frame
{
    inc frame_no
    bne no_carry
    inc frame_no+1
    .no_carry

	\\ The stream is read through get_byte's own address
	lda STREAM_ptr_LO
	sta get_byte_address
	lda STREAM_ptr_HI
	sta get_byte_address+1

	lda #0
	sta ctl                     ; empty: the first bit reads a control byte
	sta tmp
	GET_BIT                     ; P: a palette change
	php
	lda get_byte_address+1      ; the loader leaves this frame's pages alone
	sta stream_frame_HI
	GET_BIT                     ; E: how the frame ends
	rol tmp
	GET_BIT
	rol tmp
	lda #&FF
	sec
	sbc tmp
	sta frame_end
	plp
	bcc no_palette

	jsr cpc_palette_update
	jsr update_pair_tables
	.no_palette

	\\ This frame's column tables are last frame's other half
	lda cur_off
	sta prev_off
	eor #32
	sta cur_off

	lda #0
	sta col_x
	sta col_no
	.column_loop
	jsr decode_column
	inc col_no
	lda col_x
	clc
	adc #8
	sta col_x
	bcc column_loop

	lda get_byte_address
	sta STREAM_ptr_LO
	lda get_byte_address+1
	sta STREAM_ptr_HI
	lda frame_end
	rts
}

\\ The next stream byte in A
.get_byte
	inc get_byte_address
	bne get_byte_read
	inc get_byte_address+1
.get_byte_read
	lda &ffff
	rts
get_byte_address = get_byte_read + 1

\\ GET_BIT's next control byte: C = its first bit
.next_control
	jsr get_byte
	sec
	rol a
	sta ctl
	rts

\\ A bits (1-4) of the stream in A, high first
.read_bits
{
	sta cnt
	lda #0
	sta tmp
	.loop
	GET_BIT
	rol tmp
	dec cnt
	bne loop
	lda tmp
	rts
}

\\ A walker's pointers to the runs at ring position A (low), Y (high)
MACRO SET_WALKER val, end, w
	sta val
	sta end
	tya
	clc
	adc #HI(RING_VAL)
	sta val+1
	adc #HI(RING_END - RING_VAL)
	sta end+1
	lda #0
	sta w
ENDMACRO

\\ The reference column's runs at ring position A (low), Y (high): its value and
\\ end sites, and the high byte of its slopes' (in ref_slope_hi, low byte in A)
.set_reference
{
	sta site_rval+1
	sta site_rend1+1
	sta site_rend2+1
	sta site_rend3+1
	sta site_rend4+1
	sta site_rend5+1
	sta ref_slope_lo
	tya
	clc
	adc #HI(RING_VAL)
	sta site_rval+2
	adc #HI(RING_END - RING_VAL)
	sta site_rend1+2
	sta site_rend2+2
	sta site_rend3+2
	sta site_rend4+2
	sta site_rend5+2
	adc #HI(RING_SLOPE - RING_END)
	sta ref_slope_hi
	rts
}

\\ Predict ends with the reference's slopes, or without (zeros)
.slopes_on
{
	lda ref_slope_lo
	sta site_rslope1+1
	sta site_rslope2+1
	sta site_rslope3+1
	lda ref_slope_hi
	sta site_rslope1+2
	sta site_rslope2+2
	sta site_rslope3+2
	rts
}
.slopes_off
{
	lda #LO(ZERO_SLOPES)
	sta site_rslope1+1
	sta site_rslope2+1
	sta site_rslope3+1
	lda #HI(ZERO_SLOPES)
	sta site_rslope1+2
	sta site_rslope2+2
	sta site_rslope3+2
	rts
}
.ref_slope_lo EQUB 0
.ref_slope_hi EQUB 0

\\ Decode column col_no into the ring, storing each run as it is decoded
\\ (bin/colstream2.py's Decoder). X = the reference column's next run, Y =
\\ this column's.
.decode_column
{
	\\ This column's runs go where the last one's ended, or at the start of the
	\\ ring if fewer than COLUMN_MAX places are left
	lda next_lo
	cmp #LO(RING_SIZE - COLUMN_MAX + 1)
	lda next_hi
	sbc #HI(RING_SIZE - COLUMN_MAX + 1)
	bcc fits
	lda #0
	sta next_lo
	sta next_hi
	.fits
	lda col_no
	clc
	adc cur_off
	tax
	lda next_lo
	sta col_start_lo, X
	sta site_cval+1
	sta site_cend+1
	sta site_cslope+1
	sta site_kcval+1
	sta site_kcend+1
	sta site_kcslope+1
	lda next_hi
	sta col_start_hi, X
	clc
	adc #HI(RING_VAL)
	sta site_cval+2
	sta site_kcval+2
	adc #HI(RING_END - RING_VAL)
	sta site_cend+2
	sta site_kcend+2
	adc #HI(RING_SLOPE - RING_END)
	sta site_cslope+2
	sta site_kcslope+2

	\\ The walkers are set up by a new run's value (set_walkers)
	lda #0
	sta walkers_set

	\\ The mode: 0 time, 10 left, 11 tslope
	GET_BIT
	bcs mode_1x

	\\ time: the same column in the frame before, ends unmoved
	lda #LO(time_ops)
	ldy #HI(time_ops)
	jsr reference_before
	jsr slopes_off
	jmp start

	.mode_1x
	GET_BIT
	bcs mode_tslope

	\\ left: the column to the left (or background), with its slopes if it was
	\\ coded in left mode too
	lda #LO(left_ops)
	sta next_op+1
	lda #HI(left_ops)
	sta next_op+2
	lda col_no
	clc
	adc cur_off
	tax
	lda #1
	sta col_kind, X
	lda col_no
	beq left_background
	lda col_kind - 1, X
	pha
	lda col_start_lo - 1, X
	ldy col_start_hi - 1, X
	jsr set_reference
	pla
	beq left_no_slopes
	jsr slopes_on
	jmp start
	.left_background
	lda #LO(BG_POS)
	ldy #HI(BG_POS)
	jsr set_reference
	.left_no_slopes
	jsr slopes_off
	jmp start

	\\ tslope: the same column in the frame before, ends moved on by its slopes
	\\ if it was coded in a time mode
	.mode_tslope
	lda #LO(tslope_ops)
	ldy #HI(tslope_ops)
	jsr reference_before
	lda col_kind, X
	bne tslope_no_slopes
	jsr slopes_on
	jmp start
	.tslope_no_slopes
	jsr slopes_off

	.start
	ldx #0
	ldy #0
	stx last
	stx run_p                   ; nothing above the first run
	jmp next_op
}

\\ The walkers, at the start of the column to the left (or background), and of
\\ this column and the one to its right in the frame before (or background)
.set_walkers
{
	inc walkers_set
	ldx col_no
	beq lc_background
	txa
	clc
	adc cur_off
	tax
	lda col_start_lo - 1, X
	ldy col_start_hi - 1, X
	jmp lc_set
	.lc_background
	lda #LO(BG_POS)
	ldy #HI(BG_POS)
	.lc_set
	SET_WALKER lc_val, lc_end, lc_w

	lda col_no
	clc
	adc prev_off
	tax
	lda col_start_lo, X
	ldy col_start_hi, X
	SET_WALKER pc_val, pc_end, pc_w

	lda col_no
	cmp #31
	beq pr_background
	clc
	adc prev_off
	tax
	lda col_start_lo + 1, X
	ldy col_start_hi + 1, X
	jmp pr_set
	.pr_background
	lda #LO(BG_POS)
	ldy #HI(BG_POS)
	.pr_set
	SET_WALKER pr_val, pr_end, pr_w
	rts
}

\\ time and tslope: ops from A (low), Y (high); this column's kind is time and
\\ its reference the same column in the frame before. Returns X = that column's
\\ index in the column tables.
.reference_before
{
	sta next_op+1
	sty next_op+2
	lda col_no
	clc
	adc cur_off
	tax
	lda #0
	sta col_kind, X
	lda col_no
	clc
	adc prev_off
	tax
	lda col_start_lo, X
	ldy col_start_hi, X
	jsr set_reference
	ldx col_no                  ; set_reference leaves X alone, but be plain
	txa
	clc
	adc prev_off
	tax
	rts
}

\\ Each mode's op codes (bin/colstream2.py's OP_CODES)
.left_ops                       ; K0 00, K+ 010, K- 011, KJ 100, D 101, I 110, KL 1110, R 1111
{
	GET_BIT
	bcs l1
	GET_BIT
	bcs l01
	jmp op_k0
	.l01
	GET_BIT
	bcs l011
	jmp op_kplus
	.l011
	jmp op_kminus
	.l1
	GET_BIT
	bcs l11
	GET_BIT
	bcs l101
	jmp op_kj
	.l101
	jmp op_d
	.l11
	GET_BIT
	bcs l111
	jmp op_i
	.l111
	GET_BIT
	bcs l1111
	jmp op_kl
	.l1111
	jmp op_r
}

.tslope_ops                     ; K0 00, KJ 01, K+ 100, K- 101, I 110, D 1110, KL 11110, R 11111
{
	GET_BIT
	bcs t1
	GET_BIT
	bcs t01
	jmp op_k0
	.t01
	jmp op_kj
	.t1
	GET_BIT
	bcs t11
	GET_BIT
	bcs t101
	jmp op_kplus
	.t101
	jmp op_kminus
	.t11
	GET_BIT
	bcs t111
	jmp op_i
	.t111
	GET_BIT
	bcs t1111
	jmp op_d
	.t1111
	GET_BIT
	bcs t11111
	jmp op_kl
	.t11111
	jmp op_r
}

.time_ops                       ; K0 0, K+ 100, K- 101, KJ 1100, D 1101, I 1110, KL 11110, R 11111
{
	GET_BIT
	bcc op_k0
	GET_BIT
	bcs t11
	GET_BIT
	bcs t101
	jmp op_kplus
	.t101
	jmp op_kminus
	.t11
	GET_BIT
	bcs t111
	GET_BIT
	bcs t1101
	jmp op_kj
	.t1101
	jmp op_d
	.t111
	GET_BIT
	bcs t1111
	jmp op_i
	.t1111
	GET_BIT
	bcs t11111
	jmp op_kl
	.t11111
	jmp op_r
}

\\ Keep a new or recoloured run (A = end row, sl = slope, run_p = value), and
\\ store it
.store_common
.site_cend
	sta &FFFF, Y
	sta run_e
	lda sl
.site_cslope
	sta &FFFF, Y
	lda run_p
.site_cval
	sta &FFFF, Y
	iny
	jmp fill

\\ K: the reference's next run carries on, its end the predicted end plus a
\\ delta, and is stored
.op_kl
	jsr get_byte
	jmp k_end
.op_k0
.site_rend1
	lda &FFFF, X
	clc
.site_rslope1
	adc &FFFF, X
	cmp #ROWS
	bcc k_end
	lda #ROWS
	bcs k_end                   ; always
.op_kplus
	lda #1
	bne k_delta
.op_kminus
	lda #&FF
	bne k_delta
.op_kj
	jsr read_jump
.k_delta
	clc
.site_rend2
	adc &FFFF, X
	clc
.site_rslope2
	adc &FFFF, X
	cmp #ROWS
	bcc k_end
	lda #ROWS
.k_end
.site_kcend
	sta &FFFF, Y
	sta run_e
	sec
.site_rend3
	sbc &FFFF, X
.site_kcslope
	sta &FFFF, Y
.site_rval
	lda &FFFF, X
.site_kcval
	sta &FFFF, Y
	sta run_p
	inx
	iny

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

\\ Store the run just kept, from row last to run_e - 1, value run_p, in the
\\ draw buffer, and move last on to run_e; then the next op, or the next column
\\ once a run reaches the last row. X and Y kept.
.fill
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

	\\ A short run: store it row by row through dptr, X = its value
	sta run_n
	ldy run_s
	txa                         ; byte column * 8
	ora row_offset, Y           ; 2 * (row AND 3)
	sta dptr
	lda row_page, Y             ; row DIV 4
	clc
	adc draw_buffer_HI
	sta dptr+1
	ldx run_p
	tya
	lsr a
	bcs odd_row
	.even_row
	ldy #0
	lda pair_top, X
	sta (dptr), Y
	iny
	lda pair_bottom_even, X
	sta (dptr), Y
	dec run_n
	beq short_done
	lda dptr                    ; the odd row below: the same character row
	clc
	adc #2
	sta dptr
	.odd_row
	ldy #0
	lda pair_top, X
	sta (dptr), Y
	iny
	lda pair_bottom_odd, X
	sta (dptr), Y
	dec run_n
	beq short_done
	lda dptr
	clc
	adc #2
	sta dptr
	and #7
	bne even_row
	lda dptr                    ; on to the next character row
	sec
	sbc #8
	sta dptr
	inc dptr+1
	bne even_row                ; always
	.short_done
	jmp done

	.long
	lda code_or
	beq long1
	jmp long2
	.long1
	LONG_PASSES seq_top_hi1, seq_bottom_hi1, seq_even_hi1, seq_odd_hi1
	.long2
	LONG_PASSES seq_top_hi2, seq_bottom_hi2, seq_even_hi2, seq_odd_hi2

	.done
	ldx save_x
	ldy save_y
	lda run_e
	sta last
	cmp #ROWS
	bcs column_done
}
.next_op
	jmp &FFFF                   ; the mode's op codes
.column_done
	tya                         ; the next column goes after this one's runs
	clc
	adc next_lo
	sta next_lo
	bcc no_carry
	inc next_hi
	.no_carry
	rts


\\ D: drop the reference's next run
.op_d
	inx
	jmp next_op

\\ I: a new run, a value and a length (1 0; 2-3 10x; 4-7 110xx; 8-15 1110xxx; else 1111 and a byte)
.op_i
{
	jsr read_value
	sta run_p
	lda #0
	sta sl
	GET_BIT
	bcs i1
	lda #1
	bne i_length
	.i1
	GET_BIT
	bcs i11
	lda #1
	jsr read_bits
	clc
	adc #2
	bne i_length
	.i11
	GET_BIT
	bcs i111
	lda #2
	jsr read_bits
	clc
	adc #4
	bne i_length
	.i111
	GET_BIT
	bcs i_byte
	lda #3
	jsr read_bits
	clc
	adc #8
	bne i_length
	.i_byte
	jsr get_byte
	.i_length
	clc
	adc last
	jmp store_common
}

\\ R: the reference's next run carries on in a new value: the value, then a
\\ delta (K0 0, K+ 10, K- 110, KJ 1110, KL 1111)
.op_r
{
	jsr read_value
	sta run_p
	GET_BIT
	bcs r1
	lda #0
	beq r_delta
	.r1
	GET_BIT
	bcs r11
	lda #1
	bne r_delta
	.r11
	GET_BIT
	bcs r111
	lda #&FF
	bne r_delta
	.r111
	GET_BIT
	bcs r_kl
	jsr read_jump
	jmp r_delta
	.r_kl
	jsr get_byte
	jmp r_end
	.r_delta
	clc
	.^site_rend4
	adc &FFFF, X
	clc
	.^site_rslope3
	adc &FFFF, X
	cmp #ROWS
	bcc r_end
	lda #ROWS
	.r_end
	sta tmp_e
	sec
	.^site_rend5
	sbc &FFFF, X
	sta sl
	inx
	lda tmp_e
	jmp store_common
}

\\ A jump's delta in A: a sign bit (1 = minus), then 2 + 0x, 4 + 10xx or 8 + 110xxx
.read_jump
{
	GET_BIT
	php
	GET_BIT
	bcs j1
	lda #1
	jsr read_bits
	clc
	adc #2
	jmp sign
	.j1
	GET_BIT
	bcs j11
	lda #2
	jsr read_bits
	clc
	adc #4
	jmp sign
	.j11
	GET_BIT                     ; 0
	lda #3
	jsr read_bits
	clc
	adc #8
	.sign
	plp
	bcc plus
	eor #&FF
	adc #0                      ; C = 1: minus
	.plus
	rts
}

\\ Walk a walker on to the run of its column that covers row last: its value in
\\ A and out (walking again to the same row costs one look)
MACRO WALK val, end, w, out
	ldy w
	lda last
	.loop
	cmp (end), Y
	bcc found                   ; last < its end
	iny
	bne loop
	.found
	sty w
	lda (val), Y
	sta out
ENDMACRO

.walk_lc
	WALK lc_val, lc_end, lc_w, n_lc
	rts
.walk_pc
	WALK pc_val, pc_end, pc_w, n_pc
	rts
.walk_pr
	WALK pr_val, pr_end, pr_w, n_pr
	rts

\\ A new run's value: its left pixel, then its right, each ranked among its
\\ neighbours' pixels (bin/colstream2.py's left_list and right_list). Each first
\\ candidate (LC.r for the left pixel, PR.l for the right) needs one neighbour;
\\ the rest of a list is built only when it is wanted. X and Y kept.
.read_value
{
	stx vsave_x
	sty vsave_y
	lda walkers_set
	bne walkers_ready
	jsr set_walkers
	.walkers_ready
	jsr walk_lc
	GET_BIT
	bcs left_list
	lda n_lc
	and #15
	sta new_l
	jmp right

	\\ left: LC.r PC.l A.l A.r PR.l PC.r LC.l (A = run_p, the run above)
	.left_list
	jsr walk_pc
	jsr walk_pr
	lda n_lc
	and #15
	sta cand
	lda n_pc
	lsr a:lsr a:lsr a:lsr a
	sta cand+1
	lda run_p
	lsr a:lsr a:lsr a:lsr a
	sta cand+2
	lda run_p
	and #15
	sta cand+3
	lda n_pr
	lsr a:lsr a:lsr a:lsr a
	sta cand+4
	lda n_pc
	and #15
	sta cand+5
	lda n_lc
	lsr a:lsr a:lsr a:lsr a
	sta cand+6
	jsr pixel_ranked
	sta new_l

	\\ right: PR.l A.r PC.r L A.l LC.r PC.l
	.right
	jsr walk_pr
	lsr a:lsr a:lsr a:lsr a
	sta cand
	GET_BIT
	bcs right_list
	lda cand
	jmp combine
	.right_list
	jsr walk_pc
	lda run_p
	and #15
	sta cand+1
	lda n_pc
	and #15
	sta cand+2
	lda new_l
	sta cand+3
	lda run_p
	lsr a:lsr a:lsr a:lsr a
	sta cand+4
	lda n_lc
	and #15
	sta cand+5
	lda n_pc
	lsr a:lsr a:lsr a:lsr a
	sta cand+6
	jsr pixel_ranked
	.combine
	sta tmp
	lda new_l
	asl a:asl a:asl a:asl a
	ora tmp
	ldx vsave_x
	ldy vsave_y
	rts
}

\\ A pixel whose first bit was 1: the second different candidate (0), the
\\ third (10), or 11 and the pixel in 4 bits
.pixel_ranked
{
	GET_BIT
	bcs not1
	ldx #1
	lda cand
	.s1
	cmp cand, X
	bne f1
	inx
	bne s1
	.f1
	lda cand, X
	rts
	.not1
	GET_BIT
	bcs miss
	ldx #1
	lda cand
	.s2
	cmp cand, X
	bne f2
	inx
	bne s2
	.f2
	lda cand, X
	sta d1
	.s3
	inx
	lda cand, X
	cmp cand
	beq s3
	cmp d1
	beq s3
	rts
	.miss
	lda #4
	jmp read_bits
}

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
	lda #0
	tax
	.loop
	FOR page,0,SCREEN_SIZE_BYTES-1,&100
	sta screen1_addr + page, X
	NEXT
	inx
	bne loop
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

\\ Bottom line, odd rows (the other two pair tables are in low RAM)
ALIGN &100
.pair_bottom_odd
SKIP 256

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

.end

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
