\\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	BEEB-NICCC: THE COLUMN PLAYER (CPC PALETTE)
\ ******************************************************************

\\ Plays the column stream (bin/colstream.py): each frame's final picture as 32
\\ byte columns of runs, each coded against the column to its left, in place of
\\ polygons. The picture is the CPC player's (stnicc-beeb.asm with _CPC): MODE 2
\\ packing at 64 x 100, each row on both its scanlines, each colour a 2x2 block of
\\ two of the BBC's eight pens (tools/cpc_colour_map.py), its bottom line's pixels
\\ swapped on odd rows when it is three of one pen and one of the other.
\\
\\ Each frame is decoded a column at a time into a list of runs (value, end row,
\\ slope), and every byte of the screen is written once, so nothing is cleared:
\\ - a run of up to SHORT_RUN rows is stored directly, through a pointer;
\\ - a longer one goes through unrolled code, a store per row, entered at the
\\   run's first row and left by an RTS put over the store after its last row:
\\   once for the top lines and once for the bottom lines (once each for the even
\\   and the odd rows if its bottom line alternates).
\\
\\ The rest is the polygon player's: the stream streams into a ring buffer while
\\ frames are drawn in the Timer 1 interrupt, the music plays from SWRAM, and the
\\ clock and the outro follow. Built by make.bat onto its own disc
\\ (stnicc-cols-build.asm); tools/harness/run.mjs --variant cols plays it.

_DEBUG = FALSE
_NULA = TRUE                    ; for the shared modules: as the CPC build
_CPC = TRUE

include "src/zp.h.asm"
include "src/music.h.asm"
include "src/cols_disc.h.asm"

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
DISK1_first_track = COLS_FIRST_TRACK
DISK1_last_track = 80
DISK2_first_track = 79          ; side 2 runs backwards (bin/split.py)
DISK2_last_track = 0

STREAM_buffer_size = 3 * DFS_track_size
STREAM_buffer_start = screen2_addr - STREAM_buffer_size
STREAM_buffer_end = screen2_addr
\\ Pages that must be loaded before a frame starts (stream_check_ahead): the
\\ largest frame (bin/colstream.py prints it) must fit in (N-1)*256+1 bytes
STREAM_pages_ahead = 8

FLAG_CONTAINS_PALETTE = 2
POLY_DESC_END_OF_STREAM = &FD   ; a frame's end byte (bin/colstream.py)
POLY_DESC_SKIP_TO_64K = &FE     ; the next frame starts the next block

FramePeriod = 312*64-2
TimerValue = (12)*64 - 2*64

ROWS = 100
SHORT_RUN = 4                   ; runs of up to this many rows are stored directly

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
.frame_flags        skip 1
.eof_flag			skip 1
.pal_ptr_LO			skip 1
.pal_ptr_HI			skip 1
.cpc_count          skip 1
.cpc_colour_delta   skip 16     ; each colour's EOR for odd rows' bottom lines (cpc_palette_update)

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
.last               skip 1      ; end row of the run before (the next run's first row)
run_s = last                    ; the run being stored's first row
.prev_base          skip 1      ; where the column to the left's runs start in the lists (0 or 128)
.cur_base           skip 1      ; and this column's
.col_x              skip 1      ; byte column * 8
.tmp                skip 1
.colour             skip 1      ; rebuild_colour's

\\ the fill
.run_e              skip 1      ; and the row after its last
.run_p              skip 1      ; its value (pixel pair)
.run_n              skip 1      ; rows left to store (short runs)
.val_t              skip 1      ; top line byte
.val_b              skip 1      ; this row's bottom line byte
.val_bd             skip 1      ; what turns it into the next row's
.save_x             skip 1      ; the decoder's X and Y, while a run is stored
.save_y             skip 1
.vB                 skip 2      ; and the store it is stopped at
.dptr               skip 2      ; screen pointer (short runs)
.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

\\ Run lists: the column to the left's runs and this column's, at 0-127 and
\\ 128-255 by turns
LVAL = &400                     ; value
LEND = &500                     ; the row after its last
LSLOPE = &600                   ; its end less the end of the run it came from

\\ Each pixel pair's bytes, from the palette (build_pair_tables)
pair_top = &700                 ; top line
pair_bottom_even = &900         ; bottom line, even rows
pair_bottom_odd = &A00          ; bottom line, odd rows

cpc_colour_top = &B00           ; each colour's top line byte (cpc_palette_update)
cpc_colour_bottom = &B10        ; and its bottom line byte on even rows
shadow_top = &B20               ; the colours the pair tables were built from
shadow_bottom = &B30
shadow_delta = &B40

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

	lda #LO(palette_stream_start-1)
	sta pal_ptr_LO
	lda #HI(palette_stream_start-1)
	sta pal_ptr_HI

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

MACRO GET_PAL_BYTE
{
    inc pal_ptr_LO
    bne no_carry
    inc pal_ptr_HI
    .no_carry
    lda (pal_ptr_LO), y
}
ENDMACRO

.fx_start

\\\ Decode and draw a frame. Returns its end byte (&FF, &FE or &FD).
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

	jsr get_byte
	sta frame_flags
	lda get_byte_address+1      ; the loader leaves this frame's pages alone
	sta stream_frame_HI

	lda frame_flags
	and #FLAG_CONTAINS_PALETTE
	beq no_palette

	\\ Step over the STe palette (the NuLA's), then take the CPC palette's change
	jsr get_byte                ; dummy
	jsr get_byte
	tax
	.skip_palette
	jsr get_byte
	jsr get_byte
	dex
	bne skip_palette

	ldy #0
	jsr cpc_palette_update
	jsr update_pair_tables
	.no_palette

	lda #0
	sta ctl                     ; empty: the first code reads a control byte
	sta col_x
	sta prev_base
	lda #128
	sta cur_base

	.column_loop
	jsr decode_column           ; stores each run as it goes
	lda cur_base
	sta prev_base
	eor #128
	sta cur_base
	lda col_x
	clc
	adc #8
	sta col_x
	bcc column_loop

	jsr get_byte                ; the frame's end
	pha
	lda get_byte_address
	sta STREAM_ptr_LO
	lda get_byte_address+1
	sta STREAM_ptr_HI
	pla
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

\\ C = the next code's first bit. ctl holds the control byte's bits not yet
\\ taken, high first, above a sentinel bit: when shifting out a code's first bit
\\ leaves it empty, that bit was the sentinel and the next control byte is due.
MACRO CODE_FIRST_BIT
	asl ctl
	bne have
	jsr get_byte
	sec
	rol a
	sta ctl
	.have
ENDMACRO


\\ Decode a column into the lists at cur_base, against the column to the left at
\\ prev_base (bin/colstream.py's decode_column), storing each run as it is
\\ decoded. X = the left column's next run, Y = this column's.
.decode_column
{
	lda #0
	sta last
	ldx prev_base
	ldy cur_base

	.^next_op
	CODE_FIRST_BIT
	bcs code_1x
	asl ctl
	bcs k_plus
	lda LEND, X                 ; 00: K, end = left end + slope
	clc
	adc LSLOPE, X
	jmp k_store
	.k_plus                     ; 01: K, end + 1
	lda LEND, X
	sec
	adc LSLOPE, X
	jmp k_store
	.code_1x
	asl ctl
	bcs escape
	lda LEND, X                 ; 10: K, end - 1
	clc
	adc LSLOPE, X
	sec
	sbc #1

	\\ A = the run's end, 100 at most: carry the left column's run on to it
	.^k_store
	cmp #ROWS
	bcc k_in
	lda #ROWS
	.k_in
	sta LEND, Y
	sta run_e
	sec
	sbc LEND, X
	sta LSLOPE, Y
	lda LVAL, X
	sta LVAL, Y
	sta run_p
	inx
	iny
	jsr store_run
	bcs column_done
	jmp next_op
	.column_done
	rts

	.escape
	CODE_FIRST_BIT
	bcs escape_1x
	asl ctl
	bcs op_d
	jmp op_kx
	.escape_1x
	asl ctl
	bcs op_r
	jmp op_i

	\\ 11 11: R, the left column's run carries on in a new value
	.op_r
	jsr get_byte
	sta run_p
	CODE_FIRST_BIT
	bcs r_1x
	asl ctl
	bcs r_plus
	lda LEND, X                 ; +0
	clc
	adc LSLOPE, X
	jmp r_store
	.r_plus
	lda LEND, X
	sec
	adc LSLOPE, X
	jmp r_store
	.r_1x
	asl ctl
	bcs r_end
	lda LEND, X                 ; -1
	clc
	adc LSLOPE, X
	sec
	sbc #1
	jmp r_store
	.r_end
	jsr get_byte
	.r_store
	cmp #ROWS
	bcc r_in
	lda #ROWS
	.r_in
	sta LEND, Y
	sta run_e
	sec
	sbc LEND, X
	sta LSLOPE, Y
	lda run_p
	sta LVAL, Y
	inx
	iny
	jmp store_and_next

	.op_kx                      ; 11 00: K to an end row byte
	jsr get_byte
	jmp k_store

	.op_d                       ; 11 01: drop the left column's run
	inx
	jmp next_op

	.op_i                       ; 11 10: a new run
	jsr get_byte
	sta LVAL, Y
	sta run_p
	CODE_FIRST_BIT              ; the length: 00, 01, 10 for 1-3 rows, 11 for a byte
	bcs i_1x
	asl ctl
	lda #1
	adc #0                      ; 1 or 2
	bne i_length                ; always
	.i_1x
	asl ctl
	lda #3
	bcc i_length
	jsr get_byte
	.i_length
	clc
	adc last
	sta LEND, Y
	sta run_e
	lda #0
	sta LSLOPE, Y
	iny
	.store_and_next
	jsr store_run
	bcs done
	jmp next_op
	.done
	rts
}

\\ A pass through one of the column code's runs of stores: from row run_s's
\\ store to the one before row run_e's, with A = the pass's byte
MACRO FILL_PASS lo, hi, bytes
	ldy run_e
	lda lo, Y
	sta vB
	lda hi, Y
	ora code_or
	sta vB+1
	ldy run_s
	lda lo, Y
	sta pass+1
	lda hi, Y
	ora code_or
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

\\ Store the run just decoded, from row last to run_e - 1, value run_p, in the
\\ draw buffer, and move last on to run_e. Returns C = 1 once the column is
\\ done; X and Y kept.
.store_run
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

	\\ A short run: store it row by row through dptr
	sta run_n
	lda pair_top, Y
	sta val_t
	lda pair_bottom_even, Y
	eor pair_bottom_odd, Y
	sta val_bd
	lda run_s
	lsr a
	lda pair_bottom_even, Y
	bcc even_start
	lda pair_bottom_odd, Y
	.even_start
	sta val_b
	ldy run_s
	txa
	ora row_offset, Y           ; 2 * (row AND 3)
	sta dptr
	lda row_page, Y             ; row DIV 4
	clc
	adc draw_buffer_HI
	sta dptr+1
	.row
	ldy #0
	lda val_t
	sta (dptr), Y
	iny
	lda val_b
	sta (dptr), Y
	dec run_n
	bne next_row
	jmp done
	.next_row
	lda val_b
	eor val_bd
	sta val_b
	lda dptr
	clc
	adc #2
	sta dptr
	and #7
	bne row
	lda dptr                    ; on to the next character row
	sec
	sbc #8
	sta dptr
	inc dptr+1
	bne row                     ; always

	.long
	lda pair_bottom_even, Y
	cmp pair_bottom_odd, Y
	bne dithered
	FILL_PASS seq_top_lo, seq_top_hi, pair_top
	FILL_PASS seq_bottom_lo, seq_bottom_hi, pair_bottom_even
	jmp done
	.dithered                   ; its bottom line alternates: the even rows, then the odd
	FILL_PASS seq_top_lo, seq_top_hi, pair_top
	FILL_PASS seq_even_lo, seq_even_hi, pair_bottom_even
	FILL_PASS seq_odd_lo, seq_odd_hi, pair_bottom_odd

	.done
	ldx save_x
	ldy save_y
	lda run_e
	sta last
	cmp #ROWS
	rts
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

\\ The CPC palette's changes (tools/cpc_palette.py), read in place
.palette_stream_start
INCBIN "data/cpc_palette.bin"
.palette_stream_end

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

\ ******************************************************************
\ *	THE COLUMN CODE
\ ******************************************************************

\\ For each screen: a store per row, byte column in X, in four runs that each
\\ end with an RTS: every row's top line, every row's bottom line, the even
\\ rows' bottom lines and the odd rows'. The two screens' copies are
\\ COLUMN_CODE_PAGES pages apart, at a multiple of twice that, so screen 2's
\\ addresses are screen 1's with code_or ORed into the high byte.

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
\\ that the run has, or SEQ_DUMMY past the last
.seq_top_lo
FOR r,0,ROWS
IF r < ROWS:EQUB LO(column_code1 + SEQ_TOP + r * 3):ELSE:EQUB LO(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_top_hi
FOR r,0,ROWS
IF r < ROWS:EQUB HI(column_code1 + SEQ_TOP + r * 3):ELSE:EQUB HI(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_bottom_lo
FOR r,0,ROWS
IF r < ROWS:EQUB LO(column_code1 + SEQ_BOTTOM + r * 3):ELSE:EQUB LO(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_bottom_hi
FOR r,0,ROWS
IF r < ROWS:EQUB HI(column_code1 + SEQ_BOTTOM + r * 3):ELSE:EQUB HI(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_even_lo
FOR r,0,ROWS
s = (r + 1) DIV 2
IF s < ROWS/2:EQUB LO(column_code1 + SEQ_EVEN + s * 3):ELSE:EQUB LO(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_even_hi
FOR r,0,ROWS
s = (r + 1) DIV 2
IF s < ROWS/2:EQUB HI(column_code1 + SEQ_EVEN + s * 3):ELSE:EQUB HI(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_odd_lo
FOR r,0,ROWS
s = r DIV 2
IF s < ROWS/2:EQUB LO(column_code1 + SEQ_ODD + s * 3):ELSE:EQUB LO(column_code1 + SEQ_DUMMY):ENDIF
NEXT
.seq_odd_hi
FOR r,0,ROWS
s = r DIV 2
IF s < ROWS/2:EQUB HI(column_code1 + SEQ_ODD + s * 3):ELSE:EQUB HI(column_code1 + SEQ_DUMMY):ENDIF
NEXT

ALIGN COLUMN_CODE_PAGES * 2 * &100
.column_code1                   ; screen 1
COLUMN_CODE screen1_addr
ASSERT P% <= column_code1 + COLUMN_CODE_PAGES * &100
ORG column_code1 + COLUMN_CODE_PAGES * &100
.column_code2                   ; screen 2
COLUMN_CODE screen2_addr
ASSERT P% <= column_code2 + COLUMN_CODE_PAGES * &100
ORG column_code2 + COLUMN_CODE_PAGES * &100

.end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

exe_sectors_size = (end - start + &FF) AND &FF00
PRINT "Executable size =", ~end - start, "(", ~exe_sectors_size, "in sectors )"
ASSERT exe_sectors_size = EXE_SIZE_COLS
SAVE "build/CPCCOL", start, end, main

PRINT "------"
PRINT "ZP size =", ~zp_end-zp_start, "(",~&80-zp_end,"free)"
PRINT "CODE end =", ~additional_end, " column code at", ~column_code1, "and", ~column_code2
PRINT "STREAM buffer =", ~STREAM_buffer_start, "-", ~STREAM_buffer_end
PRINT "------"
