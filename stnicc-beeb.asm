\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB
\ ******************************************************************

_DEBUG = FALSE	; if you change me check the same in stnicc-build.asm
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
; Stream test: parse frames but draw nothing, so decoding outruns the disc
; (tools/harness/run.mjs --parse-only)
_TEST_PARSE_ONLY =? FALSE

; Rendering defines
_HALF_VERTICAL_RES = (_QUALITY < 2)
_DOUBLE_PLOT_Y = (_QUALITY = 1)
_WIDESCREEN = (_QUALITY = 2) AND FALSE
_SKIP_ODD_FRAMES = FALSE

; Optimised renderer, LOW only for now (see src/plot_poly_fast.asm)
_FAST_POLY = (_QUALITY = 0) AND NOT(_NULA) AND NOT(_PLOT_WIREFRAME)

; CPC: the NULA build's MODE 2 (64 x 100, each row on both its scanlines) with
; the BBC's standard eight colours, no NuLA: each polygon colour is one of the
; Amstrad CPC's 27, a chequer of two pens, pixels A,B on the top line and B,A on
; the bottom (data/cpc_palette.bin, from tools/cpc_palette.py). Built with
; -D _QUALITY=1 -D _NULA=-1 -D _CPC=-1.
_CPC =? FALSE

PRINT "------"
PRINT "STNICC-BEEB"
PRINT "------"
PRINT "_QUALITY =",_QUALITY
PRINT "_NULA =",_NULA
PRINT "_CPC =",_CPC
PRINT "_DEBUG =",_DEBUG
PRINT "_TESTS =",_TESTS
PRINT "_SHOW_STREAMING_INFO =",_SHOW_STREAMING_INFO
PRINT "_SHOW_TOTAL_VSYNC_COUNTER =",_SHOW_TOTAL_VSYNC_COUNTER
PRINT "_STOP_AT_FRAME =",_STOP_AT_FRAME
PRINT "_DOUBLE_BUFFER =",_DOUBLE_BUFFER
PRINT "_PLOT_WIREFRAME =",_PLOT_WIREFRAME
PRINT "_HALF_VERTICAL_RES =",_HALF_VERTICAL_RES
PRINT "_DOUBLE_PLOT_Y =",_DOUBLE_PLOT_Y
PRINT "_WIDESCREEN =",_WIDESCREEN
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

ULA_Mode5 	= &C4
ULA_Mode4 	= &88

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
PRINT "Lost ", P%-H%, "bytes to PAGE alignment"
ENDMACRO

MACRO BOUNDARY_ALIGN b
skip b
PRINT "Lost ",b , "bytes to avoid crossing PAGE boundary"
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
STREAMING_tracks_per_disk = 79
STREAMING_sectors_to_load = 10

DISK1_drive_no = 0
DISK1_first_track = 24      ; the stream runs to the end of the disc
DISK1_last_track = 80

\\ Side 2 runs backwards, from track 79 down to 1, so the head need not seek
\\ back across the disc when the stream moves on to it (bin/split.py)
DISK2_first_track = 79
DISK2_last_track = DISK2_first_track - STREAMING_tracks_per_disk
OPCODE_DEC_zp = &C6             ; load_next_track's track step on side 2

STREAM_buffer_size = 3 * DFS_track_size

; Pages of stream that must be loaded before a frame may start (see irq_handler).
; The largest frame of the re-encoded stream is 1548 bytes (frame 5; 1495 past the
; initial fill), and N pages from the frame's start page guarantee (N-1)*256+1
; bytes, so 8 (1793 bytes). Every player: the re-encoded stream is denser, and
; NULA without the lock outran a slower disc than jsbeeb's early on.
STREAM_pages_ahead = 8

; STNICCC scene1.bin constants
MAX_VERTS_PER_POLY = 7

FLAG_CLEAR_SCREEN = 1
FLAG_CONTAINS_PALETTE = 2
FLAG_INDEXED_DATA = 4
FLAG_CLEAR_MASK = 8         ; a clearing frame: 13 bytes follow the flags, a bit per row to clear
CLEAR_MASK_BYTES = 13

POLY_TRIM = 8               ; descriptor: a trim byte follows (rows LOW may skip: top << 4 | bottom)

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
if _CPC
.span_colour2       skip 1      ; the bottom line's byte
.cpc_count          skip 1
endif
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
; .frame_bitmask      skip 2
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

.vsync_final		skip 2
.clock_minutes		skip 1
.clock_seconds		skip 1
.clock_flashes		skip 1

.char_def			skip 9
.glyphptr			skip 2
.glyphptr_copy		skip 2

.music_enabled		skip 1
.music_lock			skip 1

.poly_trim			skip 1		; this polygon's trim byte, or 0 (_FAST_POLY)
.stream_frame_HI	skip 1		; page of the frame being decoded: its vertices are read from there

.zp_end

\ ******************************************************************
\ *	BSS DATA IN LOWER RAM
\ ******************************************************************

ORG &300
GUARD &800
.reloc_to_start
if _FAST_POLY
.lmask                  ; byte mask of pixels (x AND 3) to 3, by pixel x
skip &80
.rmask                  ; byte mask of pixels 0 to (x AND 3), by pixel x
skip &80
.fast_code_area         ; &400-&6FF: code relocated from reloc_fast_code
skip &300
else
.screen_row_LO
skip &100
.screen1_row_HI
skip &100
.screen2_row_HI
skip &100
.y_to_row
skip &100
endif
PAGE_ALIGN  ; lazy
if not(_NULA)
.poly_palette
skip &40
.dither_table
skip &40
endif
.screen_col_LO
skip &80
if _CPC
.cpc_colour_top         ; each polygon colour's top-line byte
skip 16
.cpc_colour_bottom      ; and its bottom-line byte
skip 16
endif
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
    \\ Init ZP
    lda #0
    tax
    .zp_loop
    sta &00, x
    inx
    cpx #zp_top
    bne zp_loop

	\\ Relocate data to lower RAM
	lda #HI(reloc_from_start)
	ldy #HI(reloc_to_start)
	ldx #HI(reloc_to_end - reloc_to_start + &ff)
	jsr copy_pages

if not(_NULA) OR _CPC
	lda #HI(palette_stream_start)
	ldy #HI(palette_stream_buffer)
	ldx #HI(palette_stream_end - palette_stream_start + &ff)
	jsr copy_pages
endif

if _FAST_POLY
	\\ Code that runs in spare pages; after the palette stream, which copies
	\\ whole pages up to &CFF
	lda #HI(reloc_page9_from)
	ldy #&09
	ldx #1
	jsr copy_pages

	ldx #reloc_pageC_end - reloc_pageC_start - 1
	.copy_pageC
	lda reloc_pageC_from, X
	sta reloc_pageC_start, X
	dex
	bpl copy_pageC
endif

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

	lda #LO(palette_stream_buffer-1)
	sta pal_ptr_LO
	lda #HI(palette_stream_buffer-1)
	sta pal_ptr_HI

    jsr init_span_buffer

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

	\\ Set ULA to MODE 5
	lda #ULA_Mode5
	sta &248			; OS copy - OS interupt might be running..
	sta &fe20

	\\ Set CRTC to MODE 5 => 128x200
	ldx #0
	.crtc_loop
	stx &fe00
	lda mode5_crtc_regs, X
	sta &fe01
	inx
	cpx #14
	bcc crtc_loop

if _NULA
	\\ Set NULA palette
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

if NOT(_CPC)  ; without a NuLA, &FE22 and &FE23 are the ULA's &FE20 and &FE21
; disable NuLA auxiliary palette
	lda #$10
	sta $fe22

; set colour 0 to black
    lda #$00
	sta $fe23
	sta $fe23
endif
else
	jsr set_palette
endif

	\\ Clear all screens
	jsr screen_clear_full

	\\ Set interrupts and handler
	SEI										; disable interupts

	\\ Wait for vsync
	jsr wait_for_vsync

	\\ Not stable but close enough for our purposes
	; Write T1 low now (the timer will not be written until you write the high byte)
    LDA #LO(TimerValue):STA &FE44
    ; Get high byte ready so we can write it as quickly as possible at the right moment
    LDX #HI(TimerValue):STX &FE45            ; start T1 counting		; 4c +1/2c 

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

	\\ The track at load_to can be refilled once the reader has left it: when
	\\ the page the frame being decoded starts on is a track or more past
	\\ load_to round the ring. (The frame start, not the stream pointer: the
	\\ frame's vertices are read from there while the pointer runs on through
	\\ its polygons.) Inside that track the ring is full, unless the decoder is
	\\ locked waiting for data (a block skip caught the loader up), when the
	\\ track is stale and must be loaded.
	\\ (The original measures the distance in plain address order, which stops
	\\ loading when the reader is just below load_to: with the decoder's
	\\ look-ahead lock that would wait for ever.)
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

	\\ If so, load a track's worth of data into our buffer
	JSR load_next_track

	\\ Unlock the buffer - there is some data!
	lda #0:sta buffer_lock

	.not_ready_to_load

	\\ Check for errors
	LDA error_flag
	BEQ loop

    .track_load_error
	lda vsync_counter:sta vsync_final
	lda vsync_counter+1:sta vsync_final+1

	ldx #10:jsr wait_X_frames
	jsr hide_screen

	inc decode_lock				; stop decoder from running
IF _FAST_POLY
	\\ Leave page 3 as the original LOW did: its screen_row_LO table, n AND 7.
	\\ The span masks there instead upset the MOS font pointers (&367-&36E),
	\\ which the final clock and the outro read characters through (OSWORD 10).
	ldx #0
	.restore_page3
	txa
	and #7
	sta &300, X
	inx
	bne restore_page3
ENDIF
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

	\\ Exit gracefully (in theory)
    \\ Load next part
    ldx #LO(next_part_cmd)
    ldy #HI(next_part_cmd)
    jmp oscli
}

.set_palette
	\\ Set ULA palete for MODE 5
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
	.track_step
	INC track_no                ; DEC on side 2
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

	\\ Reset track to start of disk 2, which is read backwards
	LDA #DISK2_first_track
	STA track_no
	LDA #OPCODE_DEC_zp
	STA track_step

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

	\\ Vsync
    INC vsync_counter
    BNE no_carry
    INC vsync_counter+1
    .no_carry

	\\ (vsync_done is a JMP within reach: the stream check pushes
	\\ return_to_os out of branch range)
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

if _NULA AND NOT(_DOUBLE_PLOT_Y)

	    ldy #219
		ldx #0
.delay
		inx
		bne delay
		iny
		bne delay

endif

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

	IF _SKIP_ODD_FRAMES
	lda frame_no
    lsr a
    bcs return_here_from_swap_frame_buffers
	ENDIF

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
if _FAST_POLY
		lda #LO(fps_unrolled_screen2_LO)
		sta fps_row_table_LO+1
		lda #HI(fps_unrolled_screen2_LO)
		sta fps_row_table_LO+2
		lda #LO(fps_unrolled_screen2_HI)
		sta fps_row_table_HI+1
		lda #HI(fps_unrolled_screen2_HI)
		sta fps_row_table_HI+2
else
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
endif

	\\ Continue
	bne continue

	.show_screen2
	\\ Show screen 2
    lda #12:sta &fe00
    lda #HI(screen2_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen2_addr/8):sta &fe01

	\\ Draw screen 1
if _FAST_POLY
		lda #LO(fps_unrolled_screen1_LO)
		sta fps_row_table_LO+1
		lda #HI(fps_unrolled_screen1_LO)
		sta fps_row_table_LO+2
		lda #LO(fps_unrolled_screen1_HI)
		sta fps_row_table_HI+1
		lda #HI(fps_unrolled_screen1_HI)
		sta fps_row_table_HI+2
else
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
endif

	\\ Continue
	.continue
	jmp return_here_from_swap_frame_buffers
}

.main_end

\ ******************************************************************
\ *	FX CODE
\ ******************************************************************

.fx_start

if not(_NULA) OR _CPC
MACRO GET_PAL_BYTE
{
    inc pal_ptr_LO
    bne no_carry
    inc pal_ptr_HI
    .no_carry
    lda (pal_ptr_LO), y
}
ENDMACRO
endif

;INCLUDE "lib/disksys.asm"
INCLUDE "src/parse_frame.asm"
INCLUDE "src/plot_poly.asm"
INCLUDE "src/screen.asm"

.fx_end

\ ******************************************************************
\ *	DATA
\ ******************************************************************

.data_start

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
EQUB 2,&FF,0		; only two discs now!

.next_part_cmd
EQUS "/OUTRO", 13

if not(_FAST_POLY)
include "src/plot_data.asm"
endif

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

INCLUDE "src/plot_line.asm"
INCLUDE "src/debug.asm"
INCLUDE "src/tests.asm"
INCLUDE "src/clock.asm"

\\ A renderer faster than the disc must not start a frame on data not yet
\\ loaded: lock the decoder unless STREAM_pages_ahead pages are loaded.
\\ (A subroutine here, after the plotters, not inline in irq_handler: NULA
\\ has only a few bytes to spare before plot_poly.asm's page-aligned code
\\ and below its stream buffer.)
if _CPC
\\ A palette change: the colours whose CPC colour changed, from the palette
\\ stream (data/cpc_palette.bin): count, then (colour, top byte, bottom byte).
\\ Y = 0 for GET_PAL_BYTE.
.cpc_palette_update
{
	GET_PAL_BYTE
	beq done
	sta cpc_count
	.loop
	GET_PAL_BYTE
	tax
	GET_PAL_BYTE
	sta cpc_colour_top, X
	GET_PAL_BYTE
	sta cpc_colour_bottom, X
	dec cpc_count
	bne loop
	.done
	rts
}
endif

.stream_check_ahead
{
	lda track_no
	bmi enough                  ; nothing more to load

	\\ Pages loaded ahead of the reader, less one: load_to - ptr - 1 round the
	\\ ring (ptr = load_to is a full ring, 29 here)
	lda load_to_HI
	clc
	sbc STREAM_ptr_HI
	bcs no_wrap
	adc #HI(STREAM_buffer_size)
	.no_wrap
	cmp #STREAM_pages_ahead - 1
	bcs enough
	lda #&ff:sta buffer_lock
	.enough
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
	.return
	rts
}

.additional_end

\ ******************************************************************
\ *	RELOCATABLE DATA OVERLAYING BSS DATA
\ ******************************************************************

PAGE_ALIGN
.reloc_from_start
if _FAST_POLY
.reloc_lmask
FOR n,0,127,1
p=n AND 3
IF p=0:EQUB &FF:ELIF p=1:EQUB &77:ELIF p=2:EQUB &33:ELSE:EQUB &11:ENDIF  ; pixels p..3
NEXT
.reloc_rmask
FOR n,0,127,1
p=n AND 3
IF p=0:EQUB &88:ELIF p=1:EQUB &CC:ELIF p=2:EQUB &EE:ELSE:EQUB &FF:ENDIF  ; pixels 0..p
NEXT

\\ The span plotter, assembled to run at &400 and stored here
reloc_P4 = P%
CLEAR &400, &700
ORG &400
GUARD &700
.fast_code_start
include "src/plot_spans_fast.asm"
.fast_code_end
COPYBLOCK fast_code_start, fast_code_end, reloc_P4
CLEAR fast_code_start, fast_code_end
ORG reloc_P4 + &300
PRINT "Code at &400:", ~fast_code_end-fast_code_start, "bytes"
else
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
endif

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
if _CPC
FOR n,1,32:EQUB 0:NEXT  ; cpc_colour_top, cpc_colour_bottom: black until the first palette
endif
CHECK_SAME_PAGE_AS reloc_screen_col_LO

.reloc_from_end

if _FAST_POLY
\\ Code assembled to run at &900 and &C90 (spare at runtime: the MOS RS423
\\ output buffer, and the user-defined characters after palette_stream_buffer)
\\ but stored here, under the streaming buffer; main copies it into place.
PAGE_ALIGN
.reloc_page9_from
reloc_P = P%
ORG &900
GUARD &A00
.reloc_page9_start
include "src/plot_poly_fast_up.asm"
.reloc_page9_end
COPYBLOCK reloc_page9_start, reloc_page9_end, reloc_P
CLEAR reloc_page9_start, reloc_page9_end
ORG reloc_P + reloc_page9_end - reloc_page9_start

.reloc_pageC_from
reloc_PC = P%
CLEAR &C90, &D00
ORG &C90
GUARD &D00
.reloc_pageC_start
include "src/plot_poly_fast_up2.asm"
.reloc_pageC_end
COPYBLOCK reloc_pageC_start, reloc_pageC_end, reloc_PC
CLEAR reloc_pageC_start, reloc_pageC_end
ORG reloc_PC + reloc_pageC_end - reloc_pageC_start
PRINT "Relocated code: &900 block", ~reloc_page9_end-reloc_page9_start, "bytes, &C90 block", ~reloc_pageC_end-reloc_pageC_start, "bytes"
endif

if not(_NULA)
PAGE_ALIGN
GUARD P%+&300
include "src/palette_stream.asm"
elif _CPC
PAGE_ALIGN
GUARD P%+&300
.palette_stream_start
INCBIN "data/cpc_palette.bin"
.palette_stream_end
endif

\ ******************************************************************
\ *	End address to be saved
\ ******************************************************************

.end

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

\\ The disc layout (stnicc-build.asm) depends on each executable's size in
\\ sectors: see src/exe_sizes.h.asm
include "src/exe_sizes.h.asm"
exe_sectors_size = (end - start + &FF) AND &FF00
PRINT "Executable size =", ~end - start, "(", ~exe_sectors_size, "in sectors )"

IF _CPC
ASSERT exe_sectors_size = EXE_SIZE_CPC
SAVE "build/CPC", start, end, main
ELIF _NULA
ASSERT exe_sectors_size = EXE_SIZE_NULA
SAVE "build/NULA", start, end, main
ELIF _QUALITY == 2
ASSERT exe_sectors_size = EXE_SIZE_HIGH
SAVE "build/HIGH", start, end, main
ELIF _QUALITY == 1
ASSERT exe_sectors_size = EXE_SIZE_MEDIUM
SAVE "build/MEDIUM", start, end, main
ELIF _QUALITY == 0
ASSERT exe_sectors_size = EXE_SIZE_LOW
SAVE "build/LOW", start, end, main
ENDIF

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
PRINT "ZP size =", ~zp_end-zp_start, "(",~&80-zp_end,"free)"
PRINT "MAIN size =", ~main_end-main_start
PRINT "FX size = ", ~fx_end-fx_start
PRINT "DATA size =",~data_end-data_start
PRINT "ADDITIONAL size =",~additional_end-additional_start
PRINT "RELOC size =",~reloc_from_end-reloc_from_start
if not(_NULA) OR _CPC
PRINT "PALETTE STREAM size =",~palette_stream_end-palette_stream_start
endif
PRINT "BSS size =",~bss_end-bss_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~screen2_addr-P%
PRINT "------"
