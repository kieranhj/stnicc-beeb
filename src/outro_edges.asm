\\ -*- mode:beebasm -*-
\\ ******************************************************************
\\ *	THE OUTRO'S EDGE VIEW (src/outro.asm with _COLS)
\\ ******************************************************************

\\ On the 1800-frame column disc the outro plays the column stream, with the
\\ column player's decoder (src/cols2_decode.asm), in the window where the
\\ polygon wireframe was: each frame's 64 x 100 picture as its edges, each
\\ picture pixel two screen pixels wide. A pixel is set where the colour changes
\\ from the pixel above (both its screen pixels) or from the one to its left (its
\\ left screen pixel), and the window's outermost pixels make a box. The model is
\\ tools/outro_check.py.
\\
\\ It shows every other frame, so as to take about as long as the credits:
\\ each frame drawn follows one decoded but not drawn (decode_unseen; the
\\ stream keeps frames 2n and 2n + 1 in one block, so that one never ends a
\\ block or the stream). That one is decoded as soon as the frame before is
\\ drawn (decode_ahead), while its swap waits for vsync.
\\
\\ Each column's edges are drawn once it is decoded (edge_column), down its rows
\\ a segment at a time: a segment ends where its run or the column to the left's
\\ run does. In a segment a row's bits are the left pixel's first bit if the
\\ column to the left's right pixel differs from it, and the right pixel's first
\\ bit if the run's two pixels differ; a run's first row also has the bits of
\\ the pixels that differ from the run above. (In column 0 the column to the
\\ left is background, and its left pixel's first bit is the box's left edge,
\\ set already.)

\\ The palette changes are stepped over
MACRO PALETTE_CHANGE
	jsr skip_palette
ENDMACRO

MACRO COLUMN_START
	jsr edge_column_start
ENDMACRO

MACRO COLUMN_END
	jsr edge_column
ENDMACRO

COLS_KEEP = FALSE               ; the window is cleared each frame: draw every column
COLS_INLINE_BITS = TRUE

INCLUDE "src/cols2_decode.asm"

\\ The run sink (src/cols2_decode.asm): the edges wait for the whole column
.run_sink
	lda run_e
	sta last
	cmp #ROWS
	bcs column_done

INCLUDE "src/cols2_decode_ops.asm"

\\ A palette change: a count, then three bytes for each colour
.skip_palette
{
	jsr get_byte
	beq done
	tax
	.loop
	jsr get_byte
	jsr get_byte
	jsr get_byte
	dex
	bne loop
	.done
	rts
}

\\ Each column, if drawn: where its runs and the column to the left's are in the
\\ ring (edge_column's sites), its place in the window and its pixels' bits
.edge_column_start
{
	bit drawing
	bmi draw
	rts
	.draw
	lda next_lo                 ; this column's runs
	sta site_cv+1
	sta site_ce+1
	lda next_hi
	clc
	adc #HI(RING_VAL)
	sta site_cv+2
	adc #HI(RING_END - RING_VAL)
	sta site_ce+2
	ldx col_no                  ; the column to the left's (or background)
	beq left_background
	txa
	clc
	adc cur_off
	tax
	lda col_start_lo - 1, X
	ldy col_start_hi - 1, X
	jmp left_set
	.left_background
	lda #LO(BG_POS)
	ldy #HI(BG_POS)
	.left_set
	sta site_lv+1
	sta site_le+1
	tya
	clc
	adc #HI(RING_VAL)
	sta site_lv+2
	adc #HI(RING_END - RING_VAL)
	sta site_le+2

	lda col_x                   ; byte column * 8: its character cell is (column DIV 2) * 8
	lsr a
	and #&F8
	sta ecol
	lda col_x
	and #8
	bne odd
	lda #%11000000              ; an even column: the cell's left half
	sta hl
	lda #%00110000
	sta hr
	lda #%11110000
	sta hlr
	lda #%10000000
	sta vl
	lda #%00100000
	sta vr
	rts
	.odd
	lda #%00001100
	sta hl
	lda #%00000011
	sta hr
	lda #%00001111
	sta hlr
	lda #%00001000
	sta vl
	lda #%00000010
	sta vr
	rts
}

\\ A column's edges, once it is decoded, if drawn, down to row ROWS: X = the
\\ column to the left's run, cj = this column's next run. Y kept.
.edge_column
{
	bit drawing
	bmi draw
	rts
	.draw
	sty save_y
	ldx #0
	stx cj
	stx y0

	\\ This column's run cj, from row y0: its end and pixels, and across its
	\\ first row the pixels that differ from the run above
	.run
	ldy cj
	inc cj
	.^site_ce
	lda &FFFF, Y
	sta cend
	.^site_cv
	lda &FFFF, Y
	sta tmpv
	lsr a:lsr a:lsr a:lsr a
	sta cl                      ; its left pixel
	eor tmpv
	and #&0F
	beq no_inner
	lda vr                      ; its two pixels differ
	.no_inner
	sta inner
	lda y0
	beq across_done             ; (the top run)
	lda tmpv
	eor prevv
	cmp #&10
	bcc right_only
	and #&0F
	beq left_only
	lda hlr
	bne across
	.left_only
	lda hl
	bne across
	.right_only
	lda hr
	.across
	sta vm
	ldy y0
	lda cellrow_lo, Y
	clc
	adc ecol
	sta ep
	lda cellrow_hi, Y
	adc draw_buffer_HI
	sta ep+1
	tya
	and #7
	tay
	lda (ep), Y
	ora vm
	sta (ep), Y
	.across_done
	lda tmpv
	sta prevv

	\\ A segment, from row y0 to where the left run or this run ends: its bits
	.segment
	.^site_lv
	lda &FFFF, X                ; the column to the left's right pixel against this left pixel
	and #&0F
	eor cl
	beq same
	lda vl
	.same
	ora inner
	sta vm
	.^site_le
	lda &FFFF, X
	cmp cend
	bcs with_run
	ldy vm                      ; the left run ends first, at A
	beq left_next
	jsr draw_rows
	.left_next
	sta y0
	inx
	bne segment                 ; always
	.with_run                   ; it ends with this run (and the left run too, if level)
	bne left_goes_on
	inx
	.left_goes_on
	lda cend
	ldy vm
	beq run_next
	jsr draw_rows
	.run_next
	sta y0
	cmp #ROWS
	bcs column_end
	jmp run
	.column_end
	ldy save_y
	rts
}

\\ OR vm into the column's byte on rows y0 to A - 1. X kept; A = its end.
.draw_rows
{
	sta y1
	sec
	sbc y0
	sta n
	ldy y0
	lda cellrow_lo, Y
	clc
	adc ecol
	sta ep
	lda cellrow_hi, Y
	adc draw_buffer_HI
	sta ep+1
	tya
	and #7
	tay
	.row
	lda (ep), Y
	ora vm
	sta (ep), Y
	dec n
	beq done
	iny
	cpy #8
	bne row
	lda ep                      ; on to the next character row
	clc
	adc #LO(MODE4_ROW_BYTES)
	sta ep
	lda ep+1
	adc #HI(MODE4_ROW_BYTES)
	sta ep+1
	ldy #0
	beq row                     ; always
	.done
	lda y1
	rts
}

\\ After a frame is drawn, while its swap waits for vsync: decode the next pair's
\\ first frame, unless the stream has ended or is short of data (the check before
\\ sets buffer_lock unless STREAM_FRAME_PAGES are loaded: enough for a pair).
\\ Returns C = 1 to draw the next frame at once.
.decode_ahead
{
	lda #0
	sta unseen_done
	lda eof_flag
	cmp #POLY_DESC_END_OF_STREAM
	beq done
	lda buffer_lock
	bne done
	jsr decode_unseen
	dec unseen_done

	\\ C = 1: go straight on to the next pair, without waiting for the next
	\\ tick, as the swap has happened and the loader has no track to start (as
	\\ the main loop judges it)
	lda screen_lock
	ora buffer_lock
	bne done
	sec
	lda stream_frame_HI
	sbc load_to_HI
	bcs track_distance
	eor #255
	adc #1
	.track_distance
	cmp #STREAMING_sectors_to_load
	bcs done
	sec
	rts
	.done
	clc
	rts
}

\\ A frame decoded but not drawn (the first of a pair)
.decode_unseen
{
	lda #0
	sta drawing
	jsr parse_frame
	lda #&80
	sta drawing
	rts
}
