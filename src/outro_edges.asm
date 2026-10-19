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
\\ Each run the decoder keeps draws its own edges (run_sink), down its rows a
\\ segment at a time: a segment ends where the column to the left's run does
\\ (a walker through that column, ew). In each segment a row's bits are the
\\ left pixel's first bit if the column to the left's right pixel differs from
\\ it, and the right pixel's first bit if the run's two pixels differ; the run's
\\ first row also has the bits of the pixels that differ from the run above.
\\ (In column 0 the column to the left is background, and its left pixel's
\\ first bit is the box's left edge, set already.)

\\ The palette changes are stepped over
MACRO PALETTE_CHANGE
	jsr skip_palette
ENDMACRO

MACRO COLUMN_START
	jsr edge_column_start
ENDMACRO

COLS_KEEP = FALSE               ; the window is cleared each frame: draw every column

INCLUDE "src/cols2_decode.asm"

\\ The run sink (src/cols2_decode.asm): the edges of the run just kept, from row
\\ last to run_e - 1, value run_p. X and Y kept.
.run_sink
{
	bit drawing
	bmi draw
	lda run_e                   ; a frame not shown: just the next op or column
	sta last
	cmp #ROWS
	bcc not_done
	jmp column_done
	.not_done
	jmp next_op

	.draw
	stx save_x
	sty save_y

	\\ Across its first row: the pixels that differ from the run above (none at the top)
	lda #0
	ldy last
	beq across_done
	lda run_p
	eor above
	cmp #&10
	bcc right_only
	and #&0F
	beq left_only
	lda hlr
	bne across_done
	.left_only
	lda hl
	bne across_done
	.right_only
	lda hr
	.across_done
	sta hm
	lda run_p
	sta above

	\\ Between its two pixels, if they differ
	lsr a:lsr a:lsr a:lsr a
	sta lpix
	eor run_p
	and #&0F
	beq no_inner
	lda vr
	.no_inner
	sta inner

	\\ Down its rows, a segment at a time
	lda last
	sta y0
	ldy ew
	.walk                       ; the column to the left's run at row y0
	cmp (lc_end), Y
	bcc segment
	iny
	bne walk
	.segment
	lda (lc_end), Y             ; the segment ends where either run does
	cmp run_e
	bcc y1_ok
	lda run_e
	.y1_ok
	sta y1
	lda (lc_val), Y             ; the left pixel against the right pixel there
	and #&0F
	eor lpix
	beq same
	lda vl
	.same
	ora inner
	sta vm
	ora hm
	beq next_segment

	\\ Rows y0 to y1 - 1, through the character cell: Y = the line in it
	sty ew
	ldx y0
	lda cellrow_lo, X
	clc
	adc ecol
	sta ep
	lda cellrow_hi, X
	adc draw_buffer_HI
	sta ep+1
	txa
	and #7
	tay
	lda y1
	sec
	sbc y0
	tax
	lda vm                      ; the first row, with the bits across
	ora hm
	ora (ep), Y
	sta (ep), Y
	lda #0
	sta hm
	lda vm
	beq rows_done
	dex
	beq rows_done
	.row
	iny
	cpy #8
	beq next_cell
	.in_cell
	lda (ep), Y
	ora vm
	sta (ep), Y
	dex
	bne row
	beq rows_done               ; always
	.next_cell
	lda ep                      ; on to the next character row
	clc
	adc #LO(MODE4_ROW_BYTES)
	sta ep
	lda ep+1
	adc #HI(MODE4_ROW_BYTES)
	sta ep+1
	ldy #0
	beq in_cell                 ; always
	.rows_done
	ldy ew

	.next_segment
	lda y1
	cmp run_e
	bcs done
	sta y0
	iny
	bne segment                 ; always

	.done
	sty ew
	ldx save_x
	ldy save_y
	lda run_e
	sta last
	cmp #ROWS
	bcs column_done
}

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

\\ Each column: the walker through the column to the left (set_walkers points
\\ lc_val and lc_end there), its place in the window and its pixels' bits
.edge_column_start
{
	bit drawing
	bmi draw
	rts
	.draw
	jsr set_walkers
	lda #0
	sta ew
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

\\ After a frame is drawn, while its swap waits for vsync: decode the next pair's
\\ first frame, unless the stream has ended or is short of data (the check before
\\ sets buffer_lock unless STREAM_FRAME_PAGES are loaded: enough for a pair)
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
	.done
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
