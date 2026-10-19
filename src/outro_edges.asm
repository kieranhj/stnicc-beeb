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
\\ block or the stream).
\\
\\ Each run the decoder keeps draws its own edges (run_sink): across its first
\\ row where it differs from the run above, down its rows where its two pixels
\\ differ, and down its rows where the column to the left's right pixel differs
\\ from its left pixel (a walker through that column, ew).

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

	\\ Across its first row (not the top), the pixels that differ from the run above
	ldy last
	beq no_top
	lda run_p
	eor above
	sta tmp
	lda #0
	sta m
	lda tmp
	cmp #&10
	bcc no_left
	lda hl
	sta m
	.no_left
	lda tmp
	and #&0F
	beq no_right
	lda m
	ora hr
	sta m
	.no_right
	lda edge_row_lo, Y
	sta ep
	lda edge_row_hi, Y
	clc
	adc draw_buffer_HI
	sta ep+1
	ldy ecol
	lda (ep), Y
	ora m
	sta (ep), Y
	.no_top
	lda run_p
	sta above

	\\ Down its rows between its two pixels, if they differ
	lsr a:lsr a:lsr a:lsr a
	sta lpix
	eor run_p
	and #&0F
	beq no_inner
	lda vr
	sta m
	lda last
	sta y0
	lda run_e
	sta y1
	jsr vline
	.no_inner

	\\ Down its rows where the column to the left's right pixel differs from its left
	lda col_x
	beq done
	lda vl
	sta m
	lda last
	sta y0
	.across
	ldy ew
	lda y0
	.walk
	cmp (lc_end), Y
	bcc found                   ; y0 < the end of the run there
	iny
	bne walk
	.found
	sty ew
	lda (lc_end), Y
	cmp run_e
	bcc to_its_end
	lda run_e
	.to_its_end
	sta y1
	lda (lc_val), Y
	and #&0F
	cmp lpix
	beq same
	jsr vline
	.same
	lda y1
	sta y0
	cmp run_e
	bcc across

	.done
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
	lda #%00001000
	sta vl
	lda #%00000010
	sta vr
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

\\ OR m into byte column ecol of the window, rows y0 to y1 - 1 (X lost)
.vline
{
	ldy y0
	lda edge_row_lo, Y
	sta ep
	lda edge_row_hi, Y
	clc
	adc draw_buffer_HI
	sta ep+1
	lda y1
	sec
	sbc y0
	tax
	ldy ecol
	.loop
	lda (ep), Y
	ora m
	sta (ep), Y
	dex
	beq done
	inc ep                      ; the next line (each character row's lines start
	lda ep                      ; at a multiple of 8)
	and #7
	bne loop
	lda ep                      ; on to the next character row
	clc
	adc #LO(MODE4_ROW_BYTES - 8)
	sta ep
	lda ep+1
	adc #HI(MODE4_ROW_BYTES - 8)
	sta ep+1
	bne loop                    ; always
	.done
	rts
}
