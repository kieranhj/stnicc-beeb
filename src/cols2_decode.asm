\\ -*- mode:beebasm -*-
\\ ******************************************************************
\\ *	THE VERSION 2 COLUMN DECODER, PART 1 (bin/colstream2.py)
\\ ******************************************************************

\\ Shared by the 1800-frame column player (stnicc-cols2.asm) and the outro's
\\ column build (src/outro.asm with _COLS). The includer provides:
\\ - the zero page of src/cols2_decode_zp.asm, frame_no, STREAM_ptr_LO/HI and
\\   stream_frame_HI, and the RING_* constants and tables of the ring;
\\ - PALETTE_CHANGE, a macro run on a frame with a palette change (it must
\\   read the change: a count, then three bytes for each colour, by get_byte);
\\ - COLUMN_START, a macro run as each column starts (X and Y free);
\\ - COLS_KEEP: TRUE to leave columns the draw buffer already shows (copy mode's
\\   second bit) alone, through keep_column; FALSE to redraw them;
\\ - the run sink, straight after this file, as run_sink: each run kept is in
\\   the ring, its first row in last (run_s), its end in run_e and its value in
\\   run_p; it must keep X and Y, set last to run_e, then go to column_done
\\   once run_e reaches ROWS, else fall into src/cols2_decode_ops.asm (next_op).

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

	PALETTE_CHANGE
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

	COLUMN_START

	\\ The mode: 0 time, 10 left, 110 tslope, 111 copy
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
	GET_BIT
	bcs mode_copy
	lda #LO(tslope_ops)
	ldy #HI(tslope_ops)
	jsr reference_before
	lda col_kind, X
	bne tslope_no_slopes
	jsr slopes_on
	jmp start
	.tslope_no_slopes
	jsr slopes_off
	jmp start

	\\ copy: the same column in the frame before, unchanged (as time mode's K0
	\\ every run, without codes); then 1 if the draw buffer already shows it
	.mode_copy
	lda #LO(op_k0)
	ldy #HI(op_k0)
	jsr reference_before
	jsr slopes_off
	GET_BIT
IF COLS_KEEP
	bcc start
	jmp keep_column
ELSE
	jmp start                   ; (the includer redraws every column)
ENDIF

	.start
	ldx #0
	ldy #0
	stx last
	stx run_p                   ; nothing above the first run
	jmp next_op
}

\\ copy, when the draw buffer already shows the column: its runs go into the
\\ ring and nothing is stored
.keep_column
{
	lda site_rval+1
	sta kv+1
	lda site_rval+2
	sta kv+2
	lda site_rend1+1
	sta ke+1
	lda site_rend1+2
	sta ke+2
	lda site_kcval+1
	sta kcv+1
	lda site_kcval+2
	sta kcv+2
	lda site_kcend+1
	sta kce+1
	lda site_kcend+2
	sta kce+2
	lda site_kcslope+1
	sta kcs+1
	lda site_kcslope+2
	sta kcs+2
	ldy #0
	.loop
	.kv
	lda &FFFF, Y
	.kcv
	sta &FFFF, Y
	lda #0
	.kcs
	sta &FFFF, Y
	.ke
	lda &FFFF, Y
	.kce
	sta &FFFF, Y
	iny
	cmp #ROWS
	bcc loop
	jmp column_done
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
	jmp run_sink

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
