\\ -*- mode:beebasm -*-
\\ ******************************************************************
\\ *	THE VERSION 2 COLUMN DECODER, PART 2: AFTER THE RUN SINK
\\ ******************************************************************

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
