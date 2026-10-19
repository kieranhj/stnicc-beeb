\ -*- mode:beebasm -*-

\\ Part of the fast edge stage (src/plot_poly_fast.asm): included there
\\ for LOW, and assembled to run in spare low RAM for NULA and CPC.

\ ******************************************************************
\ *	EDGE SET UP
\ ******************************************************************

\\ Edge i runs from vertex i (x0,y0) to vertex i+1 (x1,y1).
\\ The walkers take Y=y0, dy, endy=y1, edge_x0=x0, and A=dx, and return
\\ through return_here_from_fast_down_edge / _up_edge with X = i.


.fast_down_edge
{
    ; X = i, A = y1, C=1 (y1 > y0)
    stx edge_i
    sta endy
    sbc poly_verts_y, X
    sta dy
    ldy poly_verts_y, X
    lda poly_verts_x, X
    sta edge_x0
    eor #&ff                    ; x1 - x0 = x1 + ~x0 + 1
    sec
    adc poly_verts_x+1, X
    bcc going_left              ; x1 < x0
    ; right: A = dx
    cmp dy
    beq steep_right
    bcc P%+5
    jmp down_shallow_right
    .steep_right
    ldx #OPCODE_INC_ABS
    stx ds_step
    jmp down_steep
    .going_left
    eor #&ff:adc #1             ; C=0: dx = x0 - x1
    cmp dy
    beq steep_left
    bcc P%+5
    jmp down_shallow_left
    .steep_left
    ldx #OPCODE_DEC_ABS
    stx ds_step
    jmp down_steep
}

.fast_up_edge
{
    ; X = i, A = y1, y1 < y0
    stx edge_i
    sta endy
    lda poly_verts_y, X
    tay
    sec
    sbc endy
    sta dy
    lda poly_verts_x, X
    sta edge_x0
    eor #&ff
    sec
    adc poly_verts_x+1, X
    bcc going_left
    cmp dy
    beq steep_right
    bcc P%+5
    jmp up_shallow_right
    .steep_right
    ldx #OPCODE_INC_ABS
    stx us_step
    jmp up_steep
    .going_left
    eor #&ff:adc #1
    cmp dy
    beq steep_left
    bcc P%+5
    jmp up_shallow_left
    .steep_left
    ldx #OPCODE_DEC_ABS
    stx us_step
    jmp up_steep
}
