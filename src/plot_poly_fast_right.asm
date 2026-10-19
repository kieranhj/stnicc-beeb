\ -*- mode:beebasm -*-

\\ Part of the fast edge stage (src/plot_poly_fast.asm): included there
\\ for LOW, and assembled to run in spare low RAM for NULA and CPC.

\\ Shallow: each row is a run from the pixel the row starts on to the pixel
\\ where it changes (which also starts the next row).
.down_shallow_right
{
    ; A = dx, Y = y0
    sta dsr_dx1+1
    sta dsr_dx2+1
    lsr a
    tax                         ; acc = dx/2
    lda dy
    sta dsr_dy1+1
    sta dsr_dy2+1
    lda endy
    sta dsr_end+1
    txa
    ldx edge_x0
    sec
    \\ First row: far end only
    .count1
    inx
    .^dsr_dy1
    sbc #0
    bcs count1
    .^dsr_dx1
    adc #0
    sta accum
    txa
    cmp E_BUF, Y:bcc no_far:sta E_BUF, Y:.no_far
    iny
    cpy endy
    beq last

    .loop
    txa
    sta S_BUF, Y
    lda accum
    sec
    .count2
    inx
    .^dsr_dy2
    sbc #0
    bcs count2
    .^dsr_dx2
    adc #0
    sta accum
    txa
    sta E_BUF, Y
    iny
    .^dsr_end
    cpy #0
    bne loop

    \\ Last row: near end only
    .last
    txa
    cmp S_BUF, Y:bcs done:sta S_BUF, Y
    .done
    ldx edge_i
    jmp return_here_from_fast_down_edge
}
