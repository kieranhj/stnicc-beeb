\ -*- mode:beebasm -*-

\\ Part of the fast edge stage (src/plot_poly_fast.asm): included there
\\ for LOW, and assembled to run in spare low RAM for NULA and CPC.

.down_shallow_left
{
    sta dsl_dx1+1
    sta dsl_dx2+1
    lsr a
    tax
    lda dy
    sta dsl_dy1+1
    sta dsl_dy2+1
    lda endy
    sta dsl_end+1
    txa
    ldx edge_x0
    sec
    .count1
    dex
    .^dsl_dy1
    sbc #0
    bcs count1
    .^dsl_dx1
    adc #0
    sta accum
    txa
    cmp S_BUF, Y:bcs no_far:sta S_BUF, Y:.no_far
    iny
    cpy endy
    beq last

    .loop
    txa
    sta E_BUF, Y
    lda accum
    sec
    .count2
    dex
    .^dsl_dy2
    sbc #0
    bcs count2
    .^dsl_dx2
    adc #0
    sta accum
    txa
    sta S_BUF, Y
    iny
    .^dsl_end
    cpy #0
    bne loop

    .last
    txa
    cmp E_BUF, Y:bcc done:sta E_BUF, Y
    .done
    ldx edge_i
    jmp return_here_from_fast_down_edge
}
