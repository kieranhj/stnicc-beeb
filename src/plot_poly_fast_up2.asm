\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	UPWARD EDGES: COMPARE (continued)
\ ******************************************************************

.up_shallow_left
{
    sta usl_dx1+1
    sta usl_dx2+1
    lsr a
    tax
    lda dy
    sta usl_dy1+1
    sta usl_dy2+1
    lda endy
    sta usl_end+1
    txa
    ldx edge_x0
    sec
    .count1
    dex
    .^usl_dy1
    sbc #0
    bcs count1
    .^usl_dx1
    adc #0
    sta accum
    txa
    cmp S_BUF, Y:bcs loop:sta S_BUF, Y

    .loop
    dey
    .^usl_end
    cpy #0
    beq last
    txa
    cmp E_BUF, Y:bcc not_e:sta E_BUF, Y:.not_e
    lda accum
    sec
    .count2
    dex
    .^usl_dy2
    sbc #0
    bcs count2
    .^usl_dx2
    adc #0
    sta accum
    txa
    cmp S_BUF, Y:bcs loop:sta S_BUF, Y
    jmp loop

    .last
    txa
    cmp E_BUF, Y:bcc done:sta E_BUF, Y
    .done
    ldx edge_i
    jmp return_here_from_fast_up_edge
}
