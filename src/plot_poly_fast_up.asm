\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	UPWARD EDGES: COMPARE
\ ******************************************************************

.up_steep
{
    ; A = dx, Y = y0
    cmp #0                      ; Z is from the caller's LDX
    beq up_vertical
    sta us_dx+1                 ; subtracted with C=1
    lda dy
    sta us_dy+1
    lda endy
    sta us_end+1
    lda edge_x0
    sta us_x+1
    lda dy
    lsr a

    dey
    cpy endy                    ; no interior rows if dy=1
    beq done                    ; else C=1: above y1

    .loop
    .^us_dx
    sbc #0
    bcs no_step
    .^us_dy
    adc #0
    .^us_step
    inc us_x+1
    .no_step
    tax
    .^us_x
    lda #0
    cmp S_BUF, Y:bcs not_s:sta S_BUF, Y:.not_s
    cmp E_BUF, Y:bcc not_e:sta E_BUF, Y:.not_e
    txa
    dey
    .^us_end
    cpy #0                      ; C=1 while above y1
    bne loop

    .done
    ldx edge_i
    jmp return_here_from_fast_up_edge

    .up_vertical
    lda endy
    sta uv_end+1
    lda edge_x0
    .vloop
    dey
    .^uv_end
    cpy #0
    beq done
    cmp S_BUF, Y:bcs vnot_s:sta S_BUF, Y:.vnot_s
    cmp E_BUF, Y:bcc vloop:sta E_BUF, Y
    jmp vloop
}

.up_shallow_right
{
    sta usr_dx1+1
    sta usr_dx2+1
    lsr a
    tax
    lda dy
    sta usr_dy1+1
    sta usr_dy2+1
    lda endy
    sta usr_end+1
    txa
    ldx edge_x0
    sec
    .count1
    inx
    .^usr_dy1
    sbc #0
    bcs count1
    .^usr_dx1
    adc #0
    sta accum
    txa
    cmp E_BUF, Y:bcc loop:sta E_BUF, Y

    .loop
    dey
    .^usr_end
    cpy #0
    beq last
    txa
    cmp S_BUF, Y:bcs not_s:sta S_BUF, Y:.not_s
    lda accum
    sec
    .count2
    inx
    .^usr_dy2
    sbc #0
    bcs count2
    .^usr_dx2
    adc #0
    sta accum
    txa
    cmp E_BUF, Y:bcc loop:sta E_BUF, Y
    jmp loop

    .last
    txa
    cmp S_BUF, Y:bcs done:sta S_BUF, Y
    .done
    ldx edge_i
    jmp return_here_from_fast_up_edge
}

