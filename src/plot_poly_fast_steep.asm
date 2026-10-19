\ -*- mode:beebasm -*-

\\ Part of the fast edge stage (src/plot_poly_fast.asm): included there
\\ for LOW, and assembled to run in spare low RAM for NULA and CPC.

\ ******************************************************************
\ *	DOWNWARD EDGES: STORE
\ ******************************************************************

\\ Steep: one point per row; only interior rows y0+1..y1-1 are written.
.down_steep
{
    ; A = dx, Y = y0
    cmp #0                      ; Z is from the caller's LDX
    beq down_vertical
    sec
    sbc #1
    sta ds_dx+1                 ; subtracted with C=0, so dx-1
    lda dy
    sta ds_dy+1
    lda endy
    sta ds_end+1
    lda edge_x0
    sta ds_x+1
    lda dy
    lsr a                       ; acc = dy/2

    iny
    cpy endy                    ; no interior rows if dy=1
    beq done                    ; else C=0: below y1
    jmp loop                    ; (over any alignment)

    PAGE_ALIGN_FOR_SIZE DS_LOOP_SIZE        ; the loop's branches stay on one page
    .loop
    .^ds_dx
    sbc #0                      ; acc -= dx
    bcs no_step
    .^ds_dy
    adc #0                      ; acc += dy
    .^ds_step
    inc ds_x+1                  ; inc/dec: x += sx
    .no_step
    tax
    .^ds_x
    lda #0
    sta S_BUF, Y
    sta E_BUF, Y
    txa
    iny
    .^ds_end
    cpy #0                      ; C=0 while below y1
    bne loop
    ASSERT P% - loop = DS_LOOP_SIZE

    .done
    ldx edge_i
    jmp return_here_from_fast_down_edge

    .down_vertical
    lda endy
    sta dv_end+1
    lda edge_x0
    iny
    cpy endy
    beq done
    .vloop
    sta S_BUF, Y
    sta E_BUF, Y
    iny
    .dv_end
    cpy #0
    bne vloop
    beq done
}
