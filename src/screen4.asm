\ ******************************************************************
\ *	SCREEN FUNCTIONS
\ ******************************************************************

.screen_cls
{
    lda draw_buffer_HI
    cmp #HI(screen1_addr)
    bne screen2_cls
}
\\ Drop thru!
.screen1_cls
{
    lda #0
    ldx #0
    .loop
    FOR row,0,TOP_SCREEN_ROWS-1,1
    sta screen1_addr + row*MODE4_ROW_BYTES + WIREFRAME_CORNER_X, X
    NEXT
    inx
    bne loop
    rts
}

.screen2_cls
{
    lda #0
    ldx #0
    .loop
    FOR row,0,TOP_SCREEN_ROWS-1,1
    sta screen2_addr + row*MODE4_ROW_BYTES + WIREFRAME_CORNER_X, X
    NEXT
    inx
    bne loop
    rts
}
