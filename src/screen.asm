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
    tax
    .loop
    FOR page,0,SCREEN_SIZE_BYTES-1,&100
    sta screen1_addr + page, X
    NEXT
    inx
    IF _HALF_VERTICAL_RES AND NOT(_DOUBLE_PLOT_Y)
    inx
    ENDIF
    bne loop
    rts
}

.screen2_cls
{
    lda #0
    tax
    .loop
    FOR page,0,SCREEN_SIZE_BYTES-1,&100
    sta screen2_addr + page, X
    NEXT
    inx
    IF _HALF_VERTICAL_RES AND NOT(_DOUBLE_PLOT_Y)
    inx
    ENDIF
    bne loop
    rts
}
