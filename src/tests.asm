\ ******************************************************************
\ *	TEST FUNCTIONS
\ ******************************************************************

IF _TESTS

.do_tests
{
    \\ Set single buffer screen
    lda #0:sta disp_buffer
    lda draw_buffer_HI
    sta disp_buffer+1

    clc
    lsr disp_buffer+1:ror disp_buffer
    lsr disp_buffer+1:ror disp_buffer
    lsr disp_buffer+1:ror disp_buffer

    lda #12:sta &fe00
    lda disp_buffer+1:sta &fe01
    lda #13:sta &fe00
    lda disp_buffer:sta &fe01

;    jsr test_drawline
    jsr test_plot_poly      \\ Watch for hard-coded JSR/RTS -> JMP/JMP
;    jsr test_plot_span
    rts
}

.test_plot_span
{
    lda #123: sta span_start
    lda #64: sta span_end
    lda #0: sta span_y
    lda #&0f: sta span_colour

    .loop
    jsr plot_span

    ldx span_end
    inx
    txa
    and #127
    sta span_end
    
    inc span_y
    bne loop
    rts
}

.test_drawline
{
    lda #0
    sta startx
    lda #119
    sta starty

    lda #127
    sta endx
    lda #146
    sta endy

    jsr drawline
    rts
}

.test_plot_poly
{
    ldx num_verts
    stx poly_num_verts

    ldy #0
    ldx #0
    .loop
    lda vertex_data, Y
    lsr a
    sta poly_verts_x, X
    iny
    lda vertex_data, Y
    sta poly_verts_y, X
    iny
    inx
    cpx num_verts
    bne loop

;    jsr plot_poly_line
    lda #1
    sta poly_colour
    jsr plot_poly_span

    rts

    .num_verts
    EQUB 4
    .vertex_data
    EQUB 31,189
    EQUB 120,190
    EQUB 127,64
    EQUB 32,7
}

ENDIF
