\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	DEBUG FUNCTIONS
\ ******************************************************************

IF _DEBUG
.debug_plot_glyph
{
if _NULA

    asl a:asl a:asl a:asl a
	clc
	adc #lo(debug_font_data)
	sta read_glyph_data+1
	lda #hi(debug_font_data)
	adc #0
	sta read_glyph_data+2

	ldy #7
	ldx #14
	jsr loop

	ldy #7
	ldx #15
.loop
	.read_glyph_data
	lda $ffff,x
	.write_glyph_data
	sta (debug_writeptr),y

	dex
	dex
	dey
	bpl loop

else


    asl a:asl a:asl a
    clc
    adc #LO(debug_font_data)
    sta read_glyph_data+1
    lda #HI(debug_font_data)
    adc #0
    sta read_glyph_data+2

    ldy #7
    .loop

    .read_glyph_data
    lda &ffff,y
    .write_glyph_data
    sta (debug_writeptr),y

    dey
    bpl loop

endif

	lda debug_writeptr+0
	adc #8
	sta debug_writeptr+0
	bcc done_writeptr_carry
	inc debug_writeptr+1
	.done_writeptr_carry

    rts
}

.plot_two_glyphs
{
	pha
	and #&f0
	lsr a
    clc
    adc #LO(debug_font_data)
    sta read_glyph_data1+1
    lda #HI(debug_font_data)
    adc #0
    sta read_glyph_data1+2

	pla:and #&0f
    asl a:asl a:asl a
    clc
    adc #LO(debug_font_data)
    sta read_glyph_data2+1
    lda #HI(debug_font_data)
    adc #0
    sta read_glyph_data2+2

    ldy #7
    .loop

    .read_glyph_data1
    lda &ffff,y
	asl a:asl a:asl a: asl a

    .read_glyph_data2
    ora &ffff,y

    .write_glyph_data
    sta (debug_writeptr),y

    dey
    bpl loop

	lda debug_writeptr+0
	adc #8
	sta debug_writeptr+0
	bcc done_writeptr_carry
	inc debug_writeptr+1
	.done_writeptr_carry

    rts
}

.debug_reset_writeptr
{
    lda draw_buffer_HI
    sta debug_writeptr+1
    lda #0
    sta debug_writeptr
	rts
}

.debug_write_A_spc
	sec
	equb &24		; BIT zp - swallow clc
.debug_write_A
{
	clc
    php

IF 0	
	pha
    lsr a:lsr a:lsr a:lsr a
    jsr debug_plot_glyph
    pla
    and #&f
    jsr debug_plot_glyph
ELSE
	jsr plot_two_glyphs
ENDIF

	plp
	bcc return

    clc
    lda debug_writeptr
    adc #8
    sta debug_writeptr
	bcc return
	inc debug_writeptr+1
	.return
	rts
}

.debug_font_data
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,3,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 0,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,0,3,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 0,0,0,0

MODE4_PIXELS 3,3,3,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,3,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 3,0,0,0
MODE4_PIXELS 0,0,0,0
ENDIF
