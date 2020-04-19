\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	CLOCK SCREEN
\ ******************************************************************

CLOCK_PAUSE = 50
CLOCK_FLASHES = 3
CLOCK_DELAY = 10

CLOCK_X = 64
CLOCK_ROW = 11

.show_final_screen
{
	\\ Set MODE
	lda #ULA_Mode4
	sta &248			; OS copy
	sta &fe20

if _NULA

    lda #$40					; reset extended features to defaults
	sta $fe22

endif

	ldx #LO(mode4_palette):stx pal_loop+1
	ldy #HI(mode4_palette):sty pal_loop+2
	jsr set_palette

	\\ Display screen 1
    lda #12:sta &fe00
    lda #HI(screen1_addr/8):sta &fe01
    lda #13:sta &fe00
    lda #LO(screen1_addr/8):sta &fe01

	jsr screen1_cls
	jsr show_final_clock
	jsr show_screen
    ldx #CLOCK_PAUSE
    jsr wait_X_frames

    lda #CLOCK_FLASHES
    sta clock_flashes
    .loop
    ldx #CLOCK_DELAY
    jsr wait_X_frames
    jsr hide_screen

    ldx #CLOCK_DELAY
    jsr wait_X_frames
    jsr show_screen

    dec clock_flashes
    bne loop

    rts
}

.show_final_clock
{
	\\ Vsyncs * 2 = 100 ticks per second
	clc
	rol vsync_final
	rol vsync_final+1

	\\ Divide by 60*100 for minutes
	ldx #0
	.minute_loop
	lda vsync_final+1
	cmp #HI(6000)+1
	bcs minutes_Left
	cmp #HI(6000)
	bcc done_minutes
	; equal
	lda vsync_final
	cmp #LO(6000)
	bcc done_minutes

	.minutes_Left
	sec
	lda vsync_final
	sbc #LO(6000)
	sta vsync_final
	lda vsync_final+1
	sbc #HI(6000)
	sta vsync_final+1
	inx
	bne minute_loop
	.done_minutes
	stx clock_minutes

	ldx #0
	.seconds_loop
	lda vsync_final+1
	bne seconds_left
	lda vsync_final
	cmp #LO(100)
	bcc done_seconds

	.seconds_left
	sec
	lda vsync_final
	sbc #LO(100)
	sta vsync_final
	lda vsync_final+1
	sbc #HI(100)
	sta vsync_final+1
	inx
	bne seconds_loop
	.done_seconds
	stx clock_seconds
}
\\ Fall through!
.plot_clock
{
	lda #LO(screen1_addr + CLOCK_X):sta glyphptr
	lda #HI(screen1_addr + CLOCK_ROW * MODE4_ROW_BYTES):sta glyphptr+1

	lda clock_minutes
	jsr plot_decimal
	lda #':':jsr plot_char_at_ptr
	lda clock_seconds
	jsr plot_decimal
	lda #'.':jsr plot_char_at_ptr
	lda vsync_final
	jmp plot_decimal
}

.plot_decimal
{
	ldx #0
	.digit_loop
	cmp #10
	bcc done_digits
	sec
	sbc #10
	inx
	bne digit_loop
	.done_digits

	pha
	txa
	clc
	adc #'0'
	jsr plot_char_at_ptr

	pla
	clc
	adc #'0'
	jmp plot_char_at_ptr
}

MODE4_ROW_BYTES = 256

.plot_char_at_ptr
{
    sta char_def
    lda #10
    ldx #LO(char_def)
    ldy #HI(char_def)
    jsr osword

	lda glyphptr
	sta glyphptr_copy
	lda glyphptr+1
	sta glyphptr_copy+1

	ldx #0
	.loop
	lda char_def+1, X
	pha
	lsr a:lsr a
	lsr a:lsr a
	tay
	lda four_bits_to_four_pixels, y
	ldy #0:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y

	pla:and #&f:tay
	lda four_bits_to_four_pixels, y
	ldy #8:sta (glyphptr_copy), Y
	iny:sta (glyphptr_copy), Y

	inc glyphptr_copy
	inc glyphptr_copy

	inx
	cpx #4
	bne ok

	clc
	lda glyphptr_copy
	adc #LO(MODE4_ROW_BYTES-8)
	sta glyphptr_copy
	bcc ok
	inc glyphptr_copy+1
	.ok

	cpx #8
	bcc loop

	; increment glyphptr here
	clc
	lda glyphptr
	adc #16
	sta glyphptr
	bcc no_carry
	inc glyphptr+1
	.no_carry

	rts
}

.wait_for_vsync
{
	lda #2
	.vsync1
	bit &FE4D
	beq vsync1
	rts
}

.wait_X_frames
{
	jsr wait_for_vsync
	dex
	bne wait_X_frames
	rts
}
