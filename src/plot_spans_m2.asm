\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	MODE 2 SPAN PLOTTING (_M2_SPANS: NULA and CPC)
\ ******************************************************************

\\ Plots rows span_buffer_min_y..max_y of the polygon from the span buffer,
\\ in MODE 2 at 64 x 100: row y on scanlines 2y and 2y+1, the top line with
\\ span_colour and the bottom with span_colour2 (the same byte for NULA; for CPC
\\ the two halves of a chequer).
\\
\\ As src/plot_spans_fast.asm does for LOW: rows go in four passes, one per
\\ y AND 3, so within a pass the scanline is fixed and the next row of the pass
\\ is one page on. writeptr points at the top line of the character row, so byte
\\ column c is at (writeptr),Y with Y = c*8, and the bottom line at Y+1.
\\
\\ Two pixels to a byte: a span starting on an odd pixel has a partial byte at
\\ its left (its right pixel) and one ending on an even pixel a partial byte at
\\ its right (its left pixel); every byte between is whole.
\\
\\ Half the rows are 5 pixels or fewer: those go to a routine for their start
\\ parity and width (m2_short_*), which writes each byte at a fixed offset from
\\ shortptr. Longer rows write their partial ends and send the whole bytes
\\ through the unrolled row code, top line then bottom. m2_col4[x] =
\\ ((x+1) DIV 2) * 4 is both the first whole column (*4) of a span starting at x
\\ and one past the last of a span ending at x, so the run is the difference
\\ (times 4, not 8: one past the last column is column 32, and 32*8 is 256).

M2_SHORT_MAX = 5                ; pixels

m2_from = span_start            ; first whole column * 4
m2_to = count                   ; one past the last whole column * 4
m2_pass = poly_index            ; first row of this pass

\\ A partial byte at (ptr),Y on both lines. NULA's lines are alike, so its
\\ bottom line is a copy of the top; CPC's has its own byte.
MACRO M2_MASKED_WRITE ptr, mask
    lda (ptr), Y                ; 5c    top line
    eor span_colour             ; 3c
    and #mask                   ; 2c
    eor (ptr), Y                ; 5c
    sta (ptr), Y                ; 6c
    iny                         ; 2c
IF _CPC
    lda (ptr), Y                ; 5c    bottom line
    eor span_colour2            ; 3c
    and #mask                   ; 2c
    eor (ptr), Y                ; 5c
ENDIF
    sta (ptr), Y                ; 6c
ENDMACRO

\\ A whole byte at (shortptr)+off on both lines
MACRO M2_WHOLE_WRITE off
    ldy #off                    ; 2c
    lda span_colour             ; 3c
    sta (shortptr), Y           ; 6c
    iny                         ; 2c
IF _CPC
    lda span_colour2            ; 3c
ENDIF
    sta (shortptr), Y           ; 6c
ENDMACRO

\\ A short row: odd = 1 if the first pixel is a byte's right one; w pixels
MACRO M2_SHORT odd, w
    k = odd                     ; bytes before the whole ones
    IF odd
        ldy #0
        M2_MASKED_WRITE shortptr, %01010101
    ENDIF
    IF (w - odd) DIV 2 > 0      ; (a FOR runs once even when it should not run)
    FOR b, 0, (w - odd) DIV 2 - 1
        M2_WHOLE_WRITE (k + b) * 8
    NEXT
    ENDIF
    IF (w - odd) MOD 2
        ldy #(k + (w - odd) DIV 2) * 8
        M2_MASKED_WRITE shortptr, %10101010
    ENDIF
    jmp m2_next_row
ENDMACRO

.m2_plot_spans
{
    ldy span_buffer_max_y
    iny
    sty m2_max+1

    ldx span_buffer_min_y
    .pass
    stx m2_pass
    txa:and #3:asl a
    sta writeptr                ; scanline 2*(y AND 3)
    txa:lsr a:lsr a             ; character row y>>2
    clc
    adc draw_buffer_HI
    sta writeptr+1
    sta shortptr+1

    .row
    ; X = y
    ldy span_buffer_start, X    ; 4c  s
    lda span_buffer_end, X      ; 4c  e
    sec                         ; 2c
    sbc span_buffer_start, X    ; 4c  width - 1
    cmp #M2_SHORT_MAX           ; 2c
    bcs long                    ; 2/3c

    \\ Short: the routine for the start's parity and the width
    sta span_width              ; 3c
    lda m2_left8, Y             ; 4c    the start's byte column * 8
    ora writeptr                ; 3c
    sta shortptr                ; 3c
    tya                         ; 2c
    lsr a                       ; 2c
    ldy span_width              ; 3c
    bcs short_odd               ; 2/3c
    lda m2_short_even_LO, Y     ; 4c
    sta short_jump+1            ; 4c
    lda m2_short_even_HI, Y     ; 4c
    sta short_jump+2            ; 4c
    .short_jump
    jmp &ffff                   ; 3c
    .short_odd
    lda m2_short_odd_LO, Y
    sta short_jump+1
    lda m2_short_odd_HI, Y
    sta short_jump+2
    jmp short_jump

    .long
    lda m2_col4, Y              ; 4c
    sta m2_from                 ; 3c
    tya                         ; 2c
    lsr a                       ; 2c
    bcc no_left                 ; 2/3c  s even: its byte is whole
    lda m2_left8, Y             ; 4c
    tay                         ; 2c
    M2_MASKED_WRITE writeptr, %01010101     ; the right pixel
    .no_left

    ldy span_buffer_end, X      ; 4c  e
    lda m2_col4, Y              ; 4c
    sta m2_to                   ; 3c
    tya                         ; 2c
    lsr a                       ; 2c
    bcs no_right                ; 2/3c  e odd: its byte is whole
    lda m2_left8, Y             ; 4c    (e even: its own column)
    tay                         ; 2c
    M2_MASKED_WRITE writeptr, %10101010     ; the left pixel
    .no_right

    \\ The whole bytes (at least two): through the unrolled code for this
    \\ character row, on the top line then the bottom
    lda m2_to                   ; 3c
    sec                         ; 2c
    sbc m2_from                 ; 3c    whole bytes * 4
    lsr a                       ; 2c    whole bytes * 2
    tay                         ; 2c
    lda long_span_tables, Y     ; 4c    (32 - whole bytes) * 3
    clc                         ; 2c
    .^fps_row_table_LO
    adc fps_unrolled_screen1_LO, X      ; 4c    by y; set by swap_frame_buffers
    sta jump+1                  ; 4c
    .^fps_row_table_HI
    lda fps_unrolled_screen1_HI, X      ; 4c
    adc #0                      ; 2c
    sta jump+2                  ; 4c
    lda m2_from                 ; 3c
    asl a                       ; 2c    first whole column * 8
    ora writeptr                ; 3c    top line
    tay                         ; 2c
    lda span_colour             ; 3c
    .jump
    jmp &ffff                   ; 3c

    .^return_here_from_unrolled_span_loop
    tya                         ; 2c
    lsr a                       ; 2c
    bcs next_row                ; 2/3c  the bottom line is done
    iny                         ; 2c
    lda span_colour2            ; 3c
    jmp jump                    ; 3c

    .^m2_next_row
    .next_row
    inc writeptr+1              ; 5c    (one too many at the end of a pass: harmless)
    inc shortptr+1              ; 5c
    txa                         ; 2c
    clc                         ; 2c
    adc #4                      ; 2c
    tax                         ; 2c
    .m2_max
    cpx #0                      ; 2c   max_y+1
    bcs pass_done               ; 2c
    jmp row                     ; 3c    (the row's code is too long to branch back over)
    .pass_done

    ldx m2_pass
    inx
    cpx m2_max+1
    bcs done
    txa
    sec
    sbc span_buffer_min_y
    cmp #4                      ; four passes, or fewer if fewer rows
    bcs done
    jmp pass
    .done
    jmp return_here_from_plot_poly
}

\\ The short rows: by start parity, then width - 1
.m2_short_even_1:M2_SHORT 0, 1
.m2_short_even_2:M2_SHORT 0, 2
.m2_short_even_3:M2_SHORT 0, 3
.m2_short_even_4:M2_SHORT 0, 4
.m2_short_even_5:M2_SHORT 0, 5
.m2_short_odd_1:M2_SHORT 1, 1
.m2_short_odd_2:M2_SHORT 1, 2
.m2_short_odd_3:M2_SHORT 1, 3
.m2_short_odd_4:M2_SHORT 1, 4
.m2_short_odd_5:M2_SHORT 1, 5

.m2_short_even_LO
EQUB LO(m2_short_even_1), LO(m2_short_even_2), LO(m2_short_even_3), LO(m2_short_even_4), LO(m2_short_even_5)
.m2_short_even_HI
EQUB HI(m2_short_even_1), HI(m2_short_even_2), HI(m2_short_even_3), HI(m2_short_even_4), HI(m2_short_even_5)
.m2_short_odd_LO
EQUB LO(m2_short_odd_1), LO(m2_short_odd_2), LO(m2_short_odd_3), LO(m2_short_odd_4), LO(m2_short_odd_5)
.m2_short_odd_HI
EQUB HI(m2_short_odd_1), HI(m2_short_odd_2), HI(m2_short_odd_3), HI(m2_short_odd_4), HI(m2_short_odd_5)

\\ By pixel x: the first whole column * 4 of a span starting at x (one past the
\\ last of a span ending at x); x's own byte column * 8
PAGE_ALIGN_FOR_SIZE 64
.m2_col4
FOR x,0,63,1
EQUB ((x+1) DIV 2) * 4
NEXT
PAGE_ALIGN_FOR_SIZE 64
.m2_left8
FOR x,0,63,1
EQUB (x DIV 2) * 8
NEXT
