\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	FAST SPAN PLOTTING (_FAST_POLY, LOW only)
\ ******************************************************************

\\ Plots rows span_buffer_min_y..max_y of the polygon from the span buffer,
\\ writing the same bytes as plot_short_span / plot_long_span.
\\
\\ Rows go in four passes, one per y AND 3, so that within a pass the dither
\\ byte and the scanline within the character row are fixed: row y is drawn
\\ on scanline 2y, i.e. character row y>>2, scanline 2*(y AND 3). writeptr
\\ points at that scanline of the character row, so a byte column c is at
\\ (writeptr),Y with Y = c*8 (screen_col_LO), and the next row of the pass is
\\ one page on.
\\
\\ Each row: one byte (masks of both ends combined), or two end bytes, masked,
\\ plus the full bytes between them through the unrolled span row code.
\\ Assembled to run at &400 (see the relocated blocks in stnicc-beeb.asm).

fps_ofsL = span_start           ; left end byte column * 8
fps_ofsR = count                ; right end byte column * 8
fps_nb8 = span_width            ; (right column - left column) * 8
fps_mask = shortptr             ; right end mask, or the one-byte mask
fps_maskL = shortptr+1          ; left end mask
fps_off = shortptr+1            ; unrolled code entry offset (after fps_maskL)
fps_pass = poly_index           ; first row of this pass

MACRO FPS_MASKED_WRITE mask
    lda (writeptr), Y           ; 5c
    eor span_colour             ; 3c
    and mask                    ; 3c
    eor (writeptr), Y           ; 5c
    sta (writeptr), Y           ; 6c
ENDMACRO

.fast_plot_spans
{
    ldy span_buffer_max_y
    iny
    sty fps_max+1

    ldx span_buffer_min_y
    .pass
    stx fps_pass
    txa:and #3:tay
    .^load_palette
    lda poly_palette, Y         ; + poly colour*4, set by parse_frame
    sta span_colour
    tya:asl a
    sta writeptr                ; scanline 2*(y AND 3)
    txa:lsr a:lsr a             ; character row y>>2
    clc
    adc draw_buffer_HI
    sta writeptr+1

    .row
    ; X = y
    ldy span_buffer_end, X      ; 4c  e
    lda rmask, Y                ; 4c
    sta fps_mask                ; 3c
    lda screen_col_LO, Y        ; 4c
    sta fps_ofsR                ; 3c
    ldy span_buffer_start, X    ; 4c  s
    sec                         ; 2c
    sbc screen_col_LO, Y        ; 4c
    bne multi                   ; 2/3c

    \\ One byte
    lda lmask, Y                ; 4c
    and fps_mask                ; 3c
    sta fps_mask                ; 3c
    lda screen_col_LO, Y        ; 4c
    tay                         ; 2c
    FPS_MASKED_WRITE fps_mask   ; 22c

    .^return_here_from_unrolled_span_loop
    .next_row
    inc writeptr+1              ; 5c    (one too many at the end of a pass: harmless)
    txa                         ; 2c
    clc                         ; 2c
    adc #4                      ; 2c
    tax                         ; 2c
    .fps_max
    cpx #0                      ; 2c   max_y+1
    bcc row                     ; 3c

    .pass_done
    ldx fps_pass
    inx
    cpx fps_max+1
    bcs done
    txa
    sec
    sbc span_buffer_min_y
    cmp #4                      ; four passes, or fewer if fewer rows
    bcc pass
    .done
    jmp return_here_from_plot_poly

    \\ Two or more bytes: A = (right column - left column) * 8
    .multi
    sta fps_nb8                 ; 3c
    lda lmask, Y                ; 4c
    sta fps_maskL               ; 3c
    lda screen_col_LO, Y        ; 4c
    tay                         ; 2c
    sty fps_ofsL                ; 3c
    FPS_MASKED_WRITE fps_maskL  ; 22c   left end
    ldy fps_ofsR                ; 3c
    FPS_MASKED_WRITE fps_mask   ; 22c   right end
    lda fps_nb8                 ; 3c
    cmp #9                      ; 2c
    bcc next_row                ; 3c

    \\ Full bytes between: jump into the unrolled code for this character row
    \\ at the entry for nb8/8-1 stores, Y = left column*8 + 8 + scanline.
    lsr a                       ; 2c    nb8/2 = (stores+1)*4
    tay                         ; 2c
    lda long_span_tables-4, Y   ; 4c    (32-stores)*3
    clc                         ; 2c
    .^fps_row_table_LO
    adc fps_unrolled_screen1_LO, X      ; 4c    by y; set by swap_frame_buffers
    sta jump+1                  ; 4c
    .^fps_row_table_HI
    lda fps_unrolled_screen1_HI, X      ; 4c
    adc #0                      ; 2c
    sta jump+2                  ; 4c
    lda fps_ofsL                ; 3c
    adc #8                      ; 2c    C=0
    ora writeptr                ; 3c    scanline
    tay                         ; 2c
    lda span_colour             ; 3c
    .jump
    jmp &ffff                   ; 3c    returns to next_row
}

\\ Address of the unrolled span code for the character row of polygon row y
\\ (y>>2), for each screen
MACRO FPS_UNROLLED_TABLE base, hi
FOR y,0,99,1
IF hi
EQUB HI(base + (y DIV 4) * UNROLLED_ROW_SIZE)
ELSE
EQUB LO(base + (y DIV 4) * UNROLLED_ROW_SIZE)
ENDIF
NEXT
ENDMACRO

PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen1_LO:FPS_UNROLLED_TABLE span_screen1_row0_unrolled, FALSE
PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen1_HI:FPS_UNROLLED_TABLE span_screen1_row0_unrolled, TRUE
PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen2_LO:FPS_UNROLLED_TABLE span_screen2_row0_unrolled, FALSE
PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen2_HI:FPS_UNROLLED_TABLE span_screen2_row0_unrolled, TRUE
