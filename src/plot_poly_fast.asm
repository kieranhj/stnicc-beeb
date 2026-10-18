\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	FAST POLYGON SPAN BUFFER FILL (_FAST_POLY, LOW only)
\ ******************************************************************

\\ Replaces drawline_into_span_buffer and the span-buffer bookkeeping of the
\\ original plot_poly_span, producing exactly the same spans for every polygon.
\\ tools/polymodel.py fast_poly_spans() mirrors this code step by step and is
\\ checked against the original algorithm on all 37,731 polygons in the stream.
\\
\\  1. Vertex rows only: start=255, end=0. Rows are no longer reset after
\\     plotting.
\\  2. Edges going down (y1>y0), in the order n-1..0: every interior row is
\\     plain-stored with the edge's own run on that row. A shallow edge's first
\\     and last rows compare-update the far end of their runs.
\\  3. Vertices compare-update their own rows.
\\  4. Edges going up (y1<y0): compare-update every row (interior rows only for
\\     a steep edge, whose end rows hold just its vertices).
\\  Horizontal edges add nothing beyond their vertices.
\\
\\ The edge walkers follow the original Bresenham exactly: accumulator starts at
\\ major/2 and the shallow walker counts the pixel where the row changes on
\\ both rows.

S_BUF = span_buffer_start
E_BUF = span_buffer_end

OPCODE_INC_ABS = &EE
OPCODE_DEC_ABS = &CE

.plot_poly_span
{
    ; X = poly_num_verts

    \\ Close the polygon
    lda poly_verts_x:sta poly_verts_x, X
    lda poly_verts_y:sta poly_verts_y, X

    \\ 1. Vertex rows were reset, and min/max y found, as parse_frame read them
    \\ (FAST_POLY_VERTEX_ROW)
    dex
    stx poly_index                  ; n-1

    \\ 2. Edges going down: store
    ldx poly_index
    .down_loop
    lda poly_verts_y+1, X
    cmp poly_verts_y, X
    beq down_next
    bcc down_next
    jmp fast_down_edge
    .^return_here_from_fast_down_edge
    .down_next
    dex
    bpl down_loop

    \\ 3. Seed the vertices themselves: after the stores, as a vertex row can be
    \\ inside another edge going down
    ldx poly_index
    .seed_loop
    ldy poly_verts_y, X
    lda poly_verts_x, X
    cmp S_BUF, Y:bcs not_s:sta S_BUF, Y:.not_s
    cmp E_BUF, Y:bcc not_e:sta E_BUF, Y:.not_e
    dex
    bpl seed_loop

    \\ 4. Edges going up: compare
    ldx poly_index
    .up_loop
    lda poly_verts_y+1, X
    cmp poly_verts_y, X
    bcs up_next
    jmp fast_up_edge
    .^return_here_from_fast_up_edge
    .up_next
    dex
    bpl up_loop

    \\ Plot the spans (fast_plot_spans sits by the span plotters in plot_poly.asm,
    \\ which branch back to it)
    jmp fast_plot_spans
}

\ ******************************************************************
\ *	EDGE SET UP
\ ******************************************************************

\\ Edge i runs from vertex i (x0,y0) to vertex i+1 (x1,y1).
\\ The walkers take Y=y0, dy, endy=y1, edge_x0=x0, and A=dx, and return
\\ through return_here_from_fast_down_edge / _up_edge with X = i.

edge_i = temp
edge_x0 = scrn

.fast_down_edge
{
    ; X = i, A = y1, C=1 (y1 > y0)
    stx edge_i
    sta endy
    sbc poly_verts_y, X
    sta dy
    ldy poly_verts_y, X
    lda poly_verts_x, X
    sta edge_x0
    eor #&ff                    ; x1 - x0 = x1 + ~x0 + 1
    sec
    adc poly_verts_x+1, X
    bcc going_left              ; x1 < x0
    ; right: A = dx
    cmp dy
    beq steep_right
    bcc P%+5
    jmp down_shallow_right
    .steep_right
    ldx #OPCODE_INC_ABS
    stx ds_step
    jmp down_steep
    .going_left
    eor #&ff:adc #1             ; C=0: dx = x0 - x1
    cmp dy
    beq steep_left
    bcc P%+5
    jmp down_shallow_left
    .steep_left
    ldx #OPCODE_DEC_ABS
    stx ds_step
    jmp down_steep
}

.fast_up_edge
{
    ; X = i, A = y1, y1 < y0
    stx edge_i
    sta endy
    lda poly_verts_y, X
    tay
    sec
    sbc endy
    sta dy
    lda poly_verts_x, X
    sta edge_x0
    eor #&ff
    sec
    adc poly_verts_x+1, X
    bcc going_left
    cmp dy
    beq steep_right
    bcc P%+5
    jmp up_shallow_right
    .steep_right
    ldx #OPCODE_INC_ABS
    stx us_step
    jmp up_steep
    .going_left
    eor #&ff:adc #1
    cmp dy
    beq steep_left
    bcc P%+5
    jmp up_shallow_left
    .steep_left
    ldx #OPCODE_DEC_ABS
    stx us_step
    jmp up_steep
}

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

    .loop
    iny
    .^ds_end
    cpy #0                      ; C=0 while below y1
    beq done
    .^ds_dx
    sbc #0                      ; acc -= dx
    bcs no_step
    .^ds_dy
    adc #0                      ; acc += dy, C=1 after
    .^ds_step
    inc ds_x+1                  ; inc/dec: x += sx
    .no_step
    tax
    .^ds_x
    lda #0
    sta S_BUF, Y
    sta E_BUF, Y
    txa
    bcs loop                    ; always

    .done
    ldx edge_i
    jmp return_here_from_fast_down_edge

    .down_vertical
    lda endy
    sta dv_end+1
    lda edge_x0
    .vloop
    iny
    .dv_end
    cpy #0
    beq done
    sta S_BUF, Y
    sta E_BUF, Y
    bne vloop                   ; always (Z from cpy)
}

\\ Shallow: each row is a run from the pixel the row starts on to the pixel
\\ where it changes (which also starts the next row).
.down_shallow_right
{
    ; A = dx, Y = y0
    sta dsr_dx1+1
    sta dsr_dx2+1
    lsr a
    tax                         ; acc = dx/2
    lda dy
    sta dsr_dy1+1
    sta dsr_dy2+1
    lda endy
    sta dsr_end+1
    txa
    ldx edge_x0
    sec
    \\ First row: far end only
    .count1
    inx
    .^dsr_dy1
    sbc #0
    bcs count1
    .^dsr_dx1
    adc #0
    sta accum
    txa
    cmp E_BUF, Y:bcc loop:sta E_BUF, Y

    .loop
    iny
    .^dsr_end
    cpy #0
    beq last
    txa
    sta S_BUF, Y
    lda accum
    sec
    .count2
    inx
    .^dsr_dy2
    sbc #0
    bcs count2
    .^dsr_dx2
    adc #0                      ; C=1 after
    sta accum
    txa
    sta E_BUF, Y
    bcs loop                    ; always

    \\ Last row: near end only
    .last
    txa
    cmp S_BUF, Y:bcs done:sta S_BUF, Y
    .done
    ldx edge_i
    jmp return_here_from_fast_down_edge
}

.down_shallow_left
{
    sta dsl_dx1+1
    sta dsl_dx2+1
    lsr a
    tax
    lda dy
    sta dsl_dy1+1
    sta dsl_dy2+1
    lda endy
    sta dsl_end+1
    txa
    ldx edge_x0
    sec
    .count1
    dex
    .^dsl_dy1
    sbc #0
    bcs count1
    .^dsl_dx1
    adc #0
    sta accum
    txa
    cmp S_BUF, Y:bcs loop:sta S_BUF, Y

    .loop
    iny
    .^dsl_end
    cpy #0
    beq last
    txa
    sta E_BUF, Y
    lda accum
    sec
    .count2
    dex
    .^dsl_dy2
    sbc #0
    bcs count2
    .^dsl_dx2
    adc #0
    sta accum
    txa
    sta S_BUF, Y
    bcs loop

    .last
    txa
    cmp E_BUF, Y:bcc done:sta E_BUF, Y
    .done
    ldx edge_i
    jmp return_here_from_fast_down_edge
}

\\ The upward (compare) walkers are assembled to run in spare pages below the
\\ code: plot_poly_fast_up.asm at &900, plot_poly_fast_up2.asm at &C90.
\\ See the relocated blocks at the end of stnicc-beeb.asm.
