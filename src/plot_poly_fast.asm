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

DS_LOOP_SIZE = 24               ; down_steep's row loop, kept on one page

.plot_poly_span
{
    ; X = poly_num_verts
IF _TEST_PARSE_ONLY
    jmp return_here_from_plot_poly
ENDIF

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
    \\ which branch back to it; NULA and CPC have their own, at plot_poly_rows)
IF _FAST_POLY
    jmp fast_plot_spans
ELIF _M2_SPANS
    jmp m2_plot_spans
ELSE
    jmp plot_poly_rows
ENDIF
}

\\ The walkers keep the edge number and x0 here
edge_i = temp
edge_x0 = scrn

\\ The edge set-up and the down walkers follow here for LOW. NULA and CPC run
\\ them from spare low RAM: see the relocated blocks in stnicc-beeb.asm.
IF _FAST_POLY
INCLUDE "src/plot_poly_fast_setup.asm"
INCLUDE "src/plot_poly_fast_steep.asm"
INCLUDE "src/plot_poly_fast_right.asm"
INCLUDE "src/plot_poly_fast_left.asm"
ENDIF

\\ The upward (compare) walkers are assembled to run in spare pages below the
\\ code: plot_poly_fast_up.asm at &900, plot_poly_fast_up2.asm at &C90 (LOW) or
\\ &7A0 (NULA and CPC).
\\ See the relocated blocks at the end of stnicc-beeb.asm.
