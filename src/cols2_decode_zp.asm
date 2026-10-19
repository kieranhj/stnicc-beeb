\\ -*- mode:beebasm -*-
\\ ******************************************************************
\\ *	THE VERSION 2 COLUMN DECODER'S ZERO PAGE (src/cols2_decode.asm)
\\ ******************************************************************

\\ the column decoder
.ctl                skip 1      ; control byte bits not yet taken, above a sentinel bit
.frame_end          skip 1      ; parse_frame's return
.cur_off            skip 1      ; this frame's half of the column tables (0 or 32)
.prev_off           skip 1      ; and the frame before's
.col_no             skip 1
.next_lo            skip 1      ; ring position of this column's runs, then of the next column's
.next_hi            skip 1
.tmp                skip 1
.cnt                skip 1
.tmp_e              skip 1      ; a run's end row
.sl                 skip 1      ; and its slope
.last               skip 1      ; end row of the run before (the next run's first row)
run_s = last                    ; the run being stored's first row
.col_x              skip 1      ; byte column * 8
.walkers_set        skip 1      ; set_walkers has run for this column

\\ a new run's neighbours (read_value): a walker each through the column to the
\\ left (this frame), and the same column and the one to its right (the frame before)
.lc_val             skip 2
.lc_end             skip 2
.lc_w               skip 1
.pc_val             skip 2
.pc_end             skip 2
.pc_w               skip 1
.pr_val             skip 2
.pr_end             skip 2
.pr_w               skip 1
.n_lc               skip 1      ; the pixel pairs at the run's first row
.n_pc               skip 1
.n_pr               skip 1
.cand               skip 7      ; a pixel's candidates, in order
.d1                 skip 1
.new_l              skip 1
.vsave_x            skip 1
.vsave_y            skip 1

\\ the run kept, for the run sink
.run_e              skip 1      ; the run's row after its last
.run_p              skip 1      ; its value (pixel pair); the value above, for a new one
