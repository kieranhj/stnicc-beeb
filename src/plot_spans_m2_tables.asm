\ -*- mode:beebasm -*-

\\ Tables for src/plot_spans_m2.asm, which runs at &300: these stay in main RAM
\\ (swap_frame_buffers points fps_row_table_LO/HI at one screen's pair)
\\ Address of the unrolled span code for the character row of polygon row y
\\ (y>>2), for each screen
MACRO M2_UNROLLED_TABLE base, hi
FOR y,0,99,1
IF hi
EQUB HI(base + (y DIV 4) * UNROLLED_ROW_SIZE)
ELSE
EQUB LO(base + (y DIV 4) * UNROLLED_ROW_SIZE)
ENDIF
NEXT
ENDMACRO

PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen1_LO:M2_UNROLLED_TABLE span_screen1_row0_unrolled, FALSE
PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen1_HI:M2_UNROLLED_TABLE span_screen1_row0_unrolled, TRUE
PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen2_LO:M2_UNROLLED_TABLE span_screen2_row0_unrolled, FALSE
PAGE_ALIGN_FOR_SIZE 100
.fps_unrolled_screen2_HI:M2_UNROLLED_TABLE span_screen2_row0_unrolled, TRUE
