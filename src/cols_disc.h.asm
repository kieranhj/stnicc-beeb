\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	THE COLUMN DISC'S LAYOUT
\ ******************************************************************

\\ The column stream (data/scene1.cols.bin) starts at this track of side 1 and
\\ carries on down side 2 from track 79, as the polygon stream does on the main
\\ disc. stnicc-cols-build.asm pads the files ahead of it to fill exactly the
\\ tracks before; make.bat passes the same track to bin/split.py.
COLS_FIRST_TRACK = 20

\\ Sector-rounded size of the column player (asserted by stnicc-cols.asm)
EXE_SIZE_COLS = &1C00

\\ Sector-rounded size of its outro, build/OUTRON (asserted by src/outro.asm)
OUTRO_SIZE_NO_STREAM = &E00
