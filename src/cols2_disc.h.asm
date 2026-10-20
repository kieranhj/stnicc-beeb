\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	THE 1800-FRAME COLUMN DISC'S LAYOUT
\ ******************************************************************

\\ The version 2 column stream (data/scene1.cols2.bin) starts at this track of
\\ side 1 and carries on down side 2 from track 79, as on the other discs.
\\ stnicc-cols2-build.asm pads the files ahead of it to fill exactly the tracks
\\ before; make.bat passes the same track to bin/split.py.
COLS2_FIRST_TRACK = 20

\\ Sector-rounded size of the 1800-frame column player (asserted by stnicc-cols2.asm)
EXE_SIZE_COLS2 = &1E00

\\ Sector-rounded size of its outro, build/OUTROC2 (asserted by src/outro.asm)
OUTRO_SIZE_COLS2 = &1A00
