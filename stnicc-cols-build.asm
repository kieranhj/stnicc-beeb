\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	THE COLUMN DISC, SIDE 1 (build/beeb-niccc-cpc.dsd)
\ ******************************************************************

\ The CPC palette version on its own disc, drawn from the column stream
\ (stnicc-cols.asm, bin/colstream.py): the loader, the CPC-only intro, the
\ music, the column player as CPC and the outro, then the stream from
\ COLS_FIRST_TRACK. Side 2 is src/part-2-cols.asm.

LOAD_ADDRESS = &FF1100
EXEC_ADDRESS = &FF1100

\ Sector-rounded sizes, for the padding (make.bat checks where 00 lands)
INTROC_SIZE = &1C00
LOADER_COLS_SIZE = &B00

include "src/exe_sizes.h.asm"     ; EXE_SIZE_MUSIC
include "src/cols_disc.h.asm"     ; COLS_FIRST_TRACK, EXE_SIZE_COLS

PUTFILE "data/boot.bin", "!BOOT", &FFFFFF, 0
PUTBASIC "src/loader-cols.bas", "LOADER"
PUTFILE "build/INTROC", "INTRO", LOAD_ADDRESS, EXEC_ADDRESS
PUTFILE "build/MUSIC", "MUSIC", &8000, &8000
PUTFILE "build/CPCCOL", "CPC", LOAD_ADDRESS, EXEC_ADDRESS
PUTFILE "build/OUTRON", "OUTRO", LOAD_ADDRESS, EXEC_ADDRESS

DFS_sector_size = 256
DFS_sectors_per_track = 10
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

\ The catalogue and !BOOT take &300
exe_size = INTROC_SIZE + OUTRO_SIZE_NO_STREAM + EXE_SIZE_MUSIC + EXE_SIZE_COLS + LOADER_COLS_SIZE
dummy_size = (COLS_FIRST_TRACK * DFS_track_size) - exe_size - &300
PRINT "files =", ~exe_size, " padding =", ~dummy_size
ASSERT dummy_size > 0 AND dummy_size <= 64*1024

CLEAR &0000,&FFFF
ORG &0000
.dummy1
skip dummy_size
SAVE "dummy1", dummy1, P%

PUTFILE "data/scene1_cols.00.bin", "00", 0
