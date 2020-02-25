\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB
\ ******************************************************************

_DEBUG = TRUE   ; if you change me check the same in stnicc-beeb.asm
LOAD_ADDRESS = &1100
EXEC_ADDRESS = &1100

\\ NEED A BETTER WAY OF DOING THIS!
IF _DEBUG
INTRO_SIZE = &1300
EXE_SIZE = &2600
NULA_SIZE = &2300
ELSE
INTRO_SIZE = &1300
EXE_SIZE = &2600
NULA_SIZE = &2100
ENDIF

\ ******************************************************************
\ *	EXES
\ ******************************************************************

PUTFILE "build/INTRO", "INTRO", LOAD_ADDRESS, EXEC_ADDRESS
PUTFILE "build/LOW", "LOW", LOAD_ADDRESS, EXEC_ADDRESS
;PUTFILE "build/HIGH", "HIGH", LOAD_ADDRESS, EXEC_ADDRESS
;PUTFILE "build/MEDIUM", "MEDIUM", LOAD_ADDRESS, EXEC_ADDRESS
PUTFILE "build/NULA", "NULA", LOAD_ADDRESS, EXEC_ADDRESS

\ ******************************************************************
\ *	SWRAM
\ ******************************************************************

CLEAR 0, &FFFF
ORG &8000
GUARD &C000
.bank0_start

.bank0_end

PRINT "------"
PRINT "BANK 0"
PRINT "------"
PRINT "TOTAL size =", ~bank0_end-bank0_start
PRINT "------"
PRINT "HIGH WATERMARK =", ~P%
PRINT "FREE =", ~&C000-P%
PRINT "------"

;SAVE "BANK0", bank0_start, bank0_end

\ ******************************************************************
\ *	DISC LAYOUT
\ ******************************************************************

DFS_sector_size = 256
DFS_sectors_per_track = 10
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

DISK1_first_track = 30      ; 50 tracks on first disc

exe_size = EXE_SIZE + NULA_SIZE + INTRO_SIZE     ; +SWRAM size
PRINT "EXE size = ",~exe_size
; We know that Catalog + !Boot = &300
; Need to make a dummy file so 00 is at sector 20=track 2
dummy_size = (DISK1_first_track * DFS_track_size) - exe_size - &300

PRINT ~exe_size
PRINT ~dummy_size

IF dummy_size > 64*1024
    dummy1_size = 64*1024
    dummy2_size = dummy_size - 64*1024
ELSE
    dummy1_size = dummy_size
    dummy2_size = 0
ENDIF

CLEAR &0000,&FFFF
ORG &0000
.dummy1
skip dummy1_size
SAVE "dummy1", dummy1, P%

IF dummy2_size > 0
CLEAR &0000,&FFFF
ORG &0000
.dummy2
skip dummy2_size
SAVE "dummy2", dummy2, P%
ENDIF

\ ******************************************************************
\ *	Any other files for the disc
\ ******************************************************************

PUTFILE "data/scene1_disk.00.bin", "00", 0
