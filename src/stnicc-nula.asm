\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STNICC BEEB
\ ******************************************************************

; 0 = LOW quality (128 x 100 scanline skipped)
; 1 = MEDIUM quality (128 x 100 scanline doubled)
; 2 = HIGH quality (128 x 200)
_QUALITY = 2
_NULA = TRUE

include "stnicc-beeb.asm"

\ ******************************************************************
\ *	Save the code
\ ******************************************************************

SAVE "build/NULA", start, end, main
