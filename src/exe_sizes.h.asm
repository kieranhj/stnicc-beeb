\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	EXECUTABLE SIZES ON DISC 1
\ ******************************************************************

\\ Sector-rounded sizes of the player executables. stnicc-build.asm pads disc 1
\\ with them so that stream file 00 starts exactly on DISK1_first_track, and
\\ stnicc-beeb.asm asserts that the variant it builds still fills its slot
\\ exactly. If an executable grows or shrinks by a sector, change it here.
\\ Needs _DEBUG, which must agree between the two files.

IF _DEBUG
EXE_SIZE_LOW = &2800
EXE_SIZE_MEDIUM = &2700
EXE_SIZE_HIGH = &2700
EXE_SIZE_NULA = &2500
ELSE
EXE_SIZE_LOW = &2800
EXE_SIZE_MEDIUM = &2700
EXE_SIZE_HIGH = &2700
EXE_SIZE_NULA = &2400
ENDIF
