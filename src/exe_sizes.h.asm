\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	EXECUTABLE SIZES ON DISC 1
\ ******************************************************************

\\ Sector-rounded sizes of the player executables. stnicc-build.asm pads disc 1
\\ with them so that stream file 00 starts exactly on DISK1_first_track, and
\\ stnicc-beeb.asm asserts that the variant it builds still fills its slot
\\ exactly. If an executable grows or shrinks by a sector, change it here.
\\ Needs _DEBUG, which must agree between the two files.

_DEBUG =? FALSE                 ; src/music.asm has none

IF _DEBUG
EXE_SIZE_LOW = &2800
EXE_SIZE_MEDIUM = &2700
EXE_SIZE_HIGH = &2700
EXE_SIZE_NULA = &2500
ELSE
EXE_SIZE_LOW = &2800
EXE_SIZE_MEDIUM = &2700
EXE_SIZE_HIGH = &2700
EXE_SIZE_NULA = &2500
ENDIF

\\ MUSIC (src/music.asm, which asserts it): the VGI player and its three tunes
EXE_SIZE_MUSIC = &3200

\\ CPC (stnicc-beeb.asm with _CPC): the NULA build with the CPC palette and its data
EXE_SIZE_CPC = &2800
