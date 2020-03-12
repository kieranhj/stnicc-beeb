\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	DATA TABLES FOR PLOT ROUTINES
\ ******************************************************************

\ ******************************************************************
\ *	PIXEL BYTE MASKS
\ ******************************************************************

PAGE_ALIGN_FOR_SIZE 4
.screen_mask_starting_at_pixel
{
    EQUB %00000000
    EQUB %10001000
    EQUB %11001100
    EQUB %11101110
}
CHECK_SAME_PAGE_AS screen_mask_starting_at_pixel

PAGE_ALIGN_FOR_SIZE 4
.colour_mask_starting_at_pixel
{
    EQUB %11111111
    EQUB %01110111
    EQUB %00110011
    EQUB %00010001
}
CHECK_SAME_PAGE_AS colour_mask_starting_at_pixel

PAGE_ALIGN_FOR_SIZE 4
.four_minus
EQUB 4,3,2,1
CHECK_SAME_PAGE_AS four_minus

PAGE_ALIGN_FOR_SIZE 14
.minus_1_times_4
EQUB 0, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48
CHECK_SAME_PAGE_AS minus_1_times_4

\ ******************************************************************
\ *	PRE-SHIFTED DATA FOR SHORT/MEDIUM SPANS
\ ******************************************************************

\\ Can only be a maximum of 2 bytes plotted for short (<=5 pixel) spans
\\ X = [0,3] W = [1,5]

\\ p000 0000  pp00 0000  ppp0 0000  pppp 0000  pppp p000
\\ 0p00 0000  0pp0 0000  0ppp 0000  0ppp p000  0ppp pp00
\\ 00p0 0000  00pp 0000  00pp p000  00pp pp00  00pp ppp0
\\ 000p 0000  000p p000  000p pp00  000p ppp0  000p pppp

\\ Can only be a maximum of 3 bytes plotted for medium spans..
\\ X = [0,3] W = [6,9]

\\ pppp pp00 0000  pppp ppp0 0000  pppp pppp 0000  pppp pppp p000
\\ 0ppp ppp0 0000  0ppp pppp 0000  0ppp pppp p000  0ppp pppp pp00
\\ 00pp pppp 0000  00pp pppp p000  00pp pppp pp00  00pp pppp ppp0
\\ 000p pppp p000  000p pppp pp00  000p pppp ppp0  000p pppp pppp

\\ Etc.

MACRO FOUR_BYTE_MASK_SHIFTS p, e, table_index
FOR x,0,3,1
s=p>>x
h=(s AND &f000) >> 12
m=(s AND &0f00) >> 8
l=(s AND &00f0) >> 4
z=(s AND &000f)
IF table_index==0
EQUB (h OR h<<4) EOR e
ELIF table_index==1
EQUB (m OR m<<4) EOR e
ELIF table_index==2
EQUB (l OR l<<4) EOR e
ELSE
EQUB (z OR z<<4) EOR e
ENDIF
NEXT
ENDMACRO

\\ 1, 2, 3, 4, 5 pixels = 2 bytes max
MACRO SHORT_MASK_TABLE e,table_index            ; 20 bytes
FOUR_BYTE_MASK_SHIFTS &8000, e, table_index      ; 4 bytes
FOUR_BYTE_MASK_SHIFTS &C000, e, table_index
FOUR_BYTE_MASK_SHIFTS &E000, e, table_index
FOUR_BYTE_MASK_SHIFTS &F000, e, table_index
FOUR_BYTE_MASK_SHIFTS &F800, e, table_index

IF _SHORT_SPAN_MAX_PIXELS > 5
\\ 6, 7, 8, 9 pixels = 3 bytes max
FOUR_BYTE_MASK_SHIFTS &FC00, e, table_index      ; 4 bytes
FOUR_BYTE_MASK_SHIFTS &FE00, e, table_index
FOUR_BYTE_MASK_SHIFTS &FF00, e, table_index
FOUR_BYTE_MASK_SHIFTS &FF80, e, table_index
ENDIF

IF _SHORT_SPAN_MAX_PIXELS > 9
\\ 10, 11, 12, 13 pixels = 4 bytes max
FOUR_BYTE_MASK_SHIFTS &FFC0, e, table_index      ; 4 bytes
FOUR_BYTE_MASK_SHIFTS &FFE0, e, table_index
FOUR_BYTE_MASK_SHIFTS &FFF0, e, table_index
FOUR_BYTE_MASK_SHIFTS &FFF8, e, table_index
ENDIF
ENDMACRO

PAGE_ALIGN_FOR_SIZE 52
.colour_mask_short_0
SHORT_MASK_TABLE 0,0        ; 20+16+16 = 52 bytes max
CHECK_SAME_PAGE_AS colour_mask_short_0

PAGE_ALIGN_FOR_SIZE 52
.colour_mask_short_1
SHORT_MASK_TABLE 0,1
CHECK_SAME_PAGE_AS colour_mask_short_1

; tables to 3 bytes
IF _SHORT_SPAN_MAX_PIXELS > 5
PAGE_ALIGN_FOR_SIZE 52
.colour_mask_short_2
SHORT_MASK_TABLE 0,2
CHECK_SAME_PAGE_AS colour_mask_short_2
ENDIF

; tables to 4 bytes
IF _SHORT_SPAN_MAX_PIXELS > 9
PAGE_ALIGN_FOR_SIZE 52
.colour_mask_short_3
SHORT_MASK_TABLE 0,3
CHECK_SAME_PAGE_AS colour_mask_short_3
ENDIF
