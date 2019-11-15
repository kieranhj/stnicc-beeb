\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	DATA TABLES FOR PLOT ROUTINES
\ ******************************************************************

\ ******************************************************************
\ *	PIXEL BYTE MASKS
\ ******************************************************************

.screen_mask_starting_at_pixel
{
    EQUB %00000000
    EQUB %10001000
    EQUB %11001100
    EQUB %11101110
}

.colour_mask_starting_at_pixel
{
    EQUB %11111111
    EQUB %01110111
    EQUB %00110011
    EQUB %00010001
}

.screen_mask_pixel
{
    EQUB %01110111
    EQUB %10111011
    EQUB %11011101
    EQUB %11101110
}

.colour_mask_pixel
{
    EQUB %10001000
    EQUB %01000100
    EQUB %00100010
    EQUB %00010001
}

.four_minus
EQUB 4,3,2,1

.minus_1_times_4
EQUB 0, 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48

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

IF _USE_MEDIUM_SPAN_PLOT
\\ 6, 7, 8, 9 pixels = 3 bytes max
FOUR_BYTE_MASK_SHIFTS &FC00, e, table_index      ; 4 bytes
FOUR_BYTE_MASK_SHIFTS &FE00, e, table_index
FOUR_BYTE_MASK_SHIFTS &FF00, e, table_index
FOUR_BYTE_MASK_SHIFTS &FF80, e, table_index

\\ 10, 11, 12, 13 pixels = 4 bytes max
FOUR_BYTE_MASK_SHIFTS &FFC0, e, table_index
FOUR_BYTE_MASK_SHIFTS &FFE0, e, table_index
FOUR_BYTE_MASK_SHIFTS &FFF0, e, table_index
FOUR_BYTE_MASK_SHIFTS &FFF8, e, table_index
ENDIF
ENDMACRO

.colour_mask_short_0:SHORT_MASK_TABLE 0,0        ; 20+16+16 =36 bytes
.colour_mask_short_1:SHORT_MASK_TABLE 0,1
.colour_mask_short_2:SHORT_MASK_TABLE 0,2

\ ******************************************************************
\ *	PALETTE DATA - MUST BE PAGE ALIGNED DUE TO SMC
\ ******************************************************************

PAGE_ALIGN  ; lazy
.poly_palette
{
    EQUB &00,&00,&00,&00        ; black
    EQUB &0F,&0F,&0F,&0F        ; colour 1
    EQUB &F0,&F0,&F0,&F0        ; colour 2
    EQUB &FF,&FF,&FF,&FF        ; colour 3
    EQUB &05,&00,&0A,&00        ; colour 1.1
    EQUB &05,&0A,&05,&0A        ; colour 1.2
    EQUB &05,&0F,&0A,&0F        ; colour 1.3
    EQUB &0F,&00,&0F,&00        ; stripe 1
    EQUB &50,&00,&A0,&00        ; colour 2.1
    EQUB &50,&A0,&50,&A0        ; colour 2.2
    EQUB &50,&F0,&A0,&F0        ; colour 2.3
    EQUB &F0,&00,&F0,&00        ; stripe 2
    EQUB &55,&00,&AA,&00        ; colour 3.1
    EQUB &55,&AA,&55,&AA        ; colour 3.2
    EQUB &55,&FF,&AA,&FF        ; colour 3.3
    EQUB &FF,&00,&FF,&00        ; stripe 3
}

\\\

.screen_mask_short_0:SHORT_MASK_TABLE $ff,0
.screen_mask_short_1:SHORT_MASK_TABLE $ff,1
.screen_mask_short_2:SHORT_MASK_TABLE $ff,2

; tables to 4 bytes
.colour_mask_short_3:SHORT_MASK_TABLE 0,3
.screen_mask_short_3:SHORT_MASK_TABLE $ff,3
