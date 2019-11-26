\ ******************************************************************
\ *	PALETTE STREAM DATA
\ ******************************************************************

MACRO PALETTE_FRAME pm, c0, c1, c2, c3, pal_updates

num_colours = (pm AND 8)>>3 + (pm AND 4)>>2 + (pm AND 2)>>1 + (pm AND 1)

EQUB (pal_updates << 4) OR num_colours

IF pm AND 8
    EQUB &00+c0, &10+c0, &40+c0, &50+c0
ENDIF

IF pm AND 4
    EQUB &20+c1, &30+c1, &60+c1, &70+c1
ENDIF

IF pm AND 2
    EQUB &80+c2, &90+c2, &c0+c2, &d0+c2
ENDIF

IF pm AND 1
    EQUB &a0+c3, &b0+c3, &e0+c3, &f0+c3
ENDIF

ENDMACRO

MACRO PALETTE_UPDATE index, colour1, colour2, dither

IF colour1=0
    byte1=&00
ELIF colour1=1
    byte1=&0F
ELIF colour1=2
    byte1=&F0
ELSE
    byte1=&FF
ENDIF

IF colour2=0
    byte2=&00
ELIF colour2=1
    byte2=&0F
ELIF colour2=2
    byte2=&F0
ELSE
    byte2=&FF
ENDIF

    EQUB byte1, byte2, (dither-1) * 4, index * 4

ENDMACRO

MACRO PALETTE_FRAME_NO_CHANGE
    EQUB 0
ENDMACRO

MACRO PALETTE_FRAME_JUST_UPDATES pal_updates
    EQUB (pal_updates << 4)
ENDMACRO

.palette_stream_start
{
    ; Starting palette: PAL_black, PAL_red, PAL_yellow, PAL_white
    ; Oxygene
    PALETTE_FRAME_NO_CHANGE                                             ; [1]
    ; Square orange tunnel
    PALETTE_FRAME_JUST_UPDATES 2                                        ; [77]
    PALETTE_UPDATE 7, 0, 1, 9       ; [7] = [4, 2, 0]
    PALETTE_UPDATE 8, 0, 1, 6       ; [8] = [3, 1, 0]

    PALETTE_FRAME_NO_CHANGE                                             ; [80]
    PALETTE_FRAME_NO_CHANGE                                             ; [88]
    PALETTE_FRAME_NO_CHANGE                                             ; [89]
    ; Brown trench
    PALETTE_FRAME_NO_CHANGE                                             ; [90]
    PALETTE_FRAME_NO_CHANGE                                             ; [98]
    PALETTE_FRAME_NO_CHANGE                                             ; [143]
    ; First blue appears but we'll map that to grey as red dominates
    PALETTE_FRAME_NO_CHANGE                                             ; [144]
    PALETTE_FRAME_NO_CHANGE                                             ; [145]
    ; Yellow disappears
    PALETTE_FRAME_NO_CHANGE                                             ; [151]
    PALETTE_FRAME_NO_CHANGE                                             ; [152]
    PALETTE_FRAME_NO_CHANGE                                             ; [154]
    PALETTE_FRAME_JUST_UPDATES 1                                        ; [155]
    PALETTE_UPDATE 10, 0, 3, 6     ; [10] = [3, 3, 3]
    PALETTE_FRAME_NO_CHANGE                                             ; [161]
    PALETTE_FRAME_NO_CHANGE                                             ; [162]
    PALETTE_FRAME_JUST_UPDATES 1                                        ; [166]
    PALETTE_UPDATE 5, 0, 3, 8      ; [5] = [4, 4, 4]
    PALETTE_FRAME_JUST_UPDATES 1                                        ; [167]
    PALETTE_UPDATE 2, 0, 1, 2      ; [2] = [0, 0, 2]

    PALETTE_FRAME_NO_CHANGE                                             ; [171]
    PALETTE_FRAME_NO_CHANGE                                             ; [173]
    ; More blue than red now
    PALETTE_FRAME %0100, PAL_black, PAL_blue, PAL_yellow, PAL_white, 0  ; [176]
    \\ => update any palette entries using colour 1 as required.
    PALETTE_FRAME_NO_CHANGE                                             ; [178]
    ; Yellow motif appears again
    PALETTE_FRAME %0010, PAL_black, PAL_blue, PAL_yellow, PAL_white, 1  ; [190]
    \\ => update any palette entries using colour 2 as required.
    PALETTE_UPDATE 1, 0, 2, 16      ; [1] = [7, 7, 0]

    PALETTE_FRAME_NO_CHANGE                                             ; [190]
}
.palette_stream_end
