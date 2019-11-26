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
    ; Oxygene
    PALETTE_FRAME_NO_CHANGE                                             ; [1]
    ;PALETTE_FRAME %1111, PAL_black, PAL_red, PAL_yellow, PAL_white, 9
    ;PALETTE_UPDATE 0, 0, 0, 0       ; [0] = [0, 0, 0]
    ;PALETTE_UPDATE 1, 0, 1, 10      ; [1] = [5, 2, 0]
    ; Yellow motif!
    ;PALETTE_UPDATE 2, 0, 2, 16      ; [2] = [7, 7, 0]          +C2=1
    ;PALETTE_UPDATE 3, 1, 2, 4       ; [3] = [6, 3, 0]          +C2=2
    ;PALETTE_UPDATE 4, 0, 1, 8       ; [4] = [4, 1, 0]
    ;PALETTE_UPDATE 5, 1, 2, 8       ; [5] = [7, 5, 0]          +C2=3
    ;PALETTE_UPDATE 6, 0, 1, 12      ; [6] = [7, 4, 0]
    ;PALETTE_UPDATE 7, 0, 1, 4       ; [7] = [3, 0, 0]
    ;PALETTE_UPDATE 8, 0, 3, 16      ; [8] = [7, 7, 7]

    ; Square orange tunnel
    PALETTE_FRAME_JUST_UPDATES 2                                        ; [77]
    PALETTE_UPDATE 7, 0, 1, 9       ; [7] = [4, 2, 0]
    PALETTE_UPDATE 8, 0, 1, 6       ; [8] = [3, 1, 0]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [80]
    PALETTE_UPDATE 9, 0, 1, 2       ; [9] = [2, 0, 0]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [88]
    PALETTE_UPDATE 1, 0, 1, 11      ; [1] = [5, 3, 1]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [89]
    PALETTE_UPDATE 3, 1, 3, 8       ; [3] = [6, 4, 2]          -C2=2
    \\ ^-- REMOVE THE YELLOW FROM HERE?

    ; Brown trench
    PALETTE_FRAME_JUST_UPDATES 2                                        ; [90]
    ; Pure Yellow disappears
    PALETTE_UPDATE 2, 1, 3, 10      ; [2] = [7, 5, 3]          -C2=1
    PALETTE_UPDATE 4, 1, 3, 12      ; [4] = [7, 6, 4]          

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [98]
    PALETTE_UPDATE 5, 0, 1, 8       ; [5] = [4, 1, 0]          -C2=0

    PALETTE_FRAME %0010, PAL_black, PAL_red, PAL_cyan, PAL_white, 2   ; [143]
    ; ^== UPDATE ANY PALETTE ENTRY STILL CONTAINING COLOUR 2 ==v

    ; Grey appears
    ;PALETTE_UPDATE 4, 0, 3, 12      ; [4] = [5, 5, 4]
    ;PALETTE_UPDATE 5, 0, 3, 14      ; [5] = [6, 6, 5]
    PALETTE_UPDATE 6, 0, 2, 8       ; [6] = [4, 4, 3]
    PALETTE_UPDATE 10, 0, 3, 2      ; [10] = [1, 1, 0]

    ; First blue appears
    PALETTE_FRAME_JUST_UPDATES 2                                        ; [144]
    PALETTE_UPDATE 4, 0, 2, 6      ; [4] = [0, 3, 5]          
    PALETTE_UPDATE 5, 0, 2, 8      ; [5] = [1, 4, 6]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [145]
    PALETTE_UPDATE 10, 0, 2, 2      ; [10] = [0, 1, 3]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [151]
    PALETTE_UPDATE 2, 0, 2, 10      ; [2] = [2, 5, 7]

    PALETTE_FRAME_JUST_UPDATES 4                                        ; [152]
    PALETTE_UPDATE 6, 1, 3, 10      ; [6] = [7, 5, 3]
    PALETTE_UPDATE 11, 1, 3, 12     ; [11] = [7, 6, 4]
    PALETTE_UPDATE 12, 0, 3, 4      ; [12] = [2, 2, 2]
    PALETTE_UPDATE 13, 0, 3, 2      ; [13] = [1, 1, 1]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [154]
    PALETTE_UPDATE 2, 0, 3, 12      ; [2] = [6, 6, 6]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [155]
    PALETTE_UPDATE 10, 0, 3, 6      ; [10] = [3, 3, 3]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [161]
    PALETTE_UPDATE 13, 0, 2, 2      ; [13] = [0, 1, 3]

    PALETTE_FRAME_JUST_UPDATES 2                                        ; [162]
    PALETTE_UPDATE 14, 0, 3, 2      ; [14] = [1, 1, 1]
    PALETTE_UPDATE 15, 0, 2, 6      ; [15] = [0, 2, 4]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [166]
    PALETTE_UPDATE 5, 0, 3, 8      ; [5] = [4, 4, 4]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [167]
    PALETTE_UPDATE 2, 0, 2, 1      ; [2] = [0, 0, 2]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [171]
    PALETTE_UPDATE 9, 0, 3, 10     ; [9] = [5, 5, 5]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [173]
    PALETTE_UPDATE 15, 0, 3, 12    ; [15] = [6, 6, 6]

    PALETTE_FRAME_JUST_UPDATES 1                                        ; [176]
    PALETTE_UPDATE 8, 0, 2, 8    ; [8] = [1, 4, 6]

    ; At this frame there is no more red on the screen and we don't need yellow yet!
    PALETTE_FRAME_JUST_UPDATES 2                                        ; [178]
    PALETTE_UPDATE 3, 0, 2, 4    ; [3] = [0, 2, 4]
    PALETTE_UPDATE 6, 0, 2, 14   ; [6] = [2, 5, 7]

    ; Yellow motif appears again
    PALETTE_FRAME %0100, PAL_black, PAL_yellow, PAL_cyan, PAL_white, 1  ; [190]
    ; ^== UPDATE ANY PALETTE ENTRY STILL CONTAINING COLOUR 1 ==v
    PALETTE_UPDATE 1, 0, 1, 16      ; [1] = [7, 7, 7]

    PALETTE_FRAME_NO_CHANGE                                             ; [225]
    PALETTE_FRAME_NO_CHANGE                                             ; [229]
    PALETTE_FRAME_NO_CHANGE                                             ; [231]
    PALETTE_FRAME_NO_CHANGE                                             ; [232]
    PALETTE_FRAME_NO_CHANGE                                             ; [233]
    PALETTE_FRAME_NO_CHANGE                                             ; [235]
    PALETTE_FRAME_NO_CHANGE                                             ; [237]
    PALETTE_FRAME_NO_CHANGE                                             ; [238]
    PALETTE_FRAME_NO_CHANGE                                             ; [239]
    PALETTE_FRAME_NO_CHANGE                                             ; [249]
    PALETTE_FRAME_NO_CHANGE                                             ; [269]
    PALETTE_FRAME_NO_CHANGE                                             ; [272]
    PALETTE_FRAME_NO_CHANGE                                             ; [272]
    PALETTE_FRAME_NO_CHANGE                                             ; [274]
    PALETTE_FRAME_NO_CHANGE                                             ; [519]
    PALETTE_FRAME_NO_CHANGE                                             ; [520]
    PALETTE_FRAME_NO_CHANGE                                             ; [523]
    PALETTE_FRAME_NO_CHANGE                                             ; [676]
    PALETTE_FRAME_NO_CHANGE                                             ; [684]
    PALETTE_FRAME_NO_CHANGE                                             ; [685]
    PALETTE_FRAME_NO_CHANGE                                             ; [686]
    PALETTE_FRAME_NO_CHANGE                                             ; [693]
    PALETTE_FRAME_NO_CHANGE                                             ; [696]
    PALETTE_FRAME_NO_CHANGE                                             ; [699]
    PALETTE_FRAME_NO_CHANGE                                             ; [703]
    PALETTE_FRAME_NO_CHANGE                                             ; [705]
    PALETTE_FRAME_NO_CHANGE                                             ; [709]
    PALETTE_FRAME_NO_CHANGE                                             ; [715]
    PALETTE_FRAME_NO_CHANGE                                             ; [717]
    PALETTE_FRAME_NO_CHANGE                                             ; [718]
    PALETTE_FRAME_NO_CHANGE                                             ; [719]
    PALETTE_FRAME_NO_CHANGE                                             ; [721]
    PALETTE_FRAME_NO_CHANGE                                             ; [745]
    PALETTE_FRAME_NO_CHANGE                                             ; [804]
    PALETTE_FRAME_NO_CHANGE                                             ; [805]
    PALETTE_FRAME_NO_CHANGE                                             ; [806]
    PALETTE_FRAME_NO_CHANGE                                             ; [915]
    PALETTE_FRAME_NO_CHANGE                                             ; [916]
    PALETTE_FRAME_NO_CHANGE                                             ; [917]
    PALETTE_FRAME_NO_CHANGE                                             ; [918]
    PALETTE_FRAME_NO_CHANGE                                             ; [919]
    PALETTE_FRAME_NO_CHANGE                                             ; [927]
    PALETTE_FRAME_NO_CHANGE                                             ; [935]
    PALETTE_FRAME_NO_CHANGE                                             ; [1080]
    PALETTE_FRAME_NO_CHANGE                                             ; [1081]
    PALETTE_FRAME_NO_CHANGE                                             ; [1082]
    PALETTE_FRAME_NO_CHANGE                                             ; [1084]
    PALETTE_FRAME_NO_CHANGE                                             ; [1085]
    PALETTE_FRAME_NO_CHANGE                                             ; [1087]
    PALETTE_FRAME_NO_CHANGE                                             ; [1088]
    PALETTE_FRAME_NO_CHANGE                                             ; [1089]
    PALETTE_FRAME_NO_CHANGE                                             ; [1090]
    PALETTE_FRAME_NO_CHANGE                                             ; [1091]
    PALETTE_FRAME_NO_CHANGE                                             ; [1092]
    PALETTE_FRAME_NO_CHANGE                                             ; [1093]
    PALETTE_FRAME_NO_CHANGE                                             ; [1094]
    PALETTE_FRAME_NO_CHANGE                                             ; [1095]
    PALETTE_FRAME_NO_CHANGE                                             ; [1096]
    PALETTE_FRAME_NO_CHANGE                                             ; [1097]
    PALETTE_FRAME_NO_CHANGE                                             ; [1098]
    PALETTE_FRAME_NO_CHANGE                                             ; [1099]
    PALETTE_FRAME_NO_CHANGE                                             ; [1100]
    PALETTE_FRAME_NO_CHANGE                                             ; [1101]
    PALETTE_FRAME_NO_CHANGE                                             ; [1102]
    PALETTE_FRAME_NO_CHANGE                                             ; [1103]
    PALETTE_FRAME_NO_CHANGE                                             ; [1105]
    PALETTE_FRAME_NO_CHANGE                                             ; [1106]
    PALETTE_FRAME_NO_CHANGE                                             ; [1114]
    PALETTE_FRAME_NO_CHANGE                                             ; [1115]
    PALETTE_FRAME_NO_CHANGE                                             ; [1139]
    PALETTE_FRAME_NO_CHANGE                                             ; [1184]
    PALETTE_FRAME_NO_CHANGE                                             ; [1247]
    PALETTE_FRAME_NO_CHANGE                                             ; [1248]
    PALETTE_FRAME_NO_CHANGE                                             ; [1249]
    PALETTE_FRAME_NO_CHANGE                                             ; [1251]
    PALETTE_FRAME_NO_CHANGE                                             ; [1333]
    PALETTE_FRAME_NO_CHANGE                                             ; [1343]
    PALETTE_FRAME_NO_CHANGE                                             ; [1345]
    PALETTE_FRAME_NO_CHANGE                                             ; [1352]
    PALETTE_FRAME_NO_CHANGE                                             ; [1447]
    PALETTE_FRAME_NO_CHANGE                                             ; [1459]
    PALETTE_FRAME_NO_CHANGE                                             ; [1513]
    PALETTE_FRAME_NO_CHANGE                                             ; [1617]
    PALETTE_FRAME_NO_CHANGE                                             ; [1654]
}
.palette_stream_end
