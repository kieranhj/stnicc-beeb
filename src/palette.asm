\ ******************************************************************
\ *	PALETTE DATA - MUST BE PAGE ALIGNED DUE TO SMC
\ ******************************************************************

MACRO DITHER4 colour1, colour2, dither

\\ 4x4 ordered dithering patterns [0-16]
IF dither=0
    mask1=&00:mask2=&00:mask3=&00:mask4=&00
ELIF dither=1
    mask1=&88:mask2=&00:mask3=&00:mask4=&00
ELIF dither=2
    mask1=&88:mask2=&00:mask3=&22:mask4=&00
ELIF dither=3
    mask1=&AA:mask2=&00:mask3=&22:mask4=&00
ELIF dither=4
    mask1=&AA:mask2=&00:mask3=&AA:mask4=&00
ELIF dither=5
    mask1=&AA:mask2=&44:mask3=&AA:mask4=&00
ELIF dither=6
    mask1=&AA:mask2=&44:mask3=&AA:mask4=&11
ELIF dither=7
    mask1=&AA:mask2=&55:mask3=&AA:mask4=&11
ELIF dither=8
    mask1=&AA:mask2=&55:mask3=&AA:mask4=&55
ELIF dither=9
    mask1=&EE:mask2=&55:mask3=&AA:mask4=&55
ELIF dither=10
    mask1=&EE:mask2=&55:mask3=&BB:mask4=&55
ELIF dither=11
    mask1=&FF:mask2=&55:mask3=&BB:mask4=&55
ELIF dither=12
    mask1=&FF:mask2=&55:mask3=&FF:mask4=&55
ELIF dither=13
    mask1=&FF:mask2=&DD:mask3=&FF:mask4=&55
ELIF dither=14
    mask1=&FF:mask2=&DD:mask3=&FF:mask4=&77
ELIF dither=15
    mask1=&FF:mask2=&FF:mask3=&FF:mask4=&77
ELSE
    mask1=&FF:mask2=&FF:mask3=&FF:mask4=&FF
ENDIF

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

    PRINT "Colour1=",colour1,"Colour2=",colour2,"Dither=",dither

    EQUB (byte2 AND mask1) OR (byte1 AND (mask1 EOR &FF))
    EQUB (byte2 AND mask2) OR (byte1 AND (mask2 EOR &FF))
    EQUB (byte2 AND mask3) OR (byte1 AND (mask3 EOR &FF))
    EQUB (byte2 AND mask4) OR (byte1 AND (mask4 EOR &FF))

ENDMACRO

PAGE_ALIGN  ; lazy
.reloc_poly_palette
IF 0
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
ELSE
; 0=black
; 1=red
; 2=yellow
; 3=white
DITHER4 0, 0, 0         ; [0] = [0, 0, 0]
DITHER4 0, 1, 10        ; [1] = [5, 2, 0]
DITHER4 0, 2, 16        ; [2] = [7, 7, 0]
DITHER4 1, 2, 4         ; [3] = [6, 3, 0]
DITHER4 0, 1, 8         ; [4] = [4, 1, 0]
DITHER4 1, 2, 8         ; [5] = [7, 5, 0]
DITHER4 0, 1, 12        ; [6] = [7, 4, 0]
DITHER4 0, 1, 4         ; [7] = [3, 0, 0]
DITHER4 0, 3, 16        ; [8] = [7, 7, 7]

DITHER4 0, 1, 2         ; [9] = [2, 0, 0]

DITHER4 1, 2, 2         ; [10] = [1, 1, 0]
DITHER4 2, 3, 8         ; [11] = [7, 6, 4]
DITHER4 0, 3, 4         ; [12] = [2, 2, 2]
DITHER4 0, 3, 2         ; [13] = [1, 1, 1]
DITHER4 0, 3, 1         ; [14] = [1, 1, 1]
DITHER4 0, 1, 6         ; [15] = [0, 2, 4]
ENDIF
