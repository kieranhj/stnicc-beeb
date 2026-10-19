\\ -*- mode:beebasm -*-
\\ The outro's credits (src/outro.asm), typed by process_text: text, with
\\ 1 n (wait n frames), 8 (back), 12 (clear), 13 (new line), 31 x y (move) and 0 (the end)
; Page 0
;    |---------------------|
EQUS 31, 0, 0, 1, 150
EQUS "BBC Computer 32K"
EQUS 13, 13
EQUS "Acorn DFS"
EQUS 13, 13
EQUS "BASIC"
EQUS 13, 13
EQUS ">", 1, 100
EQUS 31, 5, 4, 8, 8, 8, 8, 8, 1, 50
EQUS "BITSHIFTERS"
EQUS 31, 1, 6, 1, 150, 12

; Page 1
;    |--------------------|
EQUS "ST-NICCC 2000", 1, 50
EQUS 8, 8, 8, 8, "BEEB", 1, 50, 13, 13
EQUS ">", 1, 25
EQUS "2MHz 6502 CPU", 1, 50, 13
EQUS ">", 1, 25
EQUS "32K RAM + 16K SWRAM"
EQUS ">", 1, 50
EQUS "5", QUARTER_CODE, '"'," 400K floppy", 1, 50, 13
EQUS ">", 1, 25
EQUS "No VIC, no Blitter", 1, 50, 13
EQUS ">", 1, 25
.just_3bit_colour_begin:EQUS "Just 3-bit colour!":.just_3bit_colour_end:EQUS 1, 50, 13
EQUS ">", 1, 25
EQUS "SN76489 sound chip", 1, 150, 12

; Page 2
;    |--------------------|
EQUS "Sorry we are a year", 13
EQUS "late to the STNICCC", 13
EQUS "patarty", 1, 25
EQUS 8, 8, 8, 8, 8, "rty.", 1, 50
EQUS 13, 13
EQUS "Squeezing a 16-bit", 13
EQUS "1 Mb demo into", 13
EQUS "8-bits and 48K took", 13
EQUS "us some time...", 1, 50
EQUS "!", 1, 50, 12

; Page 2
;    |--------------------|
EQUS "Original ST polygon", 13
EQUS "stream data repacked"
EQUS "with half the bits.", 13
EQUS ">> 320K vs 625K <<", 1, 100, 13
EQUS 13
EQUS "2MHz 6502", 13
EQUS 31,5,6,"vs 8MHz 68000", 1, 50
EQUS "!", 1, 50, 12

; Page 3
;    |--------------------|
EQUS "CREDITS", 1, 50, 13, 13
EQUS "Code:       kieranhj", 1, 50
EQUS "Code:     Tom Seddon", 1, 50
EQUS "Code:         Henley", 1, 50
EQUS "Logo by:    Dethmunk"
EQUS 1, 100, 12

; Page 4
;    |--------------------|
EQUS "MUSIC", 1, 50, 13, 13
EQUS "All music composed",13
EQUS "in Arkos Tracker 2",13
EQUS "by Rhino / Torment", 1, 75
EQUS 13, 13
EQUS "Checknobankh by"
EQUS 31,2,7,"Laxity / Kefrens"
EQUS 1, 100, 12

; Page 5
;    |--------------------|
EQUS "BITSHIFTERS THANKS", 1, 50, 13, 13
EQUS "Matt Godbolt.....RTW"
EQUS "scarybeasts....StewB"
EQUS "The Master....Tricky"
EQUS "0xC0DE....VectorEyes"
EQUS "HEx.....HorsenBurger"
EQUS 1, 100, 12

; Page 6
;    |--------------------|
EQUS "BITSHIFTERS GREETZ", 1, 50, 13, 13
EQUS ">", 1, 25
EQUS "Ate Bit", 1, 25, 13
EQUS ">", 1, 25
EQUS "CRTC", 1, 25, 13
EQUS ">", 1, 25
EQUS "DESiRE", 1, 25, 13
EQUS ">", 1, 25
EQUS "HOOY-PROGRAM", 1, 25, 13
EQUS ">", 1, 25
EQUS "Inverse Phase", 1, 25, 13
EQUS ">", 1, 25
EQUS "Logicoma", 1, 25
EQUS 12

; Page 7
; Greets continued
;    |--------------------|
EQUS "BITSHIFTERS GREETZ", 1, 50, 13, 13
EQUS ">", 1, 25
EQUS "Oxygene", 1, 25, 13
EQUS ">", 1, 25
EQUS "Polarity", 1, 25, 13
EQUS ">", 1, 25
EQUS "RiFT", 1, 25, 13
EQUS ">", 1, 25
EQUS "Slipstream", 1, 25, 13
EQUS ">", 1, 25
EQUS "Torment", 1, 25, 13
EQUS ">", 1, 25
EQUS "YM Rockerz", 1, 25
EQUS 12

; Page 8
; BASIC
;    |--------------------|
EQUS 1, 100
EQUS "BASIC", 13, 13
EQUS ">", 1, 75
EQUS "10 CLS", 13
EQUS ">", 1, 25
EQUS "20 PRINT ", '"', "THANKS FOR WATCHING!", '"', 13
EQUS ">", 1, 25
EQUS "30 GOTO 20", 13
EQUS ">", 1, 50
EQUS "RUN", 13
EQUS 1, 50, 12

; Page 9
; BASIC
;    |--------------------|
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING!"
EQUS "THANKS FOR WATCHING"
EQUS 1, 50, 12

; Page 10
; Bitshifters Flux
;    |--------------------|
EQUS 1, 50
EQUS 31,7,0,32,32,32,128,128,128,1,50
EQUS 31,7,1,32,32,32,128,128,128,1,50
EQUS 31,7,2,128,128,128,128,128,128,1,50
EQUS 31,7,3,128,128,128,128,128,128,1,50
EQUS 31,7,4,128,128,128,128,128,128,1,50
EQUS 31,7,5,128,128,128,32,32,32,1,50
EQUS 31,7,6,128,128,128,32,32,32
EQUS 31,19,7
EQUS 1, 255, 1, 255, 12

; Time for 9 pages max!
EQUS 0
