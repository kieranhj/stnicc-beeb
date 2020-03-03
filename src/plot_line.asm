\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	LINE DRAW FUNCTIONS
\ ******************************************************************

IF _PLOT_WIREFRAME
.plot_poly_line
{
    ; X=poly_num_verts

    \\ Duplicate first vertex to end
    lda poly_verts_x
    sta poly_verts_x, X
    lda poly_verts_y
    sta poly_verts_y, X

    \\ 'Draw' lines into our span buffer
    dex
    .line_loop
    stx poly_index              ; 3c

    lda poly_verts_x+1, X       ; 4c
    sta endx                    ; 3c
    lda poly_verts_y+1, X       ; 4c
    sta endy                    ; 4c

    ; starty
    lda poly_verts_y, X         ; 4c
	sta starty

    lda poly_verts_x, X         ; 4c
    sta startx

	jsr drawline

    ldx poly_index              ; 3c
    dex                         ; 2c
    bpl line_loop               ; 3c
	rts
}

\\ Technically this is a debug feature!
.drawline
{
	; calc screen address of (startx, starty)
	LDA startx
	AND #&FC        ; MODE4=&F8
    ASL A           ; not MODE4
	STA scrn
	LDA starty
	LSR A
	LSR A
	LSR A
	SEC
	ADC draw_buffer_HI
	STA scrn+1
	
	; calc screen row of starty
	LDA starty
	AND #7
	TAY
	
	; calc pixel within byte of startx
	LDA startx
	AND #3          ; MODE4=7
	TAX
	
	; calc dx = ABS(startx - endx)
	SEC
	LDA startx
	SBC endx
	BCS posdx
	EOR #255
	ADC #1
	.posdx
	STA dx
	
	; C=0 if dir of startx -> endx is positive, otherwise C=1
	PHP
	
	; calc dy = ABS(starty - endy)
	SEC
	LDA starty
	SBC endy
	BCS posdy
	EOR #255
	ADC #1
	.posdy
	STA dy
	
	; C=0 if dir of starty -> endy is positive, otherwise C=1
	PHP
	
	; Coincident start and end points exit early
	ORA dx
	BEQ exit_early
	
	; determine which type of line it is
	LDA dy
	CMP dx
	BCC shallowline
		
.steepline

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	LDA #goingdown-branchupdown-2
	BCC P%+4
	LDA #goingup-branchupdown-2
	STA branchupdown+1
	
	PLP
	LDA #goingright-branchleftright-2
	BCC P%+4
	LDA #goingleft-branchleftright-2
	STA branchleftright+1

	; initialise accumulator for 'steep' line
	LDA dy
	STA count
	LSR A

.steeplineloop

	STA accum
	
	; plot pixel
	LDA (scrn),Y
	ORA pixels_mode5,X      ; MODE4 EOR
	STA (scrn),Y
	
	; check if done
	DEC count
	.branchupdown
	BNE P%			; self-modified to goingdown or goingup
	.exitline
	RTS

    .exit_early
    PLP:PLP:RTS
	
	; move up to next line
	.goingup
	DEY
	BPL movetonextcolumn
	DEC scrn+1
	LDY #7
	BNE movetonextcolumn	; always taken
	
	; move down to next line
	.goingdown
	INY
	CPY #8
	BCC movetonextcolumn
	INC scrn+1
	LDY #0
	
	; check move to next pixel column
	.movetonextcolumn
	SEC
	LDA accum
	SBC dx
	BCS steeplineloop
	ADC dy
	
	.branchleftright
	BCS P%					; self-modified to goingright or goingleft
	
	; move left to next pixel column
	.goingleft
	DEX
	BPL steeplineloop
	STA accum
	LDA scrn
	SBC #8					; C set
	STA scrn
	LDX #3                  ; MODE4=7
	BNE steeplineloop+2		; always taken
	
	; move right to next pixel column
	.goingright
	INX
	CPX #4                  ; MODE4=8
	BCC steeplineloop
	STA accum
	LDA scrn
	ADC #7
	STA scrn
	LDX #0
	BEQ steeplineloop+2		; always taken
	
.shallowline

	; self-modify code so that line progresses according to direction remembered earlier
	PLP
	LDA #goingdown2-branchupdown2-2
	BCC P%+4
	LDA #goingup2-branchupdown2-2
	STA branchupdown2+1
	
	PLP
	LDA #goingright2-branchleftright2-2
	BCC P%+4
	LDA #goingleft2-branchleftright2-2
	STA branchleftright2+1

	; initialise accumulator for 'steep' line
	LDA dx
	STA count
	LSR A
	STA accum	

.shallowlineloop

	; cache byte from destination screen address
	LDA (scrn),Y
	
.shallowlineloop2

	; plot pixel in cached byte
	ORA pixels_mode5,X      ; MODE4 EOR
	
	; check if done
	DEC count
	.branchleftright2
	BNE P%					; self-modified to goingleft2 or goingright2
	STA (scrn),Y
	.exitline2
	RTS
	
	; move left to next pixel column
	.goingleft2
	DEX
	BPL movetonextline
	STA (scrn),Y			; store cached byte, advance screen address, and cache new byte
	LDA scrn
	SEC
	SBC #8
	STA scrn
	LDA (scrn),Y
	LDX #3                  ; MODE4=7
	BNE movetonextline		; always taken
	
	; move right to next pixel column
	.goingright2
	INX
	CPX #4                  ; MODE4=8
	BCC movetonextline
	STA (scrn),Y
	LDA scrn
	ADC #7
	STA scrn
	LDA (scrn),Y
	LDX #0
	
	; check whether we move to the next line
	.movetonextline
	STA temp
	SEC
	LDA accum
	SBC dy
	.branchupdown2
	BCC P%					; self-modified to goingdown2 or goingup2
	STA accum
	LDA temp
	BCS shallowlineloop2	; always taken
	
	; move down to next line
	.goingdown2
	ADC dx
	STA accum				; store new accumulator
	LDA temp
	STA (scrn),Y			; and store cached screen byte before moving to a new one
	INY
	CPY #8
	BCC shallowlineloop
	INC scrn+1
	LDY #0
	BEQ shallowlineloop		; always taken
	
	; move up to next line
	.goingup2
	ADC dx
	STA accum
	LDA temp
	STA (scrn),Y
	DEY
	BPL shallowlineloop
	DEC scrn+1
	LDY #7
	BNE shallowlineloop		; always taken
			
.pixels_mode4
	EQUB 128, 64, 32, 16, 8, 4, 2, 1
.pixels_mode5
    EQUB &88,&44,&22,&11
}
ENDIF
