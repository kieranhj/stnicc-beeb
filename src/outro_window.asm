\\ -*- mode:beebasm -*-
\\ ******************************************************************
\\ *	THE OUTRO'S WINDOW CLEAR (src/outro.asm with _COLS)
\\ ******************************************************************

\\ Assembled to run at WINDOW_CODE, in the text screen's memory below the rows
\\ it shows, and copied there by main. (The name avoids BeebAsm's CLEAR.)

\\ Blank the window in one screen to the box: its cells' bytes on lines 8 to 107
\\ (character rows 1 to 13), the left and right edges set, then the top and
\\ bottom lines
MACRO BLANK_WINDOW scr
	lda #&80
	ldx #7
	.left
	FOR r, 1, 13
	sta scr + r * MODE4_ROW_BYTES + WIREFRAME_CORNER_X, X
	NEXT
	dex
	bpl left
	lda #0
	ldx #119
	.middle
	FOR r, 1, 13
	sta scr + r * MODE4_ROW_BYTES + WIREFRAME_CORNER_X, X
	NEXT
	dex
	cpx #7
	bne middle
	lda #&01
	ldx #127
	.right
	FOR r, 1, 13
	sta scr + r * MODE4_ROW_BYTES + WIREFRAME_CORNER_X, X
	NEXT
	dex
	cpx #119
	bne right
	lda #0                      ; character row 13's last four lines are below the box
	FOR l, 4, 7
	sta scr + 13 * MODE4_ROW_BYTES + WIREFRAME_CORNER_X + l
	sta scr + 13 * MODE4_ROW_BYTES + WIREFRAME_CORNER_X + 120 + l
	NEXT
	ldx #120
	.lines
	lda #&FF
	sta scr + 1 * MODE4_ROW_BYTES + WIREFRAME_CORNER_X, X
	sta scr + 13 * MODE4_ROW_BYTES + WIREFRAME_CORNER_X + 3, X
	txa
	sec
	sbc #8
	tax
	bpl lines
	rts
ENDMACRO

\\ Blank the window in the draw buffer
.clear_window
{
	lda draw_buffer_HI
	cmp #HI(screen1_addr)
	bne clear2
	jmp clear1
	.clear2
	BLANK_WINDOW screen2_addr
	.clear1
	BLANK_WINDOW screen1_addr
}
