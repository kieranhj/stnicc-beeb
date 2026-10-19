\ -*- mode:beebasm -*-

if _CPC
\\ A palette change: the colours whose CPC colour changed, from the palette
\\ stream (data/cpc_palette.bin, after its table of the 27 colours' byte pairs):
\\ count, then (colour, CPC colour x 2). Y = 0 for GET_PAL_BYTE.
.cpc_palette_update
{
	GET_PAL_BYTE
	beq done
	sta cpc_count
	.loop
	GET_PAL_BYTE
	sta cpc_index
	GET_PAL_BYTE
	tax
	lda cpc_colours + 1, X      ; bottom line
	pha
	lda cpc_colours, X          ; top line
	ldx cpc_index
	sta cpc_colour_top, X
	pla
	sta cpc_colour_bottom, X
	dec cpc_count
	bne loop
	.done
	rts
}
endif
