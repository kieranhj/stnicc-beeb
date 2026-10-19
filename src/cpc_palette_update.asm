\ -*- mode:beebasm -*-

if _CPC
\\ A palette change: the colours whose CPC colour changed, from the palette
\\ stream (data/cpc_palette.bin): count, then (colour, top line byte, bottom
\\ line byte). Bit 7 of the colour marks a dithered one (three of one pen, one
\\ of the other), whose bottom line has its two pixels swapped on odd rows:
\\ cpc_colour_delta is what to EOR with the bottom byte for that (else 0).
\\ Y = 0 for GET_PAL_BYTE.
.cpc_palette_update
{
	GET_PAL_BYTE
	beq done
	sta cpc_count
	.loop
	GET_PAL_BYTE
	pha
	and #15
	tax
	GET_PAL_BYTE
	sta cpc_colour_top, X
	GET_PAL_BYTE
	sta cpc_colour_bottom, X
	lsr a                       ; (left EOR right) in each right pixel's bits
	eor cpc_colour_bottom, X
	and #%01010101
	sta cpc_colour_delta, X
	asl a                       ; and in the left pixel's
	ora cpc_colour_delta, X
	sta cpc_colour_delta, X
	pla
	bmi dithered
	lda #0
	sta cpc_colour_delta, X
	.dithered
	dec cpc_count
	bne loop
	.done
	rts
}
endif
