\ -*- mode:beebasm -*-
\ ******************************************************************
\ *	STREAM HELPERS (called from parse_frame and irq_handler)
\ ******************************************************************

\\ In NULA and CPC this runs from the span buffer's spare rows (see the
\\ relocated blocks in stnicc-beeb.asm); elsewhere from main RAM.

\\ A renderer faster than the disc must not start a frame on data not yet
\\ loaded: lock the decoder unless STREAM_pages_ahead pages are loaded.
\\ (A subroutine, not inline in irq_handler: NULA has only a few bytes to
\\ spare before plot_poly.asm's page-aligned code and below its stream buffer.)
.stream_check_ahead
{
	lda track_no
	bmi enough                  ; nothing more to load

	\\ Pages loaded ahead of the reader, less one: load_to - ptr - 1 round the
	\\ ring (ptr = load_to is a full ring, 29 here)
	lda load_to_HI
	clc
	sbc STREAM_ptr_HI
	bcs no_wrap
	adc #HI(STREAM_buffer_size)
	.no_wrap
	cmp #STREAM_pages_ahead - 1
	bcs enough
	lda #&ff:sta buffer_lock
	.enough
	rts
}
