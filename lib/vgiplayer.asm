; Vendored from vgm-player-bbc (commit f6356a1, lib/vgiplayer.asm); the only
; change is the labels on the v3 unrolled volume writes (vgi_vol0_ora ...).
;******************************************************************
; AI-GENERATED CODE
;------------------------------------------------------------------
; 6502 BBC Micro Incremental VGM (VGI) Music Player
; Generated with the assistance of an AI model: Claude Opus 4.8
; (claude-opus-4-8).
;
; A sibling of lib/vgcplayer.asm (by Simon Morris). It plays .vgi files
; produced by vgipacker.py (in the vgm-packer repo) and exposes the SAME
; user API as the VGC player:  vgm_init / vgm_update / sn_reset / sn_write.
;
; WHY A SECOND PLAYER
; -------------------
; The VGC player runs RLE+LZ4 per stream. Its decode cost SPIKES: most frames
; just decrement an RLE counter (cheap), occasional frames refill several LZ4
; tokens at once (expensive) - so the per-frame cost is bimodal with a long
; worst-case tail. The VGI player runs a tiny byte-aligned LZSS per register
; column with NO RLE pre-pass, decoding ONE value per stream per frame. A long
; match/run is emitted one byte at a time across successive frames, so the
; per-frame cost is bounded INDEPENDENTLY of match/run length - low, flat and
; predictable, which is what a raster-budgeted demo needs. The trade is size:
; .vgi is ~1.4x .vgc, and the workspace is 11 x 256 = 2.75 KB (vs 8 x 256).
; See docs/vgi-player.md for the analysis and the measured comparison.
;
; sn_write and sn_reset below are byte-identical to lib/vgcplayer.asm.
;
; BUILD FLAGS (both must be passed via -D on every build, like OPT in
; test/vgc_opt):
;   -D VGI_UNROLL=0  compact looped decoder  (default, smallest code)
;   -D VGI_UNROLL=1  per-stream unrolled decoder (a little faster, +code)
;   -D VGI_V3=0      play .vgi v2 files: 11 columns, an 11 x 256 workspace
;   -D VGI_V3=1      play .vgi v3 files: 8 columns and a period table per
;                    channel, so an 8 x 256 workspace - 23-29% smaller files
;                    AND three fewer streams to decode a frame
; All four combinations are byte-exact against the VGC player and all honour
; the buffer page passed to vgm_init. A build plays one format, not both: the
; version byte is checked at mount and a mismatched file is refused (the player
; reports finished rather than decoding noise).
;******************************************************************

.vgm_start

VGI_SKIP        = &0f       ; noise "skip" marker (unchanged frame, don't rewrite)

IF VGI_V3
VGI_NUM_STREAMS = 8         ; 3 period indices, noise, 4 volumes
VGI_VERSION     = 3
ELSE
VGI_NUM_STREAMS = 11        ; one per SN76489 register column
VGI_VERSION     = 2
ENDIF

;--------------------------------------------------
; user callable routines:
;  vgm_init()
;  vgm_update()
;  sn_reset()
;  sn_write()
;--------------------------------------------------

;-------------------------------------------
; vgm_init
;-------------------------------------------
; Initialise playback routine
;  A points to HI byte of a page aligned VGI_NUM_STREAMS x 256 RAM buffer
;  (2.75Kb for a v2 build, 2Kb for a v3 build)
;  X/Y point to the VGI data stream to be played
;  C=1 for looped playback
;-------------------------------------------
.vgm_init
{
    ; stash the buffer page (the ring windows live at A, A+1 .. A+n-1)
    sta vgm_buffers
    lda #0
    ror a  ; move carry into A bit7
    sta vgm_loop

    ; stash the data source addr for looping
    stx vgm_source+0
    sty vgm_source+1
    ; Prepare the data for streaming (passed in X/Y)
    jmp vgm_stream_mount
}

;-------------------------------------------
; vgm_update
;-------------------------------------------
;  call every 50Hz to play music
;  vgm_init must be called prior to this
;  returns non-zero when VGM is finished, zero while still playing.
;-------------------------------------------
.vgm_update
{
    lda vgm_finished
    bne finished_exit

    ; End-of-stream is tested at the START of the call (mirroring the VGC
    ; player, which sees its EOF marker before writing anything). So an update
    ; either plays one real frame and returns "still playing", or - when no
    ; frames remain - finishes/loops WITHOUT playing a frame. The last real
    ; frame is therefore never mixed with the silence reset below.
    lda vgm_framelo
    ora vgm_framehi
    beq at_end              ; no frames left -> finish or loop

    ; decode one byte from each of the 11 streams and write the SN76489
    jsr vgm_decode_frame

    ; 16-bit decrement of the frame counter (we played one frame)
    lda vgm_framelo
    bne dec_no_borrow
    dec vgm_framehi
.dec_no_borrow
    dec vgm_framelo

    lda #0                  ; zero = still playing
    rts

.at_end
    lda vgm_loop
    beq stop
    ; looping: re-mount the stream and play its first frame this call
    jsr vgm_stream_mount
    jmp vgm_update

.stop
    lda #&ff
    sta vgm_finished
    jmp sn_reset            ; silence the chip, also returns non-zero in A

.finished_exit
    lda #&ff
    rts
}


;-------------------------------------------
; Sound chip routines
;-------------------------------------------

; Write data to SN76489 sound chip
; A contains data to be written to sound chip
; clobbers X, A is non-zero on exit
; (byte-identical to lib/vgcplayer.asm sn_write)
.sn_write
{
    ldx #255
    stx &fe43
    sta &fe4f
    inx
    stx &fe40
    lda &fe40
    ora #8
    sta &fe40
    rts ; 21 bytes
}

; Reset SN76489 sound chip to a default (silent) state
; (byte-identical to lib/vgcplayer.asm sn_reset)
.sn_reset
{
	\\ Zero volume on all channels
	lda #&9f : jsr sn_write
	lda #&bf : jsr sn_write
	lda #&df : jsr sn_write
	lda #&ff : jmp sn_write
}


;-------------------------------------------
; VGM internal routines
; Not user callable.
;-------------------------------------------

IF VGI_V3
; Bias one 16-bit header pointer at (vgi_src),y by the load address and poke it
; into the operand of the absolute,Y load at `addr`. Y advances by 2.
MACRO VGI_MOUNT_TABLE addr
    lda (vgi_src),y
    clc
    adc vgm_source+0
    sta addr+1
    iny
    lda (vgi_src),y
    adc vgm_source+1
    sta addr+2
    iny
ENDMACRO
ENDIF

; Initialise the player for the in-memory VGI data stream at vgm_source.
; .vgi header layout (little-endian):
;   +0  'V','G','I',ver
;   +4  nframes (16-bit)
;   +6  VGI_NUM_STREAMS x stream offset (16-bit, relative to file start)
; and, for v3 only:
;   +22 6 x period table offset (16-bit): low then high table, per channel
; Each stream offset is biased by vgm_source to give an absolute read pointer,
; and each stream's decode state is zeroed. (Reused for looping.)
.vgm_stream_mount
{
    lda #0
    sta vgm_finished
    sta vgi_ring+0          ; ring window LO byte is always 0 (page aligned)

    ; point the zero-page vgi_src at the .vgi header for indirect reads
    ; (fetchbyte reloads vgi_src per byte, so reusing it here is safe)
    lda vgm_source+0
    sta vgi_src+0
    lda vgm_source+1
    sta vgi_src+1

    ; Refuse a file this build cannot play. Without this the decoder would
    ; happily read v2 columns as v3 ones and play convincing noise.
    ldy #3
    lda (vgi_src),y
    cmp #VGI_VERSION
    beq version_ok
    lda #&ff
    sta vgm_finished
    rts
.version_ok

    ; frame counter = nframes (header +4/+5)
    ldy #4
    lda (vgi_src),y
    sta vgm_framelo
    iny
    lda (vgi_src),y
    sta vgm_framehi

    ldx #0
.mount_loop
    ; Y = 6 + stream*2  (index of this stream's 16-bit offset)
    txa
    asl a
    clc
    adc #6
    tay
    ; absolute src ptr = vgm_source + offset
    lda (vgi_src),y         ; offset LO
    clc
    adc vgm_source+0
    sta st_srcL,x
    iny
    lda (vgi_src),y         ; offset HI
    adc vgm_source+1
    sta st_srcH,x

    ; clear decode state for this stream
    lda #0
    sta st_rem,x
    sta st_flag,x
    sta st_copy,x
    sta st_head,x

    inx
    cpx #VGI_NUM_STREAMS
    bne mount_loop

IF VGI_V3
    ; The six period-table pointers live at +22. Bias each by the load address
    ; and poke it straight into the operand of its table load, so a lookup in
    ; the frame loop is one absolute,Y read and nothing else.
    ldy #22
    VGI_MOUNT_TABLE v3_t0lo
    VGI_MOUNT_TABLE v3_t0hi
    VGI_MOUNT_TABLE v3_t1lo
    VGI_MOUNT_TABLE v3_t1hi
    VGI_MOUNT_TABLE v3_t2lo
    VGI_MOUNT_TABLE v3_t2hi
ENDIF
    rts
}

; Fetch one raw byte from stream X's compressed data, advancing its pointer.
; X = stream index (preserved), returns A = byte. Clobbers Y.
.fetchbyte
{
    lda st_srcL,x
    sta vgi_src+0
    lda st_srcH,x
    sta vgi_src+1
    ldy #0
    lda (vgi_src),y
    inc st_srcL,x
    bne done
    inc st_srcH,x
.done
    rts
}


IF VGI_UNROLL

;-------------------------------------------
; Unrolled decoder (-D VGI_UNROLL=TRUE)
;-------------------------------------------
; Each stream's common copy path is inlined with absolute (st_*+strm) state and
; no per-byte JSR/loop overhead; only the rare new-token parse and literal byte
; fetch use X. The ring page is held in vgi_ring+1 and INC-ed between streams
; (vgi_ring+0 stays 0), so the decoder is still fully relocatable to the buffer
; page passed in vgm_init.

; Parse the next token for stream X (called when st_rem[X] == 0). X preserved.
.newtoken
{
    jsr fetchbyte
    cmp #&80
    bcc lit                 ; 0xxxxxxx -> literal run
    cmp #&c0
    bcc run                 ; 10xxxxxx -> RUN (offset 1)
    ; 11xxxxxx -> MATCH (offset byte follows)
    and #&3f
    cmp #&3f
    bne m_short
    jsr fetchbyte           ; extended length (65..255)
    sta st_rem,x
    jmp m_off
.m_short
    clc : adc #2            ; len = field+2 (2..64)
    sta st_rem,x
.m_off
    jsr fetchbyte           ; offset
    sta vgi_tmp
    lda st_head,x
    sec : sbc vgi_tmp
    sta st_copy,x
    lda #&80 : sta st_flag,x
    rts
.run
    and #&3f
    cmp #&3f
    bne r_short
    jsr fetchbyte
    sta st_rem,x
    jmp r_set
.r_short
    clc : adc #2
    sta st_rem,x
.r_set
    lda st_head,x
    sec : sbc #1            ; offset 1 (repeat last byte), no offset byte read
    sta st_copy,x
    lda #&80 : sta st_flag,x
    rts
.lit
    and #&7f : clc : adc #1
    sta st_rem,x
    lda #0 : sta st_flag,x
    rts
}

; decode one byte from stream `strm` -> A (ring page in vgi_ring+1)
MACRO VGI_DECODE_U strm
{
    lda st_rem + strm
    bne have
    ldx #strm : jsr newtoken
.have
    lda st_flag + strm
    bpl lit
    ; match/run: copy from the ring window
    ldy st_copy + strm
    lda (vgi_ring),y
    inc st_copy + strm
    ldy st_head + strm
    sta (vgi_ring),y
    inc st_head + strm
    dec st_rem + strm
    jmp done
.lit
    ldx #strm : jsr fetchbyte
    ldy st_head + strm
    sta (vgi_ring),y
    inc st_head + strm
    dec st_rem + strm
.done                       ; A = decoded byte
}
ENDMACRO

IF VGI_V3

; one frame: decode 8 streams, expand the three period indices through their
; tables and write the SN76489 (unrolled). sn_write clobbers X but never Y, so
; the index lives in Y across the pair of writes it feeds.
.vgm_decode_frame
    lda vgm_buffers
    sta vgi_ring+1          ; ring page for stream 0 (vgi_ring+0 = 0 from mount)

    VGI_DECODE_U 0  : tay                       ; tone0 period index
.v3_t0lo
    lda &ffff,y : ora #&80 : jsr sn_write       ; latch + low 4 bits
.v3_t0hi
    lda &ffff,y :            jsr sn_write       ; high 6 bits
    inc vgi_ring+1
    VGI_DECODE_U 1  : tay                       ; tone1 period index
.v3_t1lo
    lda &ffff,y : ora #&a0 : jsr sn_write
.v3_t1hi
    lda &ffff,y :            jsr sn_write
    inc vgi_ring+1
    VGI_DECODE_U 2  : tay                       ; tone2 period index
.v3_t2lo
    lda &ffff,y : ora #&c0 : jsr sn_write
.v3_t2hi
    lda &ffff,y :            jsr sn_write
    inc vgi_ring+1
    VGI_DECODE_U 3  : cmp #VGI_SKIP : beq v3_nonoise
    ora #&e0 : jsr sn_write                     ; noise control (only if changed)
.v3_nonoise
    inc vgi_ring+1
    ; (stnicc-beeb: the volume ORs are labelled so src/music.asm can mute the
    ; player by setting their low nibbles to &F)
    VGI_DECODE_U 4
.vgi_vol0_ora
    ora #&90 : jsr sn_write                     ; vol0
    inc vgi_ring+1
    VGI_DECODE_U 5
.vgi_vol1_ora
    ora #&b0 : jsr sn_write                     ; vol1
    inc vgi_ring+1
    VGI_DECODE_U 6
.vgi_vol2_ora
    ora #&d0 : jsr sn_write                     ; vol2
    inc vgi_ring+1
    VGI_DECODE_U 7
.vgi_vol3_ora
    ora #&f0 : jsr sn_write                     ; vol3 (noise)
    rts

ELSE

; one frame: decode all 11 streams and write the SN76489 (unrolled)
.vgm_decode_frame
{
    lda vgm_buffers
    sta vgi_ring+1          ; ring page for stream 0 (vgi_ring+0 = 0 from mount)

    VGI_DECODE_U 0  : ora #&80 : jsr sn_write   ; tone0 freq lo (latch)
    inc vgi_ring+1
    VGI_DECODE_U 1  :            jsr sn_write   ; tone0 freq hi (data)
    inc vgi_ring+1
    VGI_DECODE_U 2  : ora #&a0 : jsr sn_write   ; tone1 freq lo
    inc vgi_ring+1
    VGI_DECODE_U 3  :            jsr sn_write   ; tone1 freq hi
    inc vgi_ring+1
    VGI_DECODE_U 4  : ora #&c0 : jsr sn_write   ; tone2 freq lo
    inc vgi_ring+1
    VGI_DECODE_U 5  :            jsr sn_write   ; tone2 freq hi
    inc vgi_ring+1
    VGI_DECODE_U 6  : cmp #VGI_SKIP : beq nonoise
    ora #&e0 : jsr sn_write                     ; noise control (only when changed)
.nonoise
    inc vgi_ring+1
    VGI_DECODE_U 7  : ora #&90 : jsr sn_write   ; vol0
    inc vgi_ring+1
    VGI_DECODE_U 8  : ora #&b0 : jsr sn_write   ; vol1
    inc vgi_ring+1
    VGI_DECODE_U 9  : ora #&d0 : jsr sn_write   ; vol2
    inc vgi_ring+1
    VGI_DECODE_U 10 : ora #&f0 : jsr sn_write   ; vol3 (noise)
    rts
}

ENDIF ; VGI_V3

ELSE

;-------------------------------------------
; Looped decoder (default, -D VGI_UNROLL=FALSE)
;-------------------------------------------

; decode one output byte from stream X (incremental). A = byte, X preserved.
; Worst case: fetch command (+ ext + offset) then one copy/literal - bounded,
; with no dependence on match length (a long match emits one byte per call).
.vgm_decode_stream
{
    lda st_rem,x
    bne produce             ; still inside a run/literal -> emit the next byte
    jsr fetchbyte           ; new command byte

    ; v2 tokens: 0=literal(7-bit), 10=RUN(off1,6-bit+ext), 11=MATCH(6-bit+ext,off)
    cmp #&80
    bcc lit
    cmp #&c0
    bcc run
    and #&3f                ; MATCH length field
    cmp #&3f
    bne m_short
    jsr fetchbyte           ; extended length (65..255)
    sta st_rem,x
    jmp m_off
.m_short
    clc : adc #2            ; len = field+2 (2..64)
    sta st_rem,x
.m_off
    jsr fetchbyte           ; offset
    sta vgi_tmp
    lda st_head,x
    sec : sbc vgi_tmp
    sta st_copy,x
    lda #&80 : sta st_flag,x
    jmp produce
.run
    and #&3f                ; RUN length field
    cmp #&3f
    bne r_short
    jsr fetchbyte
    sta st_rem,x
    jmp r_set
.r_short
    clc : adc #2
    sta st_rem,x
.r_set
    lda st_head,x
    sec : sbc #1            ; offset 1 (repeat last byte), no offset byte read
    sta st_copy,x
    lda #&80 : sta st_flag,x
    jmp produce
.lit
    and #&7f : clc : adc #1
    sta st_rem,x
    lda #0 : sta st_flag,x

.produce
    txa : clc : adc vgm_buffers : sta vgi_ring+1   ; ring page for this stream
    lda st_flag,x
    bmi dcopy
    jsr fetchbyte           ; literal byte -> A
    jmp dstore
.dcopy
    ldy st_copy,x
    lda (vgi_ring),y        ; A = copied byte
    inc st_copy,x
.dstore                     ; A = decoded byte (held in A through to the return)
    ldy st_head,x
    sta (vgi_ring),y
    inc st_head,x
    dec st_rem,x
    rts
}

IF VGI_V3

; one frame: decode 8 streams, then expand and write the SN76489 (looped)
.vgm_decode_frame
    ldx #0
.v3_loop
    jsr vgm_decode_stream
    sta regbuf,x
    inx
    cpx #VGI_NUM_STREAMS
    bne v3_loop

    ldy regbuf+0                                ; tone0 period index
.v3_t0lo
    lda &ffff,y : ora #&80 : jsr sn_write       ; latch + low 4 bits
.v3_t0hi
    lda &ffff,y :            jsr sn_write       ; high 6 bits
    ldy regbuf+1
.v3_t1lo
    lda &ffff,y : ora #&a0 : jsr sn_write
.v3_t1hi
    lda &ffff,y :            jsr sn_write
    ldy regbuf+2
.v3_t2lo
    lda &ffff,y : ora #&c0 : jsr sn_write
.v3_t2hi
    lda &ffff,y :            jsr sn_write
    lda regbuf+3 : cmp #VGI_SKIP : beq v3_nonoise
    ora #&e0 : jsr sn_write                     ; noise control (only if changed)
.v3_nonoise
    lda regbuf+4 : ora #&90 : jsr sn_write      ; vol0
    lda regbuf+5 : ora #&b0 : jsr sn_write      ; vol1
    lda regbuf+6 : ora #&d0 : jsr sn_write      ; vol2
    lda regbuf+7 : ora #&f0 : jsr sn_write      ; vol3 (noise)
    rts

ELSE

; one frame: decode all 11 streams, then write the SN76489 (looped)
.vgm_decode_frame
{
    ldx #0
.loop
    jsr vgm_decode_stream
    sta regbuf,x
    inx
    cpx #VGI_NUM_STREAMS
    bne loop

    lda regbuf+0  : ora #&80 : jsr sn_write   ; tone0 freq lo (latch)
    lda regbuf+1             : jsr sn_write   ; tone0 freq hi (data)
    lda regbuf+2  : ora #&a0 : jsr sn_write   ; tone1 freq lo
    lda regbuf+3            : jsr sn_write    ; tone1 freq hi
    lda regbuf+4  : ora #&c0 : jsr sn_write   ; tone2 freq lo
    lda regbuf+5            : jsr sn_write    ; tone2 freq hi
    lda regbuf+6  : cmp #VGI_SKIP : beq nonoise
    ora #&e0 : jsr sn_write                   ; noise control (only when changed)
.nonoise
    lda regbuf+7  : ora #&90 : jsr sn_write   ; vol0
    lda regbuf+8  : ora #&b0 : jsr sn_write   ; vol1
    lda regbuf+9  : ora #&d0 : jsr sn_write   ; vol2
    lda regbuf+10 : ora #&f0 : jsr sn_write   ; vol3 (noise)
    rts
}

ENDIF ; VGI_V3

ENDIF ; VGI_UNROLL


;-------------------------------------------
; local vgm workspace
;-------------------------------------------

.vgm_buffers  equb 0    ; HI byte of the 11x256 ring workspace
.vgm_finished equb 0    ; non-zero once the tune has ended (no looping)
.vgm_loop     equb 0    ; non-zero if the tune is to be looped
.vgm_source   equw 0    ; .vgi data address (for mount / looping)
.vgm_framelo  equb 0    ; 16-bit count of frames left to play
.vgm_framehi  equb 0
.vgi_tmp      equb 0    ; scratch (match offset)

; per-stream decode state (VGI_NUM_STREAMS of them)
.st_srcL  skip VGI_NUM_STREAMS   ; stream read ptr LO
.st_srcH  skip VGI_NUM_STREAMS   ; stream read ptr HI
.st_rem   skip VGI_NUM_STREAMS   ; bytes left in current run (0 => fetch new token)
.st_flag  skip VGI_NUM_STREAMS   ; bit7 set => match/run mode, clear => literal
.st_copy  skip VGI_NUM_STREAMS   ; ring read index while copying a match/run
.st_head  skip VGI_NUM_STREAMS   ; ring write index

IF VGI_UNROLL=0
.regbuf   skip VGI_NUM_STREAMS   ; this frame's decoded column values (looped)
ENDIF

.vgm_end
