# Plan: a recooked column stream for CPC (and NULA)

Status (2026-10-19, branch `column-stream`): built and working. `bin/colstream.py` writes
the stream, `stnicc-cols.asm` plays it from its own disc, `build/beeb-niccc-cpc.dsd`. All 900
frames match the CPC model byte for byte (also with `--slow-disc 700`): **117.8M render
cycles and 3,931 fields, against 249.0M and 7,424 for the polygon CPC player.** See
"What was built" at the end; the plan below is as written before it.


## The idea

CPC (and NULA) currently draw each frame from polygons: an edge stage, then the 2020
span plotter, with 1.24x overdraw. Instead, recook the stream offline into each
frame's **final 64x100 picture**, coded column by column. The player then writes every
screen byte exactly once, with no polygons, no overdraw and no clears.

This is the useful form of the "vertical XOR fill" suggestion. A real XOR fill only
gives the right colours when polygons don't overlap, and STNICCC relies on overdraw, so
the data would have to be recooked into the final image anyway. Once the data gives
absolute colours, storing runs directly (`sta abs,X`, 5 cycles a byte) beats an XOR
pass (`lda/eor/sta`, 9-10 cycles a byte, plus a cleared buffer of marks).

## Why it's worth it: where CPC's time goes now

Profile of the CPC build on branch `cpc-masked-clears` (243.3M render cycles, 7,276 fields):

| Stage | Cycles | Share |
|---|---|---|
| Edge stage (`_FAST_EDGES`, down and up walkers) | about 68M | 28% |
| Per-row set-up in the 2020 span plotter (about 653K rows, about 220 cycles each) | about 140M | 58% |
| Unrolled stores (the actual pixel writes) | 32M | 13% |

So most of the time goes on bookkeeping, not on writing pixels. Overdraw is only 1.24x
(about 3,950 bytes written per frame per scanline into a 3,200-byte screen), so simply
removing overdraw would gain little.

## Constraint: one disc of 400K

A double-sided disc is 400K in all. The recooked stream **cannot share a disc with
LOW's 344K polygon stream**, so CPC and NULA would ship on a disc of their own: loader,
intro, music, CPC and NULA executables and the column stream. LOW, MEDIUM and HIGH keep
the current disc.

Ways to share one disc with LOW were measured, and none fit:
- LOW's polygon stream plus the column stream: 344K + 198K.
- LOW recooked the same way (128-wide pixel columns) is 293K, so 293K + 198K = 491K.
- The 64-wide picture matches LOW's picture subsampled on 97% of pixels, but the
  differences come in about 181 separate runs per frame, too many to send cheaply as
  corrections.

## The coding (prototype: `tools/colstream/colcode.py`)

Measured on all 900 frames of the current stream (`data/scene1.half.7680.bin`):

| Coding | Size |
|---|---|
| Current polygon stream, for comparison | 344K |
| Plain vertical RLE of byte columns | 433K |
| RLE + LZ (zlib per frame, a ceiling for LZ4) | about 355K |
| Byte columns, each predicted from its left neighbour | 303K |
| **Pixel columns, each predicted from its left neighbour** | **198K (220 bytes per frame, largest 548)** |
| ...plus zlib / xz over the whole stream | 178K / 174K: not worth decoding on a 6502 |

Observations that led to it (`stats.py`, `colpred.py`):
- The final picture averages 295 vertical byte runs per frame, against 797 horizontal ones.
- Frame-to-frame prediction doesn't help: the camera moves every frame, and only 3.5%
  of columns repeat from frame n-2 (the buffer being drawn into).
- Column-to-column prediction is strong: 72% of runs continue from a run in the column
  to the left, and their change in slope (delta of delta) is -1, 0 or +1 82% of the time.
- In byte columns, most of the cost is the mixed two-colour bytes at edges. In pixel
  columns every run is a single colour, so a new run fits in one byte.

Format (the prototype round-trips all 900 frames exactly):
- Per frame: a flags byte, then the palette changes if any (a count, then 2 bytes per
  entry), then 64 pixel columns.
- Each column is a list of runs (colour 0-16, where 16 is the cleared background;
  end row; slope), coded against the column to its left.
- Codes are 2 bits each, four to a control byte, and a new control byte is read when
  the current one runs out (as LZ4 does with its tokens). Literal bytes are inline.
  - `00`/`01`/`10` K: the left column's next run carries on; end += slope + 0/+1/-1.
  - `11 00` Kx: the same, with the new end row in a literal byte.
  - `11 01` D: drop the left column's next run.
  - `11 10` I: a new run, in one byte: the colour (16 choices, since it can't match the
    colour just above) in the high nibble and the length in the low nibble (0 = a length
    byte follows). At the top of a column: a colour byte and a length byte.
  - `11 11` R: the next run carries on in a new colour (a colour byte), followed by a
    K-style code (`11` = an end row byte).
- A run that reaches row 100 ends the column, and the left column's remaining runs
  are dropped without codes.
- The encoder aligns each column with the one to its left using `difflib`. An optimal
  alignment might shrink it further.

## Rough 6502 cost (estimate, still to be modelled)

| Step | Cycles per frame |
|---|---|
| Decode about 387 pixel-column runs at about 40 each | about 15K |
| Merge each pair of pixel columns into byte runs; a mixed byte is the left pixel's bits OR the right pixel's (`cpc_colour_top`/`bottom` AND `%10101010` / `%01010101`) | about 7K |
| Store every byte of both scanlines once (6,400 x 5) | about 32K |
| Enter and leave the unrolled column code for each byte run, on each scanline | about 20K |
| **Total** | **about 75-80K** |

- That is about **70M render cycles against 243M today (-70%)**, about 2 fields per
  frame, so roughly 1,800-1,900 fields against 7,276.
- Disc floor: a parse-only CPC run takes 2,090 fields for 344K (about 8 KB/s), so 198K
  needs about 1,200 fields. Drawing, not the disc, would set the pace, so cheaper
  decoding matters more than further compression.
- RAM: the edge stage, span plotters, short-span routines and relocated walkers all go.
  The unrolled column code needs about 1.2K (100 rows x 3 bytes, for each scanline
  parity and each screen buffer). Each run is entered at its first row and left by
  patching an exit at its last row (the original opcode is always `sta abs,X`, so
  restoring it is cheap).
- CPC needs two passes per run, because its top and bottom lines hold different bytes
  (`sty abs,X` doesn't exist). NULA writes the same byte to both lines, so one pass of
  `sta top,X : sta bottom,X` pairs is enough.

## Steps when this resumes

1. **Cycle model in Python.** Model the 6502 decoder, merge and store loop with exact
   cycle counts over all 900 frames, to confirm the about 70M before writing 6502.
   Try trading bytes for decode speed where it pays (the disc has room to spare).
2. **Tidy the encoder.** Move `colcode.py` into `bin/` as a real stream writer: no frame
   crossing a 7680-byte block (as `encoder.py -a 7680` does now), the palette in the
   form `cpc_palette_update` and NULA's palette code expect, and a verify step against
   `tools/cpc_palette.py`'s model screens.
3. **Disc layout.** A new disc build: loader, intro, music, CPC and NULA, plus the
   stream from the earliest free track. Update `src/exe_sizes.h.asm` and the
   `DISK*_first_track` constants. The intro's C key and the NuLA detection pick the
   player.
4. **6502 player.** Replace parse and plot in the CPC/NULA build with a column decoder
   and the unrolled column store. Streaming (`load_next_track`, `stream_check_ahead`)
   stays on DFS OSWORD &7F.
5. **Check.** `run.mjs --no-inject --variant cpc --dump dir`, then
   `tools/cpc_check.py dir` (every frame byte-identical to the model); NULA against
   `baseline-nula.json`; and also with `--slow-disc 700`.

## Prototype scripts (`tools/colstream/`)

Run from the repo root.

- `pictures.py pics.pkl`: caches each frame's final 64x100 picture (run it first).
- `colcode.py pics.pkl out.bin`: the pixel-column coder, with the round-trip check.
- `stats.py pics.pkl`: run counts, column coherence, inter-frame change, zlib for reference.
- `colpred.py pics.pkl`: how well the left column predicts each column.
- `lowcode.py tools/colstream`: LOW's 128-wide picture through the same coder.
- `derive.py`: how close the 64-wide picture is to LOW's picture subsampled.

## What was built

- **Byte columns, not pixel columns.** Coding each byte column directly (its runs' values
  are pixel pairs, left colour << 4 | right, colour 0 standing for the background) saves
  merging two pixel columns per byte at run time. The stream is 305,235 bytes, which fits
  a disc of its own from track 20 (`src/cols_disc.h.asm`). Codes as in the prototype; a
  new run's length is a 2-bit code (1-3 rows) or a byte.
- **Dithered colours need three store passes.** A colour that is three of one pen and one
  of the other swaps its bottom line's pixels on odd rows, so a long run of it is stored
  as every row's top line, then the even rows' bottoms, then the odd rows' (two passes for
  a chequer). 78% of runs are dithered.
- **The fill.** Runs of up to `SHORT_RUN` (4) rows are stored through a pointer; longer
  ones through unrolled `sta abs,X` code, entered at the first row and stopped by an RTS
  put over the store after the last. Each run is stored as soon as it is decoded.
- **Measured at 129.4M (per frame, about 144K cycles),** before the second optimisation pass
  (escape codes by branch, X-indexed short runs, per-screen long-run passes, incremental
  pair tables: 117.8M): unrolled stores 31K, entering and leaving
  passes 30K, short runs 16K, decoding about 35K, per-run set-up (`store_run`) 15K.
- **Further ideas, each worth a few K a frame:** per-screen slot tables in place of
  `ora code_or`; a row-ordered store sequence (top, bottom, top, bottom, reloading the
  bytes from zero page) for medium runs, one pass at 14.5 cycles a row; cheaper short
  runs. A byte-per-run format decodes faster but comes to 378K, more than a disc holds.
- **Disc and front end.** `stnicc-cols-build.asm` and `src/part-2-cols.asm` build the disc
  (make.bat); the intro is built with `-D _CPC_ONLY` (N and C do nothing) and
  `src/loader-cols.bas` sets &7E for /CPC. `run.mjs --variant cols` plays it; check with
  `tools/cpc_check.py` as for the CPC player.

## Next: all 1800 frames (planned 2026-10-19, not started)

The stream keeps every other frame of the original (`encoder.py -f 2`). Playing all 1800
looks feasible on one disc, but only with a richer code. Modelled in Python
(`tools/colstream/frames1800.py`, `v2model.py`); nothing built yet.

| Coding, all 1800 frames | Size |
|---|---|
| Today's code (left-column prediction only) | 633K |
| Best per column of left column or previous frame | 452K |
| ...plus each edge's motion from frame to frame | 429K |
| ...plus variable-length codes | 387K |
| ...plus predicting new runs' pixels from their neighbours (estimate) | about 355K |

A disc holds about 368K from track 14, so the margin is about 13K before palettes and
block padding.

- **Modes per column:** predict from the column to the left (as now), from the same
  column in the previous frame, or from it plus each run's motion (its end less the end
  of the run it came from in the frame before). At full rate 57% of columns are cheaper
  from the previous frame. Every run keeps a spatial and a temporal slope, each known
  only when it was coded that way (else 0); the encoder mirrors that exactly.
- **Variable-length codes:** carry on as predicted 37% of ops (1 bit); +1, -1 and a
  bigger jump 3 bits, then the jump as sign and size (2-9 in 3 bits, else an escape);
  new run, drop and recolour 4-5 bits; new runs' lengths 1, 2-3, 4-7 rows or a byte.
  Mode: 1 bit for left, 2 for the others.
- **New runs' values (81K of them, 81K bytes as now):** the left pixel is the left
  column's right pixel at that row 67% of the time (one of a few neighbours 87%); the
  right pixel is the previous frame's right column's left pixel 50% (81% any). Coding
  each pixel as a hit on a ranked neighbour or 4 bits costs about 5 bits, not 8.
  Move-to-front on values does not work (34% misses).
- **The player:** bit-level decoding, three modes, and the neighbour lookups for new
  runs; it must keep every column's runs from the last frame (at most 525 runs, 35 in a
  column: about 3.2K for two frames' lists, against about 2.75K free plus today's 768).
  About 150K cycles a frame: about 270M render cycles and 5,500-6,000 fields for 1800
  frames (the 900-frame column player: 4,243).
- **Also:** `cpc_colour_map.py` and the CPC model for the 1800-frame sequence.
- **Order:** the encoder first, to confirm it fits, then the decoder, then disc and checks.
