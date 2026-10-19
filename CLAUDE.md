# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

BEEB-NICCC: a BBC Micro (6502) recreation of the Atari STe STNICCC-2000 demo (Bitshifters, Revision 2020). It plays back the original `scene1.bin` polygon stream (1800 frames of flat-shaded polygons + palette changes) by streaming re-encoded data off double-sided floppy discs while rasterising each frame. All runtime code is 6502 assembly for BeebAsm; the Python scripts in `bin/` are offline tools. There are no unit tests or linters. Changes to the LOW player are checked with the render harness (below); otherwise build the disc and run it in an emulator (jsbeeb, via its MCP server, or b2).

## Render harness (the test for LOW)

The `fast-middle` branch optimises the LOW middle section (`_FAST_POLY`). The requirement: every frame pixel-identical to the 2020 release, with as few render cycles as possible, on a BBC B with the 8271 (DFS 1.2).

```
node tools/harness/run.mjs --ref tools/harness/baseline.json   # ~85 s: PASS/FAIL, render cycles, fields
```

- It assembles LOW, boots `build/beeb-niccc.dsd` on jsbeeb (the copy `npx jsbeeb-mcp` keeps in the npx cache; `JSBEEB` overrides), clears the NuLA flags so the intro chains to /LOW, then swaps the fresh LOW into memory. For each of the 900 frames it hashes the drawn buffer plus the ULA palette at `swap_frame_buffers` and counts render cycles from `parse_frame` to its return, leaving out interrupt handlers. `tools/harness/baseline.json` is the release LOW: 297,213,266 render cycles, 8,804 fields.
- `--no-inject` runs the LOW that is on the disc (build it with `make.bat` first): the check for a full disc build. `--profile f.txt` gives cycles per label and per instruction (`tools/profile_stages.py` groups them by stage). `--dump dir` saves each frame. `--spans f.json` saves each polygon's span buffer. `--parse-only` builds with `_TEST_PARSE_ONLY` (draws nothing) to stress the streaming. `--stream-log f.json` records every track read (with the 8271's seek and settle timers) for `tools/stream_margin.py`, which shows how close each read came to starving the decoder. `--timeline` splits the wall-clock time by what the machine is doing.
- It disables jsbeeb 2.9's 8271 automatic head unload. Otherwise the first read of disc side 2 fails with &10 (drive not ready) around frame 353. This is a jsbeeb regression: real hardware and older jsbeeb are fine, and an interactive jsbeeb MCP session still hits it.
- Render cycles move by about 0.1% when interrupts land differently, so treat smaller changes as noise.
- `tools/polymodel.py` is an exact Python model of the original span generation and of the fast edge stage. Check a new edge algorithm there against all 37,731 polygons before writing 6502. `tools/streamstats.py` and `tools/reencode_stats.py` size the workload and the re-encoding options.

## Build

Windows (the main dev environment). Most tools are vendored in `bin/`, but `bin/beebasm.exe` is an old Cygwin build that no longer runs. `make.bat` uses `%BEEBASM%`, which defaults to `..\beebasm\beebasm.exe` (beebasm 1.11, which reproduces the 2020 release build byte for byte). Run `make.bat` as `.\make.bat` from PowerShell, because `cmd //c` from Git Bash does not find it.

```
make-assets.bat   # logo (png2bbc + exomizer) and music (.vgm -> .vgi v3 with vgipacker.py); needs ../vgm-packer checked out alongside this repo
make.bat          # assembles all variants + discs -> build\beeb-niccc.dsd (via bin\bbcim -interss)
run.bat           # copies the .dsd into ../jsbeeb-kieranhj/discs and opens runme2.url
```

GNU make (`bin\gmake.exe` on Windows, or `make` elsewhere) does assets and code in one step, but writes `build/stnicc-A.dsd` via `bin/dsd_create.py`, and its `build` target only assembles the LOW and NULA variants:

```
make build        # assets + assemble + disc image
make data         # regenerate the streamed data (see below)
make clean
make b2_test      # reset + run the .dsd in a b2 emulator via its HTTP API (localhost:48075)
```

One variant on its own: `..\beebasm\beebasm.exe -D _QUALITY=0 -D _NULA=0 -i stnicc-beeb.asm -v > build\low.txt`. The `-v` listing is where `PRINT` output, sizes and page-alignment/boundary warnings appear.

## Build pipeline / architecture

The build runs in a fixed order, and later steps read files the earlier steps wrote:

1. **Variant executables.** `stnicc-beeb.asm` is assembled several times with `-D _QUALITY=0|1|2` and `-D _NULA=0|-1`, and each pass `SAVE`s one of `build/LOW`, `MEDIUM`, `HIGH` or `NULA`. Quality drives the derived flags at the top of the file (`_HALF_VERTICAL_RES`, `_DOUBLE_PLOT_Y`, `_WIDESCREEN`). `_NULA` switches pixel packing to MODE 2-style for the Video NuLA 12-bit palette board (see the `MODE5_PIXELS` macro). `-D _QUALITY=1 -D _NULA=-1 -D _CPC=-1` builds `build/CPC` (see below).
2. **Separate programs.** `src/intro.asm`, `src/outro.asm` and `src/music.asm` (loaded into SWRAM at &8000; the VGI v3 player from vgm-player-bbc, `lib/vgiplayer.asm`, and the three tunes) each `SAVE` their own `build/` file.
3. **Disc 1.** `stnicc-build.asm` contains no code. It `PUTFILE`s the built executables, `src/loader.bas` (the BASIC loader: NuLA and SWRAM detection, picks which exe to run) and `data/readme.txt` onto the disc. It then pads with dummy files so that stream file `00` starts exactly at `DISK1_first_track` (track 24; the 2020 release used 30).
4. **Disc 2.** `src/part-2.asm` puts `data/scene1_disk.01.bin` at track 1. `part-3.asm` and `part-4.asm` are unused.
5. The two `.ssd` images are interleaved into one double-sided `.dsd`.

**The hand-maintained sizes must match the binaries.** The padding calculation in `stnicc-build.asm` uses the sector-rounded file sizes. The player executables' sizes are in `src/exe_sizes.h.asm`, and `stnicc-beeb.asm` ASSERTs that each variant fills its slot exactly; `EXE_SIZE_MUSIC` is there too, asserted by `src/music.asm`. `INTRO_SIZE`, `OUTRO_SIZE` and so on are still unchecked constants in `stnicc-build.asm`. If an executable changes size by a sector, the stream data no longer starts at the track the player expects. The harness injects LOW, so it cannot see this; `--no-inject` can. `DISK1_first_track` (and the per-disc track constants) are defined in both `stnicc-beeb.asm` and `stnicc-build.asm`, and `_DEBUG` must agree between the two files as well.

**Data streaming.** The player does not use DFS files at runtime. It reads raw tracks (10 sectors) with OSWORD &7F into a ring buffer while frames are being decoded. The vsync/timer IRQ handler in `stnicc-beeb.asm` paces drawing and music. The "Streaming tracks from disk" section moves between drives and sides using the `DISK*_first_track` and `STREAMING_tracks_per_disk` constants.

**Data preparation** (`make data` / `data/make-data.bat`, run in `data/`):
- `bin/encoder.py` re-encodes the original `scene1.bin` (format described in `ref/stnicc-beeb.txt`). With `-b` it halves the coordinates to 128x200, uses `-a 7680` (3 tracks) as the alignment block instead of 64K so that no frame crosses a block, and `-f 2` keeps every other frame. The `archie-encoder` branch adds `-x` (an index of frame offsets) and `-c` (a colour index per frame, plus RISC OS OSWORD palette blocks) for an Archimedes port.
- `bin/split.py` cuts the encoded stream into `scene1_disk.00.bin` (56 tracks, disc 1, tracks 24-79) and `.01` (79 tracks, side 2), the chunks that are placed on disc. Side 2 is stored and read backwards, from track 79 down to 1, so the side switch needs no seek (`load_next_track` patches its `INC track_no` to `DEC`). `docs/disc-layout.md` records where the stream could start and what each choice measured.
- `encoder.py --optimise` (`bin/stream_optimise.py`) culls polygons hidden in every player and spends the bytes up to `--capacity` on polygon row trims and LOW's clear masks, chosen by estimated cycles saved per byte. With `--trim-mode both` (the data build's setting) a trim covers only rows hidden on LOW's 128-wide raster and on NULA and CPC's 64-wide one, so all three apply it; `low` makes LOW-only trims. It verifies every player's picture of every frame.
- `encoder.py`, `split.py` and `dsd_create.py` run under Python 3 and reproduce the release's `data/*.bin` and `.dsd` byte for byte when run without `--optimise` (and with the release's split). `parse.py` is still Python 2.

**Player source layout.** `stnicc-beeb.asm` holds the zero page, the BSS at &300–&800 (tables that are relocated over it at startup), the span buffer at &E00, the main code from &1100 up to `screen2_addr` (double-buffered screens just below &8000), and the streaming and IRQ code. It `INCLUDE`s the frame parser (`src/parse_frame.asm`), the polygon/span filler (`plot_poly.asm`, `plot_data.asm`, `plot_line*.asm`), `screen.asm`, `palette.asm` and `palette_stream.asm` (palette code is self-modifying and must stay page aligned), plus debug, tests and clock modules. `lib/` has the shared Bitshifters exomizer decruncher, the VGC music player (no longer built) and the VGI player, and disc helpers.

**The CPC player (`_CPC`, branch `cpc-mode2`).** On this branch the disc has LOW, CPC and NULA. Pressing C in the intro selects CPC (bit 6 of `NULA_FLAG_ZP`, &7E; NuLA's bit 7 wins), and the intro's text turns yellow on blue as a sign. `run.mjs --variant cpc --press-c` chooses it with the key rather than setting the flag. It is the NULA build (MODE 2 packing at 64x100, ULA control &00, each row plotted on both its scanlines) with the BBC's standard eight colours and no NuLA writes. Each polygon colour is one of the Amstrad CPC's 27 colours, a chequer of two pens: `span_colour` on the top line (pixels A,B) and `span_colour2` on the bottom (B,A). `tools/cpc_palette.py` writes `data/cpc_palette.bin` (count, then colour, top byte, bottom byte, for each palette change), copied to &A00 and read by `cpc_palette_update` into `cpc_colour_top`/`cpc_colour_bottom` at &780. Short spans stop at 3 bytes so each set of short-span routines fits its page. NULA and CPC also use LOW's edge stage (`_FAST_EDGES`), with x halved as the parser reads it, ahead of their own span plotter (`plot_poly_rows`). Most of it runs from spare low RAM, copied in at startup: the upward walkers at &900 and &7A0 (stored in the relocated-table image), the edge set-up at &E64 and the shallow-right walker and `stream_check_ahead` at &F64 (the span buffers' rows 100 onwards: vertex y is 0-99, so `init_span_buffer` clears 100 rows there), and the steep and shallow-left walkers at &C30 (after CPC's 559-byte palette data). `load_next_track` follows the plotters in those builds, so the page-aligned short-span routines stay put; LOW keeps its old layout, byte for byte. Check it with `run.mjs --no-inject --variant cpc --dump dir` then `python tools/cpc_check.py dir`, which compares every frame byte for byte with the model in `cpc_palette.py`.

**The fast LOW renderer (`_FAST_POLY`, LOW only; MEDIUM, HIGH and NULA are still byte-identical to the release).** The renderer runs inside the Timer 1 IRQ.

- **Parsing.** `parse_frame` resets each vertex's span-buffer row as it reads the vertex.
- **Edge stage** (`src/plot_poly_fast.asm`): edges going down plain-store their runs into the span buffer, then vertices are seeded, then edges going up compare-update. This is exact by construction; `polymodel.fast_poly_spans` mirrors it.
- **Span plotter** (`src/plot_spans_fast.asm`): plots each polygon in four passes, one per y AND 3. Within a pass the dither byte and the scanline are fixed and the screen page steps by one per row. Rows of three or more bytes enter the unrolled `span_screenN_rowM_unrolled` code.
- **RAM is the binding constraint.** Code must end below the stream ring buffer (it starts at `reloc_from_start`). So some code is assembled to run elsewhere and copied there by `main`: the span plotter and its tables at &400–&6FF (in place of the old screen-row tables), the upward edge walkers at &900–&9FF (the MOS RS423 output buffer) and at &C90–&CFF (after `palette_stream_buffer`). See the "relocated blocks" in `stnicc-beeb.asm`.
- **Streaming (every player).** A decoder faster than the disc would read stale ring data. `irq_handler` therefore calls `stream_check_ahead`, which locks the decoder until `STREAM_pages_ahead` (8: the largest frame is 1,548 bytes) pages are loaded, and the main loop refills a track by its position round the ring. `run.mjs --slow-disc ms` adds that much to every track read; frames must still match. NULA has 69 bytes spare below its stream buffer and MEDIUM 8, so code added ahead of `plot_poly.asm`'s page-aligned tables can push them out of RAM.

## Conventions and gotchas

- The code relies heavily on cycle timing and page alignment: branch tables, `CHECK_SAME_PAGE_AS`, `PAGE_ALIGN` and `GUARD`s. After a change, look in the listing for `WARNING!` or `Lost N bytes` lines and for GUARD failures.
- Debug and feature toggles are defines at the top of `stnicc-beeb.asm` (`_DEBUG`, `_TESTS`, `_SHOW_STREAMING_INFO`, `_STOP_AT_FRAME`, `_PLOT_WIREFRAME`, ...).
- The disc 1 step (in both `make.bat` and the Makefile) writes its listing to `compile.txt` in the repo root. Check it to see whether the padding size came out as expected. `build/` is git-ignored.
