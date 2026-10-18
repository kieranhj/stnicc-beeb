# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

BEEB-NICCC: a BBC Micro (6502) recreation of the Atari STe STNICCC-2000 demo (Bitshifters, Revision 2020). It plays back the original `scene1.bin` polygon stream (1800 frames of flat-shaded polygons + palette changes) by streaming re-encoded data off double-sided floppy discs while rasterising each frame. All runtime code is 6502 assembly for BeebAsm; the Python scripts in `bin/` are offline tools. There are no tests or linters; you check a change by building it and running the disc image in an emulator (b2 or jsbeeb).

## Build

Windows (the main dev environment). Most tools are vendored in `bin/`, but `bin/beebasm.exe` is an old Cygwin build that no longer runs. `make.bat` uses `%BEEBASM%`, which defaults to `..\beebasm\beebasm.exe` (beebasm 1.11, which reproduces the 2020 release build byte for byte). Run `make.bat` as `.\make.bat` from PowerShell, because `cmd //c` from Git Bash does not find it.

```
make-assets.bat   # logo (png2bbc + exomizer) and music (.vgm -> .vgc); needs ../vgm-packer checked out alongside this repo
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

1. **Variant executables.** `stnicc-beeb.asm` is assembled several times with `-D _QUALITY=0|1|2` and `-D _NULA=0|-1`, and each pass `SAVE`s one of `build/LOW`, `MEDIUM`, `HIGH` or `NULA`. Quality drives the derived flags at the top of the file (`_HALF_VERTICAL_RES`, `_DOUBLE_PLOT_Y`, `_WIDESCREEN`). `_NULA` switches pixel packing to MODE 2-style for the Video NuLA 12-bit palette board (see the `MODE5_PIXELS` macro).
2. **Separate programs.** `src/intro.asm`, `src/outro.asm` and `src/music.asm` (loaded into SWRAM at &8000) each `SAVE` their own `build/` file.
3. **Disc 1.** `stnicc-build.asm` contains no code. It `PUTFILE`s the built executables, `src/loader.bas` (the BASIC loader: NuLA and SWRAM detection, picks which exe to run) and `data/readme.txt` onto the disc. It then pads with dummy files so that stream file `00` starts exactly at `DISK1_first_track` (track 30).
4. **Disc 2.** `src/part-2.asm` puts `data/scene1_disk.01.bin` at track 1. `part-3.asm` and `part-4.asm` are unused.
5. The two `.ssd` images are interleaved into one double-sided `.dsd`.

**The hand-maintained sizes in `stnicc-build.asm` must match the binaries.** `EXE_SIZE`, `NULA_SIZE`, `INTRO_SIZE`, `OUTRO_SIZE`, `MUSIC_SIZE` and so on are constants that the padding calculation uses. If an executable grows past its constant, the stream data no longer starts at the track the player expects. `DISK1_first_track` (and the per-disc track constants) are defined in both `stnicc-beeb.asm` and `stnicc-build.asm`, and `_DEBUG` must agree between the two files as well.

**Data streaming.** The player does not use DFS files at runtime. It reads raw tracks (10 sectors) with OSWORD &7F into a ring buffer while frames are being decoded. The vsync/timer IRQ handler in `stnicc-beeb.asm` paces drawing and music. The "Streaming tracks from disk" section moves between drives and sides using the `DISK*_first_track` and `STREAMING_tracks_per_disk` constants.

**Data preparation** (`make data` / `data/make-data.bat`, run in `data/`):
- `bin/encoder.py` re-encodes the original `scene1.bin` (format described in `ref/stnicc-beeb.txt`). With `-b` it halves the coordinates to 128x200, uses `-a 7680` (3 tracks) as the alignment block instead of 64K so that no frame crosses a block, and `-f 2` keeps every other frame. The `archie-encoder` branch adds `-x` (an index of frame offsets) and `-c` (a colour index per frame, plus RISC OS OSWORD palette blocks) for an Archimedes port.
- `bin/split.py` cuts the encoded stream into `scene1_disk.00.bin` (50 tracks, disc 1) and `.01` onwards (79 tracks each), the chunks that are placed on disc.
- `encoder.py`, `split.py` and `dsd_create.py` run under Python 3 and reproduce the committed `data/*.bin` and the release `.dsd` byte for byte. `parse.py` is still Python 2.

**Player source layout.** `stnicc-beeb.asm` holds the zero page, the BSS at &300–&800 (tables that are relocated over it at startup), the span buffer at &E00, the main code from &1100 up to `screen2_addr` (double-buffered screens just below &8000), and the streaming and IRQ code. It `INCLUDE`s the frame parser (`src/parse_frame.asm`), the polygon/span filler (`plot_poly.asm`, `plot_data.asm`, `plot_line*.asm`), `screen.asm`, `palette.asm` and `palette_stream.asm` (palette code is self-modifying and must stay page aligned), plus debug, tests and clock modules. `lib/` has the shared Bitshifters exomizer decruncher, VGC music player and disc helpers.

## Conventions and gotchas

- The code relies heavily on cycle timing and page alignment: branch tables, `CHECK_SAME_PAGE_AS`, `PAGE_ALIGN` and `GUARD`s. After a change, look in the listing for `WARNING!` or `Lost N bytes` lines and for GUARD failures.
- Debug and feature toggles are defines at the top of `stnicc-beeb.asm` (`_DEBUG`, `_TESTS`, `_SHOW_STREAMING_INFO`, `_STOP_AT_FRAME`, `_PLOT_WIREFRAME`, ...).
- The disc 1 step (in both `make.bat` and the Makefile) writes its listing to `compile.txt` in the repo root. Check it to see whether the padding size came out as expected. `build/` is git-ignored.
