# Disc layout and the side switch

Measurements from October 2026, branch `reencode`, for choosing where the stream
starts on disc 1 and where it moves to side 2. The committed layout starts the
stream at **track 24**.

## Layout

- **Disc 1 (side 1):** the executables, the loader and the readme, then stream file `00` from `DISK1_first_track` to track 79.
- **Side 2:** stream file `01` on tracks 1 to 79. It is stored and read **backwards**, from track 79 down to 1 (`bin/split.py` writes its tracks last first; `load_next_track` patches its `INC track_no` to `DEC`). The head is already at track 79 when the stream moves to side 2, so the switch needs no seek. Read forwards, it used to start with a 78-track seek: 1.87 s at DFS's step rate in jsbeeb.
- **Capacity:** `--capacity` = (80 − first track + 79) × 2560 bytes. Whatever the encoder doesn't need for the frames goes on polygon trims and clear masks for LOW.
- **Changing the start track:** set `DISK1_first_track` in `stnicc-beeb.asm`, `stnicc-build.asm` and `src/outro.asm`, `DISK1_FIRST_TRACK` in `bin/split.py`, and `--capacity` in `data/make-data.bat` and the `Makefile`. Then re-encode, split and build.
- **Earliest possible start:** track 20. Disc 1's files take &BE00 bytes, plus &300 for the catalogue and `!BOOT`: 19.3 tracks.

## Why the split point matters

The ring buffer holds three tracks. The first read of side 2 (OSWORD &7F) can start once the reader has left the ring slot it will fill. The decoder locks when the reader gets within 6 pages of that slot. So the decoder can run on what is already in the ring for as long as the reader takes to get through about 3,584 bytes of stream, and how long that takes depends on where the split falls in the stream.

In jsbeeb that first read of side 2 takes 1.29–1.49 s even with no seek. Most of it is DFS polling the 8271's drive status after it changes side. The drive reports not ready (&81) for about 1.3 s. That may not happen on a real drive. An ordinary track read takes about 0.43 s (median) and at most about 0.74 s (p99).

We can't shorten the switch with our own 8271 commands: all disc access stays on DFS's OSWORD &7F, so that MMC/SD floppy emulators work.

## Results

LOW on full disc builds (`run.mjs --no-inject --stream-log`, then `tools/stream_margin.py`). Every frame is identical to the 2020 release in all three.

| Stream starts at | Disc 1 tracks | Trims + masks | Ring lasts at the switch | Switch read | Decoder stall at the switch | Render vs release | Fields | Free on disc 1 |
|---|---|---|---|---|---|---|---|---|
| track 21 | 59 | 27,744 | 1,614 ms | 1,290 ms | none (324 ms spare) | −29.10% | 6,446 | 4.3 KB |
| **track 24 (committed)** | 56 | 21,460 | 1,153 ms | 1,441 ms | 289 ms | −28.96% | 6,439 | 12 KB |
| track 27 | 53 | 13,874 | 1,667 ms | 1,489 ms | none (178 ms spare) | −28.58% | 6,458 | 19.7 KB |

The other start tracks were only modelled with `tools/flip_split.py`, each on the stream encoded for that split. The model reads about 150 ms high, and the times below already have that taken off:

| Stream starts at | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Est. LOW cycles saved (M) | 24.15 | 23.83 | 23.59 | 23.30 | 22.89 | 22.65 | 21.86 | 20.97 | 19.90 | 18.30 | 17.84 |
| Ring lasts at the switch (ms, ~) | 1,290 | 1,470 | 1,220 | 1,140 | 1,180 | 910 | 1,250 | 1,650 | 1,830 | 1,830 | 2,330 |

Notes:

- **Fields barely move between layouts.** The stall at the switch mostly lands on time that would otherwise be spent waiting for vsync. The cycles the trims save matter more.
- **Track 24 keeps 12 KB free** for MEDIUM and HIGH compressed with zx02. They are about 9.7 KB each uncompressed and are off the disc for now. Track 21 is the best for render and gives a safe switch, but leaves only 4.3 KB.
- **A stall near the start of the stream, independent of the split:** a read around disc 1 track 26–30 has a 395–407 ms budget and takes 560–578 ms, so the decoder stalls 165–171 ms. The stream is data-heavy there.

## Tools

- `node tools/harness/run.mjs --no-inject --stream-log s.json` records every track read (start, end, drive, track, the 8271's timers and commands), every frame start and every decoder lock.
- `python tools/stream_margin.py s.json` reports each read's time against its budget, and the side switch.
- `python tools/flip_split.py stream.bin s.json first last` models the budget at the switch for each length of disc 1.
