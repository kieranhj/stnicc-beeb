# stnicc-beeb: start here

BEEB-NICCC: a BBC Micro (6502, BeebAsm) recreation of the Atari STe STNICCC-2000 demo, streaming
the original 1800-frame polygon stream off double-sided discs (Bitshifters, Revision 2020, 1st in
Old Skool Demo). Released demo; reopened 2026-10 for v1.2 (CPC/NuLA versions, faster LOW, the
column-stream player).

| | |
|---|---|
| GitHub | https://github.com/kieranhj/stnicc-beeb (public) |
| Local path | D:\beeb\mine\stnicc-beeb (moved from OneDrive\BEEB\Repos\stnicc-beeb on 2026-11-22) |
| Branch | column-stream-1800 (22 commits ahead of master, pushed); master = v1.2 |
| Other branches | column-stream, cpc-mode2 (ahead of origin by 2), reencode (ahead by 3), cpc-mode2-m2-plotter, nula-fix*, triple-buffer, xmas-*, ...; local-only: cpc-colour-map, cpc-masked-clears, fast-middle, find-slowdown{,2,3}, music/add-uservia, old-wips - **all already merged into master** |
| Worktree | D:\beeb\mine\stnicc-beeb-release (detached at the 2020 release) |
| Last commit | 2026-10-20: Docs: the column discs' outros; harness: a VideoNuLA for the NuLA runs on jsbeeb 2.10 |

## What's in here
- `stnicc-beeb.asm` the player (LOW/MEDIUM/HIGH/NULA/CPC variants); `stnicc-cols*.asm` the column-stream players
- `src/` intro, outro, music, disc parts; `lib/`; `data/` the streams; `ref/`
- `bin/` Python encoders/converters and vendored exes (beebasm.exe is an old Cygwin build that no longer runs)
- `tools/` harness (`tools/harness/run.mjs`), polymodel.py, stream stats; `docs/`
- `make.bat` (the maintained Windows build), `make-assets.bat`, `Makefile` (Tom's GNU make build)

## Build and run
- `make`: works (Tom's Makefile; builds LOW/NULA/CPC into `build/stnicc-A.dsd`). Its Python 2 helpers
  (`bin/shellcmd.py`, `png2bbc.py`, `bbc.py`, `png.py`, `adf_create.py`) were ported to Python 3 on
  2026-11-22, and its Windows `BEEBASM` default is now `..\..\third-party\beebasm\beebasm.exe` (the
  Cygwin `bin\beebasm.exe` no longer runs). `build/logo_mode1.exo`, `build/part-1.ssd` and the tracked
  `compile.txt` come out identical. Its `part-2.ssd` uses title STNICCC-2 (make.bat: BEEB-NICCC-2).
  `bin/parse.py` (not used by any build) is still Python 2.
- The real build (CLAUDE.md): `.\make.bat` (all variants + discs -> `build\beeb-niccc.dsd`,
  `beeb-niccc-cpc.dsd`, `beeb-niccc-cpc1800.dsd`). It uses `%BEEBASM%`, now defaulting to
  `..\..\third-party\beebasm\beebasm.exe` (beebasm 1.11); run on 2026-11-22, all three .dsd files
  byte-identical to the previous build and `compile.txt` unchanged.
- `make-assets.bat` needs `..\vgm-packer` (D:\beeb\mine\vgm-packer exists).
- Run: jsbeeb-kieranhj, the local jsbeeb clone it used, was deleted on 2026-11-27 (the fork is on GitHub), and `run.bat` with it; open the disc at https://bbc.xania.org or in b2.
- LOW check: `node tools/harness/run.mjs --ref tools/harness/baseline.json` (~85 s, jsbeeb headless).

## Local changes
- None. Clean working tree; no stashes. cpc-mode2 and reencode have 2/3 unpushed commits each.

## Status and next steps
- column-stream-1800: the 1800-frame column disc and its outro (136.4 s); docs updated 2026-10-20.
  Whether to merge it to master is not recorded. CLAUDE.md has the harness rules and pipeline.

## See also
README.md (links: Bitshifters page, YouTube, Pouet), CLAUDE.md, docs/; D:\beeb\mine\stnicc-beeb-release.
