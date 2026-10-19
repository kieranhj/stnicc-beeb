# Data for the CPC-palette preview page: the stream drawn in MODE 2 at the NULA
# player's 64 x 100, each STe colour shown as a 2x2 block of two of the BBC's
# eight colours (tools/cpc_colour_map.PATTERNS: a chequer, as in ebenupton/lander's
# MODE 2 CPC version, which gives the Amstrad CPC's 27 colours, or three of one
# pen and one of the other). No NuLA needed: the palette is MODE 2's standard eight.
#
# The mapping is tools/cpc_colour_map.py's, chosen over the whole stream: a colour
# keeps its CPC colour while it keeps its STe value, colours that meet on screen
# stay apart and in the STe's order of lightness, and only STe black is black.
# palette_map (the nearest of the 27 in CIELAB, the k-th further colour sharing
# one paying k * REUSE_PENALTY) was the first mapping, kept for comparison.
#
# python tools/cpc_preview.py <harness --dump dir of LOW> <out.bin>
# out.bin is zlib: a JSON header line, then the 128 x 100 STe images, the 64 x 100
# MODE 2 images, and the current LOW's dumped buffers and ULA palettes.

import json
import os
import sys
import zlib

from scipy.optimize import linear_sum_assignment

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from cmyk_preview import distance, lab, lin  # noqa: E402
from stnicc_raster import raster_spans  # noqa: E402
from stnicc_stream import FLAG_CLEAR_SCREEN, parse_stream  # noqa: E402

REUSE_PENALTY = 8.0
LEVELS = [(r, g, b) for r in range(3) for g in range(3) for b in range(3)]
CPC_LAB = [lab(tuple(v / 2 for v in lv)) for lv in LEVELS]   # pens mixed in linear light


def target(rgb4):
    return lab(tuple(lin(v * 17) for v in rgb4))


def palette_map(palette):
    colours = sorted(set(palette))
    cost = [[distance(target(c), CPC_LAB[j]) + k * REUSE_PENALTY
             for k in range(len(colours)) for j in range(len(LEVELS))] for c in colours]
    rows, cols = linear_sum_assignment(cost)
    pick = {colours[r]: int(c) % len(LEVELS) for r, c in zip(rows, cols)}
    return [pick[c] for c in palette]


def draw(image, width, raster, f):
    if f.flags & FLAG_CLEAR_SCREEN:
        image[:] = bytes(len(image))
    for colour, verts, _ in f.polys:
        for y, (s, e) in raster_spans(verts, raster).items():
            image[y * width + s:y * width + e + 1] = bytes([colour]) * (e - s + 1)


def main():
    dump_dir, out_path = sys.argv[1:3]
    frames = list(parse_stream(open("data/scene1.half.7680.bin", "rb").read()))
    low, m2 = bytearray(128 * 100), bytearray(64 * 100)
    palette = [(0, 0, 0)] * 16
    lows, m2s, palettes, maps = [], [], [], []
    from cpc_colour_map import PATTERN_LAB, PATTERNS, sequence_maps
    chosen = sequence_maps(frames)
    for f in frames:
        for i, rgb in f.palette:
            palette[i] = rgb
        draw(low, 128, "low", f)
        draw(m2, 64, "nula", f)
        lows.append(bytes(low))
        m2s.append(bytes(m2))
        palettes.append([v for rgb in palette for v in rgb])
        maps.append(chosen[len(maps)] if len(maps) in chosen else maps[-1])

    total = n = 0
    for img, pal, m in zip(m2s, palettes, maps):
        for c in set(img):
            total += distance(target(pal[3 * c:3 * c + 3]), PATTERN_LAB[m[c]])
            n += 1
    print(f"CPC: mean dE {total / n:.1f} over the colours on screen")

    dumps = [open(os.path.join(dump_dir, f"f{i:04d}.bin"), "rb").read() for i in range(len(frames))]
    header = {"frames": len(frames), "palettes": palettes, "patterns": PATTERNS, "map": maps,
              "dump_size": len(dumps[0])}
    blob = (json.dumps(header, separators=(",", ":")).encode() + b"\n"
            + b"".join(lows) + b"".join(m2s) + b"".join(dumps))
    open(out_path, "wb").write(zlib.compress(blob, 9))
    print(f"{out_path}: {len(blob)} bytes -> {os.path.getsize(out_path)} compressed")


if __name__ == "__main__":
    main()
