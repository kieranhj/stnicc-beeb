# The outro's edge view on the 1800-frame column disc (src/outro.asm with _COLS):
# each frame's 64 x 100 picture as its edges, in the outro's 128 x 100 window of
# MODE 4 (each picture pixel two screen pixels wide), checked against the frames
# the harness dumps.
#
# A pixel is set where the picture's colour changes from the pixel above (both of
# its screen pixels) or from the pixel to its left (its left screen pixel), and
# on the window's outermost pixels, a box.
#
# node tools/harness/run.mjs --no-inject --variant cols2 --outro-dump dir
# python tools/outro_check.py dir [polygon stream]

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from cpc_colour_map import pictures  # noqa: E402
from stnicc_stream import parse_stream  # noqa: E402

W, H = 64, 100
CELLS = 16                      # the window: 16 character cells (128 pixels) by 100 lines


def window(image):
    """The window's bytes, line by line (100 x 16), for a picture of colour indices."""
    out = bytearray(H * CELLS)
    for y in range(H):
        row = image[y * W:(y + 1) * W]
        above = image[(y - 1) * W:y * W] if y else None
        bits = [0] * 128
        for x in range(W):
            c = row[x]
            if above is not None and above[x] != c:
                bits[2 * x] = bits[2 * x + 1] = 1
            if x and row[x - 1] != c:
                bits[2 * x] = 1
        bits[0] = bits[127] = 1
        if y in (0, H - 1):
            bits = [1] * 128
        for k in range(CELLS):
            b = 0
            for i in range(8):
                b = b << 1 | bits[8 * k + i]
            out[y * CELLS + k] = b
    return bytes(out)


def main():
    dump_dir = sys.argv[1]
    src = sys.argv[2] if len(sys.argv) > 2 else "data/scene1.full.7680.bin"
    frames = list(parse_stream(open(src, "rb").read()))
    bad = []
    n = -1
    for n, image in enumerate(pictures(frames)):
        path = os.path.join(dump_dir, f"o{n:04d}.bin")
        if not os.path.exists(path):
            print(f"FAIL: only {n} of {len(frames)} frames dumped")
            sys.exit(1)
        got = open(path, "rb").read()
        want = window(image)
        diff = [i for i in range(len(want)) if got[i] != want[i]]
        if diff:
            bad.append((n, len(diff), [(i // CELLS, i % CELLS, got[i], want[i]) for i in diff[:3]]))
    if bad:
        print(f"FAIL: {len(bad)} of {n + 1} frames differ; first: {bad[:4]}")
        sys.exit(1)
    print(f"PASS: all {n + 1} outro frames match the edge model byte for byte")


if __name__ == "__main__":
    main()
