# Check the CPC player's frames against tools/cpc_palette.py's model, byte for
# byte, and its ULA palette (logical 0-7 = the BBC's eight colours).
#
# node tools/harness/run.mjs --no-inject --variant cpc --dump dir
# python tools/cpc_check.py dir

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from cpc_palette import model_frames  # noqa: E402
from stnicc_stream import parse_stream  # noqa: E402


def main():
    dump_dir = sys.argv[1]
    frames = list(parse_stream(open("data/scene1.half.7680.bin", "rb").read()))
    bad = []
    for n, want in enumerate(model_frames(frames)):
        got = open(os.path.join(dump_dir, f"f{n:04d}.bin"), "rb").read()
        screen, pal = got[:6400], got[6400:]
        diff = [i for i in range(6400) if screen[i] != want[i]]
        # jsbeeb's actualPal holds the value written, colour XOR 7
        pal_ok = all(pal[i] ^ 7 == i for i in range(8))
        if diff or not pal_ok:
            bad.append((n, len(diff), diff[:4], pal_ok))
    if bad:
        print(f"FAIL: {len(bad)} of {len(frames)} frames differ; first: {bad[:5]}")
        sys.exit(1)
    print(f"PASS: all {len(frames)} frames match the model byte for byte, palette fixed")


if __name__ == "__main__":
    main()
