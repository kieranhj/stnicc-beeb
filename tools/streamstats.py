# Workload statistics for the BBC-encoded stream (data/scene1.half.7680.bin):
# polygons, edges, scanline spans and edge-walk steps per frame, as the LOW
# player would see them. Used to size optimisations before writing 6502.
#
# python tools/streamstats.py [data/scene1.half.7680.bin]

import os
import sys
from collections import Counter

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
from stnicc_stream import parse_stream  # noqa: E402

FLAG_CLEAR = 1


def frames(data):
    """(flags, [(colour, vertices), ...]) per frame, read by bin/stnicc_stream.py
    (which also knows the clear masks and polygon trims of an --optimise stream)."""
    for f in parse_stream(data):
        yield f.flags, [(c, v) for c, v, _ in f.polys]


def main():
    data = open(sys.argv[1] if len(sys.argv) > 1 else "data/scene1.half.7680.bin", "rb").read()
    tot = Counter()
    nverts = Counter()
    widths = Counter()
    for flags, polys in frames(data):
        tot["frames"] += 1
        tot["clears"] += flags & FLAG_CLEAR
        for colour, v in polys:
            tot["polys"] += 1
            nverts[len(v)] += 1
            ys = [y for _, y in v]
            rows = max(ys) - min(ys) + 1
            tot["rows"] += rows
            # per-row span widths, from a simple min/max over edge pixels
            lo = {}
            hi = {}
            for i in range(len(v)):
                (x0, y0), (x1, y1) = v[i], v[(i + 1) % len(v)]
                dx, dy = abs(x1 - x0), abs(y1 - y0)
                tot["edges"] += 1
                if dy >= dx:
                    tot["steep_steps"] += dy + 1
                else:
                    tot["shallow_steps"] += dx + 1
                    tot["shallow_edges"] += 1
                steps = max(dx, dy)
                for k in range(steps + 1):
                    x = x0 + (x1 - x0) * k // steps if steps else x0
                    y = y0 + (y1 - y0) * k // steps if steps else y0
                    lo[y] = min(lo.get(y, 255), x)
                    hi[y] = max(hi.get(y, 0), x)
            for y in lo:
                w = hi[y] - lo[y] + 1
                widths[min(w, 40) if w < 14 else ("14-31" if w < 32 else "32+")] += 1
                tot["pixels"] += w
    f = tot["frames"]
    print(f"frames {f}, clears {tot['clears']}")
    for k in ["polys", "edges", "shallow_edges", "rows", "steep_steps", "shallow_steps", "pixels"]:
        print(f"{k:14s} total {tot[k]:9d}  per frame {tot[k] / f:8.1f}")
    print("verts/poly", sorted(nverts.items()))
    print("span widths", sorted(widths.items(), key=lambda kv: str(kv[0]).zfill(5)))


if __name__ == "__main__":
    main()
