# Workload statistics for the BBC-encoded stream (data/scene1.half.7680.bin):
# polygons, edges, scanline spans and edge-walk steps per frame, as the LOW
# player would see them. Used to size optimisations before writing 6502.
#
# python tools/streamstats.py [data/scene1.half.7680.bin]

import sys
from collections import Counter

FLAG_CLEAR, FLAG_PAL, FLAG_INDEXED = 1, 2, 4


def frames(data):
    p = 0
    while True:
        flags = data[p]; p += 1
        if flags & FLAG_PAL:
            p += 1                       # dummy
            n = data[p]; p += 1
            p += 2 * n                   # NuLA words, ignored by LOW
        xs = ys = None
        if flags & FLAG_INDEXED:
            n = data[p]; p += 1
            xs = data[p:p + n]; p += n
            ys = data[p:p + n]; p += n
        polys = []
        while True:
            d = data[p]; p += 1
            if d >= 0xFD:
                break
            nv = d & 15
            if xs is not None:
                idx = data[p:p + nv]; p += nv
                verts = [(xs[i], ys[i]) for i in idx]
            else:
                verts = [(data[p + 2 * i], data[p + 2 * i + 1]) for i in range(nv)]
                p += 2 * nv
            polys.append((d >> 4, verts))
        yield flags, polys
        if d == 0xFD:
            return
        if d == 0xFE:                    # skip to next 7680-byte block
            p = (p + 7679) // 7680 * 7680


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
