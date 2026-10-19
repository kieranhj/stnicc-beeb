# Sizes the re-encoding options for the LOW player with an exact host renderer
# (tools/polymodel.reference_spans): how much of each frame's drawing is never
# seen, whether clears are needed, and what precomputed spans would cost.
#
# python tools/reencode_stats.py [data/scene1.half.7680.bin]

import sys
from collections import Counter
from polymodel import reference_spans
from streamstats import frames


def main():
    data = open(sys.argv[1] if len(sys.argv) > 1 else "data/scene1.half.7680.bin", "rb").read()
    tot = Counter()
    for flags, polys in frames(data):
        tot["frames"] += 1
        # owner[y][x] = index of the last polygon to draw pixel (x, y) this frame
        owner = [[-1] * 128 for _ in range(100)]
        spans = []
        for i, (colour, v) in enumerate(polys):
            sp = reference_spans(v)
            spans.append(sp)
            for y, (s, e) in sp.items():
                row = owner[y]
                for x in range(s, e + 1):
                    row[x] = i
        visible = Counter()
        for y in range(100):
            for x in range(128):
                if owner[y][x] >= 0:
                    visible[owner[y][x]] += 1
        covered = sum(1 for y in range(100) for x in range(128) if owner[y][x] >= 0)
        for i, sp in enumerate(spans):
            px = sum(e - s + 1 for s, e in sp.values())
            rows = len(sp)
            tot["polys"] += 1
            tot["rows"] += rows
            tot["pixels"] += px
            tot["visible_pixels"] += visible[i]
            if visible[i] == 0:
                tot["hidden_polys"] += 1
                tot["hidden_rows"] += rows
                tot["hidden_pixels"] += px
            # rows whose span is entirely overdrawn later
            for y, (s, e) in sp.items():
                if all(owner[y][x] != i for x in range(s, e + 1)):
                    tot["hidden_span_rows"] += 1
        if flags & 1:
            tot["clears"] += 1
            if covered == 128 * 100:
                tot["clears_fully_covered"] += 1
            tot["clear_uncovered_bytes"] += sum(
                1 for y in range(100) for b in range(32) if all(owner[y][4 * b + k] < 0 for k in range(4)))
        tot["frame_bytes_spans"] += sum(2 * len(sp) + 2 for sp in spans)
    f = tot["frames"]
    print(f"frames {f}")
    for k in ["polys", "hidden_polys", "rows", "hidden_rows", "hidden_span_rows", "pixels", "hidden_pixels",
              "visible_pixels", "clears", "clears_fully_covered", "clear_uncovered_bytes", "frame_bytes_spans"]:
        print(f"{k:24s} {tot[k]:10d}  per frame {tot[k] / f:9.1f}")
    print(f"hidden polys {100 * tot['hidden_polys'] / tot['polys']:.1f}%, rows {100 * tot['hidden_rows'] / tot['rows']:.1f}%,"
          f" rows fully overdrawn {100 * tot['hidden_span_rows'] / tot['rows']:.1f}%")


if __name__ == "__main__":
    main()
