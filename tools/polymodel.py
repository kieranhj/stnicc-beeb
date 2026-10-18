# Exact host model of the LOW player's polygon span generation, for checking
# candidate 6502 algorithms against every polygon in the stream before writing them.
#
# reference_spans() mirrors drawline_into_span_buffer + plot_poly_span in
# src/plot_poly.asm: each edge from vertex i to i+1 is walked with that code's
# Bresenham (accumulator starts at major/2, both endpoints plotted, and on a
# shallow edge the pixel where the row changes is counted on both rows); every
# point does span_start=min, span_end=max on its row.
#
# python tools/polymodel.py [stream]   runs the checks in main()

import sys
from streamstats import frames


def edge_points(x0, y0, x1, y1):
    """Points the 6502 edge walker 'plots', in order, as (x, y)."""
    dx, dy = abs(x0 - x1), abs(y0 - y1)
    sx = 1 if x1 >= x0 else -1
    sy = 1 if y1 >= y0 else -1
    if dx == 0 and dy == 0:
        return [(x0, y0)]
    pts = []
    if dy >= dx:
        x, y, acc = x0, y0, dy >> 1
        for i in range(dy + 1):
            pts.append((x, y))
            if i == dy:
                break
            y += sy
            acc -= dx
            if acc < 0:
                acc += dy
                x += sx
    else:
        x, y, acc = x0, y0, dx >> 1
        pts.append((x, y))
        for i in range(dx):
            x += sx
            acc -= dy
            if acc < 0:
                acc += dx
                pts.append((x, y))   # end of span on current line
                y += sy
                pts.append((x, y))   # start of span on next line
        pts.append((x, y))           # last point
    return pts


def reference_spans(verts):
    """{y: (start, end)} as plot_poly_span would fill them."""
    lo, hi = {}, {}
    n = len(verts)
    for i in range(n):
        for x, y in edge_points(*verts[i], *verts[(i + 1) % n]):
            if x < lo.get(y, 255):
                lo[y] = x
            if x >= hi.get(y, 0):
                hi[y] = x
    ys = [y for _, y in verts]
    return {y: (lo[y], hi[y]) for y in range(min(ys), max(ys) + 1)}


def signed_area2(verts):
    a = 0
    n = len(verts)
    for i in range(n):
        (x0, y0), (x1, y1) = verts[i], verts[(i + 1) % n]
        a += x0 * y1 - x1 * y0
    return a


def chain_spans(verts, down_is_left):
    """Candidate: edges walked downwards (y1 > y0) only feed one side, upwards edges
    the other, horizontal edges both. A side takes min (left) or max (right)."""
    lo, hi = {}, {}
    n = len(verts)
    for i in range(n):
        (x0, y0), (x1, y1) = verts[i], verts[(i + 1) % n]
        pts = edge_points(x0, y0, x1, y1)
        if y1 == y0:
            sides = (True, True)
        else:
            left = (y1 > y0) == down_is_left
            sides = (left, not left)
        for x, y in pts:
            if sides[0] and x < lo.get(y, 255):
                lo[y] = x
            if sides[1] and x >= hi.get(y, 0):
                hi[y] = x
    ys = [y for _, y in verts]
    out = {}
    for y in range(min(ys), max(ys) + 1):
        out[y] = (lo.get(y), hi.get(y))
    return out


def hybrid_spans(verts, down_is_left):
    """Candidate: an edge's first and last rows take full min/max from its points;
    its interior rows feed only its own side (min for left, max for right)."""
    lo, hi = {}, {}
    n = len(verts)
    for i in range(n):
        (x0, y0), (x1, y1) = verts[i], verts[(i + 1) % n]
        left = (y1 > y0) == down_is_left
        for x, y in edge_points(x0, y0, x1, y1):
            end_row = y == y0 or y == y1
            if (end_row or left) and x < lo.get(y, 255):
                lo[y] = x
            if (end_row or not left) and x >= hi.get(y, 0):
                hi[y] = x
    ys = [y for _, y in verts]
    return {y: (lo.get(y), hi.get(y)) for y in range(min(ys), max(ys) + 1)}


def store_scheme_spans(verts):
    """Candidate used by the new LOW walker: reset only vertex rows (S=255, E=0);
    walk every downward edge (y1 > y0) first, plain-storing S/E for its interior rows
    (min/max of its own points there) and compare-updating its two end rows; then
    walk the other edges compare-updating every point."""
    S, E = {}, {}
    n = len(verts)
    for x, y in verts:
        S[y], E[y] = 255, 0
    edges = [(verts[i], verts[(i + 1) % n]) for i in range(n)]
    for (x0, y0), (x1, y1) in edges:
        if y1 <= y0:
            continue
        rows = {}
        for x, y in edge_points(x0, y0, x1, y1):
            lo, hi = rows.get(y, (255, -1))
            rows[y] = (min(lo, x), max(hi, x))
        for y, (lo, hi) in rows.items():
            if y == y0 or y == y1:
                if lo < S[y]:
                    S[y] = lo
                if hi >= E[y]:
                    E[y] = hi
            else:
                S[y], E[y] = lo, hi
    ys = [y for _, y in verts]
    # The 6502 does not reset rows between polygons: every row must be set by now.
    for y in range(min(ys), max(ys) + 1):
        assert y in S and y in E, f"row {y} not initialised before compare pass: {verts}"
    for (x0, y0), (x1, y1) in edges:
        if y1 > y0:
            continue
        for x, y in edge_points(x0, y0, x1, y1):
            if x < S[y]:
                S[y] = x
            if x >= E[y]:
                E[y] = x
    return {y: (S[y], E[y]) for y in range(min(ys), max(ys) + 1)}


def edge_rows(x0, y0, x1, y1):
    """{y: (lo, hi)} of one edge's own points per row."""
    rows = {}
    for x, y in edge_points(x0, y0, x1, y1):
        lo, hi = rows.get(y, (255, -1))
        rows[y] = (min(lo, x), max(hi, x))
    return rows


def fast_poly_spans(verts):
    """Mirror of src/plot_poly_fast.asm step by step: reset vertex rows; down edges
    (in the order n-1..0) store interior rows and, if shallow, compare the far end
    of their first and last rows; seed vertices; up edges compare (steep: interior
    rows; shallow: every row)."""
    S, E = {}, {}
    n = len(verts)

    def cmp_update(y, lo, hi):
        assert y in S, f"row {y} used before set: {verts}"
        if lo < S[y]:
            S[y] = lo
        if hi >= E[y]:
            E[y] = hi

    for x, y in verts:
        S[y], E[y] = 255, 0
    edges = [(verts[i], verts[(i + 1) % n]) for i in range(n)]
    for (x0, y0), (x1, y1) in reversed(edges):
        if y1 <= y0:
            continue
        dx, dy = abs(x1 - x0), y1 - y0
        for y, (lo, hi) in edge_rows(x0, y0, x1, y1).items():
            if y in (y0, y1):
                if dx > dy:
                    cmp_update(y, lo, hi)
            else:
                S[y], E[y] = lo, hi
    for x, y in verts:
        cmp_update(y, x, x)
    for (x0, y0), (x1, y1) in reversed(edges):
        if y1 >= y0:
            continue
        dx, dy = abs(x1 - x0), y0 - y1
        for y, (lo, hi) in edge_rows(x0, y0, x1, y1).items():
            if y in (y0, y1) and dx <= dy:
                continue
            cmp_update(y, lo, hi)
    ys = [y for _, y in verts]
    return {y: (S[y], E[y]) for y in range(min(ys), max(ys) + 1)}


def main():
    data = open(sys.argv[1] if len(sys.argv) > 1 else "data/scene1.half.7680.bin", "rb").read()
    n = 0
    orient = {"pos": 0, "neg": 0, "zero": 0}
    bad = {True: 0, False: 0}
    bad_either = 0
    hbad = {True: 0, False: 0}
    store_bad = 0
    fast_bad = 0
    from collections import Counter
    sizes = Counter()
    for flags, polys in frames(data):
        for colour, v in polys:
            n += 1
            a = signed_area2(v)
            orient["pos" if a > 0 else "neg" if a < 0 else "zero"] += 1
            ref = reference_spans(v)
            if store_scheme_spans(v) != ref:
                store_bad += 1
                print("store scheme mismatch", v)
            try:
                if fast_poly_spans(v) != ref:
                    fast_bad += 1
            except AssertionError:
                fast_bad += 1
            ok = {}
            for d in (True, False):
                ok[d] = chain_spans(v, d) == ref
                bad[d] += not ok[d]
            bad_either += not (ok[True] or ok[False])
            for d in (True, False):
                if hybrid_spans(v, d) != ref:
                    hbad[d] += 1
                    if d:
                        xs = [x for x, _ in v]
                        ys = [y for _, y in v]
                        sizes[(min(max(xs) - min(xs), 9), min(max(ys) - min(ys), 9))] += 1
    print(f"polys {n}  orientation {orient}")
    print(f"chain model mismatches: down=left {bad[True]}, down=right {bad[False]}, neither {bad_either}")
    print(f"hybrid mismatches: down=left {hbad[True]}, down=right {hbad[False]}")
    print("hybrid mismatch (width, height) capped at 9:", sorted(sizes.items()))
    print(f"store scheme mismatches: {store_bad} of {n}")
    print(f"fast_poly_spans (6502 mirror) mismatches: {fast_bad} of {n}")


if __name__ == "__main__":
    main()
