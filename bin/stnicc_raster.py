# Exact host rasteriser for the BBC players: the spans the original 6502 polygon
# filler (drawline_into_span_buffer + plot_poly_span in src/plot_poly.asm) makes,
# for each player's coordinate space. Shared by bin/encoder.py (culling, row trims,
# clear lists) and tools/polymodel.py (checking new 6502 algorithms).
#
# Each edge from vertex i to i+1 is walked with that code's Bresenham (accumulator
# starts at major/2, both endpoints plotted, and on a shallow edge the pixel where
# the row changes is counted on both rows); every point does start=min, end=max on
# its row.

# The players' rasters, from the stream's (already halved) vertices:
#   LOW, MEDIUM: 128 x 100 (MEDIUM plots each row on two scanlines)
#   HIGH:        128 x 200, y doubled before filling
#   NULA:        64 x 100, x halved before filling
RASTERS = {
    "low": (128, 100, lambda x, y: (x, y)),
    "high": (128, 200, lambda x, y: (x, 2 * y)),
    "nula": (64, 100, lambda x, y: (x >> 1, y)),
}


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


def raster_spans(verts, raster):
    """reference_spans of stream vertices in one player's raster ("low", "high", "nula")."""
    to = RASTERS[raster][2]
    return reference_spans([to(x, y) for x, y in verts])


def frame_owners(polys, raster):
    """Spans per polygon, and owner[y][x] = index of the last polygon drawing (x, y),
    or -1, for one frame's polygons (a list of vertex lists) in one raster."""
    width, height, _ = RASTERS[raster]
    owner = [[-1] * width for _ in range(height)]
    spans = []
    for i, verts in enumerate(polys):
        sp = raster_spans(verts, raster)
        spans.append(sp)
        for y, (s, e) in sp.items():
            row = owner[y]
            for x in range(s, e + 1):
                row[x] = i
    return spans, owner


def visible_rows(spans, owner, i):
    """Rows of polygon i with at least one pixel still its own at the end of the frame."""
    return [y for y, (s, e) in sorted(spans[i].items()) if any(owner[y][x] == i for x in range(s, e + 1))]
