# Stream optimisations for bin/encoder.py (--optimise). Every change leaves each
# player's picture of every frame exactly as before:
#
# - Cull: leave out polygons that end the frame with no pixel showing in any
#   player's raster (LOW/MEDIUM, HIGH and NULA, see stnicc_raster.RASTERS).
# - Trim (LOW): rows at the top and bottom of a polygon that end the frame
#   entirely overdrawn need not be plotted. Up to 15 each way, in one byte.
# - Clear mask (LOW): a clearing frame only needs to clear the rows that its
#   polygons leave a pixel of uncovered. 13 bytes, one bit per row.
#
# Trims and masks cost disc bytes, so they are chosen greedily by estimated
# cycles saved per byte until the stream no longer fits (see encoder.py).

from stnicc_raster import RASTERS, frame_owners, raster_spans, visible_rows
from stnicc_stream import FLAG_CLEAR_SCREEN, POLY_DESC_END_OF_STREAM, POLY_TRIM, mask_bytes, parse_stream

# Rough LOW costs, in 2 MHz cycles, used only to rank the candidates
CYCLES_PER_SPAN_ROW = 150       # a plotted row, all in
CYCLES_FULL_CLEAR = 16900       # screen_cls
CYCLES_MASKED_CLEAR = 250       # mask walk
CYCLES_PER_CLEARED_ROW = 185    # one row through the unrolled span code

TRIM_MAX = 15


def beeb_polys(frame):
    """(colour, vertices in stream coordinates) for each polygon of an encoder Frame."""
    return [(p._colour_index, [(v._x // 2, v._y // 2) for v in p._vertices]) for p in frame._polygons]


def analyse(frames):
    """Culls every frame (in place) and returns the trim and clear-mask candidates:
    (value in cycles, cost in bytes, apply function)."""
    candidates = []
    culled = 0
    for frame_no, f in frames:
        polys = beeb_polys(f)
        verts = [v for _, v in polys]
        owners = {r: frame_owners(verts, r) for r in RASTERS}
        keep = [i for i in range(len(verts)) if any(visible_rows(*owners[r], i) for r in RASTERS)]
        culled += len(verts) - len(keep)
        if len(keep) < len(verts):
            f.set_polygons([f._polygons[i] for i in keep])

        spans, owner = owners["low"]
        for i in keep:
            vis = visible_rows(spans, owner, i)
            ys = sorted(spans[i])
            if vis:
                top = min(vis[0] - ys[0], TRIM_MAX)
                bottom = min(ys[-1] - vis[-1], TRIM_MAX)
            else:
                # Kept for another player but hidden in LOW: skip what a trim can
                # (all of it when 30 rows or fewer; the plotter takes top + bottom
                # past the polygon's height as nothing to plot)
                top = min(len(ys), TRIM_MAX)
                bottom = min(len(ys) - top, TRIM_MAX)
            poly = f._polygons[keep.index(i)]
            if (poly._colour_index << 4 | POLY_TRIM | poly.get_num_verts()) >= POLY_DESC_END_OF_STREAM:
                continue                # colour 15 with 5-7 vertices: the descriptor would read as an end marker
            if top or bottom:
                candidates.append(((top + bottom) * CYCLES_PER_SPAN_ROW, 1, trim_setter(f, poly, (top, bottom))))

        if f._flags & FLAG_CLEAR_SCREEN:
            rows = [y for y in range(100) if any(o < 0 for o in owner[y])]
            value = CYCLES_FULL_CLEAR - CYCLES_MASKED_CLEAR - CYCLES_PER_CLEARED_ROW * len(rows)
            if value > 0:
                candidates.append((value, 13, mask_setter(f, mask_bytes(rows))))
    return candidates, culled


def trim_setter(frame, poly, trim):
    def apply(on):
        poly._trim = trim if on else None
        frame.invalidate_size()
    return apply


def mask_setter(frame, mask):
    def apply(on):
        frame.set_clear_mask(mask if on else None)
    return apply


def pictures(polys, raster, trims=None):
    """Colour index of every pixel at the end of a frame (None where nothing is
    drawn), plotting only the untrimmed rows of each polygon when trims are given."""
    width, height, _ = RASTERS[raster]
    pic = [[None] * width for _ in range(height)]
    for k, (colour, verts) in enumerate(polys):
        sp = raster_spans(verts, raster)
        ys = sorted(sp)
        lo, hi = ys[0], ys[-1]
        if trims and trims[k]:
            lo, hi = lo + trims[k][0], hi - trims[k][1]
        for y in ys:
            if lo <= y <= hi:
                s, e = sp[y]
                pic[y][s:e + 1] = [colour] * (e - s + 1)
    return pic


def verify(data, reference):
    """Check the written stream against the frames before optimisation (a list of
    (flags, [(colour, verts), ...])): every player's picture of every frame is
    unchanged, and every row a clear mask leaves out is fully drawn over."""
    frames = list(parse_stream(data))
    assert len(frames) == len(reference), (len(frames), len(reference))
    for n, (f, (ref_flags, ref_polys)) in enumerate(zip(frames, reference)):
        # clear and palette flags (indexed may change when polygons are culled)
        assert f.flags & 3 == ref_flags & 3, f"frame {n}: flags {f.flags:#x} against {ref_flags:#x}"
        polys = [(c, v) for c, v, _ in f.polys]
        trims = [t for _, _, t in f.polys]
        for r in RASTERS:
            want = pictures(ref_polys, r)
            got = pictures(polys, r, trims if r == "low" else None)
            assert got == want, f"frame {n}: {r} picture differs"
            if r == "low" and f.clear_mask:
                for y in range(100):
                    if not f.clear_mask[y]:
                        assert None not in got[y], f"frame {n}: row {y} left uncleared but not covered"
    return len(frames)
