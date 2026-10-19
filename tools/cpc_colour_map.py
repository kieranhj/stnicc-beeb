# The STe -> Amstrad CPC colour mapping for the CPC player, chosen over the whole
# stream rather than one palette change at a time.
#
# The colours: each is a 2x2 block of BBC pixels, the top line byte's two pixels
# over the bottom line byte's, from two of the eight pens: a chequer (A B / B A)
# or three of one pen and one of the other (A A / A B, with the bottom line
# swapped to B A on odd rows, so a dither). They average to 83 different colours
# (each channel off, a quarter, half, three quarters or full, as two pens allow),
# against the 27 a chequer alone gives (cpc_preview.LEVELS, the first mapping).
#
# A colour lifetime is a palette entry holding one STe value: from the palette
# frame that sets it until the one that changes it. Each lifetime gets one of the
# colours (PATTERNS), so a colour that keeps its STe
# value never changes on the CPC either, and the player's records
# (data/cpc_palette.bin) still change only on palette frames.
#
# Two lifetimes that are live at the same time, and whose polygons meet on screen
# (in the 64 x 100 picture) in any frame while they are, must get different CPC
# colours when their STe colours differ, so that no edge the STe shows is lost.
# The cleared background is STe colour 0 (always black, and never drawn with),
# shown as black on the CPC. Only a lifetime whose STe colour is black may be
# black on the CPC, so no polygon disappears into the background or turns into a
# black hole the STe doesn't have. And where two such lifetimes differ in
# lightness (CIELAB L*) by LIGHTNESS_MARGIN or more on the STe, the darker one
# stays darker on the CPC.
#
# Within those rules it minimises the CIELAB distance to the STe colours, weighted
# by screen area over each lifetime, solved exactly as an integer program (147
# lifetimes x 83 colours; HiGHS takes a few seconds).
#
# python tools/cpc_colour_map.py [stream.bin]   prints the metrics, old and new

import os
import sys

import numpy as np
from scipy.optimize import Bounds, LinearConstraint, milp
from scipy.sparse import lil_matrix

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from cmyk_preview import distance, lab  # noqa: E402
from cpc_preview import CPC_LAB, LEVELS, palette_map, target  # noqa: E402
from stnicc_raster import raster_spans  # noqa: E402
from stnicc_stream import FLAG_CLEAR_SCREEN, parse_stream  # noqa: E402

W, H = 64, 100
BG = 16                          # the cleared background (STe colour 0), black on the CPC
LIGHTNESS_MARGIN = 5.0


def _pen(c):
    return tuple(float(c >> i & 1) for i in range(3))   # bit 0 red, 1 green, 2 blue


def _patterns():
    out = {}
    for a in range(8):
        for b in range(8):
            for p in ((a, b, b, a), (a, a, a, b)):
                avg = tuple(sum(_pen(c)[ch] for c in p) / 4 for ch in range(3))
                out.setdefault(avg, p)
    return list(out.items())


# (top left, top right, bottom left, bottom right) pens of each colour, and its CIELAB
# (the pens mixed in linear light)
PATTERNS = [p for _, p in _patterns()]
PATTERN_LAB = [lab(avg) for avg, _ in _patterns()]
BLACK = PATTERNS.index((0, 0, 0, 0))


def dithered(pattern):
    """Three of one pen and one of the other: the player swaps the bottom line's two
    pixels on odd rows, so the odd one out makes a diagonal dither, not a column."""
    tl, tr, bl, br = pattern
    return tl == tr == bl != br


def pictures(frames):
    """Each frame's final 64 x 100 picture (colour index, BG where cleared), as the player keeps it."""
    image = [BG] * (W * H)
    for f in frames:
        if f.flags & FLAG_CLEAR_SCREEN:
            image = [BG] * (W * H)
        for colour, verts, _ in f.polys:
            for y, (s, e) in raster_spans(verts, "nula").items():
                image[y * W + s:y * W + e + 1] = [colour] * (e - s + 1)
        yield image


def frame_stats(image):
    """(pixel count per colour, boundary pixel pairs per unordered colour pair)."""
    area = {}
    adj = {}
    for y in range(H):
        row = image[y * W:(y + 1) * W]
        below = image[(y + 1) * W:(y + 2) * W] if y + 1 < H else None
        for x, c in enumerate(row):
            area[c] = area.get(c, 0) + 1
            for d in ((row[x + 1] if x + 1 < W else c), (below[x] if below else c)):
                if d != c:
                    k = (c, d) if c < d else (d, c)
                    adj[k] = adj.get(k, 0) + 1
    return area, adj


def lifetimes(frames, stats):
    """(lifetimes as [index, STe colour, first frame, area], conflicts as a set per lifetime,
    and for each frame the lifetime of each of the 16 entries). Lifetime -1 is the background."""
    lives = []
    current = [None] * 16
    palette = [None] * 16
    conflicts = []
    per_frame = []
    for n, f in enumerate(frames):
        for i, rgb in f.palette:
            if rgb != palette[i]:
                current[i] = None
            palette[i] = rgb
        for i in range(16):
            if current[i] is None:
                current[i] = len(lives)
                lives.append([i, palette[i] or (0, 0, 0), n, 0])
                conflicts.append(set())
        per_frame.append(list(current))
        area, adj = stats[n]
        for c, a in area.items():
            if c != BG:
                lives[current[c]][3] += a
        for (a, b), k in adj.items():
            la = -1 if a == BG else current[a]
            lb = -1 if b == BG else current[b]
            ca = (0, 0, 0) if la < 0 else lives[la][1]
            cb = (0, 0, 0) if lb < 0 else lives[lb][1]
            if ca != cb:
                if la >= 0:
                    conflicts[la].add(lb)
                if lb >= 0:
                    conflicts[lb].add(la)
    return lives, conflicts, per_frame


def colour_lifetimes(lives, conflicts):
    """A colour (index into PATTERNS) for each lifetime: no two conflicting ones the same,
    black only for STe black, conflicting ones in the STe's order of lightness, least
    area-weighted error. Solved exactly as an integer program."""
    n, k = len(lives), len(PATTERNS)
    cost = np.array([[max(a, 1e-3) * distance(target(rgb), c) for c in PATTERN_LAB]
                     for _, rgb, _, a in lives]).ravel()
    light = [target(rgb)[0] for _, rgb, _, _ in lives]
    pairs = sorted({(min(i, j), max(i, j)) for i in range(n) for j in conflicts[i] if j >= 0})
    ordered = [(i, j) if light[i] < light[j] else (j, i) for i, j in pairs
               if abs(light[i] - light[j]) >= LIGHTNESS_MARGIN]
    a = lil_matrix((n + len(pairs) * k + len(ordered), n * k))
    for i in range(n):
        a[i, i * k:(i + 1) * k] = 1                  # one colour each
    for p, (i, j) in enumerate(pairs):
        for lv in range(k):                          # conflicting lifetimes differ
            r = n + p * k + lv
            a[r, i * k + lv] = 1
            a[r, j * k + lv] = 1
    for p, (i, j) in enumerate(ordered):             # L*(i) <= L*(j) - 1: darker stays darker
        r = n + len(pairs) * k + p
        for lv in range(k):
            a[r, i * k + lv] = PATTERN_LAB[lv][0]
            a[r, j * k + lv] = -PATTERN_LAB[lv][0]
    lo = np.r_[np.ones(n), np.zeros(len(pairs) * k), np.full(len(ordered), -np.inf)]
    hi = np.r_[np.ones(n + len(pairs) * k), np.full(len(ordered), -1.0)]
    upper = np.ones(n * k)
    for i in range(n):
        if lives[i][1] != (0, 0, 0):
            upper[i * k + BLACK] = 0                 # only STe black is black
    res = milp(cost, constraints=LinearConstraint(a.tocsr(), lo, hi),
               integrality=np.ones(n * k), bounds=Bounds(0, upper))
    assert res.success, res.message
    return [int(v) for v in res.x.reshape(n, k).argmax(axis=1)]


def sequence_maps(frames, stats=None):
    """{palette frame number: [colour (index into PATTERNS) for each of the 16 STe colours]}."""
    if stats is None:
        stats = [frame_stats(img) for img in pictures(frames)]
    lives, conflicts, per_frame = lifetimes(frames, stats)
    level = colour_lifetimes(lives, conflicts)
    maps = {}
    for n, f in enumerate(frames):
        if f.palette or not maps:
            maps[n] = [level[k] for k in per_frame[n]]
    return maps


def old_maps(frames):
    """The previous mapping: cpc_preview.palette_map at each palette frame on its own."""
    maps = {}
    palette = [(0, 0, 0)] * 16
    for n, f in enumerate(frames):
        for i, rgb in f.palette:
            palette[i] = rgb
        if f.palette or not maps:
            maps[n] = palette_map(palette)
    return maps


def metrics(frames, stats, maps, labs, black):
    """(frames with a lost edge, lost edge pixels per frame, on-screen remaps of unchanged
    STe colours, area-weighted mean dE, pixels per frame shown black that are not STe black)."""
    lost = 0
    false_black = 0
    lost_frames = 0
    remaps = 0
    de = 0.0
    palette = [(0, 0, 0)] * 16
    m = prev_palette = None
    prev_area = {}
    for n, f in enumerate(frames):
        for i, rgb in f.palette:
            palette[i] = rgb
        area, adj = stats[n]
        if n in maps:
            if m is not None:
                remaps += sum(1 for c in range(16) if palette[c] == prev_palette[c] and maps[n][c] != m[c]
                              and c in area and c in prev_area)
            m = maps[n]
        lf = 0
        for (a, b), k in adj.items():
            ca = (0, 0, 0) if a == BG else palette[a]
            cb = (0, 0, 0) if b == BG else palette[b]
            if ca != cb and (black if a == BG else m[a]) == (black if b == BG else m[b]):
                lf += k
        lost += lf
        lost_frames += lf > 0
        for c, a in area.items():
            if c != BG:
                de += a * distance(target(palette[c]), labs[m[c]])
                if m[c] == black and palette[c] != (0, 0, 0):
                    false_black += a
        prev_palette, prev_area = list(palette), area
    n = len(frames)
    return lost_frames, lost / n, remaps, de / (n * W * H), false_black / n


def main():
    src = sys.argv[1] if len(sys.argv) > 1 else "data/scene1.half.7680.bin"
    frames = list(parse_stream(open(src, "rb").read()))
    stats = [frame_stats(img) for img in pictures(frames)]
    for name, maps, labs, black in (("old", old_maps(frames), CPC_LAB, LEVELS.index((0, 0, 0))),
                                    ("new", sequence_maps(frames, stats), PATTERN_LAB, BLACK)):
        lf, lost, remaps, de, black = metrics(frames, stats, maps, labs, black)
        print(f"{name}: lost edges in {lf} frames ({lost:.1f} px/frame), "
              f"unchanged colours remapped on screen {remaps}, mean dE {de:.1f}, "
              f"false black {black:.1f} px/frame")


if __name__ == "__main__":
    main()
