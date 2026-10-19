# Data for the CMYK preview page: every frame of the stream as LOW would draw it
# (128 x 100 colour indices), each frame's 16 STe colours, and the CMYK two-pen
# pattern each colour maps to, beside the current LOW's own frames.
#
# The CMYK scheme is 1942-beeb's (tools/colour_dither.py there): a fixed palette of
# black, cyan, magenta and yellow (BBC colours 0, 6, 5, 3), each colour drawn as
# one pen or a two-pen pattern; pens mix in linear light and are matched in CIELAB
# with chroma at half weight. Two pattern sets: CHEQUER (one pen, or two at 50%:
# 10 colours) and LEVELS (two pens at 25, 50 or 75%: 22 colours).
#
# python tools/cmyk_preview.py <harness --dump dir> <out.bin>
# out.bin is zlib: a JSON header line, then the frames' index images, then the
# current LOW's dumped buffers and ULA palettes.

import json
import os
import sys
import zlib

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
from stnicc_raster import raster_spans  # noqa: E402
from stnicc_stream import FLAG_CLEAR_SCREEN, parse_stream  # noqa: E402

W, H = 128, 100
PENS = {"K": (0, 0, 0), "C": (0, 255, 255), "M": (255, 0, 255), "Y": (255, 255, 0)}
CHROMA_WEIGHT = 0.5
L_GAIN = 1.25           # the target's L* is scaled by this before matching (1942: 1.5)
REUSE_PENALTY = 12.0    # dE added for each further colour sharing one pattern
HUE_WEIGHT = 0.5        # hue's weight when mapping (scores stay honest)


def lin(c):
    c /= 255
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


def lab(rgb_lin):
    r, g, b = rgb_lin
    x = (0.4124 * r + 0.3576 * g + 0.1805 * b) / 0.95047
    y = 0.2126 * r + 0.7152 * g + 0.0722 * b
    z = (0.0193 * r + 0.1192 * g + 0.9505 * b) / 1.08883
    f = lambda t: t ** (1 / 3) if t > 216 / 24389 else (24389 / 27 * t + 16) / 116
    fx, fy, fz = f(x), f(y), f(z)
    return 116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)


def distance(a, b, hue_weight=1.0):
    """dE with chroma at CHROMA_WEIGHT: lightness and hue honestly (hue at
    hue_weight when mapping: CMYK has no red, so a dark red is nearer black than
    any purple, and a red scene would lose its shading to hue)."""
    (l1, a1, b1), (l2, a2, b2) = a, b
    c1, c2 = (a1 * a1 + b1 * b1) ** 0.5, (a2 * a2 + b2 * b2) ** 0.5
    dh2 = max(0.0, (a1 - a2) ** 2 + (b1 - b2) ** 2 - (c1 - c2) ** 2)
    return ((l1 - l2) ** 2 + (CHROMA_WEIGHT * (c1 - c2)) ** 2 + hue_weight ** 2 * dh2) ** 0.5


def patterns(levels):
    """(name, pen A, pen B, share of B) for one pen or two."""
    names = list(PENS)
    out = [(n, n, n, 0) for n in names]
    shares = (1, 2, 3) if levels else (2,)
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            for s in shares:
                out.append((f"{a}{b}{s}", a, b, s / 4))
    return out


def mix(a, b, share):
    return tuple((1 - share) * lin(pa) + share * lin(pb) for pa, pb in zip(PENS[a], PENS[b]))


def target_lab(rgb4, gain):
    l, a, b = lab(tuple(lin(v * 17) for v in rgb4))
    return min(100.0, l * gain), a, b


def palette_map(palette, cand_labs, gain=None, penalty=None, hue=None):
    """Pattern index for each of a palette's 16 colours, keeping their order of
    lightness: with the colours and the patterns each sorted by L*, a lighter colour
    never gets a darker pattern. Within that, the closest match overall, where two
    different colours sharing one pattern costs `penalty` (a small DP)."""
    gain = L_GAIN if gain is None else gain
    penalty = REUSE_PENALTY if penalty is None else penalty
    hue = HUE_WEIGHT if hue is None else hue
    targets = {c: target_lab(c, gain) for c in set(palette)}
    colours = sorted(targets, key=lambda c: targets[c][0])
    order = sorted(range(len(cand_labs)), key=lambda j: cand_labs[j][0])
    m = len(order)
    inf = float("inf")
    # cost[i][k]: colours[:i+1] placed, colour i on pattern order[k]
    cost = [[inf] * m for _ in colours]
    back = [[0] * m for _ in colours]
    for i, c in enumerate(colours):
        for k in range(m):
            d = distance(targets[c], cand_labs[order[k]], hue)
            if i == 0:
                cost[i][k] = d
                continue
            best_k = min(range(k + 1), key=lambda q: cost[i - 1][q] + (penalty if q == k else 0))
            cost[i][k] = d + cost[i - 1][best_k] + (penalty if best_k == k else 0)
            back[i][k] = best_k
    k = min(range(m), key=lambda q: cost[-1][q])
    pick = {}
    for i in range(len(colours) - 1, -1, -1):
        pick[colours[i]] = order[k]
        k = back[i][k]
    return [pick[c] for c in palette]


def main():
    dump_dir, out_path = sys.argv[1:3]
    frames = list(parse_stream(open("data/scene1.half.7680.bin", "rb").read()))

    sets = {}
    for name, levels in (("chequer", False), ("levels", True)):
        pats = patterns(levels)
        sets[name] = {"patterns": [p[:3] + (int(p[3] * 4),) for p in pats],
                      "lab": [lab(mix(a, b, s)) for _, a, b, s in pats]}

    image = bytearray(W * H)
    palette = [(0, 0, 0)] * 16
    images, palettes, maps = [], [], {k: [] for k in sets}
    for f in frames:
        for i, rgb in f.palette:
            palette[i] = rgb
        if f.flags & FLAG_CLEAR_SCREEN:
            image[:] = bytes(W * H)
        for colour, verts, _ in f.polys:
            for y, (s, e) in raster_spans(verts, "low").items():
                image[y * W + s:y * W + e + 1] = bytes([colour]) * (e - s + 1)
        images.append(bytes(image))
        palettes.append([v for rgb in palette for v in rgb])
        for k, st in sets.items():
            # the mapping follows the palette, so it changes only when the palette does
            maps[k].append(palette_map(palette, st["lab"]) if f.palette or not maps[k] else maps[k][-1])

    # how close each set gets, over every colour actually on screen
    for k, st in sets.items():
        total = n = 0
        for img, pal, m in zip(images, palettes, maps[k]):
            for c in set(img):
                rgb = pal[3 * c:3 * c + 3]
                total += distance(lab(tuple(lin(v * 17) for v in rgb)), st["lab"][m[c]])
                n += 1
        print(f"{k}: mean dE {total / n:.1f} over the colours on screen")

    dumps = []
    for i in range(len(frames)):
        d = open(os.path.join(dump_dir, f"f{i:04d}.bin"), "rb").read()
        dumps.append(d)
    header = {"frames": len(frames), "w": W, "h": H, "palettes": palettes,
              "sets": {k: {"patterns": v["patterns"], "map": maps[k]} for k, v in sets.items()},
              "dump_size": len(dumps[0])}
    blob = json.dumps(header, separators=(",", ":")).encode() + b"\n" + b"".join(images) + b"".join(dumps)
    open(out_path, "wb").write(zlib.compress(blob, 9))
    print(f"{out_path}: {len(blob)} bytes -> {os.path.getsize(out_path)} compressed")


if __name__ == "__main__":
    main()
