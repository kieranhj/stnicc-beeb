# The CPC player's palette stream: for each frame of the stream that changes the
# palette (in order), the colours whose CPC colour changed, as MODE 2 bytes.
#
# Each STe colour becomes a 2x2 block of two of the BBC's eight colours
# (tools/cpc_colour_map.py, chosen over the whole stream). MODE 2 rows are plotted
# on both their scanlines, the block's top pixels on the top line and its bottom
# pixels on the bottom, so each colour is a top line byte and a bottom line byte,
# and the player takes them by the polygon's colour. A dithered colour (three of
# one pen, one of the other) has its bottom line's two pixels swapped on odd rows.
#
# The file: a record per palette change: count, then count x (colour index, with
# bit 7 set if dithered; top line byte; bottom line byte, for even rows).
# Also the reference model: model_frames() gives each frame's expected screen.
#
# python tools/cpc_palette.py [stream.bin] [out.bin]

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from cpc_colour_map import PATTERNS, dithered, sequence_maps  # noqa: E402
from stnicc_raster import raster_spans  # noqa: E402
from stnicc_stream import FLAG_CLEAR_SCREEN, parse_stream  # noqa: E402

W, H = 64, 100


def mode2_byte(left, right):
    """MODE 2 byte: the left pixel's colour bits 3..0 in bits 7,5,3,1, the right's in 6,4,2,0."""
    out = 0
    for bit in range(4):
        out |= ((left >> bit) & 1) << (2 * bit + 1)
        out |= ((right >> bit) & 1) << (2 * bit)
    return out


def colour_bytes(pattern):
    """(top line byte, bottom line byte on even rows) of a colour's 2x2 block."""
    tl, tr, bl, br = pattern
    return mode2_byte(tl, tr), mode2_byte(bl, br)


def odd_delta(bottom):
    """What to EOR with a dithered colour's bottom line byte on odd rows: its two pixels swapped."""
    t = ((bottom >> 1) ^ bottom) & 0x55
    return t | (t << 1)


def palette_records(frames):
    """(frame number, [(index, colour, top byte, bottom byte, odd row delta), ...]) for each palette frame."""
    maps = sequence_maps(frames)
    current = [None] * 16
    out = []
    for n, f in enumerate(frames):
        if not f.palette:
            continue
        m = maps[n]
        changed = []
        for i in range(16):
            if current[i] != m[i]:
                top, bottom = colour_bytes(PATTERNS[m[i]])
                changed.append((i, m[i], top, bottom, odd_delta(bottom) if dithered(PATTERNS[m[i]]) else 0))
        current = m
        out.append((n, changed))
    return out


def model_frames(frames):
    """Each frame's expected 6400-byte screen, as the CPC player draws it."""
    image = bytearray(W * H)
    top = [0] * 16
    bottom = [0] * 16
    delta = [0] * 16
    records = dict(palette_records(frames))
    for n, f in enumerate(frames):
        for i, _, t, b, d in records.get(n, []):
            top[i], bottom[i], delta[i] = t, b, d
        if f.flags & FLAG_CLEAR_SCREEN:
            image[:] = bytes(W * H)
        for colour, verts, _ in f.polys:
            for y, (s, e) in raster_spans(verts, "nula").items():
                image[y * W + s:y * W + e + 1] = bytes([colour | 0x10]) * (e - s + 1)
        yield screen(image, top, bottom, delta)


def screen(image, top, bottom, delta):
    """Screen bytes: row y on scanlines 2y and 2y+1 of character row y >> 2.
    Index | 0x10 marks a pixel a polygon drew (the colour's bytes); 0 is cleared."""
    buf = bytearray(6400)
    odd_bottom = [b ^ d for b, d in zip(bottom, delta)]
    for y in range(H):
        base = (y >> 2) * 256 + (y & 3) * 2
        for col in range(32):
            l, r = image[y * W + 2 * col], image[y * W + 2 * col + 1]
            for line, table in ((0, top), (1, odd_bottom if y & 1 else bottom)):
                byte = 0
                for px, mask in ((l, 0xAA), (r, 0x55)):
                    if px:
                        byte |= table[px & 15] & mask
                buf[base + col * 8 + line] = byte
    return bytes(buf)


def main():
    src = sys.argv[1] if len(sys.argv) > 1 else "data/scene1.half.7680.bin"
    dst = sys.argv[2] if len(sys.argv) > 2 else "data/cpc_palette.bin"
    frames = list(parse_stream(open(src, "rb").read()))
    out = bytearray()
    records = palette_records(frames)
    for _, changed in records:
        out.append(len(changed))
        for i, _, top, bottom, d in changed:
            out += bytes([i | (0x80 if d else 0), top, bottom])
    open(dst, "wb").write(out)
    print(f"{dst}: {len(records)} palette changes, {sum(len(c) for _, c in records)} colours, {len(out)} bytes")


if __name__ == "__main__":
    main()
