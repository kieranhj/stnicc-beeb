# The CPC player's palette stream: for each frame of the stream that changes the
# palette (in order), the colours whose CPC colour changed, as MODE 2 bytes.
#
# Each STe colour becomes one of the Amstrad CPC's 27 (tools/cpc_preview.py's
# palette_map), shown as two of the BBC's eight colours: pen A has each channel
# that is half or full, pen B each that is full. MODE 2 rows are plotted on both
# their scanlines, the top line with pixels A,B and the bottom B,A, so every pixel
# holds the chequer. The player looks the bytes up by the polygon's colour.
#
# The file: the 27 colours' (top-line byte, bottom-line byte), then a record per
# palette change: count, then count x (colour index, CPC colour x 2).
# Also the reference model: model_frames() gives each frame's expected screen.
#
# python tools/cpc_palette.py [stream.bin] [out.bin]

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from cpc_preview import LEVELS, palette_map  # noqa: E402
from stnicc_raster import raster_spans  # noqa: E402
from stnicc_stream import FLAG_CLEAR_SCREEN, parse_stream  # noqa: E402

W, H = 64, 100


def pens(level):
    """BBC colours (bit 0 red, 1 green, 2 blue) of a CPC colour's two pens."""
    a = sum(1 << i for i, v in enumerate(level) if v >= 1)
    b = sum(1 << i for i, v in enumerate(level) if v == 2)
    return a, b


def mode2_byte(left, right):
    """MODE 2 byte: the left pixel's colour bits 3..0 in bits 7,5,3,1, the right's in 6,4,2,0."""
    out = 0
    for bit in range(4):
        out |= ((left >> bit) & 1) << (2 * bit + 1)
        out |= ((right >> bit) & 1) << (2 * bit)
    return out


def colour_bytes(level):
    a, b = pens(level)
    return mode2_byte(a, b), mode2_byte(b, a)


def palette_records(frames):
    """(frame number, [(index, CPC colour, top byte, bottom byte), ...]) for each palette frame."""
    palette = [(0, 0, 0)] * 16
    current = [None] * 16
    out = []
    for n, f in enumerate(frames):
        if not f.palette:
            continue
        for i, rgb in f.palette:
            palette[i] = rgb
        m = palette_map(palette)
        changed = [(i, m[i], *colour_bytes(LEVELS[m[i]])) for i in range(16) if current[i] != m[i]]
        current = m
        out.append((n, changed))
    return out


def model_frames(frames):
    """Each frame's expected 6400-byte screen, as the CPC player draws it."""
    image = bytearray(W * H)
    top = [0] * 16
    bottom = [0] * 16
    records = dict(palette_records(frames))
    for n, f in enumerate(frames):
        for i, _, t, b in records.get(n, []):
            top[i], bottom[i] = t, b
        if f.flags & FLAG_CLEAR_SCREEN:
            image[:] = bytes(W * H)
        for colour, verts, _ in f.polys:
            for y, (s, e) in raster_spans(verts, "nula").items():
                image[y * W + s:y * W + e + 1] = bytes([colour | 0x10]) * (e - s + 1)
        yield screen(image, top, bottom)


def screen(image, top, bottom):
    """Screen bytes: row y on scanlines 2y and 2y+1 of character row y >> 2.
    Index | 0x10 marks a pixel a polygon drew (the colour's bytes); 0 is cleared."""
    buf = bytearray(6400)
    for y in range(H):
        base = (y >> 2) * 256 + (y & 3) * 2
        for col in range(32):
            l, r = image[y * W + 2 * col], image[y * W + 2 * col + 1]
            for line, table in ((0, top), (1, bottom)):
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
    for level in LEVELS:
        out += bytes(colour_bytes(level))
    records = palette_records(frames)
    for _, changed in records:
        out.append(len(changed))
        for i, c, _, _ in changed:
            out += bytes([i, 2 * c])
    open(dst, "wb").write(out)
    print(f"{dst}: {len(records)} palette changes, {sum(len(c) for _, c in records)} colours, {len(out)} bytes")


if __name__ == "__main__":
    main()
