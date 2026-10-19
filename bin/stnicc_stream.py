# Reader for the BBC-encoded stream that bin/encoder.py writes (-b), shared by the
# encoder's self-check and the tools in tools/.
#
# Frame:   flags
#          [clear mask: 13 bytes, if flags & FLAG_CLEAR_MASK]
#          [palette: dummy byte, count, count NuLA words, if flags & FLAG_CONTAINS_PALETTE]
#          [vertices: count, count x, count y, if flags & FLAG_INDEXED_DATA]
#          polygons, then &FF (end of frame), &FE (end of frame, skip to the next
#          alignment block) or &FD (end of stream)
# Polygon: descriptor = colour << 4 | trim flag (8) | vertex count (3-7)
#          [trim: top rows << 4 | bottom rows, if the trim flag is set]
#          indexed: one vertex index per vertex; else x, y per vertex

FLAG_CLEAR_SCREEN = 0x01
FLAG_CONTAINS_PALETTE = 0x02
FLAG_INDEXED_DATA = 0x04
FLAG_CLEAR_MASK = 0x08          # with FLAG_CLEAR_SCREEN: only the rows in the mask need clearing

POLY_TRIM = 0x08                # in the descriptor's vertex count
POLY_DESC_END_OF_STREAM = 0xFD
POLY_DESC_SKIP_TO_BLOCK = 0xFE
POLY_DESC_END_OF_FRAME = 0xFF

CLEAR_MASK_BYTES = 13           # 100 rows, row y = bit 7 - (y & 7) of byte y >> 3


class StreamFrame:
    def __init__(self, flags):
        self.flags = flags
        self.clear_mask = None      # list of 100 bools, or None
        self.polys = []             # (colour, [(x, y), ...], trim (top, bottom) or None)

    def rows_to_clear(self):
        return [y for y in range(100) if self.clear_mask[y]] if self.clear_mask else list(range(100))


def mask_bytes(rows):
    """13 bytes with a bit set for each row in rows (an iterable of 0-99)."""
    out = [0] * CLEAR_MASK_BYTES
    for y in rows:
        out[y >> 3] |= 0x80 >> (y & 7)
    return out


def parse_stream(data, block=7680):
    """Yields StreamFrame for each frame of the stream."""
    p = 0
    while True:
        f = StreamFrame(data[p])
        p += 1
        if f.flags & FLAG_CLEAR_MASK:
            m = data[p:p + CLEAR_MASK_BYTES]
            p += CLEAR_MASK_BYTES
            f.clear_mask = [bool(m[y >> 3] & (0x80 >> (y & 7))) for y in range(100)]
        if f.flags & FLAG_CONTAINS_PALETTE:
            p += 1                      # dummy
            n = data[p]
            p += 1 + 2 * n              # NuLA words
        xs = ys = None
        if f.flags & FLAG_INDEXED_DATA:
            n = data[p]
            p += 1
            xs = data[p:p + n]
            p += n
            ys = data[p:p + n]
            p += n
        while True:
            d = data[p]
            p += 1
            if d >= POLY_DESC_END_OF_STREAM:
                break
            nv = d & 7
            trim = None
            if d & POLY_TRIM:
                trim = (data[p] >> 4, data[p] & 15)
                p += 1
            if xs is not None:
                verts = [(xs[i], ys[i]) for i in data[p:p + nv]]
                p += nv
            else:
                verts = [(data[p + 2 * i], data[p + 2 * i + 1]) for i in range(nv)]
                p += 2 * nv
            f.polys.append((d >> 4, verts, trim))
        yield f
        if d == POLY_DESC_END_OF_STREAM:
            return
        if d == POLY_DESC_SKIP_TO_BLOCK:
            p = (p + block - 1) // block * block
