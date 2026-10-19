# The column stream: each frame's final 64 x 100 picture as 32 byte columns, for
# the column player (stnicc-cols.asm), in place of polygons.
#
# A byte column is a list of runs from the top: (value, end row, slope). The
# value is the pixel pair, left colour << 4 | right colour (colour 0, black and
# never drawn with, stands for the cleared background). Each column is coded
# against the column to its left (column 0 against an empty one): polygon edges
# are straight, so a run usually carries on from one there, its end moving by
# that run's slope (its end minus the end of the run it came from) give or take
# a row.
#
# Codes are 2 bits, four to a control byte, high bits first; a control byte is
# read when a code is wanted and the last one is used up (control bytes start
# afresh with each frame). Literal bytes follow in order:
#   00, 01, 10   K   the left column's next run carries on: end = its end + its
#                    slope + 0, +1 or -1 (100 at most)
#   11 00        Kx  the same, with the end row in a byte
#   11 01        D   drop the left column's next run
#   11 10        I   a new run: value byte, then a length code (00, 01, 10 for
#                    1, 2, 3 rows; 11 for a length byte); slope 0
#   11 11        R   the left column's next run carries on in a new value: value
#                    byte, then a K code (11: end row byte)
# A run that reaches row 100 ends the column; the left column's runs left over
# are dropped.
#
# A frame: flags (FLAG_CONTAINS_PALETTE as in the polygon stream), the palette
# block if flagged (a dummy byte, a count, then 2 bytes per entry: index << 4 |
# red, green << 4 | blue), the 32 columns, then &FF, or &FE when the next frame
# starts the next block (no frame crosses one, so the player's ring buffer never
# splits one), or &FD after the last frame.
#
# python bin/colstream.py [polygon stream] [out.bin]   writes, then decodes and checks it

import difflib
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "tools"))
from stnicc_stream import parse_stream  # noqa: E402

FLAG_CONTAINS_PALETTE = 2
FRAME_NEXT, FRAME_SKIP_TO_BLOCK, FRAME_END_OF_STREAM = 0xFF, 0xFE, 0xFD
BLOCK = 7680                    # 3 tracks: the player's ring buffer
ROWS, COLUMNS = 100, 32
KD = (0, 1, -1)


def runs(col):
    """[(value, end row)] of a column of 100 values."""
    out = []
    v = col[0]
    for y in range(1, ROWS):
        if col[y] != v:
            out.append((v, y))
            v = col[y]
    out.append((v, ROWS))
    return out


class Writer:
    def __init__(self):
        self.out = bytearray()
        self.ctl = None
        self.left = 0

    def code(self, c):
        if self.left == 0:
            self.ctl = len(self.out)
            self.out.append(0)
            self.left = 4
        self.left -= 1
        self.out[self.ctl] |= c << (2 * self.left)

    def byte(self, b):
        self.out.append(b & 255)

    def new_frame(self):
        self.left = 0


class Reader:
    def __init__(self, data, pos=0):
        self.d = data
        self.p = pos
        self.ctl = 0
        self.left = 0

    def code(self):
        if self.left == 0:
            self.ctl = self.d[self.p]
            self.p += 1
            self.left = 4
        self.left -= 1
        return (self.ctl >> (2 * self.left)) & 3

    def byte(self):
        b = self.d[self.p]
        self.p += 1
        return b


def encode_column(w, prev, col):
    """Code one column (a list of 100 values) against prev ([(value, end, slope)]); returns its runs."""
    r = runs(col)
    sm = difflib.SequenceMatcher(None, [v for v, _, _ in prev], [v for v, _ in r], autojunk=False)
    ops = []
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal":
            ops += [("K", i1 + k, j1 + k) for k in range(i2 - i1)]
            continue
        nrep = min(i2 - i1, j2 - j1) if tag == "replace" else 0
        ops += [("R", i1 + k, j1 + k) for k in range(nrep)]
        ops += [("D",)] * (i2 - i1 - nrep)
        ops += [("I", j) for j in range(j1 + nrep, j2)]
    while ops[-1][0] == "D":
        ops.pop()                               # implied once a run reaches row 100
    new = []
    for op in ops:
        if op[0] == "D":
            w.code(3)
            w.code(1)
            continue
        v, e = r[op[-1]]
        if op[0] == "I":
            n = e - (new[-1][1] if new else 0)
            w.code(3)
            w.code(2)
            w.byte(v)
            if n <= 3:
                w.code(n - 1)
            else:
                w.code(3)
                w.byte(n)
            new.append((v, e, 0))
            continue
        _, e0, d0 = prev[op[1]]
        p = e0 + d0
        dd = e - p
        if e == ROWS:
            for t in (0, 1, -1):
                if p + t >= ROWS:
                    dd = t
                    break
        if op[0] == "R":
            w.code(3)
            w.code(3)
            w.byte(v)
            if dd in KD:
                w.code(KD.index(dd))
            else:
                w.code(3)
                w.byte(e)
        elif dd in KD:
            w.code(KD.index(dd))
        else:
            w.code(3)
            w.code(0)
            w.byte(e)
        new.append((v, e, e - e0))
    assert [x[:2] for x in new] == r
    return new


def decode_column(rd, prev):
    """The player's decoder: one column's runs [(value, end, slope)]."""
    new = []
    i = 0
    last = 0

    def carry(value=None, dd=0, end=None):
        nonlocal i, last
        v, e0, d0 = prev[i]
        i += 1
        e1 = min(e0 + d0 + dd, ROWS) if end is None else end
        new.append((v if value is None else value, e1, e1 - e0))
        last = e1

    while last < ROWS:
        c = rd.code()
        if c < 3:
            carry(dd=KD[c])
            continue
        c = rd.code()
        if c == 0:
            carry(end=rd.byte())
        elif c == 1:
            i += 1
        elif c == 2:
            v = rd.byte()
            n = rd.code() + 1
            if n == 4:
                n = rd.byte()
            last += n
            new.append((v, last, 0))
        else:
            v = rd.byte()
            c = rd.code()
            if c < 3:
                carry(v, dd=KD[c])
            else:
                carry(v, end=rd.byte())
    return new


def column_values(image, c, bg):
    """Byte column c of a 64 x 100 picture of colour indices (bg = cleared) as pair values."""
    def colour(x):
        return 0 if x == bg else x
    return [colour(image[y * 64 + 2 * c]) << 4 | colour(image[y * 64 + 2 * c + 1]) for y in range(ROWS)]


def align_blocks(frames_bytes):
    """Lay out frames (each with its end byte) so none crosses a block: a frame that
    would is moved to the next block, and the frame before it ends with &FE."""
    out = bytearray()
    for k, fb in enumerate(frames_bytes):
        start = len(out)
        if start // BLOCK != (start + len(fb) - 1) // BLOCK:
            out[-1] = FRAME_SKIP_TO_BLOCK
            out += b"\0" * (BLOCK - start % BLOCK)
        out += fb
    out[-1] = FRAME_END_OF_STREAM
    return out


def encode(frames, images, bg):
    frames_bytes = []
    for f, image in zip(frames, images):
        w = Writer()
        w.byte(FLAG_CONTAINS_PALETTE if f.palette else 0)
        if f.palette:
            w.byte(0)
            w.byte(len(f.palette))
            for i, (r, g, b) in f.palette:
                w.byte(i << 4 | r)
                w.byte(g << 4 | b)
        prev = []
        for c in range(COLUMNS):
            prev = encode_column(w, prev, column_values(image, c, bg))
        frames_bytes.append(bytes(w.out) + bytes([FRAME_NEXT]))
    return align_blocks(frames_bytes)


def decode(data):
    """Each frame's (palette entries, 32 columns of runs), as the player reads them."""
    p = 0
    out = []
    while True:
        rd = Reader(data, p)
        flags = rd.byte()
        palette = []
        if flags & FLAG_CONTAINS_PALETTE:
            rd.byte()
            for _ in range(rd.byte()):
                palette.append((rd.byte(), rd.byte()))
        prev = []
        cols = []
        for c in range(COLUMNS):
            prev = decode_column(rd, prev)
            cols.append(prev)
        out.append((palette, cols))
        end = rd.byte()
        p = rd.p
        if end == FRAME_END_OF_STREAM:
            return out
        if end == FRAME_SKIP_TO_BLOCK:
            p = (p // BLOCK + 1) * BLOCK
        else:
            assert end == FRAME_NEXT, (len(out), end)


def main():
    from cpc_colour_map import BG, pictures
    src = sys.argv[1] if len(sys.argv) > 1 else "data/scene1.half.7680.bin"
    dst = sys.argv[2] if len(sys.argv) > 2 else "data/scene1.cols.bin"
    frames = list(parse_stream(open(src, "rb").read()))
    assert not any(c == 0 for f in frames for c, _, _ in f.polys), "colour 0 stands for the background"
    images = [list(img) for img in pictures(frames)]
    data = encode(frames, images, BG)
    # check: decoding gives back every picture, and every palette
    decoded = decode(data)
    assert len(decoded) == len(frames)
    for n, ((palette, cols), f, image) in enumerate(zip(decoded, frames, images)):
        assert len(palette) == len(f.palette), n
        for c, col in enumerate(cols):
            got = []
            y = 0
            for v, e, _ in col:
                got += [v] * (e - y)
                y = e
            assert got == column_values(image, c, BG), (n, c)
    open(dst, "wb").write(data)
    print(f"{dst}: {len(frames)} frames, {len(data)} bytes ({len(data) / len(frames):.0f} a frame), "
          f"{sum(len(c) for _, cols in decoded for c in cols) / len(frames):.0f} runs a frame; decodes to every picture")


if __name__ == "__main__":
    main()
