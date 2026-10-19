# The column stream, version 2: all 1800 frames, for the 1800-frame column player
# (stnicc-cols2.asm). As bin/colstream.py, each frame's final 64 x 100 picture as 32
# byte columns of runs (value = left colour << 4 | right colour, 0 = background),
# but with a richer code (docs/cpc-column-stream.md, "Next: all 1800 frames").
#
# Each column is coded against a reference column in one of three modes:
#   left    the column to its left in this frame (column 0: a column of background)
#   time    the same column in the previous frame (frame 0: a frame of background)
#   tslope  the same, each run's end moved on by its slope
# A run keeps a slope: its end less the end of the run it carried on from, and
# the kind of the column it was coded in (left, or time for both time modes). A
# run's predicted end is the reference run's end plus its slope when the slope
# is of the mode's kind (left in left mode, time in tslope mode), else just its
# end. New runs have slope 0.
#
# Codes are bits, high first, taken from control bytes as they are needed (a
# control byte is read when a bit is wanted and the last one is used up); literal
# bytes are inline, in the order they are read. Per column: a mode code, then ops
# until a run reaches row 100 (the reference column's runs left over are dropped):
#   K   the reference's next run carries on, its end coded as a delta (below)
#   D   drop the reference's next run
#   I   a new run: a value, then a length (1 '0'; 2-3 '10x'; 4-7 '110xx';
#       8-15 '1110xxx'; else '1111' and a byte)
#   R   the reference's next run carries on in a new value: a value, then a delta
# A delta from the predicted end p (8-bit sums; 100 or more is 100):
#   K0 p, K+ p+1, K- p-1, KJ p +/- m (a sign bit, 1 = minus, then m-2 as
#   '0x', '10xx' or '110xxx'), KL an end row byte
# Each mode has its own op codes (OP_CODES), and R its own delta codes.
#
# A value is two pixels, each a rank in a list of its neighbours' pixels with
# repeats left out ('0', '10', '110') or '111' and the pixel in 4 bits. The
# neighbours are the pixel pairs at the run's first row in the column to the left
# (this frame: LC), the same column and the one to its right in the previous
# frame (PC, PR), and the run above (A); any of them off the picture or above
# row 0 is 0. Left pixel: LC.r PC.l A.l A.r PR.l PC.r LC.l; right pixel: PR.l
# A.r PC.r (the new left pixel) A.l LC.r PC.l.
#
# A frame: three bits, P (a palette change follows) and E (00 the next frame
# follows; 01 it starts the next block, as no frame crosses one; 10 the stream
# ends), then if P the CPC palette change in bytes (a count, then for each colour
# its index, with bit 7 set if dithered, its top line byte and its bottom line
# byte: tools/cpc_palette.py), then the 32 columns. A frame starts with a fresh
# control byte, and the next one starts at the byte after its last.
#
# python bin/colstream2.py [polygon stream] [out.bin]   writes, then decodes and checks it

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "tools"))
from colstream import BLOCK, COLUMNS, ROWS, column_values, runs  # noqa: E402
from stnicc_stream import parse_stream  # noqa: E402

MODE_CODES = {"time": "0", "left": "10", "tslope": "11"}
OP_CODES = {
    "left": {"K0": "00", "K+": "010", "K-": "011", "KJ": "100", "D": "101", "I": "110",
             "KL": "1110", "R": "1111"},
    "time": {"K0": "0", "K+": "100", "K-": "101", "KJ": "1100", "D": "1101", "I": "1110",
             "KL": "11110", "R": "11111"},
    "tslope": {"K0": "00", "KJ": "01", "K+": "100", "K-": "101", "I": "110", "D": "1110",
               "KL": "11110", "R": "11111"},
}
R_CODES = {"K0": "0", "K+": "10", "K-": "110", "KJ": "1110", "KL": "1111"}
RANK_CODES = ("0", "10", "110")
MISS_CODE = "111"
E_NEXT, E_SKIP, E_END = 0, 1, 2

# The player's run ring (stnicc-cols2.asm): each column's runs are kept from where
# the last column's ended, or from 0 if fewer than COLUMN_MAX entries are left
RING = 640
COLUMN_MAX = 36


def jump_code(m):
    """The magnitude code of a jump of m (2 or more), or None if too big."""
    m -= 2
    if m < 2:
        return "0" + format(m, "01b")
    if m < 6:
        return "10" + format(m - 2, "02b")
    if m < 14:
        return "110" + format(m - 6, "03b")
    return None


def length_code(n):
    """(bits, literal byte or None) for a new run of n rows."""
    if n == 1:
        return "0", None
    if n <= 3:
        return "10" + format(n - 2, "01b"), None
    if n <= 7:
        return "110" + format(n - 4, "02b"), None
    if n <= 15:
        return "1110" + format(n - 8, "03b"), None
    return "1111", n


def delta_decode(pred, tok, d=0):
    """The decoder's end row from a predicted end (8 bits) and a delta."""
    v = (pred + {"K0": 0, "K+": 1, "K-": -1, "KJ": d}[tok]) & 255
    return min(v, ROWS)


def best_delta(pred, e, codes):
    """Cheapest (bits, token, jump) giving end e from predicted end pred."""
    best = None
    for tok in ("K0", "K+", "K-"):
        if delta_decode(pred, tok) == e:
            b = len(codes[tok])
            if best is None or b < best[0]:
                best = (b, tok, 0)
    for m in range(2, 16):
        jc = jump_code(m)
        if jc is None:
            break
        for d in (m, -m):
            if delta_decode(pred, "KJ", d) == e:
                b = len(codes["KJ"]) + 1 + len(jc)
                if best is None or b < best[0]:
                    best = (b, "KJ", d)
    b = len(codes["KL"]) + 8
    if best is None or b < best[0]:
        best = (b, "KL", 0)
    return best


def left_list(LC, PC, PR, A):
    return [LC & 15, PC >> 4, A >> 4, A & 15, PR >> 4, PC & 15, LC >> 4]


def right_list(LC, PC, PR, A, left):
    return [PR >> 4, A & 15, PC & 15, left, A >> 4, LC & 15, PC >> 4]


def distinct(cands):
    out = []
    for p in cands:
        if p not in out:
            out.append(p)
    return out


def pixel_code(p, cands):
    d = distinct(cands)[:len(RANK_CODES)]
    if p in d:
        return RANK_CODES[d.index(p)]
    return MISS_CODE + format(p, "04b")


def value_code(v, nb):
    LC, PC, PR, A = nb
    return pixel_code(v >> 4, left_list(LC, PC, PR, A)) + pixel_code(v & 15, right_list(LC, PC, PR, A, v >> 4))


class BitWriter:
    def __init__(self):
        self.out = bytearray()
        self.ctl = 0
        self.left = 0

    def bits(self, s):
        for ch in s:
            if self.left == 0:
                self.ctl = len(self.out)
                self.out.append(0)
                self.left = 8
            self.left -= 1
            if ch == "1":
                self.out[self.ctl] |= 1 << self.left

    def byte(self, b):
        self.out.append(b & 255)


class BitReader:
    def __init__(self, data, pos):
        self.d = data
        self.p = pos
        self.ctl = 0
        self.left = 0

    def bit(self):
        if self.left == 0:
            self.ctl = self.d[self.p]
            self.p += 1
            self.left = 8
        self.left -= 1
        return (self.ctl >> self.left) & 1

    def num(self, n):
        v = 0
        for _ in range(n):
            v = v << 1 | self.bit()
        return v

    def byte(self):
        b = self.d[self.p]
        self.p += 1
        return b

    def token(self, codes):
        s = ""
        inv = {c: t for t, c in codes.items()}
        while s not in inv:
            s += str(self.bit())
            assert len(s) <= 8
        return inv[s]


def pixels(col_runs):
    """100 values from [(value, end, ...)]."""
    out = []
    y = 0
    for x in col_runs:
        out += [x[0]] * (x[1] - y)
        y = x[1]
    return out


BACKGROUND = [(0, ROWS, 0)]          # an off-picture column: (value, end, slope)
BACKGROUND_PIXELS = [0] * ROWS


def predicted(ref, kind, mode):
    """Each reference run's predicted end (8 bits)."""
    use = (mode == "left" and kind == "left") or (mode == "tslope" and kind == "time")
    return [(e + s) & 255 if use else e for _, e, s in ref]


def neighbours(LC, PC, PR, r):
    """(LC, PC, PR, A) at each target run's first row."""
    out = []
    y = 0
    for j, (v, e) in enumerate(r):
        out.append((LC[y], PC[y], PR[y], r[j - 1][0] if j else 0))
        y = e
    return out


def align(ref, preds, r, codes, nbs):
    """Cheapest ops coding target runs r against ref: (bits, ops)."""
    n, m = len(ref), len(r)
    INF = 1 << 30
    starts = [0] + [e for _, e in r[:-1]]
    vbits = [len(value_code(v, nb)) for (v, _), nb in zip(r, nbs)]
    ibits = [len(codes["I"]) + vbits[j] + len(length_code(r[j][1] - starts[j])[0])
             + (8 if length_code(r[j][1] - starts[j])[1] is not None else 0) for j in range(m)]
    cost = [[INF] * (m + 1) for _ in range(n + 1)]
    back = [[None] * (m + 1) for _ in range(n + 1)]
    cost[0][0] = 0
    dbits = len(codes["D"])
    rbits = len(codes["R"])
    for i in range(n + 1):
        ci = cost[i]
        for j in range(m):
            c = ci[j]
            if c >= INF:
                continue
            nb = c + ibits[j]
            if nb < ci[j + 1]:
                ci[j + 1] = nb
                back[i][j + 1] = ("I", i, j)
            if i < n:
                nb = c + dbits
                if nb < cost[i + 1][j]:
                    cost[i + 1][j] = nb
                    back[i + 1][j] = ("D", i, j)
                v, e = r[j]
                if ref[i][0] == v:
                    b, tok, d = best_delta(preds[i], e, codes)
                    op = ("K", i, j, tok, d)
                else:
                    b, tok, d = best_delta(preds[i], e, R_CODES)
                    b += rbits + vbits[j]
                    op = ("R", i, j, tok, d)
                nb = c + b
                if nb < cost[i + 1][j + 1]:
                    cost[i + 1][j + 1] = nb
                    back[i + 1][j + 1] = op
    bi = min(range(n + 1), key=lambda i: cost[i][m])
    ops = []
    i, j = bi, m
    while (i, j) != (0, 0):
        b = back[i][j]
        ops.append(b)
        i, j = b[1], b[2]
    ops.reverse()
    while ops and ops[-1][0] == "D":
        ops.pop()
    return cost[bi][m], ops


def emit_column(w, mode, ref, preds, r, ops, nbs):
    """Write a column's codes; returns its runs [(value, end, slope)]."""
    codes = OP_CODES[mode]
    w.bits(MODE_CODES[mode])
    new = []
    last = 0
    for op in ops:
        if op[0] == "D":
            w.bits(codes["D"])
            continue
        j = op[2]
        v, e = r[j]
        if op[0] == "I":
            w.bits(codes["I"])
            w.bits(value_code(v, nbs[j]))
            bits, lit = length_code(e - last)
            w.bits(bits)
            if lit is not None:
                w.byte(lit)
            new.append((v, e, 0))
        else:
            i, tok, d = op[1], op[3], op[4]
            if op[0] == "R":
                w.bits(codes["R"])
                w.bits(value_code(v, nbs[j]))
                dcodes = R_CODES
            else:
                dcodes = codes
            w.bits(dcodes[tok])
            if tok == "KJ":
                w.bits(("1" if d < 0 else "0") + jump_code(abs(d)))
            elif tok == "KL":
                w.byte(e)
            else:
                assert delta_decode(preds[i], tok) == e
            new.append((v, e, (e - ref[i][1]) & 255))
        last = e
    assert [x[:2] for x in new] == r
    return new


def encode_frame(image, prev_cols, prev_kinds, prev_image, palette, stats):
    """One frame's bytes (E = 00) and its columns' runs and kinds."""
    w = BitWriter()
    w.bits("1" if palette is not None else "0")
    w.bits("00")
    if palette is not None:
        w.byte(len(palette))
        for i, _, top, bottom, delta in palette:
            w.byte(i | (0x80 if delta else 0))
            w.byte(top)
            w.byte(bottom)
    cur, kinds = [], []
    for c in range(COLUMNS):
        col = column_values(image, c, 16)
        r = runs(col)
        LC = column_values(image, c - 1, 16) if c else BACKGROUND_PIXELS
        PC = prev_image[c]
        PR = prev_image[c + 1] if c < COLUMNS - 1 else BACKGROUND_PIXELS
        nbs = neighbours(LC, PC, PR, r)
        best = None
        for mode in ("left", "time", "tslope"):
            if mode == "left":
                ref, kind = (cur[c - 1], kinds[c - 1]) if c else (BACKGROUND, "time")
            else:
                ref, kind = prev_cols[c], prev_kinds[c]
            preds = predicted(ref, kind, mode)
            b, ops = align(ref, preds, r, OP_CODES[mode], nbs)
            b += len(MODE_CODES[mode])
            if best is None or b < best[0]:
                best = (b, mode, ref, preds, ops)
        b, mode, ref, preds, ops = best
        stats[mode] = stats.get(mode, 0) + 1
        cur.append(emit_column(w, mode, ref, preds, r, ops, nbs))
        kinds.append("left" if mode == "left" else "time")
    return bytes(w.out), cur, kinds


def align_blocks(frames_bytes):
    """Lay out frames so none crosses a block; set each frame's E bits."""
    out = bytearray()
    starts = []
    for fb in frames_bytes:
        start = len(out)
        if start // BLOCK != (start + len(fb) - 1) // BLOCK:
            out[starts[-1]] |= E_SKIP << 5
            out += b"\0" * (BLOCK - start % BLOCK)
            start = len(out)
        starts.append(start)
        out += fb
        if len(out) % BLOCK == 0:
            out[start] |= E_SKIP << 5           # it fills its block: the player must wrap
    out[starts[-1]] &= ~(E_SKIP << 5)
    out[starts[-1]] |= E_END << 5
    return out


class Decoder:
    """The player's decoder, run list ring and all: each frame's (palette, 32 columns of runs)."""

    def __init__(self, data):
        self.d = data
        self.val = [0] * RING
        self.end = [0] * RING
        self.slope = [0] * RING
        # the frame before the first: background, a run in each column at 0-31
        self.start = list(range(COLUMNS))
        self.count = [1] * COLUMNS
        self.kind = ["time"] * COLUMNS
        for c in range(COLUMNS):
            self.end[c] = ROWS
        self.next_free = COLUMNS

    def column(self, start, count):
        return [(self.val[(start + k)], self.end[start + k], self.slope[start + k]) for k in range(count)]

    def frames(self):
        p = 0
        while True:
            rd = BitReader(self.d, p)
            pal = rd.bit()
            e_bits = rd.num(2)
            palette = None
            if pal:
                palette = [(rd.byte(), rd.byte(), rd.byte()) for _ in range(rd.byte())]
            prev_start, prev_count, prev_kind = self.start, self.count, self.kind
            prev_pixels = [pixels(self.column(prev_start[c], prev_count[c])) for c in range(COLUMNS)]
            cur_start, cur_count, cur_kind = [], [], []
            live = set()
            for c in range(COLUMNS):
                live |= set(range(prev_start[c], prev_start[c] + prev_count[c]))
            for c in range(COLUMNS):
                mode = rd.token(MODE_CODES)
                codes = OP_CODES[mode]
                if mode == "left":
                    ref = self.column(cur_start[c - 1], cur_count[c - 1]) if c else BACKGROUND
                    kind = cur_kind[c - 1] if c else "time"
                else:
                    ref = self.column(prev_start[c], prev_count[c])
                    kind = prev_kind[c]
                preds = predicted(ref, kind, mode)
                LC = pixels(self.column(cur_start[c - 1], cur_count[c - 1])) if c else BACKGROUND_PIXELS
                PC = prev_pixels[c]
                PR = prev_pixels[c + 1] if c < COLUMNS - 1 else BACKGROUND_PIXELS
                # the ring: start where the last column ended, or at 0 if near the end
                s = self.next_free
                if s > RING - COLUMN_MAX:
                    s = 0
                new = []
                i = 0
                last = 0
                while last < ROWS:
                    tok = rd.token(codes)
                    if tok == "D":
                        i += 1
                        continue
                    if tok in ("I", "R"):
                        A = new[-1][0] if new else 0
                        left = self.read_pixel(rd, left_list(LC[last], PC[last], PR[last], A))
                        right = self.read_pixel(rd, right_list(LC[last], PC[last], PR[last], A, left))
                        v = left << 4 | right
                    if tok == "I":
                        k = 0
                        while k < 4 and rd.bit():
                            k += 1
                        n = rd.byte() if k == 4 else (1, 2, 4, 8)[k] + rd.num(k)
                        e = last + n
                        new.append((v, e, 0))
                    else:
                        rv, re, _ = ref[i]
                        if tok == "R":
                            tok = rd.token(R_CODES)
                        else:
                            v = rv
                        if tok == "KL":
                            e = rd.byte()
                        elif tok == "KJ":
                            neg = rd.bit()
                            k = 0
                            while rd.bit():
                                k += 1
                            m = (2, 4, 8)[k] + rd.num(k + 1)
                            e = delta_decode(preds[i], "KJ", -m if neg else m)
                        else:
                            e = delta_decode(preds[i], tok)
                        new.append((v, e, (e - re) & 255))
                        i += 1
                    last = e
                # this frame's earlier columns, and the previous frame's from this one on, are live
                assert len(new) <= COLUMN_MAX
                for k, (v, e, sl) in enumerate(new):
                    assert s + k not in live, "the ring overwrote a live run"
                    self.val[s + k], self.end[s + k], self.slope[s + k] = v, e, sl
                live -= set(range(prev_start[c], prev_start[c] + prev_count[c]))
                live |= set(range(s, s + len(new)))
                cur_start.append(s)
                cur_count.append(len(new))
                cur_kind.append("left" if mode == "left" else "time")
                self.next_free = s + len(new)
            self.start, self.count, self.kind = cur_start, cur_count, cur_kind
            yield palette, [self.column(cur_start[c], cur_count[c]) for c in range(COLUMNS)]
            p = rd.p
            if e_bits == E_END:
                return
            if e_bits == E_SKIP:
                p = (p + BLOCK - 1) // BLOCK * BLOCK
            else:
                assert e_bits == E_NEXT and p % BLOCK, "the player's ring would not wrap"

    @staticmethod
    def read_pixel(rd, cands):
        d = distinct(cands)
        for k in range(len(RANK_CODES)):
            if not rd.bit():
                return d[k]
        return rd.num(4)


def main():
    from cpc_colour_map import pictures
    from cpc_palette import palette_records
    src = sys.argv[1] if len(sys.argv) > 1 else "data/scene1.full.7680.bin"
    dst = sys.argv[2] if len(sys.argv) > 2 else "data/scene1.cols2.bin"
    frames = list(parse_stream(open(src, "rb").read()))
    assert not any(c == 0 for f in frames for c, _, _ in f.polys), "colour 0 stands for the background"
    records = dict(palette_records(frames))
    images = [list(img) for img in pictures(frames)]
    prev_cols = [list(BACKGROUND) for _ in range(COLUMNS)]
    prev_kinds = ["time"] * COLUMNS
    prev_image = [BACKGROUND_PIXELS] * COLUMNS
    frames_bytes = []
    stats = {}
    for n, image in enumerate(images):
        fb, prev_cols, prev_kinds = encode_frame(image, prev_cols, prev_kinds, prev_image, records.get(n), stats)
        prev_image = [column_values(image, c, 16) for c in range(COLUMNS)]
        frames_bytes.append(fb)
        if n % 100 == 99:
            print(f"  {n + 1} frames, {sum(map(len, frames_bytes))} bytes", file=sys.stderr)
    data = align_blocks(frames_bytes)
    # check: decoding gives back every picture and every palette change
    count = 0
    for n, (palette, cols) in enumerate(Decoder(data).frames()):
        want = records.get(n)
        assert (palette is None) == (want is None), n
        if want:
            assert palette == [(i | (0x80 if d else 0), t, b) for i, _, t, b, d in want], n
        for c, col in enumerate(cols):
            assert pixels(col) == column_values(images[n], c, 16), (n, c)
        count += 1
    assert count == len(frames)
    open(dst, "wb").write(data)
    raw = sum(map(len, frames_bytes))
    pal = sum(1 + 3 * len(r) for r in records.values())
    biggest = max(map(len, frames_bytes))
    print(f"{dst}: {len(frames)} frames, {len(data)} bytes ({raw} of frames, of which {pal} palette; "
          f"{len(data) - raw} block padding), largest frame {biggest}; modes {stats}; decodes to every picture")


if __name__ == "__main__":
    main()
