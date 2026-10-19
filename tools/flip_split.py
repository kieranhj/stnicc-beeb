# Where to end disc 1: the time the decoder can run on the ring while the
# first read of side 2 is in progress, for each possible length of disc 1.
#
# The read of stream track k (the first on side 2) can start once the reader
# has left the ring slot it fills (track k-3), and the decoder locks when the
# reader comes within STREAM_pages_ahead pages of that slot. So the budget is
# the time the reader takes from stream offset (k-2)*2560 to k*2560 - 6*256.
# Frame times come from a harness --stream-log run with its decoder stalls
# taken out; frame offsets from the stream file. The budget for a split moves
# a little once the stream is re-encoded to fit it, so check the winner with
# the harness (tools/stream_margin.py).
#
# python tools/flip_split.py data/scene1.half.7680.bin s.json [first last]

import json
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin"))
import stnicc_stream  # noqa: E402

TRACK = 2560
PAGES_AHEAD = 6
CYCLES_PER_MS = 2000


def frame_starts(data):
    """Stream offset of each frame: parse_stream, made to note where each starts."""
    starts = []
    src = open(stnicc_stream.__file__).read()
    src = src.replace("        start = p\n", "        start = p\n        STARTS.append(p)\n")
    ns = {"STARTS": starts}
    exec(compile(src, stnicc_stream.__file__, "exec"), ns)
    for _ in ns["parse_stream"](data):
        pass
    return starts


def main():
    data = open(sys.argv[1], "rb").read()
    log = json.load(open(sys.argv[2]))
    lo, hi = (int(a) for a in sys.argv[3:5]) if len(sys.argv) > 4 else (40, 60)

    starts = frame_starts(data)
    # frame start times with the decoder's stalls taken out
    times = []
    for t, _, _ in log["frames"][:len(starts)]:
        times.append(t - sum(min(b, t) - a for a, b in log["locks"] if a < t))

    def time_at(offset):
        """When the reader starts the first frame at or past offset."""
        return next((t for o, t in zip(starts, times) if o >= offset), None)

    print("disc 1 tracks  budget ms")
    for k in range(lo, hi + 1):
        a, b = time_at((k - 2) * TRACK), time_at(k * TRACK - PAGES_AHEAD * 256)
        if a is not None and b is not None:
            print(f"{k:13d}  {(b - a) / CYCLES_PER_MS:9.0f}")


if __name__ == "__main__":
    main()
