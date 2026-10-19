# How close each track read comes to starving the decoder, from a harness
# --stream-log. For every read: how long it took, and its budget - from the
# read's start to the frame start at which the decoder would lock because the
# reader is within STREAM_pages_ahead pages of the ring slot being loaded.
# The reader moves through data already in the ring at the same pace however
# long the read takes, so the budget comes from the frame starts of the same run.
#
# node tools/harness/run.mjs --no-inject --stream-log s.json
# python tools/stream_margin.py s.json

import json
import sys

PAGES_AHEAD = 6
CYCLES_PER_MS = 2000


def main():
    log = json.load(open(sys.argv[1]))
    ring_lo, ring_hi = log["ring"]
    ring = ring_hi - ring_lo
    frames = log["frames"]
    locks = log["locks"]
    rows = []
    fi = 0
    for n, ld in enumerate(log["loads"]):
        start, end = ld["start"], ld["end"]
        # a lock during the read is the decoder starving: the budget ran out when it began
        stall = [(max(a, start), min(b, end)) for a, b in locks if a < end and b > start]
        if stall:
            deadline = stall[0][0]
        else:
            while fi < len(frames) and frames[fi][0] < start:
                fi += 1
            deadline = None
            for t, hi, _ in frames[fi:]:
                ahead = (ld["slot"] - hi) % ring or ring
                if ahead < PAGES_AHEAD:
                    deadline = t
                    break
        prev = log["loads"][n - 1] if n else None
        flip = prev is not None and prev["drive"] != ld["drive"]
        stalled = sum(b - a for a, b in stall)
        rows.append(dict(ld=ld, deadline=deadline, flip=flip, stalled=stalled))

    ms = lambda c: c / CYCLES_PER_MS
    pct = lambda v, p: v[min(len(v) - 1, int(p * len(v)))]
    played = [r for r in rows if r["deadline"] is not None]
    print(f"{len(rows)} track reads ({len(played)} with a deadline inside the run)")
    for r in rows:
        ld = r["ld"]
        if r["flip"] or r["stalled"]:
            took = ms(ld["end"] - ld["start"])
            budget = ms(r["deadline"] - ld["start"]) if r["deadline"] else float("nan")
            what = "side switch" if r["flip"] else "stall"
            print(f"{what}: drive {ld['drive']} track {ld['track']}: read {took:.0f} ms, "
                  f"budget {budget:.0f} ms, decoder stalled {ms(r['stalled']):.0f} ms")
    took = sorted(ms(r["ld"]["end"] - r["ld"]["start"]) for r in rows)
    budgets = sorted(ms(r["deadline"] - r["ld"]["start"]) for r in played)
    print(f"read time ms: min {took[0]:.0f}  median {pct(took, .5):.0f}  p99 {pct(took, .99):.0f}  max {took[-1]:.0f}")
    print(f"budget ms:    min {budgets[0]:.0f}  p1 {pct(budgets, .01):.0f}  median {pct(budgets, .5):.0f}")
    print(f"decoder locked {ms(sum(b - a for a, b in locks)):.0f} ms in all, "
          f"{ms(sum(r['stalled'] for r in rows)):.0f} ms of it during reads")

if __name__ == "__main__":
    main()
