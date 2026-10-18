# Group a harness --profile file into render stages by address range, using the
# build's symbol table (beebasm -d) for the range boundaries.
# python tools/profile_stages.py build/profile.txt
import re, subprocess, sys

prof = open(sys.argv[1]).read().split("\n")
i = prof.index("pc cycles %")
# the per-pc section is truncated to the top 300; re-derive totals from labels instead
labels = [l.split() for l in prof[1:i - 1] if l]
total = sum(float(c) for _, c, _ in labels)

out = subprocess.run(["../beebasm/beebasm.exe", "-D", "_QUALITY=0", "-D", "_NULA=0", "-i", "stnicc-beeb.asm", "-d"],
                     capture_output=True, text=True).stdout
syms = {m[0]: int(m[1]) for m in re.findall(r"'([A-Za-z_0-9]+)':(-?\d+)L", out)}
lst = open("build/low.txt").read().split("\n")
label_addr = {}
cur = None
for line in lst:
    m = re.match(r"^\.(\^?[A-Za-z_][A-Za-z0-9_]*)", line)
    if m:
        cur = m[1].lstrip("^")
    a = re.match(r"^\s+([0-9A-F]{4})\s{3}", line)
    if a and cur and cur not in label_addr:
        label_addr[cur] = int(a[1], 16)

stages = [  # (name, first symbol of the range); a label belongs to the last stage starting at or before it
    ("parse", "parse_frame"),
    ("palette", "handle_beeb_palette"),
]
def stage_of(addr):
    if addr >= 0x900 and addr < 0xA00 or addr >= 0xC90 and addr < 0xD00:
        return "edges (up, relocated)"
    for name, lo, hi in ranges:
        if lo <= addr < hi:
            return name
    return "other"
ranges = []
def add(name, a, b):
    if a in syms and b in syms:
        ranges.append((name, syms[a], syms[b]))
add("parse", "parse_frame", "return_here_from_plot_poly")
add("parse", "return_here_from_plot_poly", "plot_long_span")
add("long span", "plot_long_span", "plot_poly_span" if "fast_plot_spans" not in syms else "fast_plot_spans")
add("span loop", "fast_plot_spans", "plot_short_span")
add("span loop", "plot_poly_span", "plot_short_span")
add("short span", "plot_short_span", "init_span_buffer")
add("edges", "plot_poly_span", "handle_beeb_palette")
add("edges", "drawline_into_span_buffer", "handle_beeb_palette")
add("palette", "handle_beeb_palette", "span_row_table_screen1_LO")
add("unrolled fill", "span_screen1_row0_unrolled", "screen_cls")
add("cls", "screen_cls", "data_start")
acc = {}
for name, c, _ in labels:
    a = label_addr.get(name.lstrip("^"))
    st = stage_of(a) if a is not None else "other"
    acc[st] = acc.get(st, 0) + float(c)
for k, v in sorted(acc.items(), key=lambda kv: -kv[1]):
    print(f"{k:24s} {v / 1e6:8.1f}M {100 * v / total:5.1f}% {v / 900:9.0f}/frame")
