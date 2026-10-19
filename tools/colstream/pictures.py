# Cache every frame's final 64x100 picture as 32 byte columns: value = left*17 + right,
# colours 0-15, 16 = cleared background. python tools/colstream/pictures.py pics.pkl
import sys, pickle
sys.path.insert(0, "bin")
from stnicc_stream import parse_stream
from stnicc_raster import raster_spans
data = open("data/scene1.half.7680.bin", "rb").read()
out = []
for f in parse_stream(data):
    pic = [[16]*64 for _ in range(100)]       # 16 = background
    for colour, verts, trim in f.polys:
        for y,(s,e) in raster_spans(verts, "nula").items(): pic[y][s:e+1] = [colour]*(e-s+1)
    cols = [[pic[y][2*c]*17 + pic[y][2*c+1] for y in range(100)] for c in range(32)]
    out.append((f.flags, cols))
pickle.dump(out, open(sys.argv[1], "wb"))
