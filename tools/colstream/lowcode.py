# LOW's 128-wide picture through the same column coder, for size.
# python tools/colstream/lowcode.py tools/colstream
import sys, pickle
sys.path.insert(0, "bin"); sys.path.insert(0, sys.argv[1])
from stnicc_stream import parse_stream
from stnicc_raster import raster_spans
from colcode import W, encode_column
frames=list(parse_stream(open("data/scene1.half.7680.bin","rb").read()))
w=W(); diff=0
for f in frames:
    w.byte(0)
    if f.palette: w.byte(len(f.palette)); [w.byte(0) for _ in range(2*len(f.palette))]
    pic=[[16]*128 for _ in range(100)]
    for colour, verts, trim in f.polys:
        for y,(s,e) in raster_spans(verts,"low").items(): pic[y][s:e+1]=[colour]*(e-s+1)
    prev=[]
    for x in range(128): prev=encode_column(w, prev, [pic[y][x] for y in range(100)])
    w.end_frame()
print("LOW 128-wide pixel columns: %d bytes (%.0f/frame)" % (len(w.out), len(w.out)/len(frames)))
