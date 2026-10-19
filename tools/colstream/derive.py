# How close the 64-wide picture is to LOW's 128-wide one subsampled (could CPC ride on a recooked LOW?).
# python tools/colstream/derive.py
import sys
sys.path.insert(0, "bin")
from stnicc_stream import parse_stream
from stnicc_raster import raster_spans
frames=list(parse_stream(open("data/scene1.half.7680.bin","rb").read()))
best=None; tot=0; m={0:0,1:0,"either":0}; runs=0
for f in frames:
    lo=[[16]*128 for _ in range(100)]; nu=[[16]*64 for _ in range(100)]
    for colour, verts, trim in f.polys:
        for y,(s,e) in raster_spans(verts,"low").items(): lo[y][s:e+1]=[colour]*(e-s+1)
        for y,(s,e) in raster_spans(verts,"nula").items(): nu[y][s:e+1]=[colour]*(e-s+1)
    for y in range(100):
        prevbad=False
        for x in range(64):
            tot+=1; a=nu[y][x]==lo[y][2*x]; b=nu[y][x]==lo[y][2*x+1]
            m[0]+=a; m[1]+=b; m["either"]+=a or b
            bad=not a
            if bad and not prevbad: runs+=1
            prevbad=bad
print({k:"%.2f%%"%(100*v/tot) for k,v in m.items()}, "mismatch runs vs even pixel per frame %.0f" % (runs/len(frames)))
