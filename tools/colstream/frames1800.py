# All 1800 frames: their 64x100 pictures' byte columns (cached to a pickle for v2model.py), and the
# ops of the best of left-column / previous-frame / previous-frame-with-motion prediction per column.
# python bin/encoder.py data/scene1.bin -o full.bin -a 7680 -f 1 -b   (from data/: an unoptimised stream)
# python tools/colstream/frames1800.py full.bin cols1800.pkl
import sys, difflib, collections, math, pickle
sys.path[:0]=["bin","tools"]
import colstream as C
from stnicc_stream import parse_stream
from cpc_colour_map import pictures, BG
frames=list(parse_stream(open(sys.argv[1],"rb").read()))
imgs=[list(i) for i in pictures(frames)]
N=len(imgs)
cols=[[C.runs(C.column_values(img,c,BG)) for c in range(32)] for img in imgs]
pickle.dump(cols,open(sys.argv[2],"wb"))
class Count(C.Writer):
    def __init__(s): super().__init__(); s.codes=[]; s.lits=0
    def code(s,c): s.codes.append(c); super().code(c)
    def byte(s,b): s.lits+=1; super().byte(b)
def tslopes(older, newer):
    sm=difflib.SequenceMatcher(None,[v for v,_ in older],[v for v,_ in newer],autojunk=False)
    out=[(v,e,0) for v,e in newer]
    for tag,i1,i2,j1,j2 in sm.get_opcodes():
        if tag=="equal":
            for k in range(i2-i1): out[j1+k]=(newer[j1+k][0],newer[j1+k][1],newer[j1+k][1]-older[i1+k][1])
    return out
def expand(r):
    col=[]; y=0
    for v,e in r: col+=[v]*(e-y); y=e
    return col
ops=collections.Counter(); lits=0; modes=collections.Counter()
for n in range(N):
    prevleft=[]
    for c in range(32):
        r=cols[n][c]; col=expand(r)
        cands={}
        w=Count(); newleft=C.encode_column(w,prevleft,col); cands["left"]=w
        if n>=1: w=Count(); C.encode_column(w,[(v,e,0) for v,e in cols[n-1][c]],col); cands["time"]=w
        if n>=2:
            w=Count(); C.encode_column(w,tslopes(cols[n-2][c],cols[n-1][c]),col); cands["tslope"]=w
        if n>=2 and cols[n-2][c]==r: modes["skip"]+=1; prevleft=newleft; continue
        k=min(cands,key=lambda k:len(cands[k].out)); w=cands[k]; modes[k]+=1
        # tokenise codes into ops
        cs=w.codes; i=0
        while i<len(cs):
            a=cs[i]; i+=1
            if a<3: ops[("K","0","+1","-1")[a+1] if False else ("K0","K+1","K-1")[a]]+=1; continue
            b=cs[i]; i+=1
            if b==0: ops["Kx"]+=1
            elif b==1: ops["D"]+=1
            elif b==2: ops["I"]+=1; i+=1
            else: ops["R"]+=1; i+=1
        lits+=w.lits
        prevleft=newleft
tot=sum(ops.values())
print("modes", dict(modes))
print("ops", {k:v for k,v in ops.most_common()})
H=-sum(v*math.log2(v/tot) for v in ops.values())/8
print("op entropy %.0f bytes, literals %d bytes, mode bits ~%.0f bytes" % (H, lits, 2*32*N/8))
