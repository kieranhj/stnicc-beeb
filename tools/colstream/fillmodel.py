# Byte-run statistics of the 900-frame pictures and a store-cost model of fill designs
# (cached to byteruns.pkl in the given directory).  python tools/colstream/fillmodel.py dir
# Byte-run statistics of the final CPC pictures and a store-cost model of fill designs.
import sys, pickle, os, collections
sys.path[:0]=["bin","tools"]
from stnicc_stream import parse_stream
from cpc_colour_map import pictures, sequence_maps, PATTERNS, dithered, BG
S=sys.argv[1]
cache=os.path.join(S,"byteruns.pkl")
if os.path.exists(cache): frames_runs=pickle.load(open(cache,"rb"))
else:
    frames=list(parse_stream(open("data/scene1.half.7680.bin","rb").read()))
    maps=sequence_maps(frames); m=None; frames_runs=[]
    for n,(f,img) in enumerate(zip(frames,pictures(frames))):
        if n in maps: m=maps[n]
        def key(c): return ("bg",) if c==BG else PATTERNS[m[c]]
        def dith(c): return c!=BG and dithered(PATTERNS[m[c]])
        cols=[]
        for c in range(32):
            runs=[]; y=0
            while y<100:
                l,r=img[y*64+2*c],img[y*64+2*c+1]; y0=y
                while y<100 and img[y*64+2*c]==l and img[y*64+2*c+1]==r: y+=1
                runs.append((y0,y,dith(l) or dith(r)))
            cols.append(runs)
        frames_runs.append(cols)
    pickle.dump(frames_runs,open(cache,"wb"))
N=len(frames_runs)
runs=[r for f in frames_runs for c in f for r in c]
L=collections.Counter(min(e-s,20) for s,e,_ in runs)
print("byte runs/frame %.0f, dithered %.0f%%" % (len(runs)/N, 100*sum(d for *_,d in runs)/len(runs)))
tot=len(runs); acc=0
print("length CDF:", " ".join(f"{k}:{100*(acc:=acc+L[k])/tot:.0f}%" for k in (1,2,3,4,6,8,12,16,20)))
def model(entry, row_unrolled, short_max, direct_base, direct_row):
    cyc=0
    for f in frames_runs:
        for col in f:
            cyc+=30
            for s,e,d in col:
                n=e-s
                if n<=short_max:
                    cyc+=direct_base+direct_row*n
                else:
                    passes=1+((1 if (s%2==0 or n>1) else 0)+(1 if (s%2==1 or n>1) else 0) if d else 1)
                    cyc+=passes*entry+row_unrolled*n*2*1.0
    return cyc/N
for name,args in [("unrolled passes, exit patch", (65,5,0,0,0)),
                  ("unrolled, cheaper entry (45)", (45,5,0,0,0)),
                  ("direct (p),Y stores only", (0,0,100,25,22)),
                  ("hybrid: direct for <=3 rows", (65,5,3,25,22)),
                  ("hybrid: direct for <=6 rows", (65,5,6,25,22))]:
    print(f"{name:32s} {model(*args):7.0f} cycles/frame for the stores")
