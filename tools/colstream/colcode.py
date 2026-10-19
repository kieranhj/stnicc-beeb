# Prototype "edge-walked pixel column" stream for the 64x100 players, with a round-trip check.
# Per frame: [flags][palette: count, 2 bytes each] then 64 pixel columns, each coded against the
# column to its left as a list of runs (colour 0-16, end row, slope). 2-bit fields, four to a control
# byte read when needed (LZ4-style), literals inline:
#   00/01/10  K: next run of the left column carries on, end += slope + (0,+1,-1)
#   11 then   00 Kx: as K with its end row in a byte
#             01 D:  drop the left column's next run
#             10 I:  new run: byte colour(16 choices, skipping the colour above)<<4 | length (0: length byte)
#                    (top of a column: colour byte, length byte)
#             11 R:  next run carries on in a new colour: colour byte, then a K field (11: end row byte)
# A run that reaches row 100 ends the column; the left column's remaining runs are dropped.
# python tools/colstream/colcode.py pics.pkl out.bin  (pics.pkl from pictures.py)
import pickle, sys, difflib
sys.path.insert(0, "bin")
from stnicc_stream import parse_stream

def runs(col):
    out=[]; v=col[0]
    for y in range(1,100):
        if col[y]!=v: out.append((v,y)); v=col[y]
    out.append((v,100)); return out

class W:
    def __init__(s): s.out=bytearray(); s.ctl=None; s.left=0
    def field(s,f):
        if s.left==0: s.ctl=len(s.out); s.out.append(0); s.left=4
        s.left-=1; s.out[s.ctl] |= f << (2*s.left)
    def byte(s,b): s.out.append(b & 255)
    def end_frame(s): s.left=0

class R:
    def __init__(s,d,p=0): s.d=d; s.p=p; s.ctl=0; s.left=0
    def field(s):
        if s.left==0: s.ctl=s.d[s.p]; s.p+=1; s.left=4
        s.left-=1; return (s.ctl >> (2*s.left)) & 3
    def byte(s): b=s.d[s.p]; s.p+=1; return b
    def sbyte(s): b=s.byte(); return b-256 if b>127 else b

KD=(0,1,-1)
def enc_dd(w,dd):
    if dd in KD: w.field(KD.index(dd))
    else: w.field(3); w.field(0); w.byte(dd)

def encode_column(w, prev, col):
    r=runs(col)
    sm=difflib.SequenceMatcher(None,[c for c,_,_ in prev],[c for c,_ in r],autojunk=False)
    ops=[]                       # ("K", k, j) / ("R", k, j) / ("D",) / ("I", j)
    for tag,i1,i2,j1,j2 in sm.get_opcodes():
        if tag=="equal":
            ops += [("K",i1+k,j1+k) for k in range(i2-i1)]; continue
        nrep=min(i2-i1,j2-j1) if tag=="replace" else 0
        ops += [("R",i1+k,j1+k) for k in range(nrep)]
        ops += [("D",)]*(i2-i1-nrep)
        ops += [("I",j) for j in range(j1+nrep,j2)]
    while ops[-1][0]=="D": ops.pop()             # implied once a run reaches row 100
    new=[]
    for op in ops:
        if op[0]=="D": w.field(3); w.field(1); continue
        j=op[-1]; c,e=r[j]
        if op[0]=="I":
            last=new[-1][1] if new else 0; n=e-last
            w.field(3); w.field(2)
            if new:
                above=new[-1][0]; idx=c if c<above else c-1
                w.byte(idx<<4 | (n if n<16 else 0))
                if n>=16: w.byte(n)
            else: w.byte(c); w.byte(n)
            new.append((c,e,0)); continue
        _,e0,d0=prev[op[1]]; p=e0+d0
        dd=e-p
        if e==100:
            for t in (0,1,-1):
                if p+t>=100: dd=t; break
        if op[0]=="K":
            if dd in KD: w.field(KD.index(dd))
            else: w.field(3); w.field(0); w.byte(e)
        else:
            w.field(3); w.field(3); w.byte(c)
            if dd in KD: w.field(KD.index(dd))
            else: w.field(3); w.byte(e)
        new.append((c,e,e-e0))
    assert new[-1][1]==100 and [x[:2] for x in new]==r
    return new

def decode_column(rd, prev):
    new=[]; i=0; last=0
    def K(dd, colour=None, end=None):
        nonlocal i,last
        c,e0,d0=prev[i]; i+=1
        e1=min(e0+d0+dd,100) if end is None else end
        new.append((c if colour is None else colour,e1,e1-e0)); last=e1
    while last<100:
        f=rd.field()
        if f<3: K(KD[f]); continue
        g=rd.field()
        if g==0: K(0, end=rd.byte())
        elif g==1: i+=1
        elif g==2:
            if new:
                b=rd.byte(); above=new[-1][0]; idx=b>>4; c=idx if idx<above else idx+1
                n=b&15 or rd.byte()
            else: c=rd.byte(); n=rd.byte()
            last+=n; new.append((c,last,0))
        else:
            c=rd.byte(); f2=rd.field()
            K(KD[f2], c) if f2<3 else K(0, c, rd.byte())
    return new

def expand(lst):
    col=[]; y=0
    for c,e,_ in lst: col += [c]*(e-y); y=e
    return col

if __name__=="__main__":
    P=pickle.load(open(sys.argv[1],"rb"))
    frames=list(parse_stream(open("data/scene1.half.7680.bin","rb").read()))
    w=W(); starts=[]
    for (fl,cols),f in zip(P,frames):
        starts.append(len(w.out))
        w.byte(1 if f.palette else 0)
        if f.palette:
            w.byte(len(f.palette))
            for i,(r_,g,b) in f.palette: w.byte(i<<4|r_); w.byte(g<<4|b)
        prev=[]
        for col in cols:
            for k in (0,1):
                prev=encode_column(w, prev, [v//17 if k==0 else v%17 for v in col])
        w.end_frame()
    data=bytes(w.out)
    # round trip
    rd=R(data)
    for n,(fl,cols) in enumerate(P):
        assert rd.p==starts[n]; rd.left=0
        if rd.byte(): k=rd.byte(); rd.p+=2*k
        prev=[]
        for c,col in enumerate(cols):
            for k in (0,1):
                prev=decode_column(rd, prev)
                assert expand(prev)==[v//17 if k==0 else v%17 for v in col], (n,c,k)
    sizes=[b-a for a,b in zip(starts, starts[1:]+[len(data)])]
    print("round trip OK: %d frames, %d bytes (%.0f/frame, max %d)" % (len(P), len(data), len(data)/len(P), max(sizes)))
    open(sys.argv[2],"wb").write(data)
