# Size model of a v2 column code for all 1800 frames (docs/cpc-column-stream.md, "Next"):
# per-column modes, variable-length codes, decoder-exact slopes; then how predictable new runs'
# values are (move-to-front, neighbour pixels).  python tools/colstream/v2model.py cols1800.pkl
# Model of a v2 column code for all 1800 frames: per-column modes (left / time / time+motion),
# variable-length codes. Decoder-exact slope state: each run carries a spatial and a temporal
# slope, known only from how it was coded (else 0).
import sys, difflib, collections, math, pickle
cols=pickle.load(open(sys.argv[1],"rb"))
N=len(cols)
def align_ops(ref, r):
    sm=difflib.SequenceMatcher(None,[x[0] for x in ref],[v for v,_ in r],autojunk=False)
    ops=[]
    for tag,i1,i2,j1,j2 in sm.get_opcodes():
        if tag=="equal": ops+=[("K",i1+k,j1+k) for k in range(i2-i1)]; continue
        nrep=min(i2-i1,j2-j1) if tag=="replace" else 0
        ops+=[("R",i1+k,j1+k) for k in range(nrep)]+[("D",)]*(i2-i1-nrep)+[("I",j) for j in range(j1+nrep,j2)]
    while ops and ops[-1][0]=="D": ops.pop()
    return ops
stats=collections.defaultdict(collections.Counter)
def code_column(ref, r, mode, record):
    """ref: [(v,e,ss,st)]; returns (events, new list). Events for costing."""
    ops=align_ops(ref,r); new=[]; ev=[]; last=0
    for op in ops:
        if op[0]=="D": ev.append(("D",)); continue
        v,e=r[op[-1]]
        if op[0]=="I":
            n=e-last; ev.append(("I",v,n))
            new.append((v,e,0,0)); last=e; continue
        rv,re,rss,rst=ref[op[1]]
        pred = re + (rss if mode=="left" else rst if mode=="tslope" else 0)
        d=e-pred
        if e==100:
            for t in (0,1,-1):
                if pred+t>=100: d=t; break
        ev.append((op[0],d, v if op[0]=="R" else None, e))
        sl=e-re
        new.append((v,e, sl if mode=="left" else 0, sl if mode!="left" else 0)); last=e
    return ev,new
def bits_for(ev):
    b=0
    for x in ev:
        if x[0]=="D": b+=5; continue
        if x[0]=="I":
            b+=4+8; n=x[2]
            b+= 1 if n==1 else 3 if n<=3 else 5 if n<=7 else 5+7   # 0 / 10x / 110xx / 111+7 bits
            continue
        d=x[1]
        if x[0]=="R": b+=5+8
        b+= 1 if d==0 else 3 if d in (1,-1) else 3+ (5 if 2<=abs(d)<=9 else 5+7)   # K0 '0', K+-1 '10s', Kx '110'+sign+3 bits (2..9) or escape
    return b
total_bits=0; modes=collections.Counter(); kx=collections.Counter(); ilen=collections.Counter()
prevframe=None
for n in range(N):
    cur=[]
    for c in range(32):
        r=cols[n][c]
        cands=[]
        for mode in ("left","time","tslope"):
            if mode=="left": ref=cur[c-1] if c else []
            else:
                if prevframe is None: continue
                ref=prevframe[c]
            ev,new=code_column(ref,r,mode,False)
            cands.append((bits_for(ev)+(1 if mode=="left" else 2), mode, ev, new))
        b,mode,ev,new=min(cands,key=lambda t:t[0])
        total_bits+=b; modes[mode]+=1
        for x in ev:
            if x[0] in ("K","R") and abs(x[1])>1: kx[max(-12,min(12,x[1]))]+=1
            if x[0]=="I": ilen[min(x[2],12)]+=1
        cur.append(new)
    prevframe=cur
size=total_bits/8 + 3*N
print("v2 model: %.0f bytes (%.0f/frame)" % (size, size/N), dict(modes))
print("jumps:", dict(sorted(kx.items())))
print("I lengths:", dict(sorted(ilen.items())))

# value prediction: move-to-front list of values coded in this frame (I and R values, and every run value as columns are coded)
hits=collections.Counter(); tot=0
prevframe=None
for n in range(N):
    cur=[]; mtf=[]
    for c in range(32):
        r=cols[n][c]
        cands=[]
        for mode in ("left","time","tslope"):
            if mode=="left": ref=cur[c-1] if c else []
            else:
                if prevframe is None: continue
                ref=prevframe[c]
            ev,new=code_column(ref,r,mode,False)
            cands.append((bits_for(ev)+(1 if mode=="left" else 2), mode, ev, new))
        b,mode,ev,new=min(cands,key=lambda t:t[0])
        for x in ev:
            v = x[1] if x[0]=="I" else (x[2] if x[0]=="R" else None)
            if v is None: continue
            tot+=1
            if v in mtf: hits[min(mtf.index(v),16)]+=1
            else: hits["miss"]+=1
            if v in mtf: mtf.remove(v)
            mtf.insert(0,v)
        for v,_,_,_ in new:
            if v in mtf: mtf.remove(v)
            mtf.insert(0,v)
        cur.append(new)
    prevframe=cur
acc=0
print("I/R values:", tot, "MTF position CDF:", " ".join(f"{k}:{100*(acc:=acc+hits[k])/tot:.0f}%" for k in list(range(17))), "miss %.0f%%"%(100*hits['miss']/tot))

def pix(runs):
    out=[]; y=0
    for x in runs:
        v,e=x[0],x[1]; out+=[v]*(e-y); y=e
    return out
P=collections.Counter(); tot=0
prevframe=None
for n in range(N):
    cur=[]
    for c in range(32):
        r=cols[n][c]
        cands=[]
        for mode in ("left","time","tslope"):
            if mode=="left": ref=cur[c-1] if c else []
            else:
                if prevframe is None: continue
                ref=prevframe[c]
            ev,new=code_column(ref,r,mode,False)
            cands.append((bits_for(ev)+(1 if mode=="left" else 2), mode, ev, new))
        b,mode,ev,new=min(cands,key=lambda t:t[0])
        leftcol=pix(cur[c-1]) if c else None
        prevcol=pix(prevframe[c]) if prevframe else None
        rightprev=pix(prevframe[c+1]) if prevframe and c<31 else None
        y=0; above=None
        evi=iter(ev)
        for x in ev:
            if x[0]=="D": continue
            if x[0]=="I": v=x[1]; e=y+x[2]
            else: e=x[3]; v=x[2] if x[0]=="R" else None
            if v is not None:
                L,R=v>>4,v&15; tot+=1
                cl={}
                if leftcol: cl["left col R"]=leftcol[y]&15
                if prevcol: cl["prev L"]=prevcol[y]>>4
                if above is not None: cl["above L"]=above>>4; cl["above R"]=above&15
                cr={}
                if prevcol: cr["prev R"]=prevcol[y]&15
                if rightprev: cr["right prev L"]=rightprev[y]>>4
                if above is not None: cr["above R"]=above&15; cr["above L"]=above>>4
                if leftcol: cr["left col R"]=leftcol[y]&15
                for k,p in cl.items(): P[("L",k)]+= (p==L)
                for k,p in cr.items(): P[("R",k)]+= (p==R)
                P["L==R"]+= (L==R)
                P[("L any", )]+= (L in cl.values())
                P[("R any",)]+= (R in cr.values())
            above = v if v is not None else (pix(new)[y] if True else None)
            y=e
        cur.append(new)
    prevframe=cur
print("values:", tot)
for k,v in sorted(P.items(), key=lambda t:-t[1]): print(k, "%.0f%%"%(100*v/tot))
