# Column coherence, inter-frame change and run lengths of the 64-wide pictures, with zlib for reference.
# python tools/colstream/stats.py pics.pkl
import pickle, sys, collections, zlib, statistics as st
P = pickle.load(open(sys.argv[1], "rb"))
def runs(col):
    out=[]; v=col[0]; L=1
    for x in col[1:]:
        if x==v: L+=1
        else: out.append((v,L)); v=x; L=1
    out.append((v,L)); return out
n=len(P)
same_left=0; same_prev2=0; same_prev1=0; tot_cols=0
inter_tokens=0; intra_tokens=0; changed_rows2=0
lens=collections.Counter(); pure=0; mixed=0
mixed_pred=0
for i,(fl,cols) in enumerate(P):
    for c,col in enumerate(cols):
        tot_cols+=1
        r=runs(col); intra_tokens+=len(r)
        for v,L in r:
            lens[min(L,40)]+=1
            a,b=divmod(v,17)
            if a==b: pure+=1
            else:
                mixed+=1
        if c and col==cols[c-1]: same_left+=1
        if i>=2 and col==P[i-2][1][c]: same_prev2+=1
        if i>=1 and col==P[i-1][1][c]: same_prev1+=1
        # inter vs n-2: segments of changed rows, runs within them; unchanged stretches = 1 skip token
        if i>=2:
            ref=P[i-2][1][c]
            y=0; t=0
            while y<100:
                if col[y]==ref[y]:
                    while y<100 and col[y]==ref[y]: y+=1
                    t+=1
                else:
                    v=col[y]
                    while y<100 and col[y]!=ref[y] and col[y]==v: y+=1; changed_rows2+=1
                    t+=1
            inter_tokens+=t
        else: inter_tokens+=len(r)
print("columns: same as left %.1f%%, same as frame n-1 %.1f%%, n-2 %.1f%%" % (100*same_left/tot_cols, 100*same_prev1/tot_cols, 100*same_prev2/tot_cols))
print("intra runs/frame %.0f; inter(n-2) tokens/frame %.0f (skip+run), changed bytes/frame vs n-2 %.0f" % (intra_tokens/n, inter_tokens/n, changed_rows2/n))
print("pure %d mixed %d" % (pure, mixed))
tot=sum(lens.values()); acc=0
print("run length CDF:", " ".join("%d:%.0f%%"%(L,100*(acc:=acc+lens[L])/tot) for L in (1,2,3,4,6,8,12,16,24,32,40)))
# generic compressibility of intra token stream per frame (lower bound-ish reference)
raw=bytearray(); z=0
for fl,cols in P:
    fr=bytearray()
    for col in cols:
        for v,L in runs(col): fr += bytes([v if v<256 else 255, L])
    raw+=fr; z+=len(zlib.compress(bytes(fr),9))
print("token bytes raw %d; zlib per frame %d; zlib whole %d" % (len(raw), z, len(zlib.compress(bytes(raw),9))))
