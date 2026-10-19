# How well the column to the left predicts each byte column's runs (match rate, end-row delta, delta of delta).
# python tools/colstream/colpred.py pics.pkl
import pickle, sys, collections, difflib
P = pickle.load(open(sys.argv[1], "rb"))
def runs(col):
    out=[]; v=col[0]; s=0
    for y in range(1,100):
        if col[y]!=v: out.append((v,y)); v=col[y]   # (value, end row exclusive)
    out.append((v,100)); return out
d=collections.Counter(); ins=0; dele=0; match=0; ins_pure=0
# second-order: delta vs previous column's delta for the same boundary (slope continuation)
d2=collections.Counter()
for fl,cols in P:
    prev=None; prevd={}
    for c,col in enumerate(cols):
        r=runs(col)
        if prev is None: ins+=len(r); prev=r; continue
        sm=difflib.SequenceMatcher(None,[v for v,_ in prev],[v for v,_ in r],autojunk=False)
        newd={}
        for tag,i1,i2,j1,j2 in sm.get_opcodes():
            if tag=="equal":
                for k in range(i2-i1):
                    e0=prev[i1+k][1]; e1=r[j1+k][1]
                    dd=e1-e0; d[max(-9,min(9,dd))]+=1; match+=1
                    pd=prevd.get(i1+k)
                    if pd is not None: d2[max(-9,min(9,dd-pd))]+=1
                    newd[j1+k]=dd
            else:
                ins+=j2-j1; dele+=i2-i1
                ins_pure+=sum(1 for v,_ in r[j1:j2] if v//17==v%17)
        prev=r; prevd=newd
tot=match+ins
print("tokens %d: matched to left column %.1f%%, inserted %.1f%% (pure %d), deleted %d" % (tot,100*match/tot,100*ins/tot,ins_pure,dele))
print("end-row delta vs left column:", {k:"%.1f%%"%(100*v/match) for k,v in sorted(d.items())})
m2=sum(d2.values())
print("delta-of-delta:", {k:"%.1f%%"%(100*v/m2) for k,v in sorted(d2.items())})
