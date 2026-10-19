python ..\bin\encoder.py scene1.bin -o scene1.half.7680.bin -a 7680 -f 2 -b --optimise --capacity 345600 --trim-mode both --mask-mode both > out.txt
python ..\bin\split.py scene1.half.7680.bin
python ..\tools\cpc_palette.py scene1.half.7680.bin cpc_palette.bin
rem The column disc's stream (stnicc-cols.asm): from track 20, COLS_FIRST_TRACK in src\cols_disc.h.asm
python ..\bin\colstream.py scene1.half.7680.bin scene1.cols.bin
python ..\bin\split.py scene1.cols.bin 20 scene1_cols
rem The 1800-frame column disc's stream (stnicc-cols2.asm): every frame, unoptimised
python ..\bin\encoder.py scene1.bin -o scene1.full.7680.bin -a 7680 -f 1 -b > out-full.txt
rem from track 20, COLS2_FIRST_TRACK in src\cols2_disc.h.asm
python ..\bin\colstream2.py scene1.full.7680.bin scene1.cols2.bin
python ..\bin\split.py scene1.cols2.bin 20 scene1_cols2
del scene1_cols2.02.bin scene1_cols2.03.bin
