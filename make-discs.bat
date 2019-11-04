@echo off
del /Q build\*.*
..\..\Bin\beebasm.exe -i part-2.asm -do build\part-2.ssd
..\..\Bin\beebasm.exe -i part-3.asm -do build\part-3.ssd
..\..\Bin\beebasm.exe -i part-4.asm -do build\part-4.ssd
..\..\Tools\bbcim -interss sd stnicc-beeb.ssd build\part-2.ssd build\stnicc-A.dsd
..\..\Tools\bbcim -interss sd build\part-3.ssd build\part-4.ssd build\stnicc-B.dsd
