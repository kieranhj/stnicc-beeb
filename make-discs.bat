@echo off
mkdir build
del /Q build\*.*
bin\beebasm.exe -i src\part-2.asm -do build\part-2.ssd
bin\beebasm.exe -i src\part-3.asm -do build\part-3.ssd
bin\beebasm.exe -i src\part-4.asm -do build\part-4.ssd
bin\bbcim -interss sd stnicc-beeb.ssd build\part-2.ssd build\stnicc-A.dsd
bin\bbcim -interss sd build\part-3.ssd build\part-4.ssd build\stnicc-B.dsd
