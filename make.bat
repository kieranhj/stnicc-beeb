@echo off
bin\beebasm.exe -i src\stnicc-high.asm -v > build\high.txt
bin\beebasm.exe -i src\stnicc-medium.asm -v > build\medium.txt
bin\beebasm.exe -i src\stnicc-low.asm -v > build\low.txt
bin\beebasm.exe -i src\stnicc-nula.asm -v > build\nula.txt
bin\beebasm.exe -i stnicc-build.asm -do build\part-1.ssd -title STNICCC-1 -boot HIGH -v > compile.txt
call make-discs.bat
