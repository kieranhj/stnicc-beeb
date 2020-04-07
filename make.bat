@echo off
mkdir build
echo Building HiGH...
bin\beebasm.exe -D _QUALITY=2 -D _NULA=0  -i stnicc-beeb.asm -v > build\high.txt
echo Building MEDIUM...
bin\beebasm.exe -D _QUALITY=1 -D _NULA=0  -i stnicc-beeb.asm -v > build\medium.txt
echo Building LOW...
bin\beebasm.exe -D _QUALITY=0 -D _NULA=0  -i stnicc-beeb.asm -v > build\low.txt
echo Building NULA...
bin\beebasm.exe -D _QUALITY=2 -D _NULA=-1 -i stnicc-beeb.asm -v > build\nula.txt
echo Building INTRO...
bin\beebasm.exe -i src\intro.asm -v > build\intro.txt
echo Building OUTRO...
bin\beebasm.exe -i src\outro.asm -v > build\outro.txt
echo Building MUSIC...
bin\beebasm.exe -i src\music.asm -v > build\music.txt
echo Building DISC 1...
bin\beebasm.exe -i stnicc-build.asm -do build\part-1.ssd -title STNICCC-1 -boot INTRO -v > compile.txt
echo Building DISC 2...
bin\beebasm.exe -i src\part-2.asm -title STNICCC-2 -do build\part-2.ssd
rem echo Building DISC 3...
rem bin\beebasm.exe -i src\part-3.asm -title STNICCC-3 -do build\part-3.ssd
rem echo Building DISC 4...
rem bin\beebasm.exe -i src\part-4.asm -title STNICCC-4 -do build\part-4.ssd
echo Building COMBINED DISCS...
del /Q build\*.dsd
bin\bbcim -interss sd build\part-1.ssd build\part-2.ssd build\stnicc-A.dsd
rem bin\bbcim -interss sd build\part-3.ssd build\part-4.ssd build\stnicc-B.dsd
