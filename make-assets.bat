@echo off
mkdir build
echo Building ASSETS...
python bin/png2bbc.py --quiet -o build/logo_mode1.bin --palette 0436 ./data/BeeBShifters.png 1
bin\exomizer.exe level -M256 build/logo_mode1.bin@0x4E00 -o build/logo_mode1.exo
python ..\vgm-packer\vgmpacker.py data/davetest3.vgm -o build/main_theme.vgc
