@echo off
mkdir build
echo Building ASSETS...
python bin/png2bbc.py --quiet -o build/logo_mode1.bin --palette 0436 ./data/BeeBShifters.png 1
bin\exomizer.exe level -M256 build/logo_mode1.bin@0x4E00 -o build/logo_mode1.exo
python ..\vgm-packer\vgmpacker.py "data/intro_test.vgm" -o build/intro_theme.vgc
python ..\vgm-packer\vgmpacker.py data/STNICCC_BBC_Rhino_06_combined.vgm -o build/main_theme.vgc
python ..\vgm-packer\vgmpacker.py "data/outro_test.vgm" -o build/outro_theme.vgc
