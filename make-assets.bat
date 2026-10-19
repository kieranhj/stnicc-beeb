@echo off
mkdir build
echo Building ASSETS...
python bin/png2bbc.py --quiet -o build/logo_mode1.bin --palette 0436 ./data/BeeBShifters.png 1
bin\exomizer.exe level -M256 build/logo_mode1.bin@0x4E00 -o build/logo_mode1.exo
python ..\vgm-packer\vgipacker.py --v3 "data/intro_test.vgm" -o build/intro_theme.vgi
python ..\vgm-packer\vgipacker.py --v3 "data/main_test.vgm" -o build/main_theme.vgi
python ..\vgm-packer\vgipacker.py --v3 "data/outro_test.vgm" -o build/outro_theme.vgi
