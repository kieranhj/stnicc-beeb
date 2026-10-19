# -*- mode: makefile-gmake -*-
##########################################################################
##########################################################################

ifeq ($(OS),Windows_NT)

BEEBASM?=bin\beebasm.exe
PYTHON?=python
EXOMIZER?=bin\exomizer.exe

else

BEEBASM?=beebasm
PYTHON?=python
EXOMIZER?=exomizer3

endif

##########################################################################
##########################################################################

SHELLCMD:=$(PYTHON) bin/shellcmd.py

VGIPACKER:=$(PYTHON) ../vgm-packer/vgipacker.py --v3

BUILD:=./build

##########################################################################
##########################################################################

.PHONY:build
build: $(BUILD)/logo_mode1.exo $(BUILD)/intro_theme.vgi $(BUILD)/main_theme.vgi $(BUILD)/outro_theme.vgi
	$(SHELLCMD) mkdir $(BUILD)

#	$(BEEBASM) -D _QUALITY=2 -D _NULA=0 -i stnicc-beeb.asm -v > $(BUILD)/high.txt
#	$(BEEBASM) -D _QUALITY=1 -D _NULA=0 -i stnicc-beeb.asm -v > $(BUILD)/medium.txt
	$(BEEBASM) -D _QUALITY=0 -D _NULA=0 -i stnicc-beeb.asm -v > $(BUILD)/low.txt
	$(BEEBASM) -D _QUALITY=1 -D _NULA=-1 -i stnicc-beeb.asm -v > $(BUILD)/nula.txt
	$(BEEBASM) -D _QUALITY=1 -D _NULA=-1 -D _CPC=-1 -i stnicc-beeb.asm -v > $(BUILD)/cpc.txt

	$(BEEBASM) -i src/intro.asm -v > $(BUILD)/intro.txt
	$(BEEBASM) -i src/outro.asm -v > $(BUILD)/outro.txt
	$(BEEBASM) -i src/music.asm -v > $(BUILD)/music.txt

	$(BEEBASM) -i stnicc-build.asm -do $(BUILD)/part-1.ssd -title BEEB-NICCC-1 -opt 3 -v > compile.txt
	$(BEEBASM) -i src/part-2.asm -title STNICCC-2 -do $(BUILD)/part-2.ssd

	$(PYTHON) bin/dsd_create.py -o $(BUILD)/stnicc-A.dsd $(BUILD)/part-1.ssd $(BUILD)/part-2.ssd

$(BUILD)/intro_theme.vgi : data/intro_test.vgm
	$(SHELLCMD) mkdir $(BUILD)
	$(VGIPACKER) -o "$@" "$<"

$(BUILD)/main_theme.vgi : data/main_test.vgm
	$(SHELLCMD) mkdir $(BUILD)
	$(VGIPACKER) -o "$@" "$<"

$(BUILD)/outro_theme.vgi : data/outro_test.vgm
	$(SHELLCMD) mkdir $(BUILD)
	$(VGIPACKER) -o "$@" "$<"

$(BUILD)/logo_mode1.exo : ./data/BeeBShifters.png
	$(SHELLCMD) mkdir $(BUILD)
	$(PYTHON) bin/png2bbc.py --quiet -o $@.tmp --palette 0436 $< 1
	$(EXOMIZER) level -M256 $@.tmp@0x4E00 -o $@

##########################################################################
##########################################################################

.PHONY:data
data:
	cd data && $(PYTHON) ../bin/encoder.py scene1.bin -o scene1.half.7680.bin -a 7680 -f 2 -b --optimise --capacity 345600 --trim-mode both --mask-mode both > out.txt
	cd data && $(PYTHON) ../bin/split.py scene1.half.7680.bin
	cd data && $(PYTHON) ../tools/cpc_palette.py scene1.half.7680.bin cpc_palette.bin

##########################################################################
##########################################################################

.PHONY:clean
clean:
	$(SHELLCMD) rm-tree $(BUILD)

##########################################################################
##########################################################################

.PHONY:b2_test
b2_test: URL:=http://localhost:48075
b2_test: CONFIG?=Master 128 (MOS 3.20)
b2_test:
	curl -G '$(URL)/reset/b2' --data-urlencode "config=$(CONFIG)"
#	curl -H 'Content-Type:application/vnd.acorn.disc-image.dsd' --upload-file '$(BUILD)/stnicc-B.dsd' '$(URL)/mount/b2?drive=1'
	curl -H 'Content-Type:application/vnd.acorn.disc-image.dsd' --upload-file '$(BUILD)/stnicc-A.dsd' '$(URL)/run/b2'

##########################################################################
##########################################################################

.PHONY:tags
tags:
	/opt/local/bin/ctags --exclude='.#*' --langdef=beebasm --langmap=beebasm:.6502.asm '--regex-beebasm=/^\.(\^|\*)?([A-Za-z0-9_]+)/\2/l,label/' '--regex-beebasm=/^[ \t]*macro[ \t]+([A-Za-z0-9_]+)/\1/m,macro/i' '--regex-beebasm=/^[ \t]*([A-Za-z0-9_]+)[ \t]*=[^=]/\1/v,value/' -eR src lib stnicc-beeb.asm

##########################################################################
##########################################################################

# .dir-locals.el:
# 
#  (nil . ((compile-command . "cd ~/github/stnicc-beeb && make tom_emacs")))

.PHONY:tom_emacs
tom_emacs:
	$(MAKE) tags build tom_beeblink
	-$(MAKE) b2_test "CONFIG=B/Acorn 1770"

##########################################################################
##########################################################################

.PHONY:tom_beeblink
tom_beeblink:
	cp $(BUILD)/stnicc-A.dsd ~/beeb/beeb-files/stuff/ssds/0/d.stnicca
#	cp $(BUILD)/stnicc-B.dsd ~/beeb/beeb-files/stuff/ssds/0/d.stniccb

##########################################################################
##########################################################################

# where your local copy of the jsbeeb repo is held.
JSBEEB?=../jsbeeb

ifeq ($(OS),Windows_NT)

# TODO - Windows

else

# Unix

UNAME:=$(shell uname -s)

ifeq ($(UNAME),Darwin)

# Unix (macOS)

.PHONY:jsbeeb_test
jsbeeb_test:
	cp $(BUILD)/stnicc-A.dsd $(JSBEEB)/discs/
	cp $(BUILD)/stnicc-B.dsd $(JSBEEB)/discs/
	open -a '/Applications/Google Chrome.app' 'http://localhost:8000/?&model=Master&disc1=stnicc-A.dsd&disc2=stnicc-B.dsd&autoboot'

endif

ifeq ($(UNAME),Linux)

# TODO - Unix (Linux)

endif

endif
