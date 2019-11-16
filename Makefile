# -*- mode: makefile-gmake -*-
##########################################################################
##########################################################################

ifeq ($(OS),Windows_NT)

BEEBASM?=bin\beebasm.exe
PYTHON?=python

else

BEEBASM?=beebasm
PYTHON?=python

endif

##########################################################################
##########################################################################

SHELLCMD:=$(PYTHON) bin/shellcmd.py

BUILD:=./build

##########################################################################
##########################################################################

.PHONY:build
build:
	$(SHELLCMD) mkdir $(BUILD)

	$(BEEBASM) -i stnicc-beeb.asm -do $(BUILD)/stnicc-beeb.ssd -boot STNICC -v 1> compile.txt

	$(BEEBASM) -i src/part-2.asm -do $(BUILD)/part-2.ssd
	$(BEEBASM) -i src/part-3.asm -do $(BUILD)/part-3.ssd
	$(BEEBASM) -i src/part-4.asm -do $(BUILD)/part-4.ssd

	$(PYTHON) bin/dsd_create.py -o $(BUILD)/stnicc-A.dsd $(BUILD)/stnicc-beeb.ssd $(BUILD)/part-2.ssd
	$(PYTHON) bin/dsd_create.py -o $(BUILD)/stnicc-B.dsd $(BUILD)/part-3.ssd $(BUILD)/part-4.ssd

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
	curl -H 'Content-Type:application/vnd.acorn.disc-image.dsd' --upload-file '$(BUILD)/stnicc-B.dsd' '$(URL)/mount/b2?drive=1'
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
	cp $(BUILD)/stnicc-B.dsd ~/beeb/beeb-files/stuff/ssds/0/d.stniccb
