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
