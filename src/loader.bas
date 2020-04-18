MODE7
?&FE00=8:?&FE01=&30
*FX9
*FX10
?&FE20=&10
*FX19
E%=FNTM(5)
?&FE22=&44
*FX19
G%=FNTM(5)
NULA%=E%/G%>0.75
MODE7
REM All three stages need to be separate to account for bank aliasing
tstaddr = &8008
values = &70
unique = &80
RomSel = &FE30
RamSel = &FE32
UsrDat = &FE60
UsrDDR = &FE62
REM Find 16 values distinct from the 16 rom values and each other and save the original rom values
DIM CODE &100
FOR P = 0 TO 2 STEP 2
P%=CODE
[OPT P
SEI
LDY #15        \\ unique values (-1) to find
STY UsrDDR     \\ set user via DDRB low bits as output - required for Solidisk SW RAM
TYA            \\ A can start anywhere less than 256-64 as it just needs to allow for enough numbers not to clash with rom, tst and uninitialised tst values
.next_val
LDX #15        \\ sideways bank
ADC #1         \\ will inc mostly by 2, but doesn't matter
.next_slot
STX RomSel
CMP tstaddr
BEQ next_val
CMP unique,X   \\ doesn't matter that we haven't checked these yet as it just excludes unnecessary values, but is safe
BEQ next_val
DEX
BPL next_slot
STA unique,Y
LDX tstaddr
STX values,Y
DEY
BPL next_val
\\ Try to swap each rom value with a unique test value - top down wouldn't work for Solidisk
LDX #0         \\ count up to allow for Solidisk only having 3 select bits
.swap
STX UsrDat     \\ set Solidisk SWRAM index
STX RamSel     \\ set RamSel incase it is used
STX RomSel     \\ set RomSel as it will be needed to read, but is also sometimes used to select write
LDA unique,X
STA tstaddr
INX            \\ count up to allow for Solidisk only have 3 select bits
CPX #16
BNE swap
\\ count matching values and restore old values - reverse order to swapping is safe
LDY #16
LDX #15
.tst_restore
STX RomSel
LDA tstaddr
CMP unique,X   \\ if it has changed, but is not this value, it will be picked up in a later bank
BNE not_swr
STX UsrDat     \\ set Solidisk SWRAM index
STX RamSel     \\ set RamSel incase it is used
LDA values,X
STA tstaddr
DEY
STX values,Y
.not_swr
DEX
BPL tst_restore
STY values
LDA &F4
STA RomSel     \\ restore original ROM
CLI
RTS
]
NEXT
CALL CODE
PRINT"SWRAM Banks:";16-?&70
IF ?&70 <> 16 THEN FOR X% = ?&70 TO 15 : PRINTX%?&70 : NEXT
IF ?&70 = 16 THEN ?&7F = 255:PRINT"No SWRAM." ELSE PRINT"Using SWRAM Bank:";?&7F:?&7C=&80
IFNULA%:PRINT'"Video NuLA version loading."''"Press N during intro to select NuLA"'"version or original version."''"Press any key or wait 5 seconds.":K%=INKEY500
?&7D=NULA%:?&7E=NULA%
*RUN INTRO
END
DEFFNTM(N%):LOCALI%,T%:TIME=0:FORI%=1TON%:*FX19
NEXT:T%=TIME:=T%


