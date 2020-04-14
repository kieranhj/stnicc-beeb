   10 REM All three stages need to be separate to account for bank aliasing
   20 tstaddr = &8008
   30 values = &70
   40 unique = &80
   50 RomSel = &FE30
   60 RamSel = &FE32
   70 UsrDat = &FE60
   80 UsrDDR = &FE62
   90 REM Find 16 values distinct from the 16 rom values and each other and save the original rom values
  100 DIM CODE &100
  110 FOR P = 0 TO 2 STEP 2
  120 P%=CODE
  130 [
  140 OPT P
  150     SEI
  160     LDY #15        \\ unique values (-1) to find
  170     STY UsrDDR     \\ set user via DDRB low bits as output - required for Solidisk SW RAM
  180     TYA            \\ A can start anywhere less than 256-64 as it just needs to allow for enough numbers not to clash with rom, tst and uninitialised tst values
  190 .next_val
  200     LDX #15        \\ sideways bank
  210     ADC #1         \\ will inc mostly by 2, but doesn't matter
  220 .next_slot
  230     STX RomSel
  240     CMP tstaddr
  250     BEQ next_val
  260     CMP unique,X   \\ doesn't matter that we haven't checked these yet as it just excludes unnecessary values, but is safe
  270     BEQ next_val
  280     DEX
  290     BPL next_slot
  300     STA unique,Y
  310     LDX tstaddr
  320     STX values,Y
  330     DEY
  340     BPL next_val
  350                \\ Try to swap each rom value with a unique test value - top down wouldn't work for Solidisk
  360     LDX #0         \\ count up to allow for Solidisk only having 3 select bits
  370 .swap
  380     STX UsrDat     \\ set Solidisk SWRAM index
  390     STX RamSel     \\ set RamSel incase it is used
  400     STX RomSel     \\ set RomSel as it will be needed to read, but is also sometimes used to select write
  410     LDA unique,X
  420     STA tstaddr
  430     INX            \\ count up to allow for Solidisk only have 3 select bits
  440     CPX #16
  450     BNE swap
  460                \\ count matching values and restore old values - reverse order to swapping is safe
  470     LDY #16
  480     LDX #15
  490 .tst_restore
  500     STX RomSel
  510     LDA tstaddr
  520     CMP unique,X   \\ if it has changed, but is not this value, it will be picked up in a later bank
  530     BNE not_swr
  540     STX UsrDat     \\ set Solidisk SWRAM index
  550     STX RamSel     \\ set RamSel incase it is used
  560     LDA values,X
  570     STA tstaddr
  580     DEY
  590     STX values,Y
  600 .not_swr
  610     DEX
  620     BPL tst_restore
  630     STY values
  640     LDA &F4
  650     STA RomSel     \\ restore original ROM
  660     CLI
  670     RTS
  680 ]
  690 NEXT
  700 CALL CODE
  710 PRINT"SWRAM Banks:";16-?&70
  720 IF ?&70 <> 16 THEN FOR X% = ?&70 TO 15 : PRINTX%?&70 : NEXT
  730 IF ?&70 = 16 THEN ?&7F = 128:PRINT"No SWRAM." ELSE PRINT"Using SWRAM Bank:";?&7F
  740 *RUN INTRO
