;;  -*- beebasm -*-

; Stuff that has to stick around across intro, demo and outro.

; where this persistent data region begins
zp_top=$7e

; 0 = ordinary version, other = NuLA version
NULA_FLAG_ZP = $7e

MUSIC_SLOT_ZP = &7F
