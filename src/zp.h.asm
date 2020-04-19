;;  -*- beebasm -*-

; Stuff that has to stick around across intro, demo and outro.

; where this persistent data region begins
zp_top=$7b

; bit 7 clear = quality=0, set = quality=1
NULA_QUALITY_ZP = $7b

;
MUSIC_ENABLED_ZP = $7c

; bit 7 clear = normal Beeb, bit 7 set = NuLA available
NULA_AVAILABLE_ZP = $7d

; bit 7 clear = ordinary version, bit 7 set = NuLA version
NULA_FLAG_ZP = $7e

; bit 7 set = no music, bit 7 clear = value is ROM slot of music bank
MUSIC_SLOT_ZP = &7F
