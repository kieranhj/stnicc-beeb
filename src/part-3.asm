DFS_sector_size = 256
DFS_sectors_per_track = 10
DFS_sectors_to_load = 10
DFS_track_size = (DFS_sectors_per_track * DFS_sector_size)

dummy_size=DFS_track_size-&200

CLEAR &0000,&FFFF
ORG &0000
.dummy
skip dummy_size
SAVE "dummy", dummy, P%

PUTFILE "data/scene1_disk.02.bin", "02", 0
