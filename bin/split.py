# Split our large binary

import sys

TRACK_SIZE = 10 * 256
SPLIT_SIZE = 79 * TRACK_SIZE
# disc 1 holds tracks DISK1_first_track..79 (stnicc-beeb.asm, stnicc-build.asm)
DISK1_FIRST_TRACK = 24
FIRST_SIZE = (80 - DISK1_FIRST_TRACK) * TRACK_SIZE

if __name__ == '__main__':

    argv = sys.argv
    argc = len(argv)

    f = open(argv[1], 'rb')
    data = f.read()
    size = len(data)

    first_chunk = FIRST_SIZE

    # Needs to be rounded up to a full track
    if first_chunk % TRACK_SIZE != 0:
        first_chunk += TRACK_SIZE - (first_chunk % TRACK_SIZE)

    print("size=", size, " first=",first_chunk, " split=",SPLIT_SIZE)

    nf = open('scene1_disk.00.bin', 'wb')
    nf.write(data[0:first_chunk])
    nf.close()

    # Disc 2 is read backwards, from track 79 down to track 1, so that the head
    # does not seek back across the disc at the side switch: pad its chunk to
    # all 79 tracks and store them last first (src/part-2.asm puts the file
    # at track 1, so file track i is disc track i + 1).
    chunk = data[first_chunk:first_chunk + 1*SPLIT_SIZE]
    chunk += b'\x55' * (SPLIT_SIZE - len(chunk))
    tracks = [chunk[i:i + TRACK_SIZE] for i in range(0, SPLIT_SIZE, TRACK_SIZE)]
    nf = open('scene1_disk.01.bin', 'wb')
    nf.write(b''.join(reversed(tracks)))
    nf.close()

    nf = open('scene1_disk.02.bin', 'wb')
    nf.write(data[first_chunk + 1*SPLIT_SIZE:first_chunk + 2*SPLIT_SIZE])
    nf.close()

    nf = open('scene1_disk.03.bin', 'wb')
    nf.write(data[first_chunk + 2*SPLIT_SIZE:first_chunk + 3*SPLIT_SIZE])
    nf.close()
