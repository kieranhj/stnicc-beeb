# Split our large binary

import sys

TRACK_SIZE = 10 * 256
SPLIT_SIZE = 79 * TRACK_SIZE
FIRST_SIZE = 50 * TRACK_SIZE

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

    nf = open('scene1_disk.01.bin', 'wb')
    nf.write(data[first_chunk:first_chunk + 1*SPLIT_SIZE])
    nf.close()

    nf = open('scene1_disk.02.bin', 'wb')
    nf.write(data[first_chunk + 1*SPLIT_SIZE:first_chunk + 2*SPLIT_SIZE])
    nf.close()

    nf = open('scene1_disk.03.bin', 'wb')
    nf.write(data[first_chunk + 2*SPLIT_SIZE:first_chunk + 3*SPLIT_SIZE])
    nf.close()
