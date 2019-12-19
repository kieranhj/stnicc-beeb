# Parse STNICC

import sys
import binascii

FLAG_CLEAR_SCREEN = 0x01
FLAG_CONTAINS_PALETTE = 0x02
FLAG_INDEXED_DATA = 0x4

POLY_DESC_END_OF_STREAM = 0xFD
POLY_DESC_SKIP_TO_64K = 0xFE
POLY_DESC_END_OF_FRAME = 0xFF

class Vertex:

    def __init__(self, x, y):
        self._x = x
        self._y = y

    def get_size(self):
        return 2

    def write(self, data):
        data.append(self._x)
        data.append(self._y)
        # print "      ({0}, {1})".format(self._x, self._y)

    def equals(self, v):
        return self._x == v._x and self._y == v._y

class Polygon:

    def __init__(self):
        self._vertices = []
        self._colour_index = 0

    def add_vertex(self, v):
        self._vertices.append(v)

    def set_colour_index(self, i):
        self._colour_index = i

    def get_num_verts(self):
        return len(self._vertices)

    def get_descriptor(self):
        return self._colour_index << 4 | num_verts(self)

    def get_size_indexed(self):
        # 1 byte descriptor + 1 byte per vertex index
        return 1 + self.get_num_verts()

    def get_size_non_indexed(self):
        # 1 byte descriptor + 2 bytes per vertex coordinates
        return 1 + 2 * self.get_num_verts()

    def write(self, indexed, vert_array, data):
        # Write poly descriptor
        poly_descriptor = (self._colour_index << 4) + self.get_num_verts()
        # print "    Poly descriptor: {0:2x}".format(poly_descriptor)
        data.append(poly_descriptor)

        # print "    Num verts: {0}".format(self.get_num_verts())

        for v in self._vertices:
            if indexed:
                # This might be indexed from 1?
                for u in vert_array:
                    if v.equals(u):
                        data.append(vert_array.index(u))
                        break
            else:
                v.write(data)

class Palette:

    def __init__(self):
        self._entries = []

    def add_entry(self, i, rgb):
        self._entries.append([i, rgb])

    def get_size(self):
        num_entries = len(self._entries)
        if num_entries == 0:
            return 0
        else:
            # 2 bytes bitmask + 2 bytes per palette entry
            return 2 + 2 * num_entries

    def write(self, data):
        # Write mask
        bitmask = 0
        for e in self._entries:
            bitmask |= 1 << (15 - e[0])
            
        data.append(bitmask >> 8)
        data.append(bitmask & 0xff)

        print "    Entries: {0}".format(len(self._entries))
        for e in self._entries:
            print "      {0} = {1}".format(e[0], e[1])
            word = rgb_to_colour_word(e[1])
            data.append(word >> 8)
            data.append(word & 0xff)

class Frame:

    def __init__(self):
        self._polygons = []
        self._vertices = []
        self._palette = Palette()
        self._flags = 0
        self._data_size = 0

    def set_clear_screen_flag(self):
        self._flags |= FLAG_CLEAR_SCREEN

    def add_palette_entry(self, i, rgb):
        self._palette.add_entry(i, rgb)
        self._flags |= FLAG_CONTAINS_PALETTE

    def add_polygon(self, p):
        self._polygons.append(p)

    def add_vertex(self, v):
        for u in self._vertices:
            if u.equals(v):
                return
        self._vertices.append(v)

    def num_polys(self):
        return len(self._polygons)

    def num_unique_verts(self):
        return len(self._vertices)

    def get_indexed_size(self):
        # 1 byte num_verts + 2 bytes per vertex
        size = 1 + 2 * len(self._vertices)
        # Each polygon has a descriptor and a list of indices
        for p in self._polygons:
            size += p.get_size_indexed()

        return size 

    def get_non_indexed_size(self):
        size = 0
        # Each polygon has a descriptor and vertex coordinates
        for p in self._polygons:
            size += p.get_size_non_indexed()

        return size

    def calc_size(self):
        if self._data_size == 0:
            # 1 byte flag + optional palette + 1 byte terminator
            base_size = 1 + self._palette.get_size() + 1
            indexed_size = self.get_indexed_size()
            non_indexed_size = self.get_non_indexed_size()

            # Prefer the smaller data size obvs
            self._data_size = base_size + min(indexed_size, non_indexed_size)

            if indexed_size < non_indexed_size:
                self._flags |= FLAG_INDEXED_DATA

    def get_size(self):
        return self._data_size

    def write(self, data):
        # Write frame flags
        data.append(self._flags)

        if self._flags & FLAG_CLEAR_SCREEN:
            print "    Clear screen!"

        if self._flags & FLAG_CONTAINS_PALETTE:
            print "    Contains palette!"
            self._palette.write(data)

        if self._flags & FLAG_INDEXED_DATA:
            print "    Indexed data!"
            # Write num verts
            data.append(self.num_unique_verts())
            # Write each vert
            for v in self._vertices:
                v.write(data)

        else:
            print "    Non-indexed data!"

        print "    Unique verts: {0}".format(self.num_unique_verts())
        print "    Num polys: {0}".format(self.num_polys())

        # Write each poly
        for p in self._polygons:
            p.write(self._flags & FLAG_INDEXED_DATA, self._vertices, data)

        print "    Data size: {0}".format(self.get_size())

class Sequence:

    def __init__(self):
        self._frames = []
        self._alignment = 64 * 1024

    def add_frame(self, f):
        f.calc_size()
        self._frames.append(f)

    def set_alignment(self, a):
        self._alignment = a

    def get_size(self):
        total = 0
        for f in self._frames:
            total += f.get_size()
        return total

    def num_frames(self):
        return len(self._frames)

    def write(self, data):
        print "Sequence: {0} frames".format(self.num_frames())
        frame_no = 0
        total = 0
        for f in self._frames:

            frame_no+=1
            
            if frame_no & 1 == 1:
                continue

            print "  Frame: {0}".format(frame_no)

            frame_size = f.get_size()
            this_chunk = total / self._alignment
            next_chunk = (total + frame_size) / self._alignment

            eof_byte = POLY_DESC_END_OF_FRAME

            if next_chunk != this_chunk:
                print "    Align data to next chunk!"
                eof_byte = POLY_DESC_SKIP_TO_64K

            # Write EOF marker for previous frame.
            if f != self._frames[0]:
                data.append(eof_byte)

            # Pad with 0x55 if needed.
            if eof_byte == POLY_DESC_SKIP_TO_64K:
                align_by = next_chunk * self._alignment - total
                total = next_chunk * self._alignment

                for i in xrange(0,align_by):
                    data.append(0x55)

            f.write(data)

            total += frame_size

        # Write EOF marker for last frame.
        data.append(POLY_DESC_END_OF_STREAM)
        return total

# Read 1 byte from our input file
def get_byte(file):
    return ord(file.read(1))

# Convert palette word to nice RGB triple
def colour_word_to_rgb(word):
    B = word & 0x0f
    G = (word >> 4) & 0x0f
    R = (word >> 8) & 0x0f
    return [R, G, B]

def rgb_to_colour_word(rgb):
    return (rgb[0] << 8) | (rgb[1] << 4) | rgb[2]

if __name__ == '__main__':

    argv = sys.argv
    argc = len(argv)

    input_file = open(argv[1], 'rb')

    frame = 0
    end_of_frame = 0x0

    print "STNICC polygon data stream.\n"

    my_sequence = Sequence()

    while end_of_frame != 0xfd:
        frame += 1
        f = Frame()
        flags = get_byte(input_file)

        if flags & FLAG_CLEAR_SCREEN:
            f.set_clear_screen_flag()

        if flags & FLAG_CONTAINS_PALETTE:
            # Read palette mask
            palette_bitmask = get_byte(input_file) << 8 | get_byte(input_file)

            # Read palette data
            for b in xrange(15,-1,-1):
                if palette_bitmask & (1<<b):
                    palette_word = get_byte(input_file) << 8 | get_byte(input_file)
                    palette_colour = colour_word_to_rgb(palette_word)
                    f.add_palette_entry(15-b, palette_colour)

        indexed = False
        verts_x = []
        verts_y = []

        if flags & FLAG_INDEXED_DATA:
            indexed = True

            # Get num vertices
            num_verts = get_byte(input_file)

            for v in xrange(0, num_verts):
                x = get_byte(input_file)
                y = get_byte(input_file)

                verts_x.append(x)
                verts_y.append(y)

        # Read polys
        while True:
            # Get poly descriptor
            poly_descriptor = get_byte(input_file)

            # Special cases
            if poly_descriptor >= 0xfd:
                end_of_frame = poly_descriptor
                break

            poly = Polygon()

            # Derive poly colour & verts
            poly_colour_index = poly_descriptor >> 4
            poly.set_colour_index(poly_colour_index)

            poly_num_verts = poly_descriptor & 0xf

            # Indexed
            if indexed:
                # Reading vertices
                for v in xrange(0, poly_num_verts):
                    vert_idx = get_byte(input_file)
                    x = verts_x[vert_idx]
                    y = verts_y[vert_idx]
                    v = Vertex(x, y)
                    f.add_vertex(v)
                    poly.add_vertex(v)

            else:
                # Reading x, y values
                for v in xrange(0, poly_num_verts):
                    x = get_byte(input_file)
                    y = get_byte(input_file)
                    v = Vertex(x, y)
                    f.add_vertex(v)
                    poly.add_vertex(v)

            # Add this polygon to this frame
            f.add_polygon(poly)

        # Add this frame to our sequence
        my_sequence.add_frame(f)

        if end_of_frame == 0xfe:
            # Skip to 64K boundary
            input_file.seek((input_file.tell() + 0xffff) & ~0xffff)

    input_file.close()

    print "Total bytes: {0}".format(my_sequence.get_size())

    output_file = open(argv[2], 'wb')

    # Set data alignemnt (default = 64K)
    my_sequence.set_alignment(7680)

    data = bytearray()

    # Write out the sequence...
    actual = my_sequence.write(data)

    print "Actual bytes: {0} vs {1}".format(actual, len(data))

    output_file.write(data)
    output_file.close()
