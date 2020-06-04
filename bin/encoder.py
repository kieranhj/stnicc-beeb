# Parse STNICC

import argparse
import binascii
import sys
import os
import copy

FLAG_CLEAR_SCREEN = 0x01
FLAG_CONTAINS_PALETTE = 0x02
FLAG_INDEXED_DATA = 0x4

POLY_DESC_END_OF_STREAM = 0xFD
POLY_DESC_SKIP_TO_64K = 0xFE
POLY_DESC_END_OF_FRAME = 0xFF

# Read 1 byte from our input file
def get_byte(file):
    return ord(file.read(1))

def get_NuLA_value(ste_value):
    assert ste_value>=0 and ste_value<=15
    value=(ste_value&7)<<1
    if ste_value&8: value|=1
    return value

# Convert palette word to nice RGB triple
def colour_word_to_rgb(word):
    B = word & 0x0f
    G = (word >> 4) & 0x0f
    R = (word >> 8) & 0x0f
    return [R, G, B]

def rgb_to_colour_word(rgb):
    return (rgb[0] << 8) | (rgb[1] << 4) | rgb[2]

class Vertex:

    def __init__(self, x, y):
        self._x = x
        self._y = y

    def get_size(self):
        return 2

    def write(self, data, beeb):
        if beeb == True:
            data.append(self._x / 2)
            data.append(self._y / 2)
        else:
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

    def write(self, indexed, vert_array, data, beeb):
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
                v.write(data, beeb)

class Colours:

    def __init__(self):
        self._colours = [[0, 0, 0]] * 16

    def set_colour(self, i, rgb):
        assert i >= 0 and i < 16
        self._colours[i] = rgb

    def equals(self, other):
        for i in range(16):
            if self._colours[i] != other._colours[i]:
                return False
        return True

    def write(self, data):
        # Actually writing 5 bytes as RISCOS OSWORD block:
        for i in range(16):
            data.append(i)          # logical colour
            data.append(16)         # use RGB
            data.append(((self._colours[i][0] << 5) | 0x10) & 0xff)  # red
            data.append(((self._colours[i][1] << 5) | 0x10) & 0xff)  # green
            data.append(((self._colours[i][2] << 5) | 0x10) & 0xff)  # blue
            data.append(0)
            data.append(0)
            data.append(0)          # pad to 8 bytes

class Palette:

    def __init__(self):
        self._entries = []

    def add_entry(self, i, rgb):
        self._entries.append([i, rgb])

    def merge_palette(self, pal):
        for e in pal._entries:
            matched = False
            for g in self._entries:
                if g[0] == e[0]:
                    # Because we're actually merging an older frame into a newer one
                    # we actually don't want to replace the existing palette if we
                    # get a match - we want to keep the newest entry from that palette
                    # number.
                    # g[1] = e[1]
                    matched = True
                    break
                    
            if not matched:
                self.add_entry(e[0], e[1])

    def set_colours(self, colours):
        for e in self._entries:
            colours.set_colour(e[0], e[1])

    def get_size(self):
        num_entries = len(self._entries)
        if num_entries == 0:
            return 0
        else:
            # 2 bytes bitmask + 2 bytes per palette entry
            return 2 + 2 * num_entries

    def write(self, data, beeb):
        if beeb:
            assert len(self._entries)>0
            # dummy byte :( - don't fancy changing the data size...
            data.append(0)
            data.append(len(self._entries))
        else:
            # Write mask
            bitmask = 0
            for e in self._entries:
                bitmask |= 1 << (15 - e[0])
            data.append(bitmask >> 8)
            data.append(bitmask & 0xff)

        entries=sorted(self._entries,lambda a,b:cmp(a[0],b[0]))

        print "    Entries: {0}".format(len(self._entries))
        for e in entries:
            print "      {0} = {1}".format(e[0], e[1])
            if beeb:
                nula_rgb=[get_NuLA_value(e[1][0]),
                        get_NuLA_value(e[1][1]),
                        get_NuLA_value(e[1][2])]
                word = rgb_to_colour_word(nula_rgb)
                word |= e[0]<<12    # add NuLA palette index
            else:
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
        self._colours_index = 0

    def set_clear_screen_flag(self):
        self._flags |= FLAG_CLEAR_SCREEN

    def add_palette_entry(self, i, rgb):
        self._palette.add_entry(i, rgb)
        self._flags |= FLAG_CONTAINS_PALETTE

    def get_palette(self):
        return self._palette

    def merge_palette(self, pal):
        if pal.get_size() != 0:
            self._palette.merge_palette(pal)
            self._flags |= FLAG_CONTAINS_PALETTE

    def set_colours_index(self, cidx):
        self._colours_index = cidx

    def get_colours_index(self):
        return self._colours_index

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
            indexed_size = self.get_indexed_size()
            non_indexed_size = self.get_non_indexed_size()

            # Prefer the smaller data size obvs
            self._data_size = min(indexed_size, non_indexed_size)

            if indexed_size < non_indexed_size:
                self._flags |= FLAG_INDEXED_DATA

    def get_size(self):
        # 1 byte flag + optional palette (+ 1 byte terminator added by Sequence)
        base_size = 1 + self._palette.get_size()

        if self._data_size == 0:
            self.calc_size()

        return base_size + self._data_size

    def write(self, data, beeb):
        # Write frame flags
        data.append(self._flags)

        if self._flags & FLAG_CLEAR_SCREEN:
            print "    Clear screen!"

        if self._flags & FLAG_CONTAINS_PALETTE:
            print "    Contains palette!"
            self._palette.write(data, beeb)

        if self._flags & FLAG_INDEXED_DATA:
            print "    Indexed data!"
            # Write num verts
            data.append(self.num_unique_verts())
            # Write each vert
            if beeb == True:
                # Write de-interleaved
                for v in self._vertices:
                    data.append(v._x / 2)
                for v in self._vertices:
                    data.append(v._y / 2)
            else:
                # Write interleaved
                for v in self._vertices:
                    v.write(data, beeb)

        else:
            print "    Non-indexed data!"

        print "    Unique verts: {0}".format(self.num_unique_verts())
        print "    Num polys: {0}".format(self.num_polys())

        # Write each poly
        for p in self._polygons:
            p.write(self._flags & FLAG_INDEXED_DATA, self._vertices, data, beeb)

        print "    Data size: {0}".format(self.get_size())

class Sequence:

    def __init__(self):
        self._frames = []
        self._alignment = 64 * 1024
        self._frame_offsets = []
        self._all_colours = []

    def add_frame(self, f):
        f.calc_size()
        self._frames.append(f)

    def set_alignment(self, a):
        self._alignment = a

    def set_beeb(self, b):
        self._BEEB = b

    def get_size(self):
        total = 0
        for f in self._frames:
            total += f.get_size() + 1
        return total

    def num_frames(self):
        return len(self._frames)

    def write(self, data, frame_step, beeb):
        print "Sequence: {0} frames".format(self.num_frames())
        frame_no = 0
        total = 0
        merge_pal = None

        colours = Colours()

        for f in self._frames:

            frame_no+=1

            # Merge any previous palette entries that were skipped
            if merge_pal != None:
                assert frame_step > 1
                f.merge_palette(merge_pal)
                merge_pal = None
            
            # Is this frame to be skipped?
            if frame_step > 1 and frame_no % frame_step != 1:
                # Grab the palette and merge with the next frame
                merge_pal = f.get_palette()
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
                total += 1

            # Pad with 0x55 if needed.
            if eof_byte == POLY_DESC_SKIP_TO_64K:
                align_by = next_chunk * self._alignment - total
                total = next_chunk * self._alignment

                for i in xrange(0,align_by):
                    data.append(0x55)

            print "    Offset: {0}".format(len(data))
            self._frame_offsets.append(len(data))
            f.write(data, beeb)

            # Update colours for this frame
            f.get_palette().set_colours(colours)
            # Verbose:
            # print "    Colours: ", colours._colours

            cidx=0
            for c in self._all_colours:
                if c.equals(colours):
                    break
                cidx+=1
            else:
                c = None

            if c == None or cidx == len(self._all_colours):
                self._all_colours.append(copy.deepcopy(colours))
            
            f.set_colours_index(cidx)
            print "    Colours index: {0}".format(cidx)

            total += frame_size
            assert total == len(data)

        # Write EOF marker for last frame.
        data.append(POLY_DESC_END_OF_STREAM)
        total += 1

        print "Found {0} Colours sets.".format(len(self._all_colours))

        return total

    def write_index_data(self, data):
        print self._frame_offsets
        for o in self._frame_offsets:
            data.append((o >> 0) & 0xff)
            data.append((o >> 8) & 0xff)
            data.append((o >> 16) & 0xff)
            data.append((o >> 24) & 0xff)

    def write_colours_data(self, data):
        for f in self._frames:
            data.append(f.get_colours_index())
        
        for c in self._all_colours:
            c.write(data)

def make_sequence_from_file(file):
    sequence = Sequence()
    end_of_frame = 0x0

    while end_of_frame != 0xfd:
        f = Frame()
        flags = get_byte(file)

        if flags & FLAG_CLEAR_SCREEN:
            f.set_clear_screen_flag()

        if flags & FLAG_CONTAINS_PALETTE:
            # Read palette mask
            palette_bitmask = get_byte(file) << 8 | get_byte(file)

            # Read palette data
            for b in xrange(15,-1,-1):
                if palette_bitmask & (1<<b):
                    palette_word = get_byte(file) << 8 | get_byte(file)
                    palette_colour = colour_word_to_rgb(palette_word)
                    f.add_palette_entry(15-b, palette_colour)

        indexed = False
        verts_x = []
        verts_y = []

        if flags & FLAG_INDEXED_DATA:
            indexed = True

            # Get num vertices
            num_verts = get_byte(file)

            for v in xrange(0, num_verts):
                x = get_byte(file)
                y = get_byte(file)

                verts_x.append(x)
                verts_y.append(y)

        # Read polys
        while True:
            # Get poly descriptor
            poly_descriptor = get_byte(file)

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
                    vert_idx = get_byte(file)
                    x = verts_x[vert_idx]
                    y = verts_y[vert_idx]
                    v = Vertex(x, y)
                    f.add_vertex(v)
                    poly.add_vertex(v)

            else:
                # Reading x, y values
                for v in xrange(0, poly_num_verts):
                    x = get_byte(file)
                    y = get_byte(file)
                    v = Vertex(x, y)
                    f.add_vertex(v)
                    poly.add_vertex(v)

            # Add this polygon to this frame
            f.add_polygon(poly)

        # Add this frame to our sequence
        sequence.add_frame(f)

        if end_of_frame == 0xfe:
            # Skip to 64K boundary
            file.seek((file.tell() + 0xffff) & ~0xffff)

    return sequence


if __name__ == '__main__':

    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter)

    parser.add_argument("input", help="scene1.bin STNICCC data file [input]")
    parser.add_argument("-o", "--output", metavar="<output>", help="write new data stream <output> (default is '[input].new.bin')")
    parser.add_argument("-a", "--align", type=int, default=65536, metavar="<n>", help="Set data stream alignment <a>, default: 65536")
    parser.add_argument("-b", "--beeb", help="Enable BBC specific data changes", default=False, action="store_true")
    parser.add_argument("-f", "--frame_step", type=int, default=1, metavar="<n>", help="Set frame step for output <f>, default: 1")
    parser.add_argument("-v", "--verbose", help="Enable verbose mode", action="store_true")
    parser.add_argument("-x", "--index", metavar="<index>", help="write index file <index> containing 32-bit offset to each frame")
    parser.add_argument("-c", "--colours", metavar="<colours>", help="write colours file <colours> containing colous index for each frame plus colours data")
    args = parser.parse_args()

    src = args.input
    # check for missing files
    if not os.path.isfile(src):
        print("ERROR: File '" + src + "' not found")
        sys.exit()

    dst = args.output
    if dst == None:
        dst = os.path.splitext(src)[0] + ".new.bin"

    input_file = open(src, 'rb')

    print "STNICC polygon data stream.\n"

    my_sequence = make_sequence_from_file(input_file)
    input_file.close()

    print "Total frames {0}, unaligned data size: {1}".format(my_sequence.num_frames(), my_sequence.get_size())

    output_file = open(dst, 'wb')

    # Set data alignemnt (default = 64K)
    my_sequence.set_alignment(args.align)

    data = bytearray()

    # Write out the sequence...
    actual = my_sequence.write(data, args.frame_step, args.beeb)

    print "Actual written bytes: {0} vs {1}".format(actual, len(data))

    output_file.write(data)
    output_file.close()

    if args.index != None:
        index_data = bytearray()
        my_sequence.write_index_data(index_data)
        index_file = open(args.index, 'wb')
        index_file.write(index_data)
        index_file.close()
        print "Wrote {0} bytes to index.".format(len(index_data))

    if args.colours != None:
        colours_data = bytearray()
        my_sequence.write_colours_data(colours_data)
        colours_file = open(args.colours, 'wb')
        colours_file.write(colours_data)
        colours_file.close()
        print "Wrote {0} bytes to colours data.".format(len(colours_data))
