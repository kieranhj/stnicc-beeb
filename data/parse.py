# Parse STNICC

import sys
import binascii

FLAG_CLEAR_SCREEN = 0x01
FLAG_CONTAINS_PALETTE = 0x02
FLAG_INDEXED_DATA = 0x4


def get_byte(file):
    return ord(file.read(1))

    # read returns a string containing the char
    # hexlify returns the byte value as a hexidecimal string
    # can then convert that to an int
    # seems needlessly convoluted?!
    #hex_str = binascii.hexlify(byte_str)
    #byte_int = ord(byte_str)

def colour_word_to_rgb(word):
    B = word & 0x07
    G = (word >> 4) & 0x07
    R = (word >> 8) & 0x07

    return [R, G, B]


if __name__ == '__main__':

    argv = sys.argv
    argc = len(argv)

    my_data = bytearray()

    my_file = open(argv[1], 'rb')
    # my_data.extend(my_file.read())
    # my_file.close()

    frame = 0
    total_polys = 0

    max_polys = 0
    max_polys_frame = 0

    max_verts = 0
    max_verts_frame = 0

    max_poly_size = 0
    max_poly_size_frame = 0

    while True:

        print "Frame {0}".format(frame)
        
        flags = get_byte(my_file)

        # print "flags = " + str(flags)

        if flags & FLAG_CLEAR_SCREEN:
            print "  Clear screen!"

        if flags & FLAG_CONTAINS_PALETTE:
            print "  Contains palette:"

            # Read palette mask

            palette_bitmask = get_byte(my_file) << 8 | get_byte(my_file)

            print "    Palette bitmask = {0:016b}".format(palette_bitmask)

            # Read palette data

            for b in xrange(0,15):
                if palette_bitmask & (1<<b):
                    palette_word = get_byte(my_file) << 8 | get_byte(my_file)
                    palette_colour = colour_word_to_rgb(palette_word)

                    print "      Palette {0} colour = ".format(15-b,), palette_colour

        indexed = False

        if flags & FLAG_INDEXED_DATA:
            print "  Indexed data:"

            # Get num vertices

            num_verts = get_byte(my_file)

            print "    Num vertices = {0}".format(num_verts)

            for v in xrange(0, num_verts):

                x = get_byte(my_file)
                y = get_byte(my_file)
                
                print "      Vertex {0} = [ {1}, {2} ]".format(v,x,y)

            indexed = True

            if num_verts > max_verts:
                max_verts = num_verts
                max_verts_frame = frame
    
        else:
            print "  Non-indexed data:"

        # Read polys

        print "    Polygons:"
        polys = 0
        end_of_frame = 0x0

        while True:

            # Get poly descriptor

            poly_descriptor = get_byte(my_file)

            # print "      Polygon {0} descriptor = {1:02x}".format(p, poly_descriptor)

            # Special cases

            if poly_descriptor >= 0xfd:
                end_of_frame = poly_descriptor
                break
            
            # Derive poly colour & verts

            poly_colour_index = poly_descriptor >> 4
            poly_num_verts = poly_descriptor & 0xf

            if poly_num_verts > max_poly_size:
                max_poly_size = poly_num_verts
                max_poly_size_frame = frame

            print "      Polygon {0}: colour index = {1} num vertices = {2}".format(polys, poly_colour_index, poly_num_verts)
        
            # Indexed

            if indexed:

                # Reading vertices

                poly_verts = []

                for v in xrange(0, poly_num_verts):

                    poly_verts.append(get_byte(my_file))

                print "          ", poly_verts

            else:

               # Reading x, y values

                for v in xrange(0, poly_num_verts):

                    x = get_byte(my_file)
                    y = get_byte(my_file)
                    
                    print "        Vertex {0} = [ {1}, {2} ]".format(v,x,y)

            polys += 1

        # End of frame

        if polys > max_polys:
            max_polys = polys
            max_polys_frame = frame

        total_polys += polys

        frame += 1
        print

        if end_of_frame == 0xfd:
            break

        if end_of_frame == 0xfe:

            # Skip to 64K boundary

            print "--------\n"

            my_file.seek( (my_file.tell() + 0xffff) & ~0xffff)

    print "Total frames = ", frame

    print "\nMax polys = ", max_polys, " on frame ", max_polys_frame
    print "Max verts = ", max_verts, " on frame ", max_verts_frame
    print "Max poly size = ", max_poly_size, " on frame ", max_poly_size_frame

    print "Total polys = ", total_polys, " average ", total_polys/frame, " polys/frame"

    my_file.close()
