# Parse STNICC

import sys
import binascii

FLAG_CLEAR_SCREEN = 0x01
FLAG_CONTAINS_PALETTE = 0x02
FLAG_INDEXED_DATA = 0x4

POLY_DESC_END_OF_STREAM = 0xFD
POLY_DESC_SKIP_TO_64K = 0xFE
POLY_DESC_END_OF_FRAME = 0xFF

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

    verbose = False

    my_data = bytearray()
    my_file = open(argv[1], 'rb')
    # my_data.extend(my_file.read())
    # my_file.close()

    if argc > 2:
        verbose = argv[2] == '-v'

    frame = 0
    total_polys = 0
    total_bytes = 0
    total_edges = 0
    total_spans = 0

    max_polys = 0
    max_polys_frame = 0

    max_verts = 0
    max_verts_frame = 0

    max_poly_size = 0
    max_poly_size_frame = 0

    max_spans = 0
    max_spans_frame = 0

    max_edges = 0
    max_edges_frame = 0

    end_of_frame = 0x0

    print "STNICC polygon data stream.\n"

    while end_of_frame != 0xfd:

        frame += 1
        print "Frame {0}:".format(frame)
        
        start_pos = my_file.tell()
        flags = get_byte(my_file)

        # print "flags = " + str(flags)

        if flags & FLAG_CLEAR_SCREEN:
            print "  Clear screen!"

        if flags & FLAG_CONTAINS_PALETTE:
            print "  Contains palette:"

            # Read palette mask

            palette_bitmask = get_byte(my_file) << 8 | get_byte(my_file)

            if verbose:
                print "    Palette bitmask = {0:016b}".format(palette_bitmask)

            # Read palette data

            for b in xrange(0,15):
                if palette_bitmask & (1<<b):
                    palette_word = get_byte(my_file) << 8 | get_byte(my_file)
                    palette_colour = colour_word_to_rgb(palette_word)

                    if verbose:
                        print "      Palette {0} colour = ".format(15-b,), palette_colour

        indexed = False

        verts_x = []
        verts_y = []

        if flags & FLAG_INDEXED_DATA:
            print "  Indexed data:"

            # Get num vertices

            num_verts = get_byte(my_file)

            print "    Num vertices = {0}".format(num_verts)

            for v in xrange(0, num_verts):

                x = get_byte(my_file)
                y = get_byte(my_file)

                verts_x.append(x)
                verts_y.append(y)
                
                if verbose:
                    print "      Vertex {0} = [ {1}, {2} ]".format(v,x,y)

            indexed = True

            if num_verts > max_verts:
                max_verts = num_verts
                max_verts_frame = frame
    
        else:
            print "  Non-indexed data:"

        # Read polys

        if verbose:
            print "    Polygons:"

        polys = 0
        edges = 0
        frame_spans = 0

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

            edges += poly_num_verts

            if poly_num_verts > max_poly_size:
                max_poly_size = poly_num_verts
                max_poly_size_frame = frame

            if verbose:
                print "      Polygon {0}: Colour = {1} Num vertices = {2}".format(polys, poly_colour_index, poly_num_verts)
        
            poly_min_y = 255
            poly_max_y = 0

            # Indexed

            if indexed:

                # Reading vertices

                poly_verts = []

                for v in xrange(0, poly_num_verts):

                    vert_idx = get_byte(my_file)
                    poly_verts.append(vert_idx)

                if verbose:
                    print "        Vertices =", poly_verts

                for v in xrange(0, poly_num_verts):
                    vert_idx = poly_verts[v]
                    x = verts_x[vert_idx]
                    y = verts_y[vert_idx]

                    if y < poly_min_y:
                        poly_min_y = y

                    if y > poly_max_y:
                        poly_max_y = y
    
                    if verbose:
                        print "          Edge {0} = [ {1}, {2} ]".format(v, x, y)


            else:

               # Reading x, y values

                for v in xrange(0, poly_num_verts):

                    x = get_byte(my_file)
                    y = get_byte(my_file)

                    if y < poly_min_y:
                        poly_min_y = y

                    if y > poly_max_y:
                        poly_max_y = y

                    if verbose:
                        print "        Vertex {0} = [ {1}, {2} ]".format(v,x,y)

            spans = poly_max_y - poly_min_y + 1

            if verbose:
                print "        Spans = ", spans

            frame_spans += spans
            polys += 1

        # End of frame
        print "    Num polys = {0}".format(polys)
        print "    Num edges = {0}".format(edges)
        print "    Average edges / poly = {0}".format(float(edges)/float(polys))
        print "    Num spans = {0}".format(frame_spans)
        print "    Average spans / poly = {0}".format(float(frame_spans)/float(polys))

        if polys > max_polys:
            max_polys = polys
            max_polys_frame = frame

        total_polys += polys

        if edges > max_edges:
            max_edges = edges
            max_edges_frame = frame

        total_edges += edges

        if frame_spans > max_spans:
            max_spans = frame_spans
            max_spans_frame = frame

        total_spans += frame_spans

        if end_of_frame == 0xfd:
            print "  End of stream!"

        if end_of_frame == 0xfe:
            # Skip to 64K boundary
            print "  Align to 64K."

            my_file.seek( (my_file.tell() + 0xffff) & ~0xffff)

        bytes = (my_file.tell() - start_pos)
        total_bytes += bytes

        print "  Bytes = {0}".format(bytes)
        print

    print "Total frames = ", frame
    print "Total bytes = ", total_bytes
    print "Bytes / frame = ", total_bytes/frame

    print "\nTotal polys = ", total_polys, " w/ mean ", total_polys/frame, " polys/frame"
    print "Max polys = ", max_polys, " on frame ", max_polys_frame
    print "Max verts = ", max_verts, " on frame ", max_verts_frame
    print "Max poly size = ", max_poly_size, " on frame ", max_poly_size_frame

    print "\nTotal edges = ", total_edges, " w/ mean ", total_edges/frame, " edges/frame"
    print "Max edges = ", max_edges, " on frame ", max_edges_frame
    print "Mean edges / poly = ", float(total_edges) / float(total_polys)

    print "\nTotal spans = ", total_spans, " w/ mean ", total_spans/frame, "spans/frame"
    print "Max spans = ", max_spans," on frame ", max_spans_frame
    print "Mean spans / poly = ", total_spans / total_polys

    my_file.close()
