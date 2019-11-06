#!/usr/bin/python
import argparse,sys

##########################################################################
##########################################################################

g_verbose=False

def V(str):
    global g_verbose
    
    if g_verbose:
        sys.stdout.write(str)
        sys.stdout.flush()

##########################################################################
##########################################################################

def fatal(str):
    sys.stderr.write("FATAL: %s"%str)
    if str[-1]!='\n': sys.stderr.write("\n")
    
    sys.exit(1)

##########################################################################
##########################################################################

TRACK_SIZE_BYTES=10*256

##########################################################################
##########################################################################

def load_ssd(path):
    with open(path,'rb') as f:
        data=f.read()
        V('%s: %d byte(s)\n'%(path,len(data)))
        
        if len(data)>80*TRACK_SIZE_BYTES: fatal('%s: too large to be a .ssd')

        padding=(TRACK_SIZE_BYTES-len(data)%TRACK_SIZE_BYTES)%TRACK_SIZE_BYTES
        if padding!=0: data+=padding*chr(0)
            
        return data

##########################################################################
##########################################################################
    
def dsd_create(options):
    global g_verbose
    g_verbose=options.verbose

    track_size_bytes=10*256
    max_ssd_size_bytes=80*track_size_bytes
    
    # load files
    side0=load_ssd(options.side0_path)
    side1=load_ssd(options.side1_path)

    # make both files the same length
    diff=len(side1)-len(side0)
    if diff>0: side0+=diff*chr(0)
    elif diff<0: side1+=-diff*chr(0)

    dsd=''
    for i in range(0,len(side0),TRACK_SIZE_BYTES):
        dsd+=side0[i:i+TRACK_SIZE_BYTES]
        dsd+=side1[i:i+TRACK_SIZE_BYTES]

    if options.output_path is not None:
        with open(options.output_path,'wb') as f: f.write(dsd)

##########################################################################
##########################################################################

def main(argv):
    parser=argparse.ArgumentParser(description='make .dsd file from 2 .ssd files')

    parser.add_argument('-v','--verbose',action='store_true',help='be more verbose')
    parser.add_argument('-o',metavar='FILE',dest='output_path',help='write output to %(metavar)s')
    parser.add_argument('side0_path',metavar='SIDE0-FILE',help='file to use for side 0')
    parser.add_argument('side1_path',metavar='SIDE1-FILE',help='file to use for side 1')

    dsd_create(parser.parse_args(argv))

##########################################################################
##########################################################################

if __name__=='__main__': main(sys.argv[1:])
