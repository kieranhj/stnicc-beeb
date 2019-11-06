#!/usr/bin/python
import sys,os,argparse,shutil,datetime

##########################################################################
##########################################################################
#
# shellcmd.py - Python 2.x script that performs various shell
# command-type operations
# 
# Copyright (C) 2019 Tom Seddon
# 
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY# without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program. If not, see
# <https://www.gnu.org/licenses/>.
#
##########################################################################
##########################################################################

g_verbose=False

def pv(x):
    if g_verbose:
        sys.stderr.write(x)
        sys.stderr.flush()

##########################################################################
##########################################################################

def cp_cmd(options):
    shutil.copy(options.src,options.dest)

##########################################################################
##########################################################################
    
def rmtree_cmd(options):
    if os.path.isdir(options.path): shutil.rmtree(options.path)

##########################################################################
##########################################################################

def rmfile_cmd(options):
    os.unlink(options.path)

##########################################################################
##########################################################################

def mkdir_cmd(options):
    path=os.path.normpath(options.path)
    if not os.path.isdir(path): os.makedirs(path)

##########################################################################
##########################################################################

def touch_cmd(options):
    if not os.path.isfile(options.path):
        with open(options.path,'wb'): pass

    os.utime(options.path,None)  # None = now

##########################################################################
##########################################################################

def strftime_cmd(options):
    fmt=options.fmt
    if options.directive_prefix is not None:
        fmt=fmt.replace(options.directive_prefix,'%')

    pv('strftime format used: ``%s\'\'\n'%fmt)
    print datetime.datetime.now().strftime(fmt)

##########################################################################
##########################################################################

def sha1_cmd(options):
    m=hashlib.sha1()
    with open(options.path,'rb') as f: m.update(f.read())
    print '%s  %s'%(m.hexdigest(),options.path)

##########################################################################
##########################################################################

def blank_line_cmd(options):
    print
    
##########################################################################
##########################################################################

def main(options):
    global g_verbose
    g_verbose=options.verbose

    options.fun(options)

##########################################################################
##########################################################################

def create_parser():
    parser=argparse.ArgumentParser()

    parser.add_argument('-v','--verbose',action='store_true',help='be more verbose')

    subparsers=parser.add_subparsers(title='sub-command help')

    cp=subparsers.add_parser('copy-file',help='copy file')
    cp.add_argument('src',metavar='SRC',help='file to copy from')
    cp.add_argument('dest',metavar='DEST',help='file/folder path to copy to')
    cp.set_defaults(fun=cp_cmd)

    rmtree=subparsers.add_parser('rm-tree',help='remove folder tree')
    rmtree.add_argument('path',metavar='FOLDER',help='path of folder to remove')
    rmtree.set_defaults(fun=rmtree_cmd)

    rmfile=subparsers.add_parser('rm-file',help='remove single file')
    rmfile.add_argument('path',metavar='FILE',help='path of file to remove')
    rmfile.set_defaults(fun=rmfile_cmd)

    mkdir=subparsers.add_parser('mkdir',help='create folder structure')
    mkdir.add_argument('path',metavar='FOLDER',help='folder structure to create')
    mkdir.set_defaults(fun=mkdir_cmd)

    touch=subparsers.add_parser('touch',help='update file modified time, creating file if non-existent')
    touch.add_argument('path',metavar='FILE',help='file to touch')
    touch.set_defaults(fun=touch_cmd)

    strftime=subparsers.add_parser('strftime',help='format date like strftime/date +XXX')
    strftime.add_argument('-d','--directive-prefix',default=None,help='directive prefix used in place of %')
    strftime.add_argument('fmt',metavar='FMT',help='strftime format string')
    strftime.set_defaults(fun=strftime_cmd)

    sha1=subparsers.add_parser('sha1',help='print SHA1 digest of file')
    sha1.add_argument('path',metavar='FILE',help='file to process')
    sha1.set_defaults(fun=sha1_cmd)

    blank_line=subparsers.add_parser('blank-line',help='print blank line')
    blank_line.set_defaults(fun=blank_line_cmd)

    return parser

if __name__=='__main__': main(create_parser().parse_args())
