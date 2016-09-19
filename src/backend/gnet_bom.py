# xorn.geda.netlist - gEDA Netlist Extraction and Generation
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
# Copyright (C) 2013-2016 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

# Bill of Material backend written by Matt Ettus

# Bill Of Materials Generator
# You must have a file called attribs in the pwd
# The file should be a text list of attributes you want listed,
# One per line.  No comments are allowed in the file.
# Questions? Contact matt@ettus.com
# This software is released under the terms of the GNU GPL

import sys
from util_getopt import *

def run(f, netlist, args):
    options = backend_getopt(args, {
        'attrib_file': Option(False, REQUIRED_ARGUMENT, None),
        'attribs': Option(False, REQUIRED_ARGUMENT, None)
    })

    # Parse attrib file or argument.
    # Store list of read attributes in attriblist.
    try:
        attriblist = options['attribs'].split(',')
    except KeyError:
        try:
            g = open(options.get('attrib_file', 'attribs'))
            try:
                data = g.read()
            finally:
                g.close()
        except IOError as e:
            sys.stderr.write("""\
ERROR: Can't read attribute file
%s

You must do one of the following:
        - Create an 'attribs' file
        - Specify an attribute file using -Oattrib_file=<filename>
        - Specify which attributes to include using
            -Oattribs=attrib1,attrib2,... (no spaces)
""" % str(e))
            netlist.failed = True
            return

        attriblist = []
        start = 0
        while start < len(data):
            ends = [data.find(delim, start) for delim in ' \n\t'] + [len(data)]
            end = min(end for end in ends if end != -1)
            if end != start:
                attriblist.append(data[start:end])
            start = end + 1

    if not attriblist:
        return

    f.write('refdes\t%s\t\n' % '\t'.join(attriblist))

    for package in reversed(netlist.packages):
        if package.get_attribute('nobom', None) == '1':
            continue
        f.write('%s\t%s\t\n' % (
            package.refdes, '\t'.join(package.get_attribute(attrib, 'unknown')
                                      for attrib in attriblist)))
