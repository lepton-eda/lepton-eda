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

import collections
from util_getopt import *

BOMEntry = collections.namedtuple('BOMEntry', ['refdes_list', 'attribs'])

# Split a string into non-empty parts separated by a set of delimiters.
def strtok(s, delim):
    start = 0
    while start < len(s):
        found = [i for i in (s.find(d, start) for d in delim) if i != -1]
        if found:
            end = min(found)
        else:
            end = len(s)
        if end != start:
            yield s[start:end]
        start = end + 1

def run(f, netlist, args):
    options = backend_getopt(args, {
        'attrib_file': Option(False, REQUIRED_ARGUMENT, None),
        'attribs': Option(False, REQUIRED_ARGUMENT, None)
    })

    try:
        attriblist = options['attribs'].split(',')
    except KeyError:
        filename = options.get('attrib_file', 'attribs')
        try:
            g = open(filename)
        except IOError as e:
            if e.errno == errno.ENOENT and 'attribs' not in options:
                sys.stderr.write("""\
ERROR: Attribute file '%s' not found. You must do one of the following:
         - Create an 'attribs' file
         - Specify an attribute file using -Oattrib_file=<filename>
         - Specify which attributes to include using
             -Oattribs=attrib1,attrib2,... (no spaces)
""" % filename)
            else:
                sys.stderr.write("%s\n" % str(e))
            sys.exit(1)
        try:
            attriblist = list(strtok(g.read(), ' \n\t'))
        finally:
            g.close()

    if not attriblist:
        return

    bomlist = []

    for package in reversed(netlist.packages):
        if package.get_attribute('nobom', None) is not None:
            continue

        attribs = [package.get_attribute(attrib, 'unknown')
                   for attrib in attriblist]
        found = False

        for entry in bomlist:
            if entry.attribs == attribs:
                entry.refdes_list.append(package.refdes)
                found = True
                break

        if not found:
            bomlist.append(BOMEntry([package.refdes], attribs))

    bomlist.reverse()

    f.write('refdes:%s:qty\n' % ':'.join(attriblist))
    for entry in bomlist:
        f.write('%s:%s:%s\n' % (','.join(sorted(entry.refdes_list)),
                                ':'.join(entry.attribs),
                                len(entry.refdes_list)))
