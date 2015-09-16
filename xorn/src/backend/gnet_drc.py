# xorn.geda.netlist - gEDA Netlist Extraction and Generation
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
# Copyright (C) 2013-2015 Roland Lutz
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

# DRC backend written by Matt Ettus

import sys

def run(f, netlist):
    try:
        g = open('attribs')
        try:
            attriblist = [l[:-1] for l in g.readlines()]  # strip trailing \n
        finally:
            g.close()
    except IOError as e:
        sys.stderr.write("ERROR: Can't read attribute file\n")
        sys.stderr.write(str(e) + "\n")
        sys.exit(1)

    for package in reversed(netlist.packages):
        for attrib in attriblist:
            if package.get_attribute(attrib, None) is None:
                f.write("%s Does not have attribute: %s\n"
                        % (package.refdes, attrib))

    for net in reversed(netlist.nets):
        conn_count = len(net.connections)
        if conn_count == 0:
            f.write("Net %s has no connected pins\n" % net.name)
        elif conn_count == 1:
            f.write("Net %s has only 1 connected pin\n" % net.name)

    # no pin rules
