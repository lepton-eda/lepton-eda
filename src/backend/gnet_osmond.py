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

# gnetlist back end for Osmond PCB Design
# Copyright (C) 2007-2010 John P. Doty

# Export a design to Osmond PCB

def run(f, netlist):
    # The first section of the file consists of a list of packages,
    # one per line. For example:
    # Part 0603 { Name R4 }
    for package in reversed(netlist.packages):
        f.write('Part %s { Name %s }\n' %
                (package.get_attribute('footprint', 'unknown'),
                 package.refdes))

    # The next section of the file consists of a list of nets.
    # Each entry takes two lines. For example:
    # Signal "unnamed_net6"
    #   { R4-1 R3-2 C3-2 }
    for net in reversed(netlist.nets):
        f.write('Signal "%s"\n' % net.name)
        f.write('  {')
        for pin in reversed(net.connections):
            # print connection as " refdes-pinnumber"
            f.write(' %s-%s' % (pin.package.refdes, pin.number))
        f.write(' }\n')
