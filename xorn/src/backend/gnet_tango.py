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

# TANGO netlist backend written by Nuno Sucena

def run(f, netlist):
    for package in reversed(netlist.packages):
        f.write('[\n')
        f.write(package.refdes + '\n')
        f.write(package.get_attribute('footprint', 'PATTERN') + '\n')
        f.write(package.get_attribute('device', 'unknown') + '\n')
        f.write(package.get_attribute('value', '') + '\n')
        f.write('\n')
        f.write(']\n')

    for net in reversed(netlist.nets):
        # Write out a net associated with a particular package and pin
        f.write('(\n')
        f.write(net.name + '\n')
        # Properly format the name of the net and the actual net connections
        for pin in reversed(net.connections):
            f.write('%s-%s\n' % (pin.package.refdes, pin.number))
        f.write(')\n')
