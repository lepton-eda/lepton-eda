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

# gEDA's native test netlist format

def run(f, netlist):
    f.write('START header\n')
    f.write('\n')
    f.write('gEDA\'s netlist format\n')
    f.write('Created specifically for testing of gnetlist\n')
    f.write('\n')
    f.write('END header\n')
    f.write('\n')

    f.write('START components\n')
    f.write('\n')

    for package in reversed(netlist.packages):
        f.write('%s device=%s\n' % (
            package.refdes,
            package.get_attribute('device', 'unknown')))

    f.write('\n')
    f.write('END components\n')
    f.write('\n')

    f.write('START nets\n')
    f.write('\n')

    for net in reversed(netlist.nets):
        # "netname : uref pin, uref pin, ..."
        # Display the individual net connections
        f.write('%s : %s \n' % (
            net.name,
            ', '.join('%s %s' % (pin.package.refdes, pin.number)
                      for pin in reversed(net.connections))))

    f.write('\n')
    f.write('END nets\n')
    f.write('\n')
