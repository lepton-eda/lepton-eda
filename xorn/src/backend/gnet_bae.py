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

# Bartels Format
# Layout board;
# PARTS
#   part : footprint;
# CONNECT
#   /net1/ uref.pin=uref.pin=uref.pin=...uref.pin;
#   /net2/ PRIORITY(1..100) MINDIST(mm) ROUTWIDTH(mm) uref.pin(width_mm)=...;
# END.

def run(f, netlist):
    f.write('LAYOUT board;\n')
    f.write('PARTS\n')
    for package in reversed(netlist.packages):
        f.write('    %s : %s;\n' % (
            package.refdes, package.get_attribute('footprint', 'unknown')))
    f.write('CONNECT\n')
    for net in reversed(netlist.nets):
        f.write("    /'%s'/ %s;\n" % (
            net.name, '='.join('%s.%s' % (pin.package.refdes, pin.number)
                               for pin in reversed(net.connections))))
    f.write('END.\n')
