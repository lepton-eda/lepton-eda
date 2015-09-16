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

# liquid pcb gnetlist backend

def run(f, netlist):
    f.write('<LiquidPCB>\n')
    f.write('\t<netlist name="Main netlist">\n')

    for net in reversed(netlist.nets):
        f.write('\t\t<net name="%s">\n' % net.name)
        for pin in reversed(net.connections):
            f.write('\t\t\t<netnode component="%s" pin=%s />\n'
                    % (pin.package.refdes, pin.number))
        f.write('\t\t</net>\n')

    f.write('\t</netlist>\n')
    f.write('</LiquidPCB>\n')
