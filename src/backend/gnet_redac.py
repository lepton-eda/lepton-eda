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

# RACAL-REDAC / Cadstar netlist format by Wojciech Kazubski 2003

class NewlineReplacer:
    def __init__(self, f):
        self.f = f

    def write(self, s):
        self.f.write(str(s).replace('\n', '\r\n'))

def run(f, netlist):
    f = NewlineReplacer(f)
    f.write('.PCB\n')
    f.write('.REM CREATED BY gEDA GNETLIST\n')
    f.write('.CON\n')
    f.write('.COD 2\n')
    f.write('\n')
    for net in reversed(netlist.nets):
        f.write('.REM %s\n' % net.name)
        # Display the individual net connections
        k = 7
        for i, pin in enumerate(reversed(net.connections)):
            item = '%s %s' % (pin.package.refdes, pin.number)
            f.write(item)
            if i == len(net.connections) - 1:
                continue
            if k > 0:
                f.write(' ')
                k -= 1
            else:
                f.write('\n' + item + ' ')
                k += 6
        f.write('\n')
    f.write('.EOD\n')
