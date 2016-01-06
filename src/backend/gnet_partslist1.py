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

# Copyright (C) 2001-2010 MIYAMOTO Takanori

def run(f, netlist):
    f.write('.START\n')
    f.write('..refdes\tdevice\tvalue\tfootprint\tquantity\n')
    for package in sorted(netlist.packages, key = lambda x: x.refdes):
        if package.get_attribute('device', None) == 'include':
            continue
        f.write('%s\t%s\t%s\t%s\t1\n' % (
            package.refdes,
            package.get_attribute('device', 'unknown'),
            package.get_attribute('value', 'unknown'),
            package.get_attribute('footprint', 'unknown')))
    f.write('.END\n')
