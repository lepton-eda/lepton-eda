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

# PCB forward annotation script

# This is a list of attributes which are propogated to the pcb
# elements.  Note that refdes, value, and footprint need not be
# listed here.

element_attrs = [
    'device',
    'manufacturer',
    'manufacturer_part_number',
    'vendor',
    'vendor_part_number'
]

def quote_string(s):
    return '"%s"' % s.replace('"', '\\"')

def run(f, netlist):
    f.write('Netlist(Freeze)\n')
    f.write('Netlist(Clear)\n')

    for net in reversed(netlist.nets):
        for pin in reversed(net.connections):
            f.write('Netlist(Add,%s,%s-%s)\n' % (
                net.name, pin.package.refdes, pin.number))

    f.write('Netlist(Sort)\n')
    f.write('Netlist(Thaw)\n')
    f.write('ElementList(Start)\n')

    for package in reversed(netlist.packages):
        f.write('ElementList(Need,%s,%s,%s)\n' % (
            quote_string(package.refdes),
            quote_string(package.get_attribute('footprint', 'unknown')),
            quote_string(package.get_attribute('value', 'unknown'))))

        for attr in element_attrs:
            f.write('ElementSetAttr(%s,%s,%s)\n' % (
                quote_string(package.refdes),
                quote_string(attr),
                quote_string(package.get_attribute(attr, 'unknown'))))

        # write out the pins for a particular component
        for pin in reversed(package.pins):
            pinnum = pin.get_attribute('pinnumber', 'unknown')
            label = pin.get_attribute('pinlabel', pinnum)

            f.write('ChangePinName(%s, %s, %s)\n' % (
                quote_string(package.refdes), pinnum, quote_string(label)))

    f.write('ElementList(Done)\n')
