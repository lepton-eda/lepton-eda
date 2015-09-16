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

# --------------------------------------------------------------------------
#
# protelII netlist format
#
# PROTEL NETLIST 2.0
# [   -- element for list of components
# DESIGNATOR
#   REFDES attribute.
# FOOTPRINT
#   FOOTPRINT attrbute.
# PARTTYPE
#   Either:
#     If VALUE attribute exists, output VALUE attribute.
#     Otherwise, output DEVICE attrbute.
#     (This covers the case of ICs, which usually carry their part no (e.g. uA741) in the DEVICE attribute.)
# DESCRIPTION
#   DEVICE attribute
# Part Field 1
# *
# Part Field 2
# *
# Part Field 3
# *
# Part Field 4
# *
# Part Field 5
# *
# Part Field 6
# *
# Part Field 7
# *
# Part Field 8
# *
# Part Field 9
# *
# Part Field 10
# *
# Part Field 11
# *
# Part Field 12
# *
# Part Field 13
# *
# Part Field 14
# *
# Part Field 15
# *
# Part Field 16
# *
# LIBRARYFIELD1
# empty line
# LIBRARYFIELD2
# empty line
# LIBRARYFIELD3
# empty line
# LIBRARYFIELD4
# empty line
# LIBRARYFIELD5
# empty line
# LIBRARYFIELD6
# empty line
# LIBRARYFIELD7
# empty line
# LIBRARYFIELD8
# empty line
# ]
# [
# ... other components ...
# ]
# (  -- element for list of nets
# NETNAME
# PART-PIN# VALUE-PINNAME PINTYPE  -- use PASSIVE for PINTYPE
# ...more connections...
# )
# (
# ...more nets...
# )
# { -- element for net option list
# NETNAME
# OPTION
# OPTIONVALUE
# TRACK
# 24
# VIA
# 40
# NET TOPOLOGY
# SHORTEST
# ROUTING PRIORITY
# MEDIUM
# LAYER
# UNDEFINED
# }
# {
# ...more net options...
# }

class NewlineReplacer:
    def __init__(self, f):
        self.f = f

    def write(self, s):
        self.f.write(str(s).replace('\n', '\r\n'))

def run(f, netlist):
    # This file format requires Windows-style "\r\n" line endings rather
    # than Unix-style "\n" endings.
    f = NewlineReplacer(f)

    f.write('PROTEL NETLIST 2.0\n')

    for package in reversed(netlist.packages):
        f.write('[\n')
        f.write('DESIGNATOR\n')
        f.write(package.refdes + '\n')
        f.write('FOOTPRINT\n')
        f.write(package.get_attribute('footprint', 'unknown') + '\n')
        f.write('PARTTYPE\n')

        value = package.get_attribute('value', None)
        device = package.get_attribute('device', 'unknown')
        # This change by SDB on 10.12.2003.
        if value is None:
            f.write(device + '\n')
        else:
            f.write(value + '\n')

        f.write('DESCRIPTION\n')
        f.write(device + '\n')
        for i in xrange(16):
            f.write('Part Field %d\n' % (i + 1))
            f.write('*\n')
        for i in xrange(8):
            f.write('LIBRARYFIELD%d\n' % (i + 1))
            f.write('\n')
        f.write(']\n')

    for net in reversed(netlist.nets):
        f.write('(\n')
        f.write(net.name + '\n')
        f.write('\n'.join(
            '%s-%s %s-%s PASSIVE' % (
                pin.package.refdes,
                pin.number,
                pin.package.get_attribute('device', 'unknown'),
                pin.number)
            for pin in reversed(net.connections)) + ' \n')
        f.write(')\n')
