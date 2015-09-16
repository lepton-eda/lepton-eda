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

# Copyright (C) 2004-2010 Braddock Gaskill (braddock@braddock.com,
#                                           adapted PCB code to Eagle)

# EAGLE netlist format

import util_alias

def run(f, netlist):
    # initialize the net-name aliasing
    # Convert to all upper case because Eagle seems to do that
    # internally anyway and we'd rather do it here to catch shorts
    # created by not preserving case.  Plus we can eliminate lots of
    # ECO changes that will show up during backannotation.
    net_aliases = util_alias.build_net_aliases(
        lambda net: net.name.upper(), netlist.nets)

    # print out the header
    #f.write('!EAGLE-POWERPCB-V3.0-MILS!\n')
    #f.write('\n')
    #f.write('*PART*\n')
    #f.write('/* CADSoft Eagle Scripted Netlist Format */\n')
    f.write('   ;\n')

    # print out the parts
    for package in reversed(netlist.packages):
        pattern = package.get_attribute('pattern', None)
        # The above pattern should stay as 'pattern' and not 'footprint'
        if pattern is not None:
            f.write(pattern)

        lib = package.get_attribute('lib', None)
        if lib is None:
            lib = 'smd-ipc'
        f.write("ADD '%s' %s@%s (1 1);\n" % (
            package.refdes,
            package.get_attribute('footprint', 'unknown'), lib))

        value = package.get_attribute('value', None)
        device = package.get_attribute('device', None)

        if value is not None:
            f.write("VALUE '%s' '%s';\n" % (package.refdes, value))
        elif device is not None:
            f.write("VALUE '%s' '%s';\n" % (package.refdes, device))

    # print out the net information
    #f.write('\n')
    #f.write('*NET*\n')
    for net in reversed(netlist.nets):
        f.write("SIGNAL '%s'\n" % net_aliases[net])
        for pin in reversed(net.connections):
            f.write("   '%s' '%s'\r\n" % (pin.package.refdes, pin.number))
        f.write(";\n")

    # print out the footer
    #f.write('\n')
    #f.write('*END*\n')
