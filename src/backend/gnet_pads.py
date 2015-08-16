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

# PADS netlist format

import util_alias
from util_wrap import wrap

class NewlineReplacer:
    def __init__(self, f):
        self.f = f

    def write(self, s):
        self.f.write(str(s).replace('\n', '\r\n'))

def run(f, netlist):
    f = NewlineReplacer(f)

    # initialize the net-name aliasing
    # Convert to all upper case because Pads seems to do that
    # internally anyway and we'd rather do it here to catch shorts
    # created by not preserving case.  Plus we can eliminate lots of
    # ECO changes that will show up during backannotation.
    net_aliases = util_alias.build_net_aliases(
        lambda net: net.name.upper(), netlist.nets)

    # initialize the refdes aliasing
    # Convert to all upper case because Pads seems to do that
    # internally anyway and we'd rather do it here to catch name
    # clashes created by not preserving case.
    refdes_aliases = util_alias.build_refdes_aliases(
        lambda package: package.refdes.upper(), netlist.packages)

    # print out the header
    f.write('!PADS-POWERPCB-V3.0-MILS!\n')
    f.write('\n')
    f.write('*PART*\n')

    # print out the parts
    for package in reversed(netlist.packages):
        pattern = package.get_attribute('pattern', None)
        # The above pattern should stay as 'pattern' and not 'footprint'
        if pattern is not None:
            f.write(pattern)

        # print out the refdes with aliasing
        f.write('%s\t%s\n' % (refdes_aliases[package],
                              package.get_attribute('footprint', 'unknown')))

    # print out the net information
    f.write('\n')
    f.write('*NET*\n')
    for net in reversed(netlist.nets):
        f.write('*SIGNAL* %s\n' % net_aliases[net])
        f.write(wrap(
            ' ' + ' '.join(
                '%s.%s' % (refdes_aliases[pin.package], pin.number)
                for pin in reversed(net.connections)),
            78, '', ' '))

    # print out the footer
    f.write('\n')
    f.write('*END*\n')
