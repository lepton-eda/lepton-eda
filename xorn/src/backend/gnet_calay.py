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

# Copyright (C) 2006-2010 John P. Doty

# Calay format (modified from Ales's gnet-PCB.scm by jpd)
# Netname translation cleaned up at Dan McMahill'suggestion -jpd

import util_alias
from util_wrap import wrap

def run(f, netlist):
    net_aliases = util_alias.build_net_aliases(
        lambda net: net.name.replace('_', '-'), netlist.nets)

    for net in reversed(netlist.nets):
        f.write('/')
        f.write(net_aliases[net])
        f.write('\t')
        f.write(wrap(''.join(' %s(%s)' % (pin.package.refdes, pin.number)
                             for pin in reversed(net.connections)) + ';',
                     76, ',', '          '))
