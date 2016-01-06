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

# Netlister for GOSSIP system simulation system
#  For more info see http://gossip.sourceforge.net

def run(f, netlist):
    f.write(';; Gossip Netlist Created by gNetlist\n')
    f.write('\n')
    f.write(';; Created By Matt Ettus <matt@ettus.com>\n')
    f.write(';; Libraries:\n')
    f.write('\n')

    done = set()
    for package in reversed(netlist.packages):
        lib = package.get_attribute('library', 'unknown')
        if lib == 'unknown':
            package.warn("does not have a library attribute")
        if lib in done:
            continue

        f.write('(use-library %s *)\n' % lib)
        done.add(lib)

    blockname = netlist.get_toplevel_attribute('blockname', 'not found')
    f.write('(define-block (%s (\n' % blockname)
    f.write('(signals (%s))\n' % ' '.join(
        net.name for net in reversed(netlist.nets)))

    for package in reversed(netlist.packages):
        f.write('   (')
        f.write(package.refdes)

        i = 1
        while True:
            if str(i) not in package.pins_by_number:
                break
            pin = package.pins_by_number[str(i)]
            pinname = pin.get_attribute('label', None)
            if pinname is None:
                break

            f.write(' :%s ' % pinname)

            if not pin.net.is_unconnected_pin:
                f.write(pin.net.name)
            else:
                f.write('Not Connected')

            i += 1

        f.write(')\n')
