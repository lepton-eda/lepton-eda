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

# Ultiboard (ewnet) backend
# Copyright (C) 2011 Dan McMahill

# Notes about Ultiboard (.ewnet) netlists.
# "ew" stands for Electronic Workbench.

# FIXME -- the biggest problem with this backend is that I
# had no documentation at all on the file format.  I just
# found a .ewnet file online somewhere, read it, and guessed
# as to the right way to create these.  Full documentation would
# help considerably!

import sys

def run(f, netlist):
    sys.stderr.write("""\

---------------------------------
gEDA/gnetlist ewnet Backend
This backend is EXPERIMENTAL
Use at your own risk!

You may need to run the output netlist
through unix2dos before importing to
windows based layout tools
---------------------------------

""")

    # write the header
    f.write('(ToolInfo\n')
    f.write('\t(netlist "ULTIboard" 7 0 0)\n')
    f.write('\t(tool "Multisim" 7 0 0)\n')
    f.write('\t(timestamp "15:19:8" "9-6-2006")\n')
    f.write('\t(version 3 0 0)\n')
    f.write('\t(gateswap 2)\n')
    f.write('\t(pinswap 1)\n')
    f.write('\t(crossprobe {D6EE9C01-C93E-4246-9BC1-214A35C4954C})\n')
    f.write('\t(units Mil)\n')
    f.write(')\n')

    # write the nets
    f.write('(nets\n')
    for net in reversed(netlist.nets):
        f.write('\t( net "%s"\n' % net.name)
        f.write('\t\t(trackwidth "-1.00000000e+000")\n')
        f.write('\t\t(trackwidth_max "-1.00000000e+000")\n')
        f.write('\t\t(trackwidth_min "-1.00000000e+000")\n')
        f.write('\t\t(tracklength_max "-1.00000000e+000")\n')
        f.write('\t\t(tracklength_min "-1.00000000e+000")\n')
        f.write('\t\t(clearance_to_trace "-1.00000000e+000")\n')
        f.write('\t\t(clearance_to_pad "-1.00000000e+000")\n')
        f.write('\t\t(clearance_to_via "-1.00000000e+000")\n')
        f.write('\t\t(clearance_to_copper "-1.00000000e+000")\n')
        f.write('\t\t(routing_layer "")\n')
        f.write('\t\t(settings_locked "0")\n')
        f.write('\t\t(net_group "")\n')
        f.write('\t)\n')
    f.write(')\n')

    # write the components
    f.write('(components\n')

    for package in reversed(netlist.packages):
        # start the instance, write the footprint and the reference designator
        f.write('\t(instance "%s" "%s"\n' % (
            package.get_attribute('footprint', 'unknown'), package.refdes))

        # If there is a "value" attribute, output that.
        # Otherwise output the "device" attribute (the symbol name).
        device = package.get_attribute('device', 'unknown')
        value = package.get_attribute('value', device)
        f.write('\t\t(device "%s")\n' % device)
        f.write('\t\t(value "%s")\n' % value)

        f.write('\t\t(gateswap "0")\n')
        f.write('\t\t(pinswap "0")\n')
        f.write('\t\t(component_space "0.00000000e+000")\n')
        f.write('\t\t(component_group "")\n')
        f.write('\t\t(settings_locked "0")\n')
        f.write('\t\t(comp_variants "Default1;")\n')
        f.write('\t\t(comp_variant_independent "0")\n')

        # write the pins for this component
        for pin in reversed(package.pins):
            # pin number
            f.write('\t\t(pin "%s"\n' %
                    pin.get_attribute('pinnumber', 'unknown'))

            if pin.net.is_unconnected_pin:
                netname = '#f'
            else:
                netname = pin.net.name
            f.write('\t\t\t(net "%s")\n' % netname)

            # pin type.  I have seen "PWR", "GND", "IN", "OUT", "BIDIR"
            # FIXME -- need to translate between geda and the allowed
            # ewnet types here
            f.write('\t\t\t(pintype "%s")\n' % 'BIDIR')

            f.write('\t\t\t(gategroup "")\n')
            f.write('\t\t\t(pingroup "")\n')

            f.write('\t\t\t(label "%s")\n' % pin.number)  # label (pin name)

            f.write('\t\t\t(gate "")\n')

            f.write('\t\t)\n')

        # close the part
        f.write('\t)\n')

    f.write(')\n')

    # write the board layers
    # write out the layer information
    # FIXME -- can this just be left out?  If not, how shall we define
    # stackup in a way where gnetlist can find it?
    f.write('(layers\n')
    f.write('\t(layer "Copper Bottom"\n')
    f.write('\t\t(routable "1")\n')
    f.write('\t\t(type "Signal")\n')
    f.write('\t)\n')
    f.write('\t(layer "Copper Top"\n')
    f.write('\t\t(routable "1")\n')
    f.write('\t\t(type "Signal")\n')
    f.write('\t)\n')
    f.write(')\n')
