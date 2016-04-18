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

# FutureNet2 backend
# Copyright (C) 2003, 2005-2010 Dan McMahill

# Notes about futurenet2 (.NV2) format pinlists
#
#  - Case does not seem to be preserved so to avoid issues,
#    simply output the netnames in all caps.
#
#  - Netname length is 8 characters max.  +,-, and _ are allowed
#
#  - How are DATA,3 and DATA,4 used?  In one example, DATA,4 is
#    not used.  In the other DATA,3 and DATA,4 are identical and
#    appear to be set to the value (10.0k for example) of the part.
#
#    From Ferenc Marton (martonf at datapress dot hu):
#
#    These get combined in Ranger2 to produce the device
#    description with a "," separating the two entries.
#    DATA,3 is the "device name", max of 5 characters
#    DATA,4 is the "device value", max of 13 characters.
#    We could put the footprint into DATA,4
#
#  - In the "PIN" and "SIG" lines, what are the various fields really
#    doing?
#
#    It seems that for a PIN line, the format is:
#
#    PIN,,<netname>,1-1,5,<net attribute number>,<pinnumber>
#
#    What are the "1-1" and the "5"?  On the <net attribute
#    number> I've seen "23", "25", and "100".  Maybe these
#    indicate signal vs power or some sort of routing preference?
#
#    For a SIG line, the format seems to be:
#
#    SIG,<netname>,1-1,5,<netname>
#
#    What exactly are "1-1" and "5"?  I think maybe the "1-1" part
#    has something to do with sheet number in a multipage schematic.

import sys
import util_alias

# This procedure takes a net name as determined by gnetlist and
# modifies it to be a valid FutureNet2 net name.
def map_net_names(net):
    net_name = net.name

    # Remove "unnamed_net" and replace with "NET"
    if net.unnamed_counter is not None:
        net_name = 'NET' + str(net.unnamed_counter)

    # Truncate to 8 characters
    net_name = net_name[:8]

    # Convert to all upper case
    return net_name.upper()

# This procedure takes a refdes as determined by gnetlist and
# modifies it to be a valid FutureNet2 refdes.
def map_refdes(package):
    # XXX do we need to truncate to 8 characters
    # like with net names?
    #refdes = refdes[:8]

    # Convert to all upper case
    return package.refdes.upper()

def run(f, netlist):
    sys.stderr.write("""\

---------------------------------
gEDA/gnetlist FutureNet2 Backend
This backend is EXPERIMENTAL
Use at your own risk!

You may need to run the output netlist
through unix2dos before importing to
Ranger2 or other windows based layout tools
---------------------------------

""")

    # initialize the net-name aliasing
    net_aliases = util_alias.build_net_aliases(map_net_names, netlist.nets)

    # initialize the refdes aliasing
    refdes_aliases = util_alias.build_refdes_aliases(map_refdes, netlist.packages)

    # write the header
    f.write('PINLIST,2\n')
    f.write('(DRAWING,GEDA.PIN,1-1\n')

    # write the components
    symcnt = 1
    for package in reversed(netlist.packages):
        f.write('(SYM,%s\n' % symcnt)

        # write the reference designator
        f.write('DATA,2,%s\n' % refdes_aliases[package])

        # If there is a "value" attribute, output that.
        # Otherwise output the "device" attribute (the symbol name).
        val = package.get_attribute('value', None)
        if val is None:
             val = package.get_attribute('device', 'unknown')
        f.write('DATA,3,%s\n' % val)

        # write the footprint
        f.write('DATA,4,%s\n' % package.get_attribute('footprint', 'unknown'))

        # write the pins for this component
        for pin in reversed(package.pins):
            #  PIN,,NetName,1-1,5,20/23,pinnum
            f.write('PIN,,')

            if pin.net.is_unconnected_pin:
                f.write('#f')
            else:
                f.write(net_aliases[pin.net])

            # XXX I've seen 20, 23, and 100 in the position where the
            # "23" is here.  Seems to be a property like signal vs
            # power net.  Not sure how to support that.
            f.write(',1-1,5,23,')

            f.write(pin.get_attribute('pinnumber', 'unknown'))
            f.write('\n')

        # close the part
        f.write(')\n')

        symcnt += 1
    f.write(')\n')

    # write the nets
    for net in reversed(netlist.nets):
        f.write('SIG,%s,1-1,5,%s\n' % (net_aliases[net], net_aliases[net]))

    # terminating ")"
    f.write(')\n')
