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

# SPICE netlist backend written by S. Gieltjes

# Common functions for the `spice' and `spice-sdb' backends
from spice_common import *

# write mos transistor

def write_mos_transistor(f, package):
    write_one_component(f, package)
    # create list of attributes which can be attached to a mosfet
    attrib_list = ['l', 'w', 'as', 'ad', 'pd', 'ps', 'nrd', 'nrs', 'temp', 'ic']
    write_list_of_attributes(f, package, attrib_list)
    # write the off attribute separately
    off_value = package.get_attribute('off', 'unknown')
    if off_value in ['#t', '1']:
        f.write(' off')
    f.write('\n')

# Include a file

def write_include(f, package):
    f.write('%s %s\n' % (package.refdes, component_value(package)))

# write the refdes, the net name connected to pin# and the
# component value. No extra attributes.

def write_one_component(f, package):
    f.write(package.refdes + ' ')
    # write net names, slotted components not implemented
    write_net_names_on_component(f, package)
    # write component value, if components have a label "value=<hash>"
    # what if a component has no value label, currently unknown is written
    f.write(component_value(package))

# Spice netlist generation

def run(f, netlist):
    f.write('* Spice netlister for gnetlist\n')
    # search for specific device labels
    for package in reversed(netlist.packages):
        # write the refdes, to the pin# connected net and component
        # value and optional extra attributes
        # check if the component is a special spice component
        device = package.get_attribute('device', 'unknown')
        if device == 'SPICE-ccvs':
            write_ccvs(f, package)
        elif device == 'SPICE-cccs':
            write_cccs(f, package)
        elif device == 'SPICE-vcvs':
            write_vcvs(f, package)
        elif device == 'SPICE-vccs':
            write_vccs(f, package)
        elif device == 'SPICE-nullor':
            write_nullor(f, package)
        elif device == 'PMOS_TRANSISTOR':
            write_mos_transistor(f, package)
        elif device == 'NMOS_TRANSISTOR':
            write_mos_transistor(f, package)
        elif device == 'include':
            write_include(f, package)
        else:
            write_one_component(f, package)
            f.write('\n')

    f.write('.END\n')
