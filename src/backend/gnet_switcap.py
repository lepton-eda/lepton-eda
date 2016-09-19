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

# Copyright (C) 2003, 2005-2010 Dan McMahill

# This is gnetlist backend for the SWITCAP switched capacitor
# simulator.  This backend was written by Dan McMahill
# 'mcmahill at alum dot mit dotedu' who used the SPICE backend by
# S. Gieltjes as a starting point.

import errno, sys
import util_alias

# This procedure takes a net name as determined by gnetlist and
# modifies it to be a valid SWITCAP net name.

def map_net_names(net):
    net_name = net.name
    # Change "GND" to "0"
    if net_name == 'GND':
        net_name = '0'
    # remove the 'unnamed_net' part
    elif net.unnamed_counter is not None:
        net_name = str(net.unnamed_counter)

    # Truncate to 7 characters and convert to all upper case
    return net_name[:7].upper()

# This procedure takes a refdes as determined by gnetlist and
# modifies it to be a valid SWITCAP refdes.  In particular,
# we need to make sure that
#
# - the first character is correct for the component type
#
# - we do not exceed 8 characters.  Note the 8 comes from
#   the first character which denotes component type plus
#   7 for the unique identifier part.
#
# - we are all caps (switcap is not case sensitive)

def map_refdes(package):
    # Convert to all upper case
    refdes_alias = package.refdes.upper()

    # Make sure the first character is correct for this component type
    device = package.get_attribute('device', None)
    if device == 'SWITCAP-switch':
        if refdes_alias[0] != 'S':
            refdes_alias = 'S' + refdes_alias
    elif device == 'SWITCAP-capacitor':
        if refdes_alias[0] != 'C':
            refdes_alias = 'C' + refdes_alias
    elif device == 'SWITCAP-vcvs':
        if refdes_alias[0] != 'E':
            refdes_alias = 'E' + refdes_alias
    elif device == 'SWITCAP-vsrc':
        if refdes_alias[0] != 'V':
            refdes_alias = 'V' + refdes_alias

    # Truncate to 8 characters (1 for the first character and
    # 7 for the identifier)
    refdes_alias = refdes_alias[:8]

    #f.write('(switcap:map-refdes %s) ===> %s\n' % (refdes, refdes_alias))

    return refdes_alias

# Given a reference designator and attribute name
# write out the attribute with warnings if the attribute has
# not been set

def get_attrib(package, attrib):
    val = package.get_attribute(attrib, 'unknown')
    if val == 'unknown':
        f.write("*** WARNING ***\n")
        f.write("Required attribute \"%s\" is not set on component \"%s\"."
                "  Please correct this.\n" % (attrib, package))
        f.write("\n")
    return val

# Write analysis block
#
# For now, we only support writing the analysis blocks in a file
# and including that via a switcap-analysis-1.sym instantiation

def write_analysis(fname):
    f.write('/* reading analysis from "%s" */\n' % fname)
    try:
        g = open(fname)
    except IOError as e:
        if e.errno == errno.ENOENT:
            sys.stderr.write("ERROR: Analysis file '%s' not found.\n" % fname)
        else:
            sys.stderr.write("ERROR: Can't open analysis file '%s': %s\n"
                             % (fname, e.strerror))
        sys.exit(3)
    try:
        for line in g:
            f.write(line)
    finally:
        g.close()

# Switcap netlist generation -- top level

def run(f, netlist):
    # initialize the net-name and refdes aliasing
    net_aliases = util_alias.build_net_aliases(map_net_names, netlist.nets)
    refdes_aliases = util_alias.build_refdes_aliases(map_refdes, netlist.packages)

    # Write top Switcap netlist header
    f.write('/* Switcap netlist produced by gnetlist (part of gEDA)     */\n')
    f.write('/* See http://www.geda-project.org/ for more information.  */\n')
    f.write('/* Switcap backend written by Dan McMahill                 */\n')
    f.write('\n')
    f.write('\n')

    # Write the main TITLE and OPTIONS block
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', None)
        if device == 'SWITCAP-options':
            # write options
            # OPTIONS; OPT1; OPT2; ...; END;
            # valid options are: WIDTH132 NOLIST REPORT NOOVRLAY GRID
            f.write('OPTIONS; %s END;\n' % get_attrib(package, 'OPTIONS'))
            f.write('\n')
        elif device == 'SWITCAP-title':
            # write title
            # TITLE: my title;
            # Can only have 64 characters in the title
            # TODO: need to truncate to 64 chars
            f.write('TITLE:%s;\n' % get_attrib(package, 'TITLE'))
            f.write('\n')

    f.write('TIMING;\n')

    # Write the main TIMING block
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', None)
        if device == 'SWITCAP-clock':
            # write clock definition
            # CLOCK clock_name period (phi_start phi_stop)
            f.write('     CLOCK %s %s (%s %s);\n' % (
                package.refdes,
                get_attrib(package, 'PERIOD'),
                get_attrib(package, 'PSTART'),
                get_attrib(package, 'PSTOP')))
        elif device == 'SWITCAP-timing':
            # write master clock period
            # PERIOD clock_period;
            f.write('     PERIOD %s;\n' % get_attrib(package, 'PERIOD'))

    f.write('END;\n')
    f.write('\n')
    f.write('CIRCUIT;\n')

    # Write the main CIRCUIT block netlist
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', None)
        if device == 'SWITCAP-capacitor':
            # write capacitor: refdes, nodes and value
            # C### (N1 N2) value;
            f.write('     %s (%s %s) %s;\n' % (
                refdes_aliases[package],
                net_aliases[package.pins_by_number['1'].net],
                net_aliases[package.pins_by_number['2'].net],
                get_attrib(package, 'value')))
        elif device == 'SWITCAP-switch':
            # write switch: refdes, nodes and clock
            # S### (N1 N2) clk;
            f.write('     %s (%s %s) %s;\n' % (
                refdes_aliases[package],
                net_aliases[package.pins_by_number['1'].net],
                net_aliases[package.pins_by_number['2'].net],
                get_attrib(package, 'clock')))
        elif device == 'SWITCAP-vcvs':
            # write voltage controlled voltage source: refdes, nodes and clock
            # E### (OUTP OUTM INP INM) gain;
            f.write('     %s (%s %s %s %s) %s;\n' % (
                refdes_aliases[package],
                net_aliases[package.pins_by_number['1'].net],
                net_aliases[package.pins_by_number['2'].net],
                net_aliases[package.pins_by_number['3'].net],
                net_aliases[package.pins_by_number['4'].net],
                get_attrib(package, 'gain')))
        elif device == 'SWITCAP-vsrc':
            # write voltage source: refdes and nodes
            # V### (OUTP OUTM);
            f.write('     %s (%s %s);\n' % (
                refdes_aliases[package],
                net_aliases[package.pins_by_number['1'].net],
                net_aliases[package.pins_by_number['2'].net]))

    f.write('END;\n')
    f.write('\n')

    # Write the main ANALYSIS block
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', None)
        if device == 'SWITCAP-analysis':
            write_analysis(get_attrib(package, 'file'))

    f.write('\n')
    f.write('\n')
    f.write('/* End of SWITCAP netlist */\n')
    f.write('END;\n')
