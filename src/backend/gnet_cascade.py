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

# Backend for cascade (http://rfcascade.sourceforge.net)
# Copyright (C) 2003-2010 Dan McMahill

def find_package(netlist, device):
    return next((package for package in reversed(netlist.packages)
                 if package.get_attribute('device', None) == device), None)

def run(f, netlist):
    # write the header
    f.write('# Cascade (http://rfcascade.sourceforge.net)\n')
    f.write('# Created with gEDA/gnetlist\n')
    f.write('\n')

    # Write out an initial "defaults" line if it exists
    # Locate and print out the global defaults if the element exists.
    package = find_package(netlist, 'cascade-defaults-top')
    if package is not None:
        f.write('# Initial global defaults\n')
        f.write('defaults ')
        for attrib in ['rin', 'RIN', 'rout', 'ROUT', 'rho', 'RHO']:
            val = package.get_attribute(attrib, None)
            if val is not None:
                f.write('%s=%s ' % (attrib, val))
        f.write('\n')
        f.write('\n')

    # Write out the "source" line and keep track of what it's connected to.
    f.write('# Source definition\n')

    package = find_package(netlist, 'cascade-source')
    if package is None:
        # If we couldn't find the source, then exit out.
        netlist.error("You must include a source element in your schematic!")
        return

    f.write('source ')
    for attrib in ['c', 'C', 'cn0', 'CN0', 'cn', 'CN', 'bw', 'BW']:
        val = package.get_attribute(attrib, None)
        if val is not None:
            f.write('%s=%s ' % (attrib, val))
    f.write('\n')

    # Find the first element in the cascade.
    sourcenet = package.pins_by_number['1'].net.connections
    if sourcenet[-1].package == package:
        package = sourcenet[-2].package
    else:
        package = sourcenet[-1].package

    # write the components
    f.write('\n')
    f.write('# Cascaded system\n')

    # Recursively follow the cascade and print out each element as it's found.
    while True:
        # Is this a "defaults" element or a normal element?
        # If its a defaults element, then print "defaults"
        # instead of the reference designator because that's
        # a keyword for cascade.
        if package.get_attribute('device', None) == 'cascade-defaults':
            f.write('defaults ')
        else:
            f.write(package.refdes + ' ')

        # spit out all the relevant attributes for element or
        # defaults lines
        for attrib in ['g', 'G', 'gp', 'GP', 'gv', 'GV', 'nf', 'NF',
                       'iip3', 'IIP3', 'r', 'R', 'rin', 'RIN', 'rout', 'ROUT',
                       'rho', 'RHO']:
            val = package.get_attribute(attrib, None)
            if val is not None:
                f.write('%s=%s ' % (attrib, val))
        f.write('\n')

        #f.write('cascade:follow-cascade  -- outnet = %s\n' % outnet)
        outnet = package.pins_by_number['2'].net.connections
        if len(outnet) < 2:
            break

        if outnet[-1].package == package:
            package = outnet[-2].package
        else:
            package = outnet[-1].package

    # write the footer
    f.write('\n')
    f.write('# End of netlist created by gEDA/gnetlist\n')
    f.write('\n')
