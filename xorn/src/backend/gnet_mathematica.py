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

# Copyright (C) 2007-2010 John P. Doty

# Netlister for symbolic circuit analysis using Mathematica.
# See the Mathematica notebook gEDA.nb (obtainable at www.noqsi.com)
# for usage.

def write_model(package):
    model = package.get_attribute('model', None)
    if model is not None:
        return '%s["%s"]' % (model, package.refdes)

    device = package.get_attribute('device', 'unknown')
    value = package.get_attribute('value', package.refdes.lower())

    return '%s[value->%s]["%s"]' % (device.lower(), value, package.refdes)

def run(f, netlist):
    # write voltages
    for net in reversed(netlist.nets):
        # write pin voltages
        for pin in reversed(net.connections):
            f.write('v["%s","%s"]=v["%s"];\n' % (
                pin.package.refdes, pin.number, net.name))

    # write currents
    f.write('nodeEquations={\n')
    f.write(',\n'.join(
        '+'.join('i["%s","%s"]' % (pin.package.refdes, pin.number)
                 for pin in reversed(net.connections)) + '==0'
        for net in reversed(netlist.nets) if net.name != 'GND'))
    f.write('};\n')

    # write models
    f.write('modelEquations={\n')
    f.write(',\n'.join(write_model(package)
                       for package in reversed(netlist.packages)))
    f.write('};\n')

    # list voltages and currents
    f.write('variables={\n')
    f.write(',\n'.join('v["%s"]' % net.name
                       for net in reversed(netlist.nets) if net.name != 'GND'))
    for net in reversed(netlist.nets):
        for pin in reversed(net.connections):
            f.write(',\ni["%s","%s"]' % (pin.package.refdes, pin.number))
    f.write('};\n')
