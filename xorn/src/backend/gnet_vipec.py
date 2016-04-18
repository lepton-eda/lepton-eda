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

import collections

Template = collections.namedtuple('Template', ['keyword', 'attribs'])
Attrib = collections.namedtuple('Attrib', ['name', 'prefix', 'default'])

component_templates = {
    'RESISTOR': Template('RES', [
        Attrib('value', 'R=', 'use value attrib for resistance'),
    ]),
    'INDUCTOR': Template('IND', [
        Attrib('value', 'L=', 'use value attrib for inductance'),
        Attrib('Q', 'Q=', None)
    ]),
    'CAPACITOR': Template('CAP', [
        Attrib('value', 'C=', 'use value attrib for capacitance')
    ]),
    'TLIN': Template('TLIN', [
        Attrib('Z', 'Z=', 50),
        Attrib('length', 'E=', 'length attrib for length'),
        Attrib('F', 'F=', 'F attrib for frequency')
    ]),
    'CLIN': Template('CLIN', [
        Attrib('ZE', 'ZE=', None),
        Attrib('ZO', 'ZO=', None),
        Attrib('E', 'E=', None),
        Attrib('F', 'F=', None)
    ]),
    'SPARAMBLOCK': Template('BLOCK', [
        Attrib('filename', '', 'filename attrib for sparams')
    ])
}

def run(f, netlist):
    netnumbers = {}
    number = 1
    for net in reversed(netlist.nets):
        if net.name == 'GND':
            netnumbers[net.name] = 0
        else:
            netnumbers[net.name] = number
            number += 1

    f.write('% ViPEC RF Netlister\n')
    f.write('% Written by Matthew Ettus\n')
    f.write('% Based on code by Bas Gieltjes\n')
    f.write('CKT\n')

    # component writing
    for package in reversed(netlist.packages):
        device = package.get_attribute('device', 'unknown')
        if device not in ['VIPEC', 'SMITH', 'GRID']:
            # get template
            try:
                template = component_templates[device]
            except KeyError:
                package.warn("template \"%s\" not found" % device)
                template = Template('error', [])

            f.write('\t%s\t' % template.keyword)

            # write net name of node
            for i in xrange(len(package.pins)):
                try:
                    pin = package.get_pin_by_pinseq(i + 1)
                except KeyError:
                    netname = 'ERROR_INVALID_PIN'
                else:
                    netname = pin.net.name
                f.write(str(netnumbers.get(netname, '#<unspecified>')))
                f.write(' ')

            # write attribs
            for attrib in template.attribs:
                value = package.get_attribute(attrib.name, attrib.default)
                if value is None:
                    continue
                f.write('%s%s\t' % (attrib.prefix, value))

            f.write('\t%% %s\n' % package.refdes)

    # misc components
    f.write('\tDEF2P\t%s  %s\n' % (netnumbers.get('PORT1', '#<unspecified>'),
                                   netnumbers.get('PORT2', '#<unspecified>')))
    f.write('\tTERM\t50 50\n')
    f.write('\n')

    # analysis block
    for package in reversed(netlist.packages):
        if package.get_attribute('device', None) == 'VIPEC':
            value = package.get_attribute('value', None)
            if value is not None:
                f.write('R=%s\n' % value)
            f.write('\n')
