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

# gnetlist back end for dumping all attributes and netlist in lihata format
# Copyright (C) 2014 Tibor 'Igor2' Palinkas

from dump_common import *

class Callback:
    def write_begin(self, f):
        f.write('ha:gschem {\n')
        f.write('\tha:elements {\n')

    # Formatting: write (element) pins

    def write_pin_attribs_pre(self, f, component):
        f.write('\t\t\tha:pins {\n')

    def write_pin_attrib_pre(self, f, component, cpin):
        f.write('\t\t\t\tha:%s-%s {\n' % (get_refdes(component),
                                          cpin.blueprint.number))

    def write_pin_attrib(self, f, component, cpin, pinattrib):
        f.write('\t\t\t\t\t%s={%s}\n' % (
            pinattrib, cpin.blueprint.get_attribute(pinattrib, 'unknown')))

    def write_pin_attrib_post(self, f, component, cpin):
        f.write('\t\t\t\t}\n')

    def write_pin_attribs_post(self, f, component):
        f.write('\t\t\t}\n')

    def write_pin_nets_pre(self, f, component):
        f.write('\t\t\tha:pin-conns {\n')

    def write_pin_net_pre(self, f, component, cpin):
        pass

    def write_pin_net(self, f, component, cpin):
        f.write('\t\t\t\t%s-%s = {%s}\n' % (get_refdes(component),
                                            cpin.blueprint.number,
                                            cpin.local_net.net.name))

    def write_pin_net_post(self, f, component, cpin):
        pass

    def write_pin_nets_post(self, f, component):
        f.write('\t\t\t}\n')

    def write_pinlist(self, f, component, cpin):
        pass

    def write_pinlist_pre(self, f, component):
        pass

    def write_pinlist_post(self, f, component):
        pass

    # Formatting: write elements

    def write_element_pre(self, f, component):
        f.write('\t\tha:%s {\n' % get_refdes(component))

    def write_element_attrib_pre(self, f, component):
        f.write('\t\t\tha:attribs {\n')

    def write_element_attrib(self, f, component, attribname):
        f.write('\t\t\t\t%s={%s}\n' % (
            attribname, component.blueprint.get_attribute(attribname)))

    def write_element_attrib_post(self, f, component):
        f.write('\t\t\t}\n')

    def write_element_post(self, f, component):
        f.write('\t\t}\n')

    def write_middle(self, f):
        f.write('\t}\n')
        f.write('\tha:nets {\n')

    # Formatting: print netlists

    def write_netlist_pin(self, f, cpin):
        f.write('\t\t\t%s-%s\n' % (get_refdes(cpin.component),
                                   cpin.blueprint.number))

    def write_netlist_pre(self, f, net):
        f.write('\t\tli:%s {\n' % net.name)

    def write_netlist_post(self, f, net):
        f.write('\t\t}\n')

    def write_end(self, f):
        f.write('\t}\n')
        f.write('}\n')

def run(f, netlist):
    dump(f, netlist, Callback())
