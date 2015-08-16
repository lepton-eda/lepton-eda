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

# gnetlist back end for dumping all attributes and netlist in custom format
# Copyright (C) 2014 Tibor 'Igor2' Palinkas

# Generic element list

import xorn.geda.attrib

# Custom function to append ".${SLOT}" where a component has a
# "slot=${SLOT}" attribute attached.

def get_refdes(component):
    # Replicate buggy behavior of "get-attrib-value-by-attrib-name":
    # if there is no "slot" attribute attached to the component, treat
    # it as non-slotted even if it inherits a "slot" attribute.
    if not xorn.geda.attrib.search_attached(component.blueprint.ob, 'slot'):
        return component.refdes

    try:
        slot = component.blueprint.get_attribute('slot')
    except KeyError:
        return component.refdes
    else:
        return '%s.%s' % (component.refdes, slot)

def dump(f, netlist, callback):
    callback.write_begin(f)

    # write all attributes of an element
    for component in reversed(netlist.components):
        if component.refdes is None:
            # skip power symbols
            continue

        callback.write_element_pre(f, component)

        # write all attributes of a pin
        callback.write_pinlist_pre(f, component)

        # run these on pinlist
        # print the pin list
        for cpin in reversed(component.cpins):
            callback.write_pinlist(f, component, cpin)

        callback.write_pinlist_post(f, component)

        callback.write_pin_attribs_pre(f, component)
        # print each pin
        for cpin in reversed(component.cpins):
            callback.write_pin_attrib_pre(f, component, cpin)
            callback.write_pin_attrib(f, component, cpin, "pinseq")
            callback.write_pin_attrib(f, component, cpin, "pintype")
            callback.write_pin_attrib(f, component, cpin, "pinlabel")
            callback.write_pin_attrib_post(f, component, cpin)

        callback.write_pin_attribs_post(f, component)

        callback.write_pin_nets_pre(f, component)
        # print each pin
        for cpin in reversed(component.cpins):
            callback.write_pin_net_pre(f, component, cpin)
            callback.write_pin_net(f, component, cpin)
            callback.write_pin_net_post(f, component, cpin)
        callback.write_pin_nets_post(f, component)

        callback.write_element_attrib_pre(f, component)
        for attribname in reversed(
                component.blueprint.get_attribute_names(False)):
            callback.write_element_attrib(f, component, attribname)
        callback.write_element_attrib_post(f, component)
        callback.write_element_post(f, component)

    callback.write_middle(f)

    # Write out all the signals
    for net in reversed(netlist.nets):
        # Write out each signal
        callback.write_netlist_pre(f, net)
        # List all connections to a net
        for cpin in reversed(net.component_pins):
            if cpin.component.refdes is None:
                # skip power symbols
                continue
            callback.write_netlist_pin(f, cpin)
        callback.write_netlist_post(f, net)

    callback.write_end(f)
