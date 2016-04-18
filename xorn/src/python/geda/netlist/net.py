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

## \namespace xorn.geda.netlist.net
## Grouping local nets with the same name into a net.

import sys
from gettext import gettext as _

class Net:
    def __init__(self, netlist):
        self.netlist = netlist
        self.local_nets = []
        self.names = { False: [], True: [] }
        self.namespace = None
        self.unmangled_name = None
        self.name = None  # set by netlist ctor
        self.unnamed_counter = None  # only for unnamed nets/buses
        self.is_unconnected_pin = False

        self.component_pins = []
        self.connections = None  # populated by xorn.geda.netlist.package

        self.sheets_and_net_blueprints = []

        # populated by xorn.geda.netlist.pp_graphical
        self.graphical_component_pins = None

    ## Given an attribute and a wanted attribute, return all the given
    ## attribute of all the graphical objects connected to this net.

    def graphical_objs_with_attrib_get_attrib(
            self, has_attrib_name, has_attrib_value, wanted_attrib):
        if not isinstance(has_attrib_name, basestring) or \
           not isinstance(has_attrib_value, basestring) or \
           not isinstance(wanted_attrib, basestring):
            raise ValueError

        # walk through the list of components, and through the list
        # of individual pins on each, adding net names to the list
        # being careful to ignore duplicates, and unconnected pins
        l = []
        for cpin in self.graphical_component_pins:
            if cpin.component.blueprint.get_attribute(has_attrib_name, None) \
                   != has_attrib_value:
                continue
            try:
                l.append(cpin.component.blueprint.get_attribute(wanted_attrib))
            except KeyError:
                pass
        return l

    def merge_into(self, other):
        if not isinstance(other, Net):
            raise ValueError
        if other.netlist != self.netlist:
            raise ValueError
        if other == self:
            raise ValueError

        for local_net in self.local_nets:
            assert local_net.net == self
            local_net.net = other

        other.local_nets += self.local_nets
        for from_netattrib in [False, True]:
            for name in self.names[from_netattrib]:
                if name not in other.names[from_netattrib]:
                    other.names[from_netattrib].append(name)

        del self.local_nets[:]
        del self.names[False][:]
        del self.names[True][:]

        self.netlist.nets.remove(self)

    def error(self, msg):
        sys.stderr.write(_("net `%s': error: %s\n") % (self.name, msg))
        self.netlist.failed = True

    def warn(self, msg):
        sys.stderr.write(_("net `%s': warning: %s\n") % (self.name, msg))

def postproc_instances(netlist, flat_namespace, prefer_netname_attribute,
                                default_net_name, default_bus_name):
    netlist.nets = []
    net_dict = {}

    # Naming nets
    for sheet in netlist.sheets:
        for local_net in sheet.local_nets:
            # handle netname= attributes
            # TODO: Ignore netname attributes on buses

            for net_name, from_netattrib in \
                    [(name, False) for name in local_net.blueprint.names] + \
                    [(net_name, True) for net_name in
                         local_net.blueprint.names_from_net_attribute]:
                if not isinstance(net_name, basestring):
                    raise ValueError

                if flat_namespace[from_netattrib] \
                       or sheet.instantiating_component is None:
                    net_name = None, net_name
                else:
                    net_name = sheet, net_name

                try:
                    net = net_dict[net_name]
                except KeyError:
                    net = Net(netlist)
                    netlist.nets.append(net)
                    net_dict[net_name] = net

                if net_name not in net.names[from_netattrib]:
                    net.names[from_netattrib].append(net_name)

                if local_net.net == net:
                    continue

                if local_net.net is None:
                    local_net.net = net
                else:
                    a = netlist.nets.index(local_net.net)
                    b = netlist.nets.index(net)
                    dst = netlist.nets[min(a, b)]
                    src = netlist.nets[max(a, b)]

                    for name in set(src.names[False] + src.names[True]):
                        assert net_dict[name] == src
                        net_dict[name] = dst

                    src.merge_into(dst)

                net.local_nets.append(local_net)

    # prioritize net names
    prio = not prefer_netname_attribute

    for net in netlist.nets:
        if len(net.names[prio]) == 0:
            # looking for non-prioritized name
            if len(net.names[not prio]) == 0:
                raise AssertionError  # shouldn't happen
            elif len(net.names[not prio]) == 1:
                net.namespace, net.unmangled_name = net.names[not prio][0]
            else:
                netlist.warn(_("net has more than one name: %s")
                             % _(" vs. ").join(
                                 _("\"%s\"") % name
                                 for ns, name in net.names[not prio]))
                net.namespace, net.unmangled_name = net.names[not prio][-1]
        elif len(net.names[prio]) == 1:
            net.namespace, net.unmangled_name = net.names[prio][0]
        else:
            netlist.warn(_("net has more than one name: %s")
                         % _(" vs. ").join("\"%s\"" % name
                                           for ns, name in net.names[prio]))
            net.namespace, net.unmangled_name = net.names[prio][-1]

    # give all remaining unnamed nets a name

    unnamed_net_bus_counter = { False: 1, True: 1 }
    unnamed_pin_counter = 1

    for component in netlist.components:
        for cpin in component.cpins:
            local_net = cpin.local_net
            assert local_net is not None
            assert cpin in local_net.cpins

            net = local_net.net
            if net is not None:
                continue

            # didn't find named net
            # didn't find previously named
            net = Net(netlist)
            netlist.nets.append(net)
            local_net.net = net
            net.local_nets.append(local_net)

            if len(net.local_nets) == 1 and \
               len(net.local_nets[0].blueprint.pins) == 1 and \
               not net.local_nets[0].blueprint.net_segments:
                net.is_unconnected_pin = True

            net_name = None
            unnamed_counter = None

            while net_name is None or net_name in net_dict:
                # we don't want to assign a dangling pin
                # which is signified by having only a head node
                # which is just a place holder
                # and the head node shows up here

                if net.is_unconnected_pin:
                    net_name = 'unconnected_pin-%d' % unnamed_pin_counter
                    unnamed_pin_counter += 1
                else:
                    is_bus = False  # there are no busses yet

                    unnamed_counter = unnamed_net_bus_counter[is_bus]
                    unnamed_net_bus_counter[is_bus] += 1

                    if not is_bus:
                        unnamed_string = default_net_name
                    else:
                        unnamed_string = default_bus_name

                    net_name = '%s%d' % (unnamed_string, unnamed_counter)

                if flat_namespace[False] \
                       or component.sheet.instantiating_component is None:
                    net_name = None, net_name
                else:
                    net_name = component.sheet, net_name

            net.namespace, net.unmangled_name = net_name
            net.unnamed_counter = unnamed_counter
            net_dict[net_name] = net

    # reorder nets

    # walk through the list of components, and through the list of
    # individual pins on each, adding nets to the list being careful
    # to ignore duplicates

    # Searching for an element in a list is incredibly inefficient.
    # It is *much* better to keep a set as reference.

    nets = []
    net_set = set()

    for component in netlist.components:
        for cpin in component.cpins:
            if cpin.local_net.net not in net_set:
                # add the net to the list
                nets.append(cpin.local_net.net)
                net_set.add(cpin.local_net.net)

                #for local_net in cpin.local_net.net.local_nets:
                #    net._cpins += local_net.cpins

    assert sorted(netlist.nets) == sorted(nets)
    netlist.nets = nets
