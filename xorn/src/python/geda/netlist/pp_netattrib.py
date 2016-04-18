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

## \namespace xorn.geda.netlist.pp_netattrib
## Post-processing: Artificial pins.

from gettext import gettext as _
import xorn.geda.attrib
import xorn.geda.netlist.blueprint

## Characters used to separate the pin numbers in a \c "net=" attribute.
NET_ATTRIB_DELIMITERS = ',; '

## Split a string into non-empty parts separated by a set of delimiters.

def strtok(s, delim):
    start = 0
    while start < len(s):
        found = [i for i in (s.find(d, start) for d in delim) if i != -1]
        if found:
            end = min(found)
        else:
            end = len(s)
        if end != start:
            yield s[start:end]
        start = end + 1

def postproc_blueprints(netlist):
    # Handle a "net=name:pin,pin..." attribute by creating appropriate
    # Pin objects.

    for schematic in netlist.schematics:
        for component in schematic.components:
            # first look inside the component, then outside the component
            pinnumbers = []
            assignments = {}

            for is_inherited, func in [
                    (True, xorn.geda.attrib.search_inherited),
                    (False, xorn.geda.attrib.search_attached)]:
                for value in func(component.ob, 'net'):
                    # A "net=" attribute has been found in the component.

                    try:
                        pos = value.index(':')
                    except ValueError:
                        component.error(
                            _("invalid net= attribute: \"%s\"") % value)
                        continue

                    # skip over first colon
                    for pinnumber in strtok(
                            value[pos + 1:], NET_ATTRIB_DELIMITERS):
                        try:
                            l = assignments[pinnumber]
                        except KeyError:
                            pinnumbers.append(pinnumber)
                            l = assignments[pinnumber] = []
                        l.append((value[:pos], is_inherited))

            for pinnumber in pinnumbers:
                try:
                    pin = component.pins_by_number[pinnumber]
                except KeyError:
                    net = xorn.geda.netlist.blueprint.Net(schematic, [])
                    pin = xorn.geda.netlist.blueprint.Pin(component, None)
                    pin.number = pinnumber
                    component.pins_by_number[pinnumber] = pin
                    pin.net = net
                    net.pins.append(pin)

                netnames = [netname for netname, is_inherited in
                                      assignments[pinnumber]
                            if not is_inherited]
                if not netnames:
                    netnames = [netname for netname, is_inherited in
                                          assignments[pinnumber]
                                if is_inherited]

                if len(netnames) > 1:
                    pin.error(_("more than one netname assigned "
                                "via \"net=\" attribute: %s") %
                              _(" vs. ").join(netnames))

                pin.has_netattrib = True
                pin.net.names_from_net_attribute += netnames
