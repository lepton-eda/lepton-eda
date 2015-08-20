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

## \namespace xorn.geda.netlist.pp_graphical
## Post-processing: Graphical components.

from gettext import gettext as _
import xorn.geda.attrib

def postproc_blueprints(netlist):
    for schematic in netlist.schematics:
        for component in schematic.components:
            # gnetlist once checks for the existence of a "graphical="
            # attribute and once checks whether the first value is 1,
            # so if these tests give different results, gnetlist has
            # an inconsistent internal state.  -> warn about this
            graphical_attribs = \
                xorn.geda.attrib.search_all(component.ob, 'graphical')
            if graphical_attribs and graphical_attribs[0] != '1':
                component.error(_("invalid value for graphical= attribute"))

            component.is_graphical = \
                component.get_attribute('graphical', None) == '1'

            if component.is_graphical and component.composite_sources:
                # Do not bother traversing the hierarchy if the symbol
                # has an graphical attribute attached to it.
                component.warn(_("source= is set for graphical component"))
                component.composite_sources = []

def postproc_instances(netlist):
    for net in netlist.nets:
        net.graphical_component_pins = []

    remove_components = set()

    for component in netlist.components:
        if not component.blueprint.is_graphical:
            continue

        # remove component
        remove_components.add(component)
        component.sheet.components.remove(component)
        del component.sheet.components_by_blueprint[component.blueprint]

        for cpin in component.cpins:
            # remove cpin
            cpin.local_net.cpins.remove(cpin)
            cpin.local_net.net.component_pins.remove(cpin)

            # but add it to graphical_component_pins
            cpin.local_net.net.graphical_component_pins.append(cpin)

    netlist.components = [component for component in netlist.components
                          if component not in remove_components]
