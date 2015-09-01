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

## \namespace xorn.geda.netlist.pp_hierarchy
## Post-processing: Hierarchy traversal.

from gettext import gettext as _

## Connect subsheet I/O ports to the instantiating component's pins.
#
# Disconnect all connections from and to composite components as
# they have been replaced with the actual subschematics.
#
# remove all composite components and ports

def postproc_instances(netlist):
    remove_components = set()

    for component in netlist.components:
        if not component.is_composite:
            continue

        for cpin in component.cpins:
            label = cpin.blueprint.get_attribute('pinlabel', None)
            if label is None:
                cpin.error(_("pin on composite component is missing a label"))
                continue

            dest_net = cpin.local_net.net

            # search for the matching port
            ports = [potential_port for subsheet in component.subsheets
                                    for potential_port in subsheet.components
                     if potential_port.blueprint.refdes == label]

            if not ports:
                cpin.warn(_("missing I/O symbol with refdes `%s' "
                            "inside schematic") % label)
            elif len(ports) > 1:
                cpin.warn(_("multiple I/O symbols with refdes `%s' "
                            "inside schematic") % label)

            for port in ports:
                if not port.cpins:
                    port.error(_("I/O symbol doesn't have pins"))
                    continue
                if len(port.cpins) > 1:
                    port.error(_("multiple pins on I/O symbol"))
                    continue
                if port.blueprint.is_graphical:
                    port.error(_("I/O symbol can't be graphical"))

                src_net = port.cpins[0].local_net.net

                # merge nets
                if src_net != dest_net:
                    src_net.merge_into(dest_net)
                    dest_net.component_pins += src_net.component_pins
                    del src_net.component_pins[:]

                # remove port component
                remove_components.add(port)
                port.sheet.components.remove(port)
                del port.sheet.components_by_blueprint[port.blueprint]

                port.cpins[0].local_net.cpins.remove(port.cpins[0])
                dest_net.component_pins.remove(port.cpins[0])

            # After the pin has been connected, remove it.
            cpin.local_net.cpins.remove(cpin)
            dest_net.component_pins.remove(cpin)

        # After all pins have been connected, remove the component.
        remove_components.add(component)
        component.sheet.components.remove(component)
        del component.sheet.components_by_blueprint[component.blueprint]

    netlist.components = [component for component in netlist.components
                          if component not in remove_components]
