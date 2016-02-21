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

## \namespace xorn.geda.netlist.package
## Grouping components with the same refdes into a package.

import sys
from gettext import gettext as _

class Package:
    def __init__(self, netlist, namespace, unmangled_refdes):
        self.netlist = netlist
        self.namespace = namespace
        self.unmangled_refdes = unmangled_refdes
        self.refdes = None  # set by netlist ctor
        self.components = []
        self.pins = []
        self.pins_by_number = {}

    ## Get attribute value(s) from a package with given refdes.
    #
    # This function returns the values of a specific attribute type
    # attached to the symbol instances with this package's refdes.
    #
    # For each symbol instance, the found attribute value is added to
    # the return list.  \c None is added if the instance has no such
    # attribute.
    #
    # \note The order of the values in the return list is the order of
    #       symbol instances within the netlist (the first element is
    #       the value associated with the first symbol instance).
    #
    # \returns a list of attribute values as strings and \c None

    def get_all_attributes(self, name):
        if not isinstance(name, basestring):
            raise ValueError

        # search for refdes instances and through the entire list
        l = []
        for component in self.components:
            try:
                value = component.blueprint.get_attribute(name)
            except KeyError:
                l.append(None)
            else:
                l.append(value)
        return l

    ## Return the value associated with attribute \a name on the package.
    #
    # It actually computes a single value from the full list of values
    # produced by \ref get_all_attributes.
    #
    # Returns the value associated with the first symbol instance for
    # \a refdes which has a matching attribute.  If all instances of
    # \a refdes do not have the same value for \a name and \a name is
    # not \c "slot", raises an error.

    def get_attribute(self, name, default = KeyError):
        values = []
        for value in self.get_all_attributes(name):
            if value is not None and value not in values:
                values.append(value)

        if len(values) > 1:
            self.error(_("attribute conflict for \"%s\": %s") % (
                name, _(" vs. ").join(_("\"%s\"") % value
                                      for value in values)))
            values = False

        if not values:
            if default is not KeyError:
                return default
            raise KeyError

        assert isinstance(values[0], basestring)
        return values[0]

    ## Takes a pinseq string and returns that pinseq pin of this package.

    def get_pin_by_pinseq(self, pinseq):
        if not isinstance(pinseq, int):
            raise ValueError

        for component in self.components:
            try:
                pin_blueprint = component.blueprint.pins_by_pinseq[pinseq]
            except KeyError:
                continue
            return self.pins_by_number[pin_blueprint.number]

        raise KeyError

    ## Return a sorted list of slots used by this package.
    #
    # It collects the slot attribute values of each symbol instance of
    # this package.  As a result, slots may be repeated in the
    # returned list.

    def get_slots(self):
        l = []
        for slot in self.get_all_attributes('slot'):
            if slot is None:
                # no slot attribute, assume slot number is 1
                l.append(1)
                continue

            # convert string attribute value to number
            try:
                l.append(int(slot))
            except ValueError:
                # conversion failed, invalid slot, ignore value
                self.error(_("bad slot number: %s") % slot)
        l.sort()
        return l

    ## Return a sorted list of unique slots used by this package.

    def get_unique_slots(self):
        l = list(set(self.get_slots()))
        l.sort()
        return l

    def get_attribute_names(self, search_inherited):
        # search outside the symbol (attached attributes only)
        l = []
        for component in self.components:
            for name in \
                    component.blueprint.get_attribute_names(search_inherited):
                if name not in l:
                    l.append(name)
        return l

    def error(self, msg):
        sys.stderr.write(_("package `%s': error: %s\n") % (self.refdes, msg))
        self.netlist.failed = True

    def warn(self, msg):
        sys.stderr.write(_("package `%s': warning: %s\n") % (self.refdes, msg))

# ============================================================================

class PackagePin:
    def __init__(self, package, number):
        self.package = package
        self.number = number
        self.cpins = []

    ## Returns the appropriate attribute values on this pin.
    #
    # This function returns the values of a specific attribute type
    # attached to the instances of this pin.  For each instance, the
    # found attribute value is added to the return list.  \c None is
    # added if the instance has no such attribute.
    #
    # \note The order of the values in the return list is the order of
    #       pin instances within the netlist (the first element is the
    #       value associated with the first pin instance).
    #
    # \returns a list of attribute values as strings and \c None

    def get_all_attributes(self, name):
        if not isinstance(name, basestring):
            raise ValueError

        l = []
        for cpin in self.cpins:
            try:
                value = cpin.blueprint.get_attribute(name)
            except KeyError:
                l.append(None)
            else:
                l.append(value)
        return l

    ## Return the value associated with attribute \a name on the pin.
    #
    # It actually computes a single value from the full list of values
    # produced by \ref get_all_attributes.
    #
    # If all instances do not have the same value for \a name, raises
    # an error.

    def get_attribute(self, name, default = KeyError):
        # Treat "pinnumber" specially: return the value of self.number
        # which recognizes slotting.  For backwards compatibility,
        # artificial pins do not have a pinnumber.

        if name == 'pinnumber':
            has_real_pins = False
            for cpin in self.cpins:
                if cpin.blueprint.ob is not None:
                    has_real_pins = True

            if has_real_pins:
                return self.number
            else:
                if default is not KeyError:
                    return default
                raise KeyError

        values = []
        for value in self.get_all_attributes(name):
            if value is not None and value not in values:
                values.append(value)

        if len(values) > 1:
            self.error(_("attribute conflict for \"%s\": %s") % (
                name, _(" vs. ").join(_("\"%s\"") % value
                                      for value in values)))
            values = False

        if not values:
            if default is not KeyError:
                return default
            raise KeyError

        assert isinstance(values[0], basestring)
        return values[0]

    def error(self, msg):
        sys.stderr.write(_("package `%s', pin `%s': error: %s\n") % (
            self.package.refdes, self.number, msg))
        self.package.netlist.failed = True

    def warn(self, msg):
        sys.stderr.write(_("package `%s', pin `%s': warning: %s\n") % (
            self.package.refdes, self.number, msg))

# ============================================================================

def postproc_blueprints(netlist):
    # find components without a refdes
    for schematic in netlist.schematics:
        for component in schematic.components:
            if component.refdes is not None:
                # component has a refdes -> ok
                continue
            if component.is_graphical:
                # graphical components don't need a refdes -> ok
                continue

            # Maybe the symbol isn't a component but a power/gnd symbol?

            if not component.pins:
                component.error(_("component has neither refdes nor pins"))
                continue

            for pin in component.pins:
                if not pin.has_netattrib:
                    # pin is missing a net= attribute
                    pin.error(_(
                        "could not find refdes on component and "
                        "could not find net= attribute on pin"))

def postproc_instances(netlist, flat_namespace):
    netlist.packages = []
    pkg_dict = {}

    for component in netlist.components:
        if component.blueprint.refdes is None:
            continue

        if flat_namespace or component.sheet.instantiating_component is None:
            namespace = None
        else:
            namespace = component.sheet

        try:
            package = pkg_dict[namespace, component.blueprint.refdes]
        except KeyError:
            package = Package(netlist, namespace, component.blueprint.refdes)
            netlist.packages.append(package)
            pkg_dict[namespace, component.blueprint.refdes] = package

        package.components.append(component)

        for cpin in component.cpins:
            try:
                ppin = package.pins_by_number[cpin.blueprint.number]
            except KeyError:
                ppin = PackagePin(package, cpin.blueprint.number)
                package.pins.append(ppin)
                package.pins_by_number[cpin.blueprint.number] = ppin
            ppin.cpins.append(cpin)

    for package in netlist.packages:
        for ppin in package.pins:
            nets = []
            for cpin in ppin.cpins:
                if cpin.local_net.net not in nets:
                    nets.append(cpin.local_net.net)
            assert nets
            if len(nets) > 1:
                ppin.error(_("multiple nets connected to pin: %s")
                           % _(" vs. ").join(_("\"%s\"") % net.name
                                             for net in nets))
            ppin.net = nets[0]

    for net in netlist.nets:
        # walk through the list of components, and through the list
        # of individual pins on each, adding net names to the list
        # being careful to ignore duplicates, and unconnected pins
        net.connections = []

        # add the net name to the list
        for cpin in net.component_pins:
            if cpin.component.blueprint.refdes is None:
                continue
            if flat_namespace \
                   or cpin.component.sheet.instantiating_component is None:
                namespace = None
            else:
                namespace = cpin.component.sheet
            ppin = pkg_dict[namespace, cpin.component.blueprint.refdes] \
                     .pins_by_number[cpin.blueprint.number]
            if ppin not in net.connections:
                net.connections.append(ppin)
