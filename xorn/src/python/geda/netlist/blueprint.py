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

## \namespace xorn.geda.netlist.blueprint
## Netlists for individual schematic files.
#
# This module pre-processes a schematic as far as possible without
# knowing anything about the rest of the hierarchy.

import sys
from gettext import gettext as _
import xorn.proxy
import xorn.storage
import xorn.geda.attrib
import xorn.geda.netlist.conn

def traverse_net(cmap, instance, netsY_by_instance, netY):
    if instance in netsY_by_instance:
        return

    netY.append(instance)
    netsY_by_instance[instance] = netY

    for other_instance in cmap.connected_to(instance):
        traverse_net(cmap, other_instance, netsY_by_instance, netY)

## A netlist for a single schematic.

class Schematic:
    def __init__(self, rev, filename, netlister_run):
        if rev.is_transient():
            raise ValueError

        self.rev = rev
        self.filename = filename
        self.netlister_run = netlister_run
        self.components = []
        self.components_by_ob = {}
        self.nets = []

        for ob in rev.toplevel_objects():
            data = ob.data()
            if not isinstance(data, xorn.storage.Component):
                continue

            if data.symbol.prim_objs is None:
                error_object(
                    ob, _("%s: symbol not found") % data.symbol.basename)
                continue

            Component(self, ob)

        cmap = xorn.geda.netlist.conn.ConnectionMap(rev)
        netsY = []
        netsY_by_instance = {}

        for instance in \
                xorn.geda.netlist.conn.all_net_instances_in_revision(rev):
            if instance not in netsY_by_instance:
                netY = []
                netsY.append(netY)
                traverse_net(cmap, instance, netsY_by_instance, netY)

        for netY in netsY:
            net = Net(self, netY)
            for pin in net.pins:
                pin.net = net

    def error(self, msg):
        sys.stderr.write(_("%s: error: %s\n") % (self.filename, msg))
        self.netlister_run.failed = True

    def warn(self, msg):
        sys.stderr.write(_("%s: warning: %s\n") % (self.filename, msg))

    def error_object(self, ob, msg):
        data = ob.data()
        sys.stderr.write(_("%s:%sx%s: error: %s\n") % (
            self.filename, format_coord(data.x), format_coord(data.y), msg))
        self.netlister_run.failed = True

    def warn_object(self, ob, msg):
        data = ob.data()
        sys.stderr.write(_("%s:%sx%s: warning: %s\n") % (
            self.filename, format_coord(data.x), format_coord(data.y), msg))

## Format an integer coordinate for printing in an error message.
#
# Divides the coordinate value by 100 and returns it as a string,
# keeping as many decimal digits as necessary.
#
# gEDA uses a coordinate space where two adjacent grid points have a
# distance of 100, so most coordinates are multiples of 100.

def format_coord(coord):
    coord = int(coord)
    if coord % 100 == 0:
        return '%d' % (coord / 100, )
    if coord % 10 == 0:
        return '%d.%d' % (coord / 100, (coord % 100) / 10)
    return '%d.%02d' % (coord / 100, coord % 100)

## %Component in a single schematic's netlist.

class Component:
    def __init__(self, schematic, ob):
        self.schematic = schematic
        self.ob = ob
        self.pins = []
        self.pins_by_ob = {}

        # populated by schematic loader
        self.composite_sources = None

        # set by xorn.geda.netlist.pp_graphical
        self.is_graphical = False

        # populated by xorn.geda.netlist.pp_slotting
        self.pins_by_pinseq = None
        self.pins_by_number = None
        self.slotdef = None

        self.error = lambda msg: self.schematic.error_object(self.ob, msg)
        self.warn = lambda msg: self.schematic.warn_object(self.ob, msg)

        schematic.components.append(self)
        schematic.components_by_ob[ob] = self

        ## Determine the refdes to use for a particular object.
        self.refdes = None

        # check refdes, then uref, then return None.
        try:
            self.refdes = self.get_attribute(
                'refdes', search_inherited = False)
        except KeyError:
            try:
                self.refdes = self.get_attribute(
                    'uref', search_inherited = False)
            except KeyError:
                self.refdes = None
            else:
                self.warn(_("Found uref=%s. uref= is deprecated, please use "
                            "refdes=%s") % (self.refdes, self.refdes))

        # collect pins

        data = ob.data()
        assert isinstance(data, xorn.storage.Component)
        assert data.symbol.prim_objs is not None

        for pin_ob in xorn.proxy.RevisionProxy(
                data.symbol.prim_objs).toplevel_objects():
            pin_data = pin_ob.data()
            if not isinstance(pin_data, xorn.storage.Net):
                continue

            assert pin_data.is_pin
            assert pin_ob not in self.pins_by_ob

            pin = Pin(self, pin_ob)
            self.pins_by_ob[pin_ob] = pin


    ## Get all attribute values for a given attribute name.
    #
    # Searches the attributes attached to this component for
    # attributes with the name \a name and returns a list with their
    # values.  If no matching attributes are attached and \a
    # search_inherited is not \c False, searches the attributes
    # inherited from the symbol instead.

    def get_attributes(self, name, search_inherited = True):
        # look outside first
        values = xorn.geda.attrib.search_attached(self.ob, name)

        if not values and search_inherited:
            # okay we were looking outside and didn't find anything,
            # so now we need to look inside the symbol
            values = xorn.geda.attrib.search_inherited(self.ob, name)

        return values

    ## Get the value of an attached or inherited attribute.
    #
    # Returns the value of the attribute with the name \a name, or
    # raises a \a KeyError if the attribute doesn't exist.  If \a
    # default is given, returns that value instead.
    #
    # Searches the attributes attached to the component first.  If no
    # matching attributes are found and \a search_inherited is not \c
    # False, searches the attributes inherited from the symbol.  It is
    # an error for the component to contain multiple attached or
    # inherited values with different values.
    #
    # If an attribute has the value \c unknown, it is treated as if it
    # didn't exist.  This can be used to un-set an attribute inherited
    # from the symbol.

    def get_attribute(self, name, default = KeyError, search_inherited = True):
        raw_values = self.get_attributes(name, search_inherited)
        values = []
        for value in raw_values:
            if value not in values:
                values.append(value)

        if len(values) > 1:
            self.error(_("inconsistent values for \"%s\": %s") % (
                name, _(" vs. ").join(_("\"%s\"") % value
                                      for value in values)))
            values = False
        elif len(raw_values) > 1:
            self.warn(_("multiple definitions of \"%s=%s\"")
                      % (name, values[0]))

        if not values or values[0] == 'unknown':
            if default != KeyError:
                return default
            raise KeyError

        return values[0]

    def get_attribute_names(self, search_inherited):
        attribs = self.ob.attached_objects()
        if search_inherited:
            attribs = \
                xorn.geda.attrib.find_inherited_attribs(self.ob) + attribs

        l = []
        for attrib in attribs:
            data = attrib.data()
            assert isinstance(data, xorn.storage.Text)
            try:
                found_name, found_value = \
                    xorn.geda.attrib.parse_string(data.text)
            except xorn.geda.attrib.MalformedAttributeException:
                pass
            else:
                if found_value != 'unknown':
                    if found_name not in l:
                        l.append(found_name)
                else:
                    if found_name in l:
                        l.remove(found_name)
        return l

## %Pin in a single schematic's netlist.

class Pin:
    def __init__(self, component, ob):
        self.component = component
        self.net = None
        self.ob = ob

        ## The "identifier" of the pin.
        #
        # set by xorn.geda.netlist.pp_slotting and
        # xorn.geda.netlist.pp_netattrib
        self.number = None

        # set by xorn.geda.netlist.pp_netattrib
        self.has_netattrib = False

        component.pins.append(self)

    ## Get all attribute values for a given attribute name.
    #
    # Searches the attributes attached to this pin for attributes with
    # the name \a name and returns a list with their values.

    def get_attributes(self, name):
        if self.ob is None:
            return []
        return xorn.geda.attrib.search_attached(self.ob, name)

    ## Get the value of an attribute.
    #
    # Returns the value of the attribute with the name \a name, or
    # raises a \a KeyError if the attribute doesn't exist.  If \a
    # default is given, returns that value instead.
    #
    # It is an error for the pin to contain multiple attributes with
    # the same name and different values.
    #
    # An attribute with the value \c unknown isn't treated specially
    # in any way.  Since pin attributes can't be overridden, there
    # wouldn't be a use case for this.

    def get_attribute(self, name, default = KeyError):
        raw_values = self.get_attributes(name)
        values = []
        for value in raw_values:
            if value not in values:
                values.append(value)

        if len(values) > 1:
            self.error(_("inconsistent values for \"%s\": %s") % (
                name, _(" vs. ").join(_("\"%s\"") % value
                                      for value in values)))
            values = False
        elif len(raw_values) > 1:
            self.warn(_("multiple definitions of \"%s=%s\"")
                      % (name, values[0]))

        if not values:
            if self.ob is None and name == 'pintype':
                # supply pintype 'pwr' for artificial pin
                return 'pwr'
            if default != KeyError:
                return default
            raise KeyError

        return values[0]

    ## Calculate the position of the pin's active end in the schematic.

    def position(self):
        data = self.ob.data()
        assert isinstance(data, xorn.storage.Net)

        x = data.x
        y = data.y

        data = self.component.ob.data()
        assert isinstance(data, xorn.storage.Component)

        if data.mirror:
            x = -x

        if data.angle == 90:
            x, y = -y, x
        elif data.angle == 180:
            x, y = -x, -y
        elif data.angle == 270:
            x, y = y, -x

        x += data.x
        y += data.y

        return x, y

    def error(self, msg):
        refdes = self.component.refdes
        if refdes is None:
            refdes = '<no refdes>'
        number = self.number
        if number is None:
            number = '?'
        x, y = self.position()
        sys.stderr.write(_("%s:%s-%s(%sx%s): error: %s\n") % (
            self.component.schematic.filename, refdes, number,
            format_coord(x), format_coord(y), msg))
        self.component.schematic.netlister_run.failed = True

    def warn(self, msg):
        refdes = self.component.refdes
        if refdes is None:
            refdes = '<no refdes>'
        number = self.number
        if number is None:
            number = '?'
        x, y = self.position()
        sys.stderr.write(_("%s:%s-%s(%sx%s): warning: %s\n") % (
            self.component.schematic.filename, refdes, number,
            format_coord(x), format_coord(y), msg))

## Visually connected net piece in a single schematic's netlist.

class Net:
    def __init__(self, schematic, instances):
        self.schematic = schematic
        self.net_segments = []
        self.names = []
        self.pins = []
        self.is_bus = instances and instances[0][1].data().is_bus
        self.names_from_net_attribute = []

        for path, ob in instances:
            assert ob.data().is_bus == self.is_bus
            if not path:
                self.net_segments.append(ob)
            else:
                self.pins.append(
                    schematic.components_by_ob[path[0]].pins_by_ob[ob])

        # sort net segments and pins to achieve a stable output order
        self.net_segments.sort(key = lambda ob: ob.location()[1])
        self.pins.sort(key = lambda pin: (pin.component.ob.location()[1],
                                          pin.ob.location()[1]))

        for ob in self.net_segments:
            self.names += xorn.geda.attrib.search_attached(ob, 'netname')

            # search for the old label= attribute on nets
            values = xorn.geda.attrib.search_attached(ob, 'label')
            if values:
                self.schematic.warn_object(ob,
                    _("label= is deprecated, please use netname="))
                self.names += values

        if len(self.names) > 1:
            self.schematic.warn_object(self.net_segments[0],
                _("multiple names on a single net: %s")
                % _(" vs. ").join(_("\"%s\"") % name for name in self.names))

        schematic.nets.append(self)
