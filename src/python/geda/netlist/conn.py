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

## \namespace xorn.geda.netlist.conn
## Find and track the connections between net segments.
#
# Instances of net segments (including bus segments and pins) are
# represented by a pair <tt>(path, ob)</tt> where \c path is a
# sequence of component objects, the symbol of each containing the
# next component or (in the case of the last component) the net, and
# \c ob is the actual net object.

import collections

import xorn.proxy
import xorn.storage

CONN_ENDPOINT, CONN_MIDPOINT = xrange(2)

## A single connection between two objects.
#
# Each instance represents a single unidirectional connection between
# two objects.  The connection type is always in reference to how ob1
# is connected to ob0.

Connection = collections.namedtuple('Connection', [
    'x', 'y', 'type', 'path0', 'ob0', 'path1', 'ob1', 'whichend0', 'whichend1'
])

## Return the absolute coordinates of the endpoints of a net instance.
#
# \returns a pair of pairs <tt>((x0, x1), (y0, y1))</tt>
#
# \throws ValueError if \c path contains an object which is not a component
# \throws ValueError if \c ob is not a net

def endpoints((path, ob)):
    data = ob.data()
    if not isinstance(data, xorn.storage.Net):
        raise ValueError

    x0 = data.x
    y0 = data.y
    x1 = data.x + data.width
    y1 = data.y + data.height

    for component in reversed(path):
        data = component.data()
        if not isinstance(data, xorn.storage.Component):
            raise ValueError

        if data.mirror:
            x0 = -x0
            x1 = -x1

        if data.angle == 90:
            x0, y0 = -y0, x0
            x1, y1 = -y1, x1
        elif data.angle == 180:
            x0, y0 = -x0, -y0
            x1, y1 = -x1, -y1
        elif data.angle == 270:
            x0, y0 = y0, -x0
            x1, y1 = y1, -x1

        x0 += data.x
        y0 += data.y
        x1 += data.x
        y1 += data.y

    return (x0, x1), (y0, y1)

## Checks if a point is on an orthogonal net segment and between its
## endpoints.
#
# \returns whether the net segment has horizontal or vertical
#          orientation, the point is on the net segment, and it is not
#          identical with one of its endpoints
#
# \throws ValueError if \c path contains an object which is not a component
# \throws ValueError if \c ob is not a net

def s_conn_check_midpoint(instance, x, y):
    (x0, x1), (y0, y1) = endpoints(instance)
    return x0 == x and x1 == x and y > min(y0, y1) and y < max(y0, y1) \
        or y0 == y and y1 == y and x > min(x0, x1) and x < max(x0, x1)

## Returns all net instances in a given revision.
#
# This includes all net objects in the revision as well as net objects
# inside symbols.

def all_net_instances_in_revision(rev, path = ()):
    for ob in rev.toplevel_objects():
        for instance in all_net_instances_in_object(ob, path):
            yield instance

## Returns all net instances in a given object.
#
# If \a ob is a net, returns the appropriate net segment.  If \a is a
# component, returns net objects inside its symbol.

def all_net_instances_in_object(ob, path = ()):
    data = ob.data()
    if isinstance(data, xorn.storage.Net):
        return [(path, ob)]
    elif isinstance(data, xorn.storage.Component):
        return all_net_instances_in_revision(
            xorn.proxy.RevisionProxy(data.symbol.prim_objs), path + (ob, ))
    else:
        return []


## Return all connections of a net instance.
#
# This function searches for all geometrical conections of the net
# instance <tt>(path, ob)</tt> to all other connectable objects in the
# revision.

def s_conn_update_line_object((path, ob), instances_by_endpoint,
                                          horizontal_instances,
                                          vertical_instances):
    if path:
        rev = xorn.proxy.RevisionProxy(path[-1].rev)
    else:
        rev = xorn.proxy.RevisionProxy(ob.rev)

    # rename argument variables
    path0 = path; del path
    ob0 = ob; del ob

    data0 = ob0.data()
    if not isinstance(data0, xorn.storage.Net):
        raise ValueError
    x0, y0 = endpoints((path0, ob0))  # those are both PAIRS of coordinates

    # If ob0 is a pin, only check the correct end
    if data0.is_pin:
        ends0 = [0]
    else:
        ends0 = [0, 1]

    def can_connect((path0, ob0), (path1, ob1)):
        # An object inside a symbol can only be connected up to another
        # object if they are (a) both inside the same object, or (b)
        # the object inside a symbol is a pin.

        # 1. Both objects are inside a symbol
        if path0 and path1:
            # If inside different symbols, both must be pins to connect.
            if path0 != path1 and not data0.is_pin or not data1.is_pin:
                return False

        # 2. Updating object is inside a symbol, but ob1 is not.
        elif path0 and not path1:
            if not data0.is_pin:
                return False

        # 3. Updating object not inside symbol, but ob1 is.
        elif not path0 and path1:
            if not data1.is_pin:
                return False

        return True

    for end in ends0:
      for path1, ob1 in instances_by_endpoint.get((x0[end], y0[end]), []):
        if ob1 == ob0:
            continue

        data1 = ob1.data()
        if not isinstance(data1, xorn.storage.Net):
            raise ValueError
        x1, y1 = endpoints((path1, ob1))

        # If ob1 is a pin, only check the correct end
        if data1.is_pin:
            ends1 = [0]
        else:
            ends1 = [0, 1]

        if not can_connect((path0, ob0), (path1, ob1)):
            continue

        # Here is where you check the end points
        # Check both end points of ob1
        for w1 in ends1:
            # Check both end points of ob0
            for w0 in ends0:
                # Check for coincidence and compatability between
                # the objects being tested.
                if x0[w0] == x1[w1] and y0[w0] == y1[w1] \
                       and data0.is_bus == data1.is_bus:
                    yield Connection(
                        x = x0[w0], y = y0[w0], type = CONN_ENDPOINT,
                        path0 = path0, ob0 = ob0, whichend0 = w0,
                        path1 = path1, ob1 = ob1, whichend1 = w1)
                    yield Connection(
                        x = x0[w0], y = y0[w0], type = CONN_ENDPOINT,
                        path0 = path1, ob0 = ob1, whichend0 = w1,
                        path1 = path0, ob1 = ob0, whichend1 = w0)

    for end in ends0:
      for path1, ob1 in \
                vertical_instances.get(x0[end], []) + \
                horizontal_instances.get(y0[end], []):
        if ob1 == ob0:
            continue

        data1 = ob1.data()
        if not isinstance(data1, xorn.storage.Net):
            raise ValueError
        x1, y1 = endpoints((path1, ob1))

        # If ob1 is a pin, only check the correct end
        if data1.is_pin:
            ends1 = [0]
        else:
            ends1 = [0, 1]

        if not can_connect((path0, ob0), (path1, ob1)):
            continue

        # Check both end points of ob0 against midpoints of ob1
        for w0 in ends0:
            # check for midpoint of ob1, w0 endpoint of current obj

            # Pins are not allowed midpoint connections onto them.
            # Allow nets to connect to the middle of buses.
            # Allow compatible objects to connect.
            if s_conn_check_midpoint((path1, ob1), x0[w0], y0[w0]) and \
               not data1.is_pin and (
                    (not data0.is_pin and not data0.is_bus and data1.is_bus)
                    or data0.is_bus == data1.is_bus):
                yield Connection(
                    x = x0[w0], y = y0[w0], type = CONN_MIDPOINT,
                    path0 = path0, ob0 = ob0, whichend0 = w0,
                    path1 = path1, ob1 = ob1, whichend1 = -1)
                yield Connection(
                    x = x0[w0], y = y0[w0], type = CONN_MIDPOINT,
                    path0 = path1, ob0 = ob1, whichend0 = -1,
                    path1 = path0, ob1 = ob0, whichend1 = w0)

def _append_or_create(d, key, item):
    try:
        l = d[key]
    except KeyError:
        l = d[key] = []
    l.append(item)

## Tracks the connections of the net segments in a revision at a time.

class ConnectionMap:
    def __init__(self, rev):
        self.rev = None
        self.connections = []
        self.connection_dict = {}

        self.instances_by_endpoint = {}
        self.horizontal_instances = {}
        self.vertical_instances = {}

        self.goto(rev)

    def goto(self, rev):
        if rev.is_transient():
            raise ValueError

        if self.rev is not None:
            removed_objects = \
                xorn.storage.get_removed_objects(self.rev, rev)
            modified_objects = \
                xorn.storage.get_modified_objects(self.rev, rev)
            added_objects = \
                xorn.storage.get_added_objects(self.rev, rev)
        else:
            removed_objects = []
            modified_objects = []
            added_objects = rev.rev.get_objects()

        # remove objects
        removed_instances = [instance
                             for ob in removed_objects + modified_objects
                             for instance in all_net_instances_in_object(
                                 xorn.proxy.ObjectProxy(self.rev.rev, ob))]

        for instance in removed_instances:
            (x0, x1), (y0, y1) = endpoints(instance)
            self.instances_by_endpoint[x0, y0].remove(instance)
            if not instance[1].data().is_pin:
                self.instances_by_endpoint[x1, y1].remove(instance)
                if x0 == x1:
                    self.vertical_instances[x0].remove(instance)
                if y0 == y1:
                    self.horizontal_instances[y0].remove(instance)

            for conn in self.connections:
                if (conn.path0, conn.ob0) == instance or \
                   (conn.path1, conn.ob1) == instance:
                    self.connections.remove(conn)
                    self.connection_dict[conn.path0, conn.ob0].remove(conn)

        self.rev = rev

        # add objects
        added_instances = [instance
                           for ob in modified_objects + added_objects
                           for instance in all_net_instances_in_object(
                               xorn.proxy.ObjectProxy(self.rev.rev, ob))]

        for instance in added_instances:
            (x0, x1), (y0, y1) = endpoints(instance)
            _append_or_create(self.instances_by_endpoint, (x0, y0), instance)
            if not instance[1].data().is_pin:
                _append_or_create(self.instances_by_endpoint, (x1, y1), instance)
                if x0 == x1:
                    _append_or_create(self.vertical_instances, x0, instance)
                if y0 == y1:
                    _append_or_create(self.horizontal_instances, y0, instance)

        for instance in added_instances:
            for conn in s_conn_update_line_object(instance,
                                                  self.instances_by_endpoint,
                                                  self.horizontal_instances,
                                                  self.vertical_instances):
                if (conn.path0, conn.ob0) not in self.connection_dict or \
                       conn not in self.connection_dict[conn.path0, conn.ob0]:
                    self.connections.append(conn)
                    _append_or_create(
                        self.connection_dict, (conn.path0, conn.ob0), conn)

    ## Return all net instances directly connected to a net instance.

    def connected_to(self, instance):
        result = []
        result_set = set()
        for conn in self.connection_dict.get(instance, []):
            if (conn.path1, conn.ob1) not in result_set:
                result.append((conn.path1, conn.ob1))
                result_set.add((conn.path1, conn.ob1))
        return result
