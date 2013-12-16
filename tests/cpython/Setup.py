# Copyright (C) 2013 Roland Lutz
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

import xorn.storage

def setup():
    global line_data, box_data, circle_data, net_data

    rev0 = xorn.storage.Revision(None)
    assert rev0 is not None
    rev0.mtswach()

    # first change
    rev1 = xorn.storage.Revision(rev0)
    assert rev1 is not None

    line_data = xorn.storage.Line()
    line_data.x = 0
    line_data.y = 1
    line_data.width = 3
    line_data.height = 2
    line_data.color = 3
    line_data.line.width = 1

    ob0 = rev1.add_object(line_data)
    assert ob0 is not None

    rev1.mtswach()

    # second change
    rev2 = xorn.storage.Revision(rev1)
    assert rev2 is not None

    box_data = xorn.storage.Box()
    box_data.x = 1
    box_data.y = 1
    box_data.width = 2
    box_data.height = 2
    box_data.color = 3
    box_data.line.width = 1

    ob1a = rev2.add_object(box_data)
    assert ob1a is not None

    circle_data = xorn.storage.Circle()
    circle_data.x = -1
    circle_data.y = -1
    circle_data.radius = 2
    circle_data.color = 3
    circle_data.line.width = 1
    circle_data.fill.type = 1

    ob1b = rev2.add_object(circle_data)
    assert ob1b is not None

    rev2.mtswach()

    # third change
    rev3 = xorn.storage.Revision(rev2)
    assert rev3 is not None

    net_data = xorn.storage.Net()
    net_data.x = 0
    net_data.y = 1
    net_data.width = 3
    net_data.height = 2
    net_data.color = 4

    rev3.set_object_data(ob0, net_data)

    rev3.delete_object(ob1a)

    rev3.mtswach()

    return (rev0, rev1, rev2, rev3, ob0, ob1a, ob1b)
