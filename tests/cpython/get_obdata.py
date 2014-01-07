# Copyright (C) 2013, 2014 Roland Lutz
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

import xorn.storage, Setup

def assert_cannot_get(rev, ob):
    try:
        rev.get_object_data(ob)
    except KeyError:
        pass
    else:
        raise AssertionError

(rev0, rev1, rev2, rev3, ob0, ob1a, ob1b) = Setup.setup()

assert_cannot_get(rev0, ob0)
assert_cannot_get(rev0, ob1a)
assert_cannot_get(rev0, ob1b)

data = rev1.get_object_data(ob0)
assert data is not None
assert data != Setup.line_data
assert type(data) == type(Setup.line_data)
assert_cannot_get(rev1, ob1a)
assert_cannot_get(rev1, ob1b)

data = rev2.get_object_data(ob0)
assert data is not None
assert data != Setup.line_data
assert type(data) == type(Setup.line_data)
data = rev2.get_object_data(ob1a)
assert data is not None
assert data != Setup.box_data
assert type(data) == type(Setup.box_data)
data = rev2.get_object_data(ob1b)
assert data is not None
assert data != Setup.circle_data
assert type(data) == type(Setup.circle_data)

data = rev3.get_object_data(ob0)
assert data is not None
assert data != Setup.net_data
assert type(data) == type(Setup.net_data)
assert_cannot_get(rev3, ob1a)
data = rev3.get_object_data(ob1b)
assert data is not None
assert data != Setup.circle_data
assert type(data) == type(Setup.circle_data)
