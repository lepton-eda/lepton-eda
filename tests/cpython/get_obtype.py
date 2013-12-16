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

import xorn.storage, Setup

(rev0, rev1, rev2, rev3, ob0, ob1a, ob1b) = Setup.setup()

def assert_object_type(rev, ob, expected_type):
    if expected_type is not None:
        assert type(rev.get_object_data(ob)) == expected_type
        return

    try:
        rev.get_object_data(ob)
    except KeyError:
        pass
    else:
        raise AssertionError

assert_object_type(rev0, ob0, None)
assert_object_type(rev0, ob1a, None)
assert_object_type(rev0, ob1b, None)

assert_object_type(rev1, ob0, xorn.storage.Line)
assert_object_type(rev1, ob1a, None)
assert_object_type(rev1, ob1b, None)

assert_object_type(rev2, ob0, xorn.storage.Line)
assert_object_type(rev2, ob1a, xorn.storage.Box)
assert_object_type(rev2, ob1b, xorn.storage.Circle)

assert_object_type(rev3, ob0, xorn.storage.Net)
assert_object_type(rev3, ob1a, None)
assert_object_type(rev3, ob1b, xorn.storage.Circle)
