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

import xorn.storage

def assert_attached_objects(rev, attached_to, result):
    try:
        objects = xorn.storage.get_objects_attached_to(rev, attached_to)
    except Exception as e:
        assert type(e) == result
    else:
        assert objects == result

    try:
        sel = xorn.storage.select_attached_to(rev, attached_to)
    except Exception as e:
        assert type(e) == result
    else:
        objects = xorn.storage.get_selected_objects(rev, sel)
        objects.sort()
        result = result[:]
        result.sort()
        assert objects == result

rev = xorn.storage.Revision()
assert rev is not None

N = rev.add_object(xorn.storage.Net())
a = rev.add_object(xorn.storage.Text())
b = rev.add_object(xorn.storage.Text())

assert N is not None
assert a is not None
assert b is not None

assert_attached_objects(rev, None, [N, a, b])
assert_attached_objects(rev, N, [])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.relocate_object(N, None, None)

assert_attached_objects(rev, None, [a, b, N])
assert_attached_objects(rev, N, [])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.relocate_object(N, None, b)

assert_attached_objects(rev, None, [a, N, b])
assert_attached_objects(rev, N, [])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.relocate_object(a, N, None)

assert_attached_objects(rev, None, [N, b])
assert_attached_objects(rev, N, [a])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.relocate_object(b, N, None)

assert_attached_objects(rev, None, [N])
assert_attached_objects(rev, N, [a, b])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.relocate_object(a, N, None)

assert_attached_objects(rev, None, [N])
assert_attached_objects(rev, N, [b, a])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.relocate_object(a, N, b)

assert_attached_objects(rev, None, [N])
assert_attached_objects(rev, N, [a, b])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, [])

rev.delete_object(b)

assert_attached_objects(rev, None, [N])
assert_attached_objects(rev, N, [a])
assert_attached_objects(rev, a, [])
assert_attached_objects(rev, b, MemoryError)

rev.delete_object(N)

assert_attached_objects(rev, None, [])
assert_attached_objects(rev, N, MemoryError)
assert_attached_objects(rev, a, MemoryError)
assert_attached_objects(rev, b, MemoryError)
