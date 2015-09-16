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

import xorn.storage

_ = None

rev = xorn.storage.Revision()
assert rev is not None

N = rev.add_object(xorn.storage.Net())
a = rev.add_object(xorn.storage.Text())
b = rev.add_object(xorn.storage.Text())

assert N is not None
assert a is not None
assert b is not None

assert rev.get_object_location(N) == (_, 0)
assert rev.get_object_location(a) == (_, 1)
assert rev.get_object_location(b) == (_, 2)

rev.relocate_object(a, N, _)

assert rev.get_object_location(N) == (_, 0)
assert rev.get_object_location(a) == (N, 0)
assert rev.get_object_location(b) == (_, 1)

rev.relocate_object(b, N, _)

assert rev.get_object_location(N) == (_, 0)
assert rev.get_object_location(a) == (N, 0)
assert rev.get_object_location(b) == (N, 1)

rev.relocate_object(b, N, a)

assert rev.get_object_location(N) == (_, 0)
assert rev.get_object_location(a) == (N, 1)
assert rev.get_object_location(b) == (N, 0)

rev.relocate_object(a, _, N)

assert rev.get_object_location(N) == (_, 1)
assert rev.get_object_location(a) == (_, 0)
assert rev.get_object_location(b) == (N, 0)
