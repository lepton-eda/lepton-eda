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

rev = xorn.storage.Revision()
ob0 = rev.add_object(xorn.storage.Line())
ob1, = rev.get_objects()
ob2 = rev.add_object(xorn.storage.Line())

assert ob0 is not ob1
assert ob0 == ob1
assert hash(ob0) == hash(ob1)

assert ob0 is not ob2
assert ob0 != ob2
assert hash(ob0) != hash(ob2)

assert ob1 is not ob2
assert ob1 != ob2
assert hash(ob1) != hash(ob2)
