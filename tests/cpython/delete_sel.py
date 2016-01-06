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

import xorn.storage, Setup

rev0, rev1, rev2, rev3, ob0, ob1a, ob1b = Setup.setup()

sel = xorn.storage.select_none()
assert sel is not None
rev2a = xorn.storage.Revision(rev2)
assert rev2a is not None
rev2a.delete_objects(sel)
rev2a.finalize()

assert rev2a.get_objects() == [ob0, ob1a, ob1b]

sel = xorn.storage.select_object(ob1a)
assert sel is not None
rev2b = xorn.storage.Revision(rev2)
assert rev2b is not None
rev2b.delete_objects(sel)
rev2b.finalize()

assert rev2b.get_objects() == [ob0, ob1b]

sel = xorn.storage.select_all(rev1)
assert sel is not None
rev2c = xorn.storage.Revision(rev2)
assert rev2c is not None
rev2c.delete_objects(sel)
rev2c.finalize()

assert rev2c.get_objects() == [ob1a, ob1b]

sel = xorn.storage.select_all(rev2)
assert sel is not None
rev2d = xorn.storage.Revision(rev2)
assert rev2d is not None
rev2d.delete_objects(sel)
rev2d.finalize()

assert rev2d.get_objects() == []
