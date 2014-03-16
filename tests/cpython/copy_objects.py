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

(rev0, rev1, rev2, rev3, ob0, ob1a, ob1b) = Setup.setup()

rev4 = xorn.storage.Revision(rev2)
assert rev4 is not None

sel0 = xorn.storage.select_all(rev3)
assert sel0 is not None

sel1 = rev4.copy_objects(rev2, sel0)
assert sel1 is not None

rev4.finalize()

assert xorn.storage.object_is_selected(rev4, sel0, ob0) == True
assert xorn.storage.object_is_selected(rev4, sel0, ob1a) == False
assert xorn.storage.object_is_selected(rev4, sel0, ob1b) == True

assert xorn.storage.object_is_selected(rev4, sel1, ob0) == False
assert xorn.storage.object_is_selected(rev4, sel1, ob1a) == False
assert xorn.storage.object_is_selected(rev4, sel1, ob1b) == False

objects = rev4.get_objects()
assert type(objects) == list
assert len(objects) == 5

for ob in objects:
    if ob in [ob0, ob1a, ob1b]:
        continue

    assert xorn.storage.object_is_selected(rev4, sel0, ob) == False
    assert xorn.storage.object_is_selected(rev4, sel1, ob) == True
