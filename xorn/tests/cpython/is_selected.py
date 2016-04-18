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
assert type(sel) == xorn.storage.Selection

assert xorn.storage.object_is_selected(rev0, sel, ob0) == False
assert xorn.storage.object_is_selected(rev0, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev0, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev1, sel, ob0) == False
assert xorn.storage.object_is_selected(rev1, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev1, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev2, sel, ob0) == False
assert xorn.storage.object_is_selected(rev2, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev2, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev3, sel, ob0) == False
assert xorn.storage.object_is_selected(rev3, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev3, sel, ob1b) == False

sel = xorn.storage.select_object(ob1a)
assert type(sel) == xorn.storage.Selection

assert xorn.storage.object_is_selected(rev0, sel, ob0) == False
assert xorn.storage.object_is_selected(rev0, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev0, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev1, sel, ob0) == False
assert xorn.storage.object_is_selected(rev1, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev1, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev2, sel, ob0) == False
assert xorn.storage.object_is_selected(rev2, sel, ob1a) == True
assert xorn.storage.object_is_selected(rev2, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev3, sel, ob0) == False
assert xorn.storage.object_is_selected(rev3, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev3, sel, ob1b) == False

sel = xorn.storage.select_all(rev3)
assert type(sel) == xorn.storage.Selection

assert xorn.storage.object_is_selected(rev0, sel, ob0) == False
assert xorn.storage.object_is_selected(rev0, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev0, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev1, sel, ob0) == True
assert xorn.storage.object_is_selected(rev1, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev1, sel, ob1b) == False
assert xorn.storage.object_is_selected(rev2, sel, ob0) == True
assert xorn.storage.object_is_selected(rev2, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev2, sel, ob1b) == True
assert xorn.storage.object_is_selected(rev3, sel, ob0) == True
assert xorn.storage.object_is_selected(rev3, sel, ob1a) == False
assert xorn.storage.object_is_selected(rev3, sel, ob1b) == True
