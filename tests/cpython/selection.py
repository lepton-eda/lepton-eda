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

def assert_selected_objects_0(rev, sel):
    assert xorn.storage.get_selected_objects(rev, sel) == []
    assert xorn.storage.selection_is_empty(rev, sel) == True

def assert_selected_objects_1(rev, sel, ob):
    assert xorn.storage.get_selected_objects(rev, sel) == [ob]
    assert xorn.storage.selection_is_empty(rev, sel) == False

def assert_selected_objects_2(rev, sel, ob0, ob1):
    assert xorn.storage.get_selected_objects(rev, sel) in [[ob0, ob1],
                                                           [ob1, ob0]]
    assert xorn.storage.selection_is_empty(rev, sel) == False

def assert_selected_objects_3(rev, sel, ob0, ob1, ob2):
    assert xorn.storage.get_selected_objects(rev, sel) in [
        [ob0, ob1, ob2], [ob0, ob2, ob1], [ob1, ob0, ob2],
        [ob1, ob2, ob0], [ob2, ob0, ob1], [ob2, ob1, ob0]]
    assert xorn.storage.selection_is_empty(rev, sel) == False

(rev0, rev1, rev2, rev3, ob0, ob1a, ob1b) = Setup.setup()

# select none
sel = xorn.storage.select_none()
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_0(rev1, sel)
assert_selected_objects_0(rev2, sel)
assert_selected_objects_0(rev3, sel)

# select object
sel = xorn.storage.select_object(ob0)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_1(rev1, sel, ob0)
assert_selected_objects_1(rev2, sel, ob0)
assert_selected_objects_1(rev3, sel, ob0)

sel = xorn.storage.select_object(ob1a)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_0(rev1, sel)
assert_selected_objects_1(rev2, sel, ob1a)
assert_selected_objects_0(rev3, sel)

sel = xorn.storage.select_object(ob1b)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_0(rev1, sel)
assert_selected_objects_1(rev2, sel, ob1b)
assert_selected_objects_1(rev3, sel, ob1b)

# select all
sel = xorn.storage.select_all(rev0)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_0(rev1, sel)
assert_selected_objects_0(rev2, sel)
assert_selected_objects_0(rev3, sel)

sel = xorn.storage.select_all(rev1)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_1(rev1, sel, ob0)
assert_selected_objects_1(rev2, sel, ob0)
assert_selected_objects_1(rev3, sel, ob0)

sel = xorn.storage.select_all(rev2)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_1(rev1, sel, ob0)
assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b)
assert_selected_objects_2(rev3, sel, ob0, ob1b)

sel = xorn.storage.select_all(rev3)
assert sel is not None
assert_selected_objects_0(rev0, sel)
assert_selected_objects_1(rev1, sel, ob0)
assert_selected_objects_2(rev2, sel, ob0, ob1b)
assert_selected_objects_2(rev3, sel, ob0, ob1b)

# select all except
sel1 = xorn.storage.select_none()
assert sel1 is not None

if True:
    sel = xorn.storage.select_all_except(rev0, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_0(rev2, sel)
    assert_selected_objects_0(rev3, sel)

    sel = xorn.storage.select_all_except(rev1, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_1(rev1, sel, ob0)
    assert_selected_objects_1(rev2, sel, ob0)
    assert_selected_objects_1(rev3, sel, ob0)

    sel = xorn.storage.select_all_except(rev2, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_1(rev1, sel, ob0)
    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b)
    assert_selected_objects_2(rev3, sel, ob0, ob1b)

    sel = xorn.storage.select_all_except(rev3, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_1(rev1, sel, ob0)
    assert_selected_objects_2(rev2, sel, ob0, ob1b)
    assert_selected_objects_2(rev3, sel, ob0, ob1b)

sel1 = xorn.storage.select_object(ob0)
assert sel1 is not None

if True:
    sel = xorn.storage.select_all_except(rev0, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_0(rev2, sel)
    assert_selected_objects_0(rev3, sel)

    sel = xorn.storage.select_all_except(rev1, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_0(rev2, sel)
    assert_selected_objects_0(rev3, sel)

    sel = xorn.storage.select_all_except(rev2, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_2(rev2, sel, ob1a, ob1b)
    assert_selected_objects_1(rev3, sel, ob1b)

    sel = xorn.storage.select_all_except(rev3, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_1(rev2, sel, ob1b)
    assert_selected_objects_1(rev3, sel, ob1b)

sel1 = xorn.storage.select_all(rev3)
assert sel1 is not None

if True:
    sel = xorn.storage.select_all_except(rev0, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_0(rev2, sel)
    assert_selected_objects_0(rev3, sel)

    sel = xorn.storage.select_all_except(rev1, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_0(rev2, sel)
    assert_selected_objects_0(rev3, sel)

    sel = xorn.storage.select_all_except(rev2, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_1(rev2, sel, ob1a)
    assert_selected_objects_0(rev3, sel)

    sel = xorn.storage.select_all_except(rev3, sel1)
    assert sel is not None
    assert_selected_objects_0(rev0, sel)
    assert_selected_objects_0(rev1, sel)
    assert_selected_objects_0(rev2, sel)
    assert_selected_objects_0(rev3, sel)

# select union
sel1 = xorn.storage.select_all(rev1)
assert sel1 is not None

if True:
    sel2 = xorn.storage.select_none()
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_1(rev2, sel, ob0)

    sel2 = xorn.storage.select_object(ob1a)
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_2(rev2, sel, ob0, ob1a)

    sel2 = xorn.storage.select_all(rev2)
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b)

    sel2 = xorn.storage.select_all(rev3)
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_2(rev2, sel, ob0, ob1b)

sel1 = xorn.storage.select_all_except(rev2, sel1)
assert sel1 is not None

if True:
    sel2 = xorn.storage.select_none()
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_2(rev2, sel, ob1a, ob1b)

    sel2 = xorn.storage.select_object(ob1a)
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_2(rev2, sel, ob1a, ob1b)

    sel2 = xorn.storage.select_all(rev2)
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b)

    sel2 = xorn.storage.select_all(rev3)
    assert sel2 is not None
    sel = xorn.storage.select_union(sel1, sel2)
    assert sel is not None
    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b)

# select intersection
sel1 = xorn.storage.select_all(rev1)
assert sel1 is not None

if True:
    sel2 = xorn.storage.select_none()
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_0(rev2, sel)

    sel2 = xorn.storage.select_object(ob1a)
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_0(rev2, sel)

    sel2 = xorn.storage.select_all(rev2)
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_1(rev2, sel, ob0)

    sel2 = xorn.storage.select_all(rev3)
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_1(rev2, sel, ob0)

sel1 = xorn.storage.select_all_except(rev2, sel1)
assert sel1 is not None

if True:
    sel2 = xorn.storage.select_none()
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_0(rev2, sel)

    sel2 = xorn.storage.select_object(ob1a)
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_1(rev2, sel, ob1a)

    sel2 = xorn.storage.select_all(rev2)
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_2(rev2, sel, ob1a, ob1b)

    sel2 = xorn.storage.select_all(rev3)
    assert sel2 is not None
    sel = xorn.storage.select_intersection(sel1, sel2)
    assert sel is not None
    assert_selected_objects_1(rev2, sel, ob1b)
