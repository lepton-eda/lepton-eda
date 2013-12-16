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

def assert_cannot_modify(rev, existing_ob):
    try:
        rev.add_object(line_data)
    except ValueError:
        pass
    else:
        raise AssertionError

    try:
        rev.set_object_data(existing_ob, box_data)
    except ValueError:
        pass
    else:
        raise AssertionError

    try:
        rev.copy_object(rev, existing_ob)
    except ValueError:
        pass
    else:
        raise AssertionError

    try:
        rev.delete_object(existing_ob)
    except ValueError:
        pass
    else:
        raise AssertionError

    sel0 = xorn.storage.select_all(rev)
    assert sel0 is not None

    try:
        rev.copy_objects(rev, sel0)
    except ValueError:
        pass
    else:
        raise AssertionError

    try:
        rev.delete_objects(sel0)
    except ValueError:
        pass
    else:
        raise AssertionError

def assert_can_modify(rev, existing_ob):
    assert rev.add_object(line_data) is not None

    rev.set_object_data(existing_ob, box_data)

    ob = rev.copy_object(rev, existing_ob)
    assert ob is not None

    rev.delete_object(ob)
    assert rev.object_exists(ob) == False

    sel = xorn.storage.select_all(rev)
    assert sel is not None

    assert rev.copy_objects(rev, sel) is not None

    rev.delete_objects(sel)
    assert rev.object_exists(existing_ob) == False

line_data = xorn.storage.Line()
line_data.x = 0
line_data.y = 1
line_data.width = 3
line_data.height = 2
line_data.color = 3
line_data.line.width = 1

box_data = xorn.storage.Box()
box_data.x = 1
box_data.y = 1
box_data.width = 2
box_data.height = 2
box_data.color = 3
box_data.line.width = 1

rev0 = xorn.storage.Revision(None)
assert rev0 is not None
assert rev0.is_transient() == True
rev0.mtswach()
assert rev0.is_transient() == False

rev1 = xorn.storage.Revision(rev0)
assert rev1 is not None
assert rev1.is_transient() == True

ob = rev1.add_object(line_data)
assert ob is not None

rev1.mtswach()
assert rev1.is_transient() == False

assert_cannot_modify(rev1, ob)

rev2 = xorn.storage.Revision(rev1)
assert rev2 is not None
assert rev2.is_transient() == True

assert_can_modify(rev2, ob)

rev2.mtswach()
assert rev2.is_transient() == False
