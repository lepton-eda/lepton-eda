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

import xorn.storage

def check_not_attached(rev, ob0, ob1):
    assert rev.get_objects() == [ob0, ob1]
    rev.relocate_object(ob0, None, None)
    assert rev.get_objects() == [ob1, ob0]
    rev.relocate_object(ob0, None, ob1)

def check_attached(rev, ob0, ob1):
    assert rev.get_objects() == [ob0, ob1]
    rev.relocate_object(ob0, None, None)
    assert rev.get_objects() == [ob0, ob1]

def set_object_data_fails(rev, ob, data):
    try:
        rev.set_object_data(ob, data)
    except ValueError:
        return True
    else:
        return False

line_data = xorn.storage.Line()
net_data = xorn.storage.Net()
component_data = xorn.storage.Component()
text_data = xorn.storage.Text()

rev = xorn.storage.Revision()
ob0 = rev.add_object(line_data)
ob1 = rev.add_object(line_data)

assert rev is not None
assert ob0 is not None
assert ob1 is not None

rev.set_object_data(ob0, net_data)
rev.set_object_data(ob1, text_data)

check_not_attached(rev, ob0, ob1)


# data can be set without the object being reordered

rev.set_object_data(ob0, line_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob0, net_data)
check_not_attached(rev, ob0, ob1)

rev.set_object_data(ob1, line_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob1, net_data)
check_not_attached(rev, ob0, ob1)

# objects are re-added at the end

rev.delete_object(ob0)
rev.set_object_data(ob0, line_data)
check_not_attached(rev, ob1, ob0)
rev.delete_object(ob0)
rev.set_object_data(ob0, line_data)
check_not_attached(rev, ob1, ob0)

rev.delete_object(ob1)
rev.set_object_data(ob1, line_data)
check_not_attached(rev, ob0, ob1)
rev.delete_object(ob1)
rev.set_object_data(ob1, line_data)
check_not_attached(rev, ob0, ob1)

rev.delete_object(ob0)
rev.set_object_data(ob0, net_data)
check_not_attached(rev, ob1, ob0)
rev.delete_object(ob0)
rev.set_object_data(ob0, net_data)
check_not_attached(rev, ob1, ob0)

rev.delete_object(ob1)
rev.set_object_data(ob1, net_data)
check_not_attached(rev, ob0, ob1)
rev.delete_object(ob1)
rev.set_object_data(ob1, net_data)
check_not_attached(rev, ob0, ob1)


# can change ob0 to any type while no object is attached

rev.set_object_data(ob0, line_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob0, component_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob0, text_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob0, net_data)
check_not_attached(rev, ob0, ob1)

# can change type of ob1 while not attached

rev.set_object_data(ob1, line_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob1, net_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob1, component_data)
check_not_attached(rev, ob0, ob1)
rev.set_object_data(ob1, text_data)
check_not_attached(rev, ob0, ob1)


rev.relocate_object(ob1, ob0, None)
check_attached(rev, ob0, ob1)


# can't change ob0 to line or text while an object is attached

assert set_object_data_fails(rev, ob0, line_data)
check_attached(rev, ob0, ob1)
rev.set_object_data(ob0, net_data)
check_attached(rev, ob0, ob1)
rev.set_object_data(ob0, component_data)
check_attached(rev, ob0, ob1)
assert set_object_data_fails(rev, ob0, text_data)
check_attached(rev, ob0, ob1)

# can't change type of ob1 while attached

assert set_object_data_fails(rev, ob1, line_data)
check_attached(rev, ob0, ob1)
assert set_object_data_fails(rev, ob1, net_data)
check_attached(rev, ob0, ob1)
assert set_object_data_fails(rev, ob1, component_data)
check_attached(rev, ob0, ob1)
rev.set_object_data(ob1, text_data)
check_attached(rev, ob0, ob1)
