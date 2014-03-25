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

def object_location_fails(rev, ob):
    try:
        attached_to, position = rev.get_object_location(ob)
    except KeyError:
        return True
    else:
        return False

rev0, rev1, rev2, rev3, ob0, ob1a, ob1b = Setup.setup()

assert object_location_fails(rev0, ob0)
assert object_location_fails(rev0, ob1a)
assert object_location_fails(rev0, ob1b)

assert rev1.get_object_location(ob0) == (None, 0)
assert object_location_fails(rev1, ob1a)
assert object_location_fails(rev1, ob1b)

assert rev2.get_object_location(ob0) == (None, 0)
assert rev2.get_object_location(ob1a) == (None, 1)
assert rev2.get_object_location(ob1b) == (None, 2)

assert rev3.get_object_location(ob0) == (None, 0)
assert object_location_fails(rev3, ob1a)
assert rev3.get_object_location(ob1b) == (None, 1)

rev4 = xorn.storage.Revision(rev3)
rev4.set_object_data(ob1a, xorn.storage.Arc())

assert rev4.get_object_location(ob0) == (None, 0)
assert rev4.get_object_location(ob1a) == (None, 2)
assert rev4.get_object_location(ob1b) == (None, 1)

ob2 = rev4.copy_object(rev1, ob0)
assert ob2 is not None

assert rev4.get_object_location(ob0) == (None, 0)
assert rev4.get_object_location(ob1a) == (None, 2)
assert rev4.get_object_location(ob1b) == (None, 1)
assert rev4.get_object_location(ob2) == (None, 3)

rev4.delete_object(ob0)

assert object_location_fails(rev4, ob0)
assert rev4.get_object_location(ob1a) == (None, 1)
assert rev4.get_object_location(ob1b) == (None, 0)
assert rev4.get_object_location(ob2) == (None, 2)
