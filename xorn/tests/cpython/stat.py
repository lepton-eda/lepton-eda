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

assert xorn.storage.get_added_objects(rev0, rev1) == [ob0]
assert xorn.storage.get_removed_objects(rev0, rev1) == []
assert xorn.storage.get_modified_objects(rev0, rev1) == []

assert xorn.storage.get_added_objects(rev1, rev2) in [[ob1a, ob1b],
                                                      [ob1b, ob1a]]
assert xorn.storage.get_removed_objects(rev1, rev2) == []
assert xorn.storage.get_modified_objects(rev1, rev2) == []

assert xorn.storage.get_added_objects(rev2, rev3) == []
assert xorn.storage.get_removed_objects(rev2, rev3) == [ob1a]
assert xorn.storage.get_modified_objects(rev2, rev3) == [ob0]
