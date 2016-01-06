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

assert rev0.object_exists(ob0) == False
assert rev0.object_exists(ob1a) == False
assert rev0.object_exists(ob1b) == False

assert rev1.object_exists(ob0) == True
assert rev1.object_exists(ob1a) == False
assert rev1.object_exists(ob1b) == False

assert rev2.object_exists(ob0) == True
assert rev2.object_exists(ob1a) == True
assert rev2.object_exists(ob1b) == True

assert rev3.object_exists(ob0) == True
assert rev3.object_exists(ob1a) == False
assert rev3.object_exists(ob1b) == True
