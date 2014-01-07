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

assert rev0.get_objects() == []

assert rev1.get_objects() == [ob0]

assert rev2.get_objects() in [[ob0, ob1a, ob1b], [ob0, ob1b, ob1a],
                              [ob1a, ob0, ob1b], [ob1a, ob1b, ob0],
                              [ob1b, ob0, ob1a], [ob1b, ob1a, ob0]]

assert rev3.get_objects() in [[ob0, ob1b], [ob1b, ob0]]
