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

import xorn.storage

## [add object]
rev = xorn.storage.Revision()

data = xorn.storage.Line()
data.x = 0.
data.y = 0.
data.width = 100.
data.height = 100.
data.line.width = 1.

ob = rev.add_object(data)
## [add object]

## [get/set object data]
data = rev.get_object_data(ob)
data.color = 3
rev.set_object_data(ob, data)
## [get/set object data]

## [get objects]
for ob in rev.get_objects():
    data = rev.get_object_data(ob)
    # do something with ob and data
## [get objects]
