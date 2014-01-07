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

text_data = xorn.storage.Text()
text_data.text = "Hello world"

rev0 = xorn.storage.Revision(None)
assert rev0 is not None
rev0.mtswach()

rev1 = xorn.storage.Revision(rev0)
assert rev1 is not None
ob = rev1.add_object(text_data)
assert ob is not None
rev1.mtswach()

text_return = rev1.get_object_data(ob)
assert type(text_return) == xorn.storage.Text
assert text_return != text_data
assert text_return.text == "Hello world"
