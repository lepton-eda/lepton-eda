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

import StringIO
import xorn.geda.attrib
import xorn.geda.fileformat
import xorn.geda.read

data = """v 20150930 2
C 0 0 1 0 0 EMBEDDEDsym.sym
[
T 0 0 8 10 0 0 0 0 1
a=x
T 0 0 8 10 1 0 0 0 1
b=x
T 0 0 8 10 0 0 0 0 1
c=x
T 0 0 8 10 1 0 0 0 1
d=x
]
{
T 0 0 5 10 1 0 0 0 1
c=y
T 0 0 5 10 0 0 0 0 1
d=y
}
"""

rev = xorn.geda.read.read_file(StringIO.StringIO(data), '<test data>',
                               xorn.geda.fileformat.FORMAT_SCH)
ob, = rev.toplevel_objects()
a, b, c, d = xorn.geda.attrib.inherited_objects(ob)
assert a.visibility == False
assert b.visibility == True
assert c.visibility == True
assert d.visibility == False
