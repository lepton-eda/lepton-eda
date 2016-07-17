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

import cStringIO, sys
import xorn.geda.fileformat
import xorn.geda.read
import xorn.geda.write

f = open(sys.argv[0][:-3] + '.sch')
data = f.read()
f.close()

f = cStringIO.StringIO(data)
rev = xorn.geda.read.read_file(f, '<test data>',
                               xorn.geda.fileformat.FORMAT_SCH)
f.close()

f = cStringIO.StringIO()
xorn.geda.write.write_file(f, rev, xorn.geda.fileformat.FORMAT_SCH)
assert f.getvalue() == data
f.close()
