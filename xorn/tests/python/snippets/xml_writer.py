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

import sys
import xorn.xml_writer

f = sys.stdout

## [use XMLWriter]
w = xorn.xml_writer.XMLWriter(f.write)
w.start_element('document')
w.write_attribute('attribute', 'value')
w.write_character_data('some text')
w.end_element()
assert w.is_done()
## [use XMLWriter]
