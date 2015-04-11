# Copyright (C) 2013-2015 Roland Lutz
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
import xorn.proxy
import xorn.storage
import xorn.geda.read

COMPONENT0_SCH = """v 20140308 2
C 0 0 1 0 0 symbol.sym
{
}
"""

COMPONENT1_SCH = """v 20140308 2
C 0 0 1 0 0 EMBEDDEDsymbol.sym
[
P 100 200 200 200 1 0 0
{
T 25 250 5 8 0 1 0 0 1
pinnumber=1
T 25 250 5 8 0 1 0 0 1
pinseq=1
T 25 250 5 8 0 1 0 0 1
pinlabel=1
}
B 200 0 400 400 3 10 1 0 -1 -1 0 -1 -1 -1 -1 -1
]
{
}
"""

PICTURE0_SCH = """v 20140308 2
G 0 0 1000 1000 0 0 0
hello-world
"""

PICTURE1_SCH = """v 20140308 2
G 0 0 1000 1000 0 0 1
hello-world
SGVsbG8gd29ybGQK
.
"""

for data, embedded in [(COMPONENT0_SCH, False),
                       (COMPONENT1_SCH, True)]:
    rev = xorn.geda.read.read_file(StringIO.StringIO(data), '<test data>')
    ob, = rev.toplevel_objects()
    symbol = ob.data().symbol
    assert isinstance(symbol, xorn.geda.read.Symbol)
    assert symbol.basename == 'symbol.sym'
    assert symbol.embedded == embedded

    if not embedded:
        assert symbol.prim_objs is None
        continue

    assert isinstance(symbol.prim_objs, xorn.storage.Revision)
    pin, box = xorn.proxy.RevisionProxy(symbol.prim_objs).toplevel_objects()
    assert isinstance(pin.data(), xorn.storage.Net)
    assert isinstance(box.data(), xorn.storage.Box)
    assert len(pin.attached_objects()) == 3

for data, embedded in [(PICTURE0_SCH, False),
                       (PICTURE1_SCH, True)]:
    rev = xorn.geda.read.read_file(StringIO.StringIO(data), '<test data>')
    ob, = rev.toplevel_objects()
    pixmap = ob.data().pixmap
    assert isinstance(pixmap, xorn.geda.read.Pixmap)
    assert pixmap.filename == 'hello-world'
    assert pixmap.file_content == ('Hello world\n' if embedded else None)
    assert pixmap.embedded == embedded
