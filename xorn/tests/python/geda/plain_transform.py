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

import cStringIO, subprocess, sys, tempfile
import xorn.proxy
import xorn.storage
import xorn.geda.fileformat
import xorn.geda.read
import xorn.geda.write

def diff(a, b):
    if a == b:
        return

    fa = tempfile.NamedTemporaryFile()
    fa.write(a)
    fa.flush()
    fb = tempfile.NamedTemporaryFile()
    fb.write(b)
    fb.flush()
    subprocess.call(['diff', '-u', fa.name, fb.name])
    sys.exit(1)

# test writing embedded symbols

rev = xorn.geda.read.read(sys.argv[0][:-3] + '.sch.xml')

f = open(sys.argv[0][:-3] + '.sch')
reference_sch = 'v 20121203 2' + f.read()[12:]
f.close()

f = cStringIO.StringIO()
xorn.geda.write.write_file(f, rev, xorn.geda.fileformat.FORMAT_SCH)
diff(reference_sch, f.getvalue())
f.close()

# serialize known-good symbol in libgeda format

symbols = set()
for ob in rev.toplevel_objects():
    data = ob.data()
    if isinstance(data, xorn.storage.Component):
        symbols.add(data.symbol)
symbol, = symbols

f = cStringIO.StringIO()
xorn.geda.write.write_file(
    f, xorn.proxy.RevisionProxy(symbol.prim_objs),
    xorn.geda.fileformat.FORMAT_SYM)
symbol_data = f.getvalue()
f.close()

# extract symbols from libgeda schematic
# and match them against known-good symbol

f = cStringIO.StringIO(reference_sch)
rev = xorn.geda.read.read_file(f, '<reference data>',
                               xorn.geda.fileformat.FORMAT_SCH)
f.close()

for ob in rev.toplevel_objects():
    data = ob.data()
    assert isinstance(data, xorn.storage.Component)

    f = cStringIO.StringIO()
    xorn.geda.write.write_file(
        f, xorn.proxy.RevisionProxy(data.symbol.prim_objs),
        xorn.geda.fileformat.FORMAT_SCH)
    diff(symbol_data, f.getvalue())
    f.close()
