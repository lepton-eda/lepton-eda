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

import os, sys
import xorn.proxy
import xorn.storage
import xorn.geda.fileformat
import xorn.geda.ref
import xorn.geda.write

OUTPUT_FILE = 'python/geda/xmlwrite.out'

PIXMAP_DATA = \
    '\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f' \
    '\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f' \
    ' !"#$%&\'()*+,-./0123456789:;<=>?' \
    '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_' \
    '`abcdefghijklmnopqrstuvwxyz{|}~\x7f' \
    '\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f' \
    '\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f' \
    '\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf' \
    '\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf' \
    '\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf' \
    '\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf' \
    '\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef' \
    '\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff'

rev = xorn.storage.Revision()
sym_rev0 = xorn.storage.Revision()
sym_rev0.add_object(xorn.storage.Net(color = 17, is_pin = True))
sym_rev1 = xorn.storage.Revision()
sym_rev1.add_object(xorn.storage.Net(color = 18, is_pin = True))
sym_rev2 = xorn.storage.Revision()
sym_rev2.add_object(xorn.storage.Net(color = 19, is_pin = True))
sym_rev3 = xorn.storage.Revision()
sym_rev3.add_object(xorn.storage.Net(color = 20, is_pin = True))

symbol0 = xorn.geda.ref.Symbol('a.sym', sym_rev0, False)
symbol1 = xorn.geda.ref.Symbol('b.sym', sym_rev1, False)
symbol2 = xorn.geda.ref.Symbol('c.sym', sym_rev2, True)
symbol3 = xorn.geda.ref.Symbol('d.sym', sym_rev3, True)
symbol4 = xorn.geda.ref.Symbol('a.sym', sym_rev3, False)
pixmap0 = xorn.geda.ref.Pixmap('pixmap0.png', PIXMAP_DATA, False)
pixmap1 = xorn.geda.ref.Pixmap('pixmap1.png', PIXMAP_DATA, False)
pixmap2 = xorn.geda.ref.Pixmap('pixmap2.png', PIXMAP_DATA, True)
pixmap3 = xorn.geda.ref.Pixmap('pixmap3.png', PIXMAP_DATA, True)

line = xorn.storage.LineAttr(width = 1, cap_style = 1, dash_style = 2,
                             dash_length = 3141, dash_space = 5926)
fill = xorn.storage.FillAttr(type = 2, width = 1, angle0 = 53, pitch0 = 58,
                                                  angle1 = 97, pitch1 = 93)

for data in [
        xorn.storage.Arc(x = 3141, y = 5926, radius = 5358,
                         startangle = 97, sweepangle = 93, color = 3),
        xorn.storage.Arc(color = 17, line = line),

        xorn.storage.Box(x = 3141, y = 5926,
                         width = 5358, height = 9793, color = 3),
        xorn.storage.Box(color = 17, line = line, fill = fill),

        xorn.storage.Circle(x = 3141, y = 5926, radius = 5358, color = 3),
        xorn.storage.Circle(color = 17, line = line, fill = fill),

        xorn.storage.Component(x = 3141, y = 5926, selectable = True,
                               symbol = symbol0),
        xorn.storage.Component(selectable = False, angle = 270, mirror = True,
                               symbol = symbol0),
        xorn.storage.Component(selectable = True, symbol = symbol1),
        xorn.storage.Component(selectable = True, symbol = symbol2),
        xorn.storage.Component(selectable = True, symbol = symbol2),
        xorn.storage.Component(selectable = True, symbol = symbol3),

        xorn.storage.Line(x = 3141, y = 5926, width = 5358, height = 9793,
                          color = 3),
        xorn.storage.Line(color = 17, line = line),

        xorn.storage.Net(x = 3141, y = 5926, width = 5358, height = 9793,
                         color = 4),
        xorn.storage.Net(color = 17),
        xorn.storage.Net(color = 4, is_inverted = True),
        xorn.storage.Net(color = 10, is_bus = True),
        xorn.storage.Net(color = 17, is_bus = True),
        xorn.storage.Net(color = 1, is_pin = True),
        xorn.storage.Net(color = 17, is_pin = True),
        xorn.storage.Net(color = 1, is_pin = True, is_inverted = True),
        xorn.storage.Net(color = 1, is_pin = True, is_bus = True),
        xorn.storage.Net(color = 17, is_pin = True, is_bus = True),

        xorn.storage.Path(pathdata = 'M 0,0\nL 100,100\nz', color = 3),
        xorn.storage.Path(pathdata = '', color = 17, line = line, fill = fill),

        xorn.storage.Picture(x = 3141, y = 5926, width = 5358, height = 9793,
                             pixmap = pixmap0),
        xorn.storage.Picture(angle = 270, mirror = True, pixmap = pixmap0),
        xorn.storage.Picture(pixmap = pixmap1),
        xorn.storage.Picture(pixmap = pixmap2),
        xorn.storage.Picture(pixmap = pixmap2),
        xorn.storage.Picture(pixmap = pixmap3),

        xorn.storage.Text(x = 3141, y = 5926, color = 9, text_size = 10,
                          visibility = True, show_name_value = 1,
                          angle = 0, alignment = 0, text = 'foo'),
        xorn.storage.Text(x = 3141, y = 5926, color = 9, text_size = 10,
                          visibility = True, show_name_value = 2,
                          angle = 270, alignment = 8, text = 'foo'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo bar'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo\nbar'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo\nbar=baz'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo\\bar'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo\\\\bar'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo\\_bar'),
        xorn.storage.Text(color = 9, text_size = 10, visibility = True,
                          text = 'foo\\_bar\\_baz'),
        xorn.storage.Text(color = 5, text_size = 10, visibility = True,
                          text = 'foo=bar'),
        xorn.storage.Text(color = 5, text_size = 10, visibility = True,
                          text = 'foo=bar\nbaz'),
        xorn.storage.Text(color = 5, text_size = 10, visibility = True,
                          text = 'foo=bar\\baz'),
        xorn.storage.Text(color = 5, text_size = 10, visibility = True,
                          text = 'foo=bar\\\\baz'),
        xorn.storage.Text(color = 5, text_size = 10, visibility = True,
                          text = 'foo=bar\\_baz'),
        xorn.storage.Text(color = 5, text_size = 10, visibility = True,
                          text = 'foo\\_bar=baz')]:
    rev.add_object(data)

ob = rev.add_object(xorn.storage.Net(color = 4))
attrib0 = rev.add_object(xorn.storage.Text(
    color = 17, text_size = 10, visibility = True, text = 'foo'))
attrib1 = rev.add_object(xorn.storage.Text(
    color = 17, text_size = 10, visibility = True, text = 'foo=bar'))
rev.relocate_object(attrib0, ob, None)
rev.relocate_object(attrib1, ob, None)

ob = rev.add_object(xorn.storage.Component(
    selectable = True, symbol = symbol4))
attrib0 = rev.add_object(xorn.storage.Text(
    color = 17, text_size = 10, visibility = True, text = 'foo'))
attrib1 = rev.add_object(xorn.storage.Text(
    color = 17, text_size = 10, visibility = True, text = 'foo=bar'))
rev.relocate_object(attrib0, ob, None)
rev.relocate_object(attrib1, ob, None)

xorn.geda.write.write(xorn.proxy.RevisionProxy(rev), OUTPUT_FILE,
                      xorn.geda.fileformat.FORMAT_SCH_XML,
                      write_kwds = { 'backup': False, 'fsync': False })

os.execvp('diff', ['diff', '-u', sys.argv[0][:-3] + '.sch.xml', OUTPUT_FILE])
