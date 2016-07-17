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

import StringIO, os, sys
import xorn.geda.fileformat
import xorn.geda.read
import xorn.geda.ref

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

PICTURES_SCH = """v 20150930 2
G 0 0 100 100 0 0 0
pixmap.data
G 0 0 100 100 0 0 0
pixmap.data
G 0 0 100 100 0 0 1
pixmap.data
AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1
Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWpr
bG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6Ch
oqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX
2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==
.
G 0 0 100 100 0 0 1
pixmap.data
AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1
Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWpr
bG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6Ch
oqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX
2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==
.
"""

for load_pixmaps in [False, True]:
    rev = xorn.geda.read.read_file(
        StringIO.StringIO(PICTURES_SCH), '<test data>',
        xorn.geda.fileformat.FORMAT_SCH,
        load_pixmaps = load_pixmaps,
        pixmap_basepath = os.path.dirname(sys.argv[0]))
    p0, p1, p2, p3 = (ob.data().pixmap for ob in rev.toplevel_objects())
    assert p0 == p1
    assert p0 != p2
    assert p0 != p3
    assert p2 != p3

    for pixmap, embedded in [(p0, False), (p1, False), (p2, True), (p3, True)]:
        assert isinstance(pixmap, xorn.geda.ref.Pixmap)
        assert pixmap.filename == 'pixmap.data'
        assert pixmap.embedded == embedded

        if load_pixmaps or embedded:
            assert isinstance(pixmap.data, str)
            assert pixmap.data == PIXMAP_DATA
        else:
            assert pixmap.data is None
