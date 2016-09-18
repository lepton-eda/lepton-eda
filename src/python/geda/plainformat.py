# xorn.geda - Python library for manipulating gEDA files
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

## \namespace xorn.geda.plainformat
## %gEDA schematic/symbol file format constants

import xorn.proxy
import xorn.storage

# release version which had file format changes
# New file format changes after 20030921 use the above version
# and not these #defines anymore.
VERSION_20000220 = 20000220
VERSION_20000704 = 20000704
VERSION_20020825 = 20020825
VERSION_20030921 = 20030921
# 20030921 wasn't a real version, just a MinGW test version, but it is
# out there

# Object types
OBJ_LINE = 'L'
OBJ_PATH = 'H'
OBJ_BOX = 'B'
OBJ_PICTURE = 'G'
OBJ_CIRCLE = 'V'
OBJ_NET = 'N'
OBJ_BUS = 'U'
OBJ_COMPLEX = 'C'
OBJ_TEXT = 'T'
OBJ_PIN = 'P'
OBJ_ARC = 'A'
# OBJ_PLACEHOLDER = 'X'
# can't occur in files

STARTATTACH_ATTR = '{'
ENDATTACH_ATTR = '}'
START_EMBEDDED = '['
END_EMBEDDED = ']'

# font stuff
INFO_FONT = 'F'
VERSION_CHAR = 'v'

# misc stuff
COMMENT = '#'

# for color mechanism used in gschem
MAX_COLORS = 25
DEFAULT_COLOR = 3

# for text alignment
#   2 -- 5 -- 8
#   |    |    |
#   1 -- 4 -- 7
#   |    |    |
#   0 -- 3 -- 6
LOWER_LEFT = 0
MIDDLE_LEFT = 1
UPPER_LEFT = 2
LOWER_MIDDLE = 3
MIDDLE_MIDDLE = 4
UPPER_MIDDLE = 5
LOWER_RIGHT = 6
MIDDLE_RIGHT = 7
UPPER_RIGHT = 8

## Rotate/translate objects in an embedded symbol for saving.

def transform(rev, delta_x, delta_y, angle, mirror):
    def translate(x, y):
        return x + delta_x, y + delta_y

    def rotate(x, y):
        if not mirror:
            if angle == 0:
                return x, y
            if angle == 90:
                return -y, x
            if angle == 180:
                return -x, -y
            if angle == 270:
                return y, -x
        else:
            if angle == 0:
                return -x, y
            if angle == 90:
                return -y, -x
            if angle == 180:
                return x, -y
            if angle == 270:
                return y, x

    def rotate_rect(x, y, width, height):
        if not mirror:
            if angle == 0:
                return x, y, width, height
            if angle == 90:
                return -y - height, x, height, width
            if angle == 180:
                return -x - width, -y - height, width, height
            if angle == 270:
                return y, -x - width, height, width
        else:
            if angle == 0:
                return -x - width, y, width, height
            if angle == 90:
                return -y - height, -x - width, height, width
            if angle == 180:
                return x, -y - height, width, height
            if angle == 270:
                return y, x, height, width

    for ob in xorn.proxy.RevisionProxy(rev).all_objects():
        data = ob.data()

        if isinstance(data, xorn.storage.Arc):
            ob.x, ob.y = rotate(ob.x, ob.y)
            ob.x, ob.y = translate(ob.x, ob.y)
            if mirror:
                ob.startangle = 180 - ob.startangle
                ob.sweepangle = -ob.sweepangle
            ob.startangle += angle
            ob.startangle %= 360
        elif isinstance(data, xorn.storage.Box):
            ob.x, ob.y, ob.width, ob.height = \
                rotate_rect(ob.x, ob.y, ob.width, ob.height)
            ob.x, ob.y = translate(ob.x, ob.y)
        elif isinstance(data, xorn.storage.Circle):
            ob.x, ob.y = rotate(ob.x, ob.y)
            ob.x, ob.y = translate(ob.x, ob.y)
        elif isinstance(data, xorn.storage.Component):
            ob.x, ob.y = rotate(ob.x, ob.y)
            ob.x, ob.y = translate(ob.x, ob.y)
            if mirror:
                ob.angle = -ob.angle
            ob.angle += angle
            ob.angle %= 360
            ob.mirror ^= mirror
        elif isinstance(data, xorn.storage.Line):
            ob.x, ob.y = rotate(ob.x, ob.y)
            ob.width, ob.height = rotate(ob.width, ob.height)
            ob.x, ob.y = translate(ob.x, ob.y)
        elif isinstance(data, xorn.storage.Net):
            ob.x, ob.y = rotate(ob.x, ob.y)
            ob.width, ob.height = rotate(ob.width, ob.height)
            ob.x, ob.y = translate(ob.x, ob.y)
        elif isinstance(data, xorn.storage.Path):
            pass  # not supported
        elif isinstance(data, xorn.storage.Picture):
            ob.x, ob.y, ob.width, ob.height = \
                rotate_rect(ob.x, ob.y, ob.width, ob.height)
            ob.x, ob.y = translate(ob.x, ob.y)
            if mirror:
                ob.angle = -ob.angle
            ob.angle += angle
            ob.angle %= 360
            ob.mirror ^= mirror
        elif isinstance(data, xorn.storage.Text):
            ob.x, ob.y = rotate(ob.x, ob.y)
            ob.x, ob.y = translate(ob.x, ob.y)
            if mirror:
                v_align = ob.alignment % 3
                if ob.angle % 180 == 90:
                    ob.alignment = (ob.alignment - v_align) + (2 - v_align)
                else:
                    ob.alignment = 6 - (ob.alignment - v_align) + v_align
            ob.angle += angle
            ob.angle %= 360

## Rotate/translate objects in an embedded symbol back to normal.

def untransform(rev, delta_x, delta_y, angle, mirror):
    def untranslate(x, y):
        return x - delta_x, y - delta_y

    def unrotate(x, y):
        if not mirror:
            if angle == 0:
                return x, y
            if angle == 90:
                return y, -x
            if angle == 180:
                return -x, -y
            if angle == 270:
                return -y, x
        else:
            if angle == 0:
                return -x, y
            if angle == 90:
                return -y, -x
            if angle == 180:
                return x, -y
            if angle == 270:
                return y, x

    def unrotate_rect(x, y, width, height):
        if not mirror:
            if angle == 0:
                return x, y, width, height
            if angle == 90:
                return y, -x - width, height, width
            if angle == 180:
                return -x - width, -y - height, width, height
            if angle == 270:
                return -y - height, x, height, width
        else:
            if angle == 0:
                return -x - width, y, width, height
            if angle == 90:
                return -y - height, -x - width, height, width
            if angle == 180:
                return x, -y - height, width, height
            if angle == 270:
                return y, x, height, width

    for ob in xorn.proxy.RevisionProxy(rev).all_objects():
        data = ob.data()

        if isinstance(data, xorn.storage.Arc):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y = unrotate(ob.x, ob.y)
            ob.startangle -= angle
            if mirror:
                ob.startangle = 180 - ob.startangle
                ob.sweepangle = -ob.sweepangle
            ob.startangle %= 360
        elif isinstance(data, xorn.storage.Box):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y, ob.width, ob.height = \
                unrotate_rect(ob.x, ob.y, ob.width, ob.height)
        elif isinstance(data, xorn.storage.Circle):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y = unrotate(ob.x, ob.y)
        elif isinstance(data, xorn.storage.Component):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y = unrotate(ob.x, ob.y)
            ob.angle -= angle
            if mirror:
                ob.angle = -ob.angle
            ob.angle %= 360
            ob.mirror ^= mirror
        elif isinstance(data, xorn.storage.Line):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y = unrotate(ob.x, ob.y)
            ob.width, ob.height = unrotate(ob.width, ob.height)
        elif isinstance(data, xorn.storage.Net):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y = unrotate(ob.x, ob.y)
            ob.width, ob.height = unrotate(ob.width, ob.height)
        elif isinstance(data, xorn.storage.Path):
            pass  # not supported
        elif isinstance(data, xorn.storage.Picture):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y, ob.width, ob.height = \
                unrotate_rect(ob.x, ob.y, ob.width, ob.height)
            ob.angle -= angle
            if mirror:
                ob.angle = -ob.angle
            ob.angle %= 360
            ob.mirror ^= mirror
        elif isinstance(data, xorn.storage.Text):
            ob.x, ob.y = untranslate(ob.x, ob.y)
            ob.x, ob.y = unrotate(ob.x, ob.y)
            ob.angle -= angle
            if mirror:
                v_align = ob.alignment % 3
                if ob.angle % 180 == 90:
                    ob.alignment = (ob.alignment - v_align) + (2 - v_align)
                else:
                    ob.alignment = 6 - (ob.alignment - v_align) + v_align
            ob.angle %= 360
