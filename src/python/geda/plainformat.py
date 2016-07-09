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
