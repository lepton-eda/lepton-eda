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

## \namespace xorn.geda.ref
## Referenced symbols and pixmaps.

class Symbol:
    def __init__(self, basename, prim_objs, embedded):
        ## Last part of the symbol filename.
        self.basename = basename
        ## Revision object containing the symbol data.
        self.prim_objs = prim_objs
        ## Whether the symbol is embedded.
        self.embedded = embedded

class Pixmap:
    def __init__(self, filename, data, embedded):
        ## Path and filename of the pixmap.
        #
        # Not used if the picture is embedded.
        self.filename = filename
        ## String object containing the pixmap data.
        self.data = data
        ## Whether the pixmap is embedded.
        self.embedded = embedded
