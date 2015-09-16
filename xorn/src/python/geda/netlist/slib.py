# xorn.geda.netlist - gEDA Netlist Extraction and Generation
# Copyright (C) 1998-2010 Ales Hvezda
# Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

## \namespace xorn.geda.netlist.slib
## Source library.
#
# slib stands for source (project/schematic/hdl/model source) library

import os

## Search path.

slib = []

## Search the source library for a particular file name substring.
#
# Search the source library for a directory, starting with the last
# one, which contains a file whose filename contains the substring \a
# basename.
#
# \returns the found directory plus \a basename, or \c None if \a
#          basename was not found in the source library
#
# If, for example, the directory \c path/to is in the source library
# and contains the file \c myschematic.sch, then a search for \c
# "schematic" would return \c "path/to/schematic" [sic].

def s_slib_search_single(basename):
    # search slib paths backwards
    for i, dir_name in reversed(list(enumerate(slib))):
        for d_name in os.listdir(dir_name):
            # Do a substring comp for a match
            if basename in d_name:
                return os.path.join(dir_name, basename)

    return None
