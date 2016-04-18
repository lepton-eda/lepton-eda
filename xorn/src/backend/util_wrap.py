# xorn.geda.netlist - gEDA Netlist Extraction and Generation
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

import sys

## Wrap a string into lines no longer than \a width.
#
# \a insert_before_newline is put on the end of each wrapped line,
# before the newline character.  \a insert_after_newline is put on the
# start of each continued line.
#
# A newline character is appended at the end of the returned string.

def wrap(s, width, insert_before_newline, insert_after_newline):
    if width >= len(s):
        return s + '\n'

    lines = []
    start = 0

    while start + width - len(insert_after_newline) < len(s):
        search_until = start + width - len(insert_before_newline) \
                                     - len(insert_after_newline) + 1
        try:
            end = s.rindex(' ', start, search_until)
        except ValueError:
            sys.stderr.write("Couldn't wrap string at requested position\n")
            # return " Wrap error!"
            try:
                end = s.index(' ', search_until)
            except ValueError:
                return s

        lines.append(s[start:end])
        start = end + 1

    lines.append(s[start:])
    return (insert_before_newline + '\n' +
            insert_after_newline).join(lines) + '\n'
