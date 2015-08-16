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

# Copyright (C) 2001-2010 MIYAMOTO Takanori

from partslist_common import *

def count_same_parts(ls):
    if not ls:
        return []

    first_ls = ls[0][1:]
    match_length = 0
    for i, l in enumerate(reversed(ls)):
        if l[1:] == first_ls:
            match_length = len(ls) - i
            break
    rest_ls = ls[match_length:]
    uref_ls = [l[0] for l in ls[:match_length]]

    return [(uref_ls, first_ls + (str(match_length), ))] + \
           count_same_parts(rest_ls)

def run(f, netlist):
    parts_table = count_same_parts(marge_sort_with_multikey(
        get_parts_table(netlist), [1, 2, 3, 0]))

    f.write('.START\n')
    f.write('..device\tvalue\tfootprint\tquantity\trefdes\n')
    for a, b in parts_table:
        write_one_row(f, b, '\t', '\t')
        write_one_row(f, a, ' ', '\n')
    f.write('.END\n')
