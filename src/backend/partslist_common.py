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

# Copyright (C) 2001-2010 MIYAMOTO Takanori

def get_parts_table(netlist):
    return [(package.refdes,
             package.get_attribute('device', 'unknown'),
             package.get_attribute('value', 'unknown'),
             package.get_attribute('footprint', 'unknown'))  # sdb change
            for package in reversed(netlist.packages)
            if package.get_attribute('device', 'unknown') != 'include']

def write_one_row(f, ls, separator, end_char):
    if ls:
        f.write(separator.join(ls) + end_char)

def marge_sort_sub(ls1, ls2, key_column):
    if not ls1 or not ls2:
        return ls1 + ls2

    if ls1[0][key_column].lower() <= ls2[0][key_column].lower():
        return [ls1[0]] + marge_sort_sub(ls1[1:], ls2, key_column)
    else:
        return [ls2[0]] + marge_sort_sub(ls1, ls2[1:], key_column)

def marge_sort(ls, key_column):
    if len(ls) <= 1:
        return ls

    return marge_sort_sub(
        marge_sort(ls[:len(ls) - len(ls) / 2], key_column),
        marge_sort(ls[len(ls) - len(ls) / 2:], key_column), key_column)

def marge_sort_with_multikey(ls, key_columns):
    if len(ls) <= 1 or not key_columns:
        return ls

    key_column = key_columns[0]
    sorted_ls = marge_sort(ls, key_column)

    first_value = sorted_ls[0][key_column]
    match_length = 0
    for i, x in enumerate(reversed(sorted_ls)):
        if x[key_column] == first_value:
            match_length = len(sorted_ls) - i
            break

    return marge_sort_with_multikey(list(reversed(sorted_ls[:match_length])),
                                    key_columns[1:]) + \
           marge_sort_with_multikey(sorted_ls[match_length:], key_columns)
