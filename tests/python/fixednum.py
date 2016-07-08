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

import xorn.fixednum

def throws(fun, *args):
    try:
        fun(*args)
    except Exception as e:
        return type(e)

assert throws(xorn.fixednum.format, 42., 0) == TypeError
assert throws(xorn.fixednum.format, 42, None) == TypeError
assert throws(xorn.fixednum.format, 42, -1) == ValueError

assert throws(xorn.fixednum.parse, 42, 0) == TypeError
assert throws(xorn.fixednum.parse, 42., 0) == TypeError
assert throws(xorn.fixednum.parse, '42', None) == TypeError
assert throws(xorn.fixednum.parse, '42', -1) == ValueError

assert throws(xorn.fixednum.parse, '', 0) == ValueError
assert throws(xorn.fixednum.parse, '.42', 0) == ValueError
assert throws(xorn.fixednum.parse, '4.2', 0) == ValueError
assert throws(xorn.fixednum.parse, ':42', 0) == ValueError
assert throws(xorn.fixednum.parse, 'x', 0) == ValueError

assert throws(xorn.fixednum.parse, '', 3) == ValueError
assert throws(xorn.fixednum.parse, '.4242', 3) == ValueError
assert throws(xorn.fixednum.parse, ':42', 3) == ValueError
assert throws(xorn.fixednum.parse, 'x', 3) == ValueError
assert throws(xorn.fixednum.parse, '.x', 3) == ValueError

assert xorn.fixednum.format(0, 0) == '0'
assert xorn.fixednum.format(0, 3) == '0'
assert str(xorn.fixednum.parse('0', 0)) == '0'
assert str(xorn.fixednum.parse('0', 3)) == '0'
assert str(xorn.fixednum.parse('-0', 0)) == '0'
assert str(xorn.fixednum.parse('-0', 3)) == '0'

for x, decimal_digits, s in [
        (42, 0, '42'),
        (42, 1, '4.2'),
        (42, 2, '.42'),
        (42, 3, '.042'),
        (42, 4, '.0042'),
        (42, 5, '.00042'),

        (-42, 0, '-42'),
        (-42, 1, '-4.2'),
        (-42, 2, '-.42'),

        (12345, 0, '12345'),
        (12345, 1, '1234.5'),
        (12345, 2, '123.45'),
        (12345, 3, '12.345'),
        (12345, 4, '1.2345'),
        (12345, 5, '.12345'),

        (-12345, 0, '-12345'),
        (-12345, 1, '-1234.5'),
        (-12345, 2, '-123.45'),
        (-12345, 3, '-12.345'),
        (-12345, 4, '-1.2345'),
        (-12345, 5, '-.12345'),

        (-42000, 3, '-42'),
        (-42090, 3, '-42.09'),
        (-90, 3, '-.09')]:
    assert xorn.fixednum.format(x, decimal_digits) == s
    assert xorn.fixednum.parse(s, decimal_digits) == x
