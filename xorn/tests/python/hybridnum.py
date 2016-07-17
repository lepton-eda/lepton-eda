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

import xorn.hybridnum

def throws(fun, *args):
    try:
        fun(*args)
    except Exception as e:
        return type(e)

assert throws(xorn.hybridnum.format, 42, None) == TypeError
assert throws(xorn.hybridnum.format, 42, -1) == ValueError

assert throws(xorn.hybridnum.parse, 42, 0) == TypeError
assert throws(xorn.hybridnum.parse, 42., 0) == TypeError
assert throws(xorn.hybridnum.parse, '42', None) == TypeError
assert throws(xorn.hybridnum.parse, '42', -1) == ValueError

assert throws(xorn.hybridnum.parse, '', 0) == ValueError
assert throws(xorn.hybridnum.parse, ':', 0) == ValueError
assert throws(xorn.hybridnum.parse, '42.:', 0) == ValueError
assert throws(xorn.hybridnum.parse, '.42', 0) == ValueError
assert throws(xorn.hybridnum.parse, '.42:', 0) == ValueError
assert throws(xorn.hybridnum.parse, '4.2', 0) == ValueError
assert throws(xorn.hybridnum.parse, '4.2:', 0) == ValueError
assert throws(xorn.hybridnum.parse, 'x', 0) == ValueError
assert throws(xorn.hybridnum.parse, ':x', 0) == ValueError

assert throws(xorn.hybridnum.parse, '', 3) == ValueError
assert throws(xorn.hybridnum.parse, ':', 3) == ValueError
assert throws(xorn.hybridnum.parse, '4:2', 3) == ValueError
assert throws(xorn.hybridnum.parse, '.:42', 3) == ValueError
assert throws(xorn.hybridnum.parse, '.42:', 3) == ValueError
assert throws(xorn.hybridnum.parse, '.4242', 3) == ValueError
assert throws(xorn.hybridnum.parse, 'x', 3) == ValueError
assert throws(xorn.hybridnum.parse, '.x', 3) == ValueError
assert throws(xorn.hybridnum.parse, ':x', 3) == ValueError

assert xorn.hybridnum.format(0., 0) == '0'
assert xorn.hybridnum.format(0., 3) == '0'
assert xorn.hybridnum.format(-0., 0) == '0'
assert xorn.hybridnum.format(-0., 3) == '0'
assert str(xorn.hybridnum.parse('0', 0)) == '0.0'
assert str(xorn.hybridnum.parse('0', 3)) == '0.0'
assert str(xorn.hybridnum.parse('-0', 0)) == '0.0'
assert str(xorn.hybridnum.parse('-0', 3)) == '0.0'

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

        (12345.75, 0, '12345:c'),
        (12345.75, 1, '1234.5:c'),
        (12345.75, 2, '123.45:c'),
        (12345.75, 3, '12.345:c'),
        (12345.75, 4, '1.2345:c'),
        (12345.75, 5, '.12345:c'),

        (-12345.75, 0, '-12345:c'),
        (-12345.75, 1, '-1234.5:c'),
        (-12345.75, 2, '-123.45:c'),
        (-12345.75, 3, '-12.345:c'),
        (-12345.75, 4, '-1.2345:c'),
        (-12345.75, 5, '-.12345:c'),

        (1. * 8, 0, '8'),
        (1. * 4, 0, '4'),
        (1. * 2, 0, '2'),
        (1., 0, '1'),
        (1. / 2, 0, ':8'),
        (1. / 4, 0, ':4'),
        (1. / 8, 0, ':2'),
        (1. / 16, 0, ':1'),
        (1. / 32, 0, ':08'),
        (1. / 64, 0, ':04'),
        (1. / 128, 0, ':02'),
        (1. / 256, 0, ':01'),

        (-1., 0, '-1'),
        (-1.5, 0, '-1:8'),

        (-.5, 3, '-:8'),
        (-42000.5, 3, '-42.000:8'),
        (-42000., 3, '-42'),

        (-42090.5, 3, '-42.090:8'),
        (-90.5, 3, '-.090:8'),
        (-42090., 3, '-42.09'),
        (-90., 3, '-.09')]:
    assert xorn.hybridnum.format(x, decimal_digits) == s
    assert xorn.hybridnum.parse(s, decimal_digits) == x
