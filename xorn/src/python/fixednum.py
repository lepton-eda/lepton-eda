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

## \namespace xorn.fixednum
## Fixed-point numbers.
#
# This module provides formatting and parsing functions for
# fixed-point number notation.

## Convert an integer to its fixed-point string representation.
#
# \param [in] x               value to convert (must be either \c int
#                             or \c long)
# \param [in] decimal_digits  number of decimal digits used in the
#                             notation
#
# TODO: For efficiency reasons, this should be ported to C.

def format(x, decimal_digits):
    if not isinstance(x, int) and not isinstance(x, long):
        raise TypeError, 'invalid argument type (must be int or long)'
    if not isinstance(decimal_digits, int):
        raise TypeError, 'number of decimals must be an integer'
    if decimal_digits < 0:
        raise ValueError, 'number of decimals must be non-negative'

    s = str(x)

    if s[0] == '-':
        sign = '-'
        s = s[1:]
    else:
        sign = ''

    if decimal_digits == 0:
        if s == '0':
            return '0'  # signless zero
        return sign + s

    if len(s) < decimal_digits:
        s = '0' * (decimal_digits - len(s)) + s
    s0 = s[:-decimal_digits]
    s1 = s[-decimal_digits:]

    if s1 == '0' * decimal_digits:
        if not s0:
            return '0'  # signless zero
        return sign + s0
    else:
        return sign + s0 + '.' + s1.rstrip('0')

## Convert a fixed-point string representation to an integer.
#
# \param [in] x               value to convert (must be either \c str
#                             or \c unicode)
# \param [in] decimal_digits  number of decimal digits used in the
#                             notation
#
# TODO: For efficiency reasons, this should be ported to C.

def parse(s, decimal_digits):
    if not isinstance(s, str) and not isinstance(s, unicode):
        raise TypeError, 'invalid argument type (must be str or unicode)'
    if not isinstance(decimal_digits, int):
        raise TypeError, 'number of decimals must be an integer'
    if decimal_digits < 0:
        raise ValueError, 'number of decimals must be non-negative'

    if s and s[0] == '-':
        sign = -1
        s = s[1:]
    else:
        sign = 1

    if not s:
        raise ValueError

    if decimal_digits == 0:
        if '.' in s:
            raise ValueError
        s0 = s
        s1 = ''
    else:
        try:
            pos = s.index('.')
        except ValueError:
            s0 = s
            s1 = ''
        else:
            s0 = s[:pos]
            s1 = s[pos + 1:]
            if not s0 and not s1:
                raise ValueError
            if len(s1) < decimal_digits:
                s1 = s1 + (decimal_digits - len(s1)) * '0'

        if len(s1) > decimal_digits:
            raise ValueError

    for c in s0 + s1:
        if c not in '0123456789':
            raise ValueError

    if not s0:
        s0 = '0'
    if not s1:
        s1 = '0'

    return sign * (int(s0) * 10 ** decimal_digits + int(s1))
