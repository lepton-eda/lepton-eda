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

## \namespace xorn.hybridnum
## Hybrid fixed-/floating-point numbers.
#
# This module provides formatting and parsing functions for a hybrid
# number format which uses floating-point numbers as the base of a
# fixed-point notation.  This way, decimal fractions down to an
# arbitrary (but fixed) number of digits can be represented exactly
# while still allowing the benefits from a floating-point number
# format.
#
# As an example, in a format with three fixed digits, both the number
# \c 1 (represented as the floating-point value \c 1000.0) and the
# number \c 0.001 (represented as the floating-point value \c 1.0) can
# be represented exactly, whereas the number \c 0.0001 (represented as
# the floating-point number \c 0.1) would be subject to conversion
# errors.
#
# To avoid the usual errors when converting a floating-point number to
# a string and vice versa, hexadecimal notation is used for the
# decimals to the floating-point representation.

## Convert a floating-point number to its hybrid string representation.
#
# TODO: For efficiency reasons, this should probably be ported to C.

def format(x, decimal_digits):
    if not isinstance(decimal_digits, int):
        raise TypeError, 'number of decimals must be an integer'
    if decimal_digits < 0:
        raise ValueError, 'number of decimals must be non-negative'

    s = float(x).hex()

    if s[0] == '-':
        sign = '-'
        s = s[1:]
    else:
        sign = ''

    assert s[0] == '0' and s[1] == 'x'
    s = s[2:]

    pos = s.index('p')
    assert s[pos + 1] == '+' or s[pos + 1] == '-'
    mant, exp = s[:pos], int(s[pos + 1:])

    bits = []
    if mant[0] == '0':
        bits.append(0)  # shouldn't normally happen, though
    elif mant[0] == '1':
        bits.append(1)
    else:
        raise AssertionError
    assert mant[1] == '.'
    for c in mant[2:]:
        d = int(c, 16)
        bits.append((d >> 3) & 1)
        bits.append((d >> 2) & 1)
        bits.append((d >> 1) & 1)
        bits.append((d >> 0) & 1)

    if exp < 0:
        bits_before = []
        bits_after = [0] * -(exp + 1) + bits
    else:
        while len(bits) < exp + 1:
            bits.append(0)
        bits_before = bits[:exp + 1]
        bits_after = bits[exp + 1:]

    while bits_after and bits_after[-1] == 0:
        del bits_after[-1]
    while len(bits_after) % 4 != 0:
        bits_after.append(0)

    before = str(sum(b << i for i, b in enumerate(reversed(bits_before))))
    after = ''
    for i in xrange(0, len(bits_after), 4):
        after += '0123456789abcdef'[(bits_after[i] << 3) +
                                    (bits_after[i + 1] << 2) +
                                    (bits_after[i + 2] << 1) +
                                     bits_after[i + 3]]

    if decimal_digits == 0:
        if after:
            if before == '0':
                return sign + ':' + after
            return sign + before + ':' + after
        if before == '0':
            return '0'  # signless zero
        return sign + before

    if len(before) < decimal_digits:
        before = '0' * (decimal_digits - len(before)) + before
    before0 = before[:-decimal_digits]
    before1 = before[-decimal_digits:]

    if before1 == '0' * decimal_digits:
        if after:
            if not before0:
                return sign + ':' + after
            return sign + before0 + '.' + before1 + ':' + after
        else:
            if not before0:
                return '0'  # signless zero
            return sign + before0
    else:
        if after:
            return sign + before0 + '.' + before1 + ':' + after
        else:
            return sign + before0 + '.' + before1.rstrip('0')

## Convert a hybrid string representation to a floating-point number.
#
# TODO: For efficiency reasons, this should probably be ported to C.

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

    try:
        pos = s.index(':')
    except ValueError:
        after = None
        if not s:
            raise ValueError
    else:
        after = s[pos + 1:]
        s = s[:pos]
        if not after and not s:
            raise ValueError

    if decimal_digits == 0:
        if '.' in s:
            raise ValueError
        before0 = s
        before1 = ''
    else:
        try:
            pos = s.index('.')
        except ValueError:
            if s and after is not None:
                raise ValueError
            before0 = s
            before1 = ''
        else:
            before0 = s[:pos]
            before1 = s[pos + 1:]
            if not before0 and not before1:
                raise ValueError
            if len(before1) < decimal_digits:
                if after is not None:
                    raise ValueError
                before1 = before1 + (decimal_digits - len(before1)) * '0'

        if len(before1) > decimal_digits:
            raise ValueError

    if not after:
        after = '0'

    for c in before0 + before1:
        if c not in '0123456789':
            raise ValueError
    for c in after:
        if c not in '0123456789abcdef':
            raise ValueError

    if not before0:
        before0 = '0'
    if not before1:
        before1 = '0'

    x = float(int(before0) * 10 ** decimal_digits + int(before1)) + \
        float(int(after, 16)) / float(1 << len(after) * 4)

    if not x:
        return 0.  # signless zero
    return sign * x
