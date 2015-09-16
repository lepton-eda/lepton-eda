# xorn.geda - Python library for manipulating gEDA files
#**********************************************************************
#                   _     _                     __   _  _
#   __ _ _ __   ___| |_  | |__   __ _ ___  ___ / /_ | or |
#  / _` | '_ \ / _ \ __| | '_ \ / _` / __|/ _ \ '_ \| or |_
# | (_| | | | |  __/ |_  | |_) | (_| \__ \  __/ (_) |__   _|
#  \__, |_| |_|\___|\__| |_.__/ \__,_|___/\___|\___/   |_|
#  |___/
#
#  created by Alfred Reibenschuh <alfredreibenschuh@gmx.net>,
#  under the "GNU Library General Public License" (see below).
#
#**********************************************************************
# Copyright (C) 2003 Free Software Foundation
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

## \namespace xorn.base64
## Reading and writing base64-encoded data

from gettext import gettext as _

BASE64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
PAD64 = '='
RANK = [
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0x00-0x0f
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0x10-0x1f
    255,255,255,255,255,255,255,255,255,255,255, 62,255,255,255, 63, # 0x20-0x2f
     52, 53, 54, 55, 56, 57, 58, 59, 60, 61,255,255,255,255,255,255, # 0x30-0x3f
    255,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, # 0x40-0x4f
     15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,255,255,255,255,255, # 0x50-0x5f
    255, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, # 0x60-0x6f
     41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,255,255,255,255,255, # 0x70-0x7f
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0x80-0x8f
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0x90-0x9f
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0xa0-0xaf
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0xb0-0xbf
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0xc0-0xcf
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0xd0-0xdf
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0xe0-0xef
    255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, # 0xf0-0xff
]

## Write a binary string to a file in %base64 representation.
#
# If \a columns is not \c None, insert a newline every \a columns
# characters.  This is required by RFC 2045, but some applications
# don't require it.  \a columns must positive and a multiple of \c 4.
#
# If \a delim is not \c None, it is written on a separate line after
# the data.  This argument is provided for symmetry with \ref decode.
#
# \return \c None.

def encode(f, src, columns = 72, delim = None):
    # bulk encoding
    blen = len(src) - len(src) % 3
    ocnt = 0

    for pos in xrange(0, blen, 3):
        # Convert 3 bytes of src to 4 bytes of output
        #
        # output[0] = input[0] 7:2
        # output[1] = input[0] 1:0 input[1] 7:4
        # output[2] = input[1] 3:0 input[2] 7:6
        # output[3] = input[1] 5:0

        i0, i1, i2 = [ord(ch) for ch in src[pos:pos + 3]]

        # Map output to the Base64 alphabet
        f.write(BASE64[i0 >> 2] +
                BASE64[((i0 & 0x03) << 4) + (i1 >> 4)] +
                BASE64[((i1 & 0x0f) << 2) + (i2 >> 6)] +
                BASE64[i2 & 0x3f])

        if columns is not None:
            ocnt += 1
            if ocnt % (columns / 4) == 0 and pos != len(src) - 3:
                f.write('\n')

    # Now worry about padding with remaining 1 or 2 bytes
    if blen != len(src):
        i0 = ord(src[blen])
        if blen == len(src) - 1:
            i1 = 0
        else:
            i1 = ord(src[blen + 1])
        i2 = 0

        f.write(BASE64[i0 >> 2] +
                BASE64[((i0 & 0x03) << 4) + (i1 >> 4)])
        if blen == len(src) - 1:
            f.write(PAD64)
        else:
            f.write(BASE64[((i1 & 0x0f) << 2) + (i2 >> 6)])
        f.write(PAD64)

    if src:
        f.write('\n')

    if delim is not None:
        f.write(delim + '\n')

## Raised when reading invalid or unterminated base64-encoded data.

class DecodingError(Exception):
    pass

## Read a string in %base64 representation from a file.
#
# This function is liberal in what it will accept.  It ignores
# non-base64 symbols.
#
# If \a delim is \c None, read until the end of the file.  If \a delim
# is not \c None, read until a line containing exactly \a delim is
# found.
#
# \return A string containing the decoded data.
#
# \throw DecodingError if reading something that is not valid
#                      base64-encoded data
# \throw DecodingError if the end of the file is hit and \a delim is
#                      not \c None

def decode(f, delim = None):
    ch = 0
    state = 0
    res = 0
    dst = []
    pad = 0

    while True:
      try:
        line = f.next()
      except StopIteration:
        if delim is not None:
          raise DecodingError, _("Unexpected end-of-file")
        break

      if delim is not None and line == delim + '\n':
        break

      for ch in line:
        if ch == PAD64:
            pad += 1
            continue
        pos = RANK[ord(ch)]
        if pos == 255:
            # Skip any non-base64 anywhere
            continue
        if pad != 0:
            raise DecodingError

        if state == 0:
            dst += [pos << 2]
            state = 1
        elif state == 1:
            dst[-1] |= pos >> 4
            res = (pos & 0x0f) << 4
            state = 2
        elif state == 2:
            dst += [res | (pos >> 2)]
            res = (pos & 0x03) << 6
            state = 3
        elif state == 3:
            dst += [res | pos]
            state = 0

    # We are done decoding Base-64 chars.  Let's see if we ended
    # on a byte boundary, and/or with erroneous trailing characters.
    if pad != 0:
        # We got a pad char.
        if state == 0:
            # Invalid = in first position
            raise DecodingError
        elif state == 1:
            # Invalid = in second position
            raise DecodingError
        elif state == 2:
            # Valid, means one byte of info
            # Make sure there is another trailing = sign.
            if pad != 2:
                raise DecodingError
        elif state == 3:
            # Valid, means two bytes of info
            # We know this char is an =.  Is there anything but
            # whitespace after it?
            if pad != 1:
                raise DecodingError
        if state == 2 or state == 3:
            # Now make sure for cases 2 and 3 that the "extra"
            # bits that slopped past the last full byte were
            # zeros.  If we don't check them, they become a
            # subliminal channel.
            if res != 0:
                raise DecodingError
    else:
        # We ended by seeing the end of the string.  Make sure we
        # have no partial bytes lying around.
        if state != 0:
            raise DecodingError
    return ''.join(chr(b) for b in dst)
