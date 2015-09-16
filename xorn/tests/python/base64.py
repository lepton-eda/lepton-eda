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

import StringIO
import xorn.base64

def throws(fun, *args, **kwds):
    try:
        fun(*args, **kwds)
    except Exception as e:
        return type(e)

data = \
    '\x3f\x98\xd2\xd0\xad\x4b\xa8\xbf\x5b\x58\x7b\xcd\x40\x6c\x27\xba' \
    '\x80\xc1\x29\x86\x86\x26\xc4\x0b\xc5\x29\x32\x8a\x29\x36\xfd\xa9' \
    '\x1b\x0b\x1b\xfd\x18\xfb\xbb\xc5\x45\xb3\x66\x54\x3c\x2b\x9e\x9b' \
    '\xff\x5d\xb7\x64\x3c\x92\xf2\xdb\x8c\xb0\x02\x96\x3f\x51\xd0\x35' \
    '\x6d\x70\x2c\xb9\x8c\x0b\x89\xc6\xbb\xcd\xf9\x1d\xc6\x27\x33\x58' \
    '\xd5\x84\x32\x11\xe5\xd3\xd2\x77\x36\x17\xc3\xa3\xa8\xcb\x0e\x16' \
    '\x4d\x1d\xc9\x22\x61\xed\xb0\xb2\xbb\xfe\xd7\x03\xae\x03\xa8\x37' \
    '\x11\xda\xc3\x57\x11\xf1\x35\x01'

lines = [''.join([
    'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQLxSkyiik2\n',
    '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS8tuMsAKW\n',
    'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR5dPSdzYX\n',
    'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHaw1cR8TUB\n'
][:i]) for i in xrange(5)]

expected = [
    lines[0],
    lines[0] + 'Pw==\n',
    lines[0] + 'P5g=\n',
    lines[0] + 'P5jS\n',
    lines[0] + 'P5jS0A==\n',
    lines[0] + 'P5jS0K0=\n',
    lines[0] + 'P5jS0K1L\n',
    lines[0] + 'P5jS0K1LqA==\n',
    lines[0] + 'P5jS0K1LqL8=\n',
    lines[0] + 'P5jS0K1LqL9b\n',
    lines[0] + 'P5jS0K1LqL9bWA==\n',
    lines[0] + 'P5jS0K1LqL9bWHs=\n',
    lines[0] + 'P5jS0K1LqL9bWHvN\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQA==\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGw=\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwn\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnug==\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoA=\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDB\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKQ==\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYY=\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaG\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJg==\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQ=\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQL\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQLxQ==\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQLxSk=\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQLxSky\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQLxSkyig==\n',
    lines[0] + 'P5jS0K1LqL9bWHvNQGwnuoDBKYaGJsQLxSkyiik=\n',
    lines[1],
    lines[1] + '/Q==\n',
    lines[1] + '/ak=\n',
    lines[1] + '/akb\n',
    lines[1] + '/akbCw==\n',
    lines[1] + '/akbCxs=\n',
    lines[1] + '/akbCxv9\n',
    lines[1] + '/akbCxv9GA==\n',
    lines[1] + '/akbCxv9GPs=\n',
    lines[1] + '/akbCxv9GPu7\n',
    lines[1] + '/akbCxv9GPu7xQ==\n',
    lines[1] + '/akbCxv9GPu7xUU=\n',
    lines[1] + '/akbCxv9GPu7xUWz\n',
    lines[1] + '/akbCxv9GPu7xUWzZg==\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ=\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8Kw==\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K54=\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/w==\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/10=\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZA==\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDw=\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS8g==\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS8ts=\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS8tuM\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS8tuMsA==\n',
    lines[1] + '/akbCxv9GPu7xUWzZlQ8K56b/123ZDyS8tuMsAI=\n',
    lines[2],
    lines[2] + 'Pw==\n',
    lines[2] + 'P1E=\n',
    lines[2] + 'P1HQ\n',
    lines[2] + 'P1HQNQ==\n',
    lines[2] + 'P1HQNW0=\n',
    lines[2] + 'P1HQNW1w\n',
    lines[2] + 'P1HQNW1wLA==\n',
    lines[2] + 'P1HQNW1wLLk=\n',
    lines[2] + 'P1HQNW1wLLmM\n',
    lines[2] + 'P1HQNW1wLLmMCw==\n',
    lines[2] + 'P1HQNW1wLLmMC4k=\n',
    lines[2] + 'P1HQNW1wLLmMC4nG\n',
    lines[2] + 'P1HQNW1wLLmMC4nGuw==\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu80=\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HQ==\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcY=\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYn\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnMw==\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1g=\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jV\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhA==\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDI=\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR5Q==\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR5dM=\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR5dPS\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR5dPSdw==\n',
    lines[2] + 'P1HQNW1wLLmMC4nGu835HcYnM1jVhDIR5dPSdzY=\n',
    lines[3],
    lines[3] + 'ww==\n',
    lines[3] + 'w6M=\n',
    lines[3] + 'w6Oo\n',
    lines[3] + 'w6Ooyw==\n',
    lines[3] + 'w6Ooyw4=\n',
    lines[3] + 'w6Ooyw4W\n',
    lines[3] + 'w6Ooyw4WTQ==\n',
    lines[3] + 'w6Ooyw4WTR0=\n',
    lines[3] + 'w6Ooyw4WTR3J\n',
    lines[3] + 'w6Ooyw4WTR3JIg==\n',
    lines[3] + 'w6Ooyw4WTR3JImE=\n',
    lines[3] + 'w6Ooyw4WTR3JImHt\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsA==\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLI=\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/g==\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tc=\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcD\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrg==\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgM=\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOo\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNw==\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxE=\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHa\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHaww==\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHaw1c=\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHaw1cR\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHaw1cR8Q==\n',
    lines[3] + 'w6Ooyw4WTR3JImHtsLK7/tcDrgOoNxHaw1cR8TU=\n',
    lines[4]
]

for count in xrange(len(expected)):
    # encode

    s = StringIO.StringIO()
    xorn.base64.encode(s, data[:count], columns = 40)
    assert s.getvalue() == expected[count]

    s = StringIO.StringIO()
    xorn.base64.encode(s, data[:count], columns = None)
    if count == 0:
        assert s.getvalue() == ''
    else:
        assert s.getvalue() == expected[count].replace('\n', '') + '\n'

    s = StringIO.StringIO()
    xorn.base64.encode(s, data[:count], columns = 40, delim = '@@@')
    assert s.getvalue() == expected[count] + '@@@\n'

    s = StringIO.StringIO()
    xorn.base64.encode(s, data[:count], columns = None, delim = '@@@')
    if count == 0:
        assert s.getvalue() == '@@@\n'
    else:
        assert s.getvalue() == expected[count].replace('\n', '') + '\n@@@\n'

    # decode

    s = StringIO.StringIO(expected[count])
    assert xorn.base64.decode(s) == data[:count]
    assert throws(s.next) == StopIteration

    s = StringIO.StringIO(expected[count])
    assert throws(xorn.base64.decode, s, delim = '@@@') \
        == xorn.base64.DecodingError

    s = StringIO.StringIO(expected[count] + '@@@\n%%%\n')
    assert xorn.base64.decode(s, delim = '@@@') == data[:count]
    assert s.next() == '%%%\n'
    assert throws(s.next) == StopIteration

    s = StringIO.StringIO(expected[count] + '=')
    assert throws(xorn.base64.decode, s) == xorn.base64.DecodingError
