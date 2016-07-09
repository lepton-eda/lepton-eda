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

import cStringIO
import xorn.geda.fileformat
import xorn.geda.read

class TestLog:
    def __init__(self, messages):
        self.messages = messages
        self.lineno = 0

    def error(self, message):
        message = '%d: error: %s' % (self.lineno + 1, message)
        print message
        assert self.messages[0] == message
        del self.messages[0]

    def warn(self, message):
        message = '%d: warning: %s' % (self.lineno + 1, message)
        print message
        assert self.messages[0] == message
        del self.messages[0]

def assert_read(data, messages, **kwds):
    log = TestLog(messages)
    try:
        rev = xorn.geda.read.read_file(
            cStringIO.StringIO(data), '<test data>',
            xorn.geda.fileformat.FORMAT_SCH, log, **kwds)
    except xorn.geda.read.ParseError:
        pass
    assert not log.messages

### general ###

assert_read("""
""", [
    '1: error: read garbage',
    '2: error: file is lacking pin orientation information'
])
assert_read("""v
""", [
    '1: error: failed to parse version string',
    '2: error: file is lacking pin orientation information'
])

### arc objects ###

assert_read("""v 20150930 2
A 0 0 100 0 90 3 0 0 0 -1 -1
""", [
])

assert_read("""v 20150930 2
A
""", [
    '2: error: failed to parse arc object'
])

assert_read("""v 20150930 2
A 0 0 0 0 90 3 0 0 0 -1 -1
""", [
    '2: warning: arc has radius zero'
])

assert_read("""v 20150930 2
A 0 0 -100 0 90 3 0 0 0 -1 -1
""", [
    '2: warning: arc has negative radius (-100), setting to 0'
])

assert_read("""v 20150930 2
A 0 0 100 0 90 -1 0 0 0 -1 -1
""", [
    '2: warning: arc has invalid color (-1), setting to 3'
])

### box objects ###

assert_read("""v 20150930 2
B 0 0 100 100 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
])

assert_read("""v 20150930 2
B
""", [
    '2: error: failed to parse box object'
])

assert_read("""v 20150930 2
B 0 0 0 100 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
    '2: warning: box has width/height zero'
])

assert_read("""v 20150930 2
B 0 0 100 0 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
    '2: warning: box has width/height zero'
])

assert_read("""v 20150930 2
B 0 0 100 100 -1 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
    '2: warning: box has invalid color (-1), setting to 3'
])

### circle objects ###

assert_read("""v 20150930 2
V 0 0 100 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
])

assert_read("""v 20150930 2
V
""", [
    '2: error: failed to parse circle object'
])

assert_read("""v 20150930 2
V 0 0 0 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
    '2: warning: circle has radius zero'
])

assert_read("""v 20150930 2
V 0 0 -100 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
    '2: warning: circle has negative radius (-100), setting to 0'
])

assert_read("""v 20150930 2
V 0 0 100 -1 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
""", [
    '2: warning: circle has invalid color (-1), setting to 3'
])

### component objects ###

assert_read("""v 20150930 2
C 0 0 1 0 0 sym.sym
""", [
])

assert_read("""v 20150930 2
C
""", [
    '2: error: failed to parse complex object'
])

assert_read("""v 20150930 2
C 0 0 1 1 0 sym.sym
""", [
    '2: warning: component has invalid angle (1), setting to 0'
])

assert_read("""v 20150930 2
C 0 0 1 0 2 sym.sym
""", [
    '2: warning: component has invalid mirror flag (2), setting to 0'
])

assert_read("""v 20150930 2
C 0 0 1 0 0 missing.sym
""", [
    '2: error: symbol "missing.sym" not found in library'
], load_symbols = True)

### line objects ###

assert_read("""v 20150930 2
L 0 0 100 100 3 0 0 0 -1 -1
""", [
])

assert_read("""v 20150930 2
L
""", [
    '2: error: failed to parse line object'
])

assert_read("""v 20150930 2
L 0 0 0 0 3 0 0 0 -1 -1
""", [
    '2: warning: line has length zero'
])

assert_read("""v 20150930 2
L 0 0 100 100 -1 0 0 0 -1 -1
""", [
    '2: warning: line has invalid color (-1), setting to 3'
])

### net objects ###

assert_read("""v 20150930 2
N 0 0 100 100 4
""", [
])

assert_read("""v 20150930 2
N
""", [
    '2: error: failed to parse net object'
])

assert_read("""v 20150930 2
N 0 0 0 0 4
""", [
    '2: warning: net has length zero'
])

assert_read("""v 20150930 2
N 0 0 100 100 -1
""", [
    '2: warning: net has invalid color (-1), setting to 3'
])

assert_read("""v 20150930 2
P 0 0 100 100 1 0 0
""", [
])

assert_read("""v 20150930 2
P
""", [
    '2: error: failed to parse pin object'
])

assert_read("""v 20150930 2
P 0 0 100 100 1 0 2
""", [
    '2: warning: pin has invalid whichend (2), setting to first end'
])

assert_read("""v 20150930 2
P 0 0 100 100 -1 0 0
""", [
    '2: warning: pin has invalid color (-1), setting to 3'
])

assert_read("""v 20150930 2
P 0 0 100 100 1 2 0
""", [
    '2: warning: pin has invalid type (2), setting to 0'
])

assert_read("""v 20150930 2
U 0 0 100 100 10 0
""", [
])

assert_read("""v 20150930 2
U
""", [
    '2: error: failed to parse bus object'
])

assert_read("""v 20150930 2
U 0 0 0 0 10 0
""", [
    '2: warning: bus has length zero'
])

assert_read("""v 20150930 2
U 0 0 100 100 -1 0
""", [
    '2: warning: bus has invalid color (-1), setting to 3'
])

assert_read("""v 20150930 2
U 0 0 100 100 10 2
""", [
    '2: warning: bus has invalid ripper direction (2)'
])

### path objects ###

assert_read("""v 20150930 2
H 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1 1
z
""", [
])

assert_read("""v 20150930 2
H
""", [
    '2: error: failed to parse path object'
])

assert_read("""v 20150930 2
H -1 0 0 0 -1 -1 0 -1 -1 -1 -1 -1 1
z
""", [
    '2: warning: path has invalid color (-1), setting to 3'
])

assert_read("""v 20150930 2
H 3 0 0 0 -1 -1 0 -1 -1 -1 -1 -1 1
""", [
    '3: error: unexpected end of file after 0 lines while reading path'
])

### picture objects ###

assert_read("""v 20150930 2
G 0 0 100 100 0 0 0
pixmap.png
""", [
])

assert_read("""v 20150930 2
G 0 0 100 100 0 0 1
pixmap.png
.
""", [
])

assert_read("""v 20150930 2
G
pixmap.png
""", [
    '2: error: failed to parse picture definition',
    '3: error: read garbage'
])

assert_read("""v 20150930 2
G 0 0 0 100 0 0 0
pixmap.png
""", [
    '2: warning: picture has width/height zero'
])

assert_read("""v 20150930 2
G 0 0 100 0 0 0 0
pixmap.png
""", [
    '2: warning: picture has width/height zero'
])

assert_read("""v 20150930 2
G 0 0 100 100 1 0 0
pixmap.png
""", [
    '2: warning: picture has unsupported angle (1), setting to 0'
])

assert_read("""v 20150930 2
G 0 0 100 100 0 2 0
pixmap.png
""", [
    '2: warning: picture has wrong \'mirrored\' parameter (2), setting to 0'
])

assert_read("""v 20150930 2
G 0 0 100 100 0 0 0
""", [
    '3: error: unexpected end of file while reading picture file name'
])

assert_read("""v 20150930 2
G 0 0 100 100 0 0 0

""", [
    '3: warning: image has no filename'
])

assert_read("""v 20150930 2
G 0 0 100 100 0 0 2
pixmap.png
""", [
    '3: warning: picture has wrong \'embedded\' parameter (2), setting to not embedded'
])

assert_read("""v 20150930 2
G 0 0 100 100 0 0 1
pixmap.png
!
""", [
    '5: error: failed to load image from embedded data: Unexpected end-of-file'
])

### text objects ###

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 1
blah
""", [
])

assert_read("""v 20150930 2
T
blah
""", [
    '2: error: failed to parse text object',
    '3: error: read garbage'
])

assert_read("""v 20150930 2
T 0 0 -1 12 1 0 0 0 1
blah
""", [
    '2: warning: text has invalid color (-1), setting to 3'
])

assert_read("""v 20150930 2
T 0 0 9 0 1 0 0 0 1
blah
""", [
    '2: warning: text has size zero'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 1 0 1
blah
""", [
    '2: warning: text has unsupported angle (1), setting to 0'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 9 1
blah
""", [
    '2: warning: text has unsupported alignment (9), setting to LOWER_LEFT'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 0
""", [
    '2: error: text has invalid number of lines (0)'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 1
""", [
    '3: error: unexpected end of file after 0 lines of text'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 1
A\\B
""", [
    '3: warning: stray backslash character(s)'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 1
A\\\\B
""", [
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 1
A\\_B\\_C\\_D
""", [
    '3: warning: mismatched overbar markers'
])

assert_read("""v 20150930 2
T 0 0 9 12 1 0 0 0 1
A\\_B\\_C\\_D\\_E
""", [
])

### attribute lists ###

assert_read("""v 20150930 2
{
}
""", [
    '2: error: unexpected attribute list start marker',
    '3: error: unexpected attribute list end marker'
])
assert_read("""v 20150930 2
L 0 0 1 1 3 0 0 0 -1 -1
{
}
""", [
    '3: error: can\'t attach attributes to this object type',
    '4: error: unexpected attribute list end marker'
])
assert_read("""v 20150930 2
N 0 0 1 1 4
{
""", [
    '4: error: unterminated attribute list'
])
assert_read("""v 20150930 2
N 0 0 1 1 4
{
L 0 0 1 1 3 0 0 0 -1 -1
}
""", [
    '4: error: tried to attach a non-text item as an attribute'
])
assert_read("""v 20150930 2
}
""", [
    '2: error: unexpected attribute list end marker'
])

### embedded symbols ###

assert_read("""v 20150930 2
[
]
""", [
    '2: error: unexpected embedded symbol start marker',
    '3: error: unexpected embedded symbol end marker'
])
assert_read("""v 20150930 2
N 0 0 1 1 4
[
]
""", [
    '3: error: embedded symbol start marker following non-component object',
    '4: error: unexpected embedded symbol end marker'
])
assert_read("""v 20150930 2
C 0 0 1 0 0 sym.sym
[
]
""", [
    '3: error: embedded symbol start marker following component with '
              'non-embedded symbol',
    '4: error: unexpected embedded symbol end marker'
])
assert_read("""v 20150930 2
C 0 0 1 0 0 EMBEDDEDsym.sym
""", [
    '3: error: embedded symbol is missing'
])
assert_read("""v 20150930 2
C 0 0 1 0 0 EMBEDDEDsym.sym
[
]
[
]
""", [
    '5: error: embedded symbol start marker following embedded symbol',
    '6: error: unexpected embedded symbol end marker'
])
assert_read("""v 20150930 2
]
""", [
    '2: error: unexpected embedded symbol end marker'
])
