# xorn.geda - Python library for manipulating gEDA files
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

## \namespace xorn.geda.read
## Reading gEDA schematic/symbol files.
#
# The gEDA file format is a space separated list of characters and
# numbers in plain ASCII.  The meaning of each item is described in
# the file format documentation which can be found in \c
# doc/geda/file_format_spec or
# [here](http://wiki.geda-project.org/geda:file_format_spec).

import codecs
from gettext import gettext as _
import xorn.storage
import xorn.proxy
import xorn.base64
from xorn.geda.fileformat import *

## Raised when parsing a malformed file.

class ParseError(Exception):
    pass

## Describes the properties of a gEDA schematic/symbol file format version.
#
# The file format version of a gEDA file is described by a line of the form
#
# \verbatim
# v release_version fileformat_version
# \endverbatim
#
# where \a release_version is an eight-digit number representing a
# date and \c fileformat_version is a low integer (usually \c 1 or \c
# 2).  These represent the file format features that are used:
#
# Feature                            | Release version | File format version |
# -------------------------------------------|------------------------|------|
# Text alignment attribute                   | >= February 20th, 2000 |      |
# Line and fill attributes                   | > September 4th, 2000  |      |
# Bus pins, whichend and ripperdir attribute | > August 25th, 2002    |      |
# Text objects with multiple lines           | (October 2003)         | >= 1 |
# Path objects                               | (November 2008)        | >= 2 |
#
# Depending on the version of the file format, the file is parsed
# differently.  The unspecified parameters in the older file formats
# are set to default values.
#
# In older libgeda file format versions there was no information about
# the active end of pins.

class FileFormat:
    ## Create a new instance from a version number pair and derive its
    ## properties.
    def __init__(self, release_ver, fileformat_ver):
        ## libgeda release version number
        self.release_ver = release_ver
        ## libgeda file format version number
        self.fileformat_ver = fileformat_ver

        ## Is the text alignment attribute supported?
        self.supports_text_alignment = release_ver >= VERSION_20000220
        # yes, above greater or equal (not just greater) is correct.
        # The format change occurred in 20000220

        ## Are line and fill attributes supported?
        self.supports_linefill_attributes = release_ver > VERSION_20000704

        ## Are bus pins, whichend and ripperdir attributes supported?
        self.enhanced_pinbus_format = release_ver > VERSION_20020825

        ## Can text objects have multiple lines?
        self.supports_multiline_text = fileformat_ver >= 1

class EmbeddedSymbol:
    # basename: The filename of the component

    # The just basename is the filename of the component. This
    # filename is not the full path.

    def __init__(self, basename):
        self.basename = basename
        self.prim_objs = None

class EmbeddedPixmap:
    # In gEDA, the filename is not used if the picture is embedded.

    # embedded: Embedded or link to the picture file.
    # filename: Path and filename of a not embedded picture.
    # picture_data: Serialized picture

    def __init__(self, file_content, filename):
        self.file_content = file_content
        self.filename = filename

## Helper function for \ref sscanf.

def parse_token(s, fmt, message):
    if fmt == '%c':
        if len(s) != 1:
            raise ParseError, message
        return s

    if fmt == '%d':
        try:
            return int(s)
        except ValueError:
            raise ParseError, message

    if fmt == '%u':
        try:
            val = int(s)
        except ValueError:
            raise ParseError, message
        if val < 0:
            raise ParseError, message
        return val

    if fmt == '%s':
        return s

    raise ValueError, "Invalid format token: '%s'" % fmt

## Parse a string of space-separated values.
#
# This is a mock-up version of the standard \c sscanf(3).  The format
# string must consist of zero or more tokens separated by a space,
# optionally followed by a newline character.  The format string must
# exactly match this pattern.  Only the tokens \c %%c, \c %%d, \c %%s,
# and \c %%u are allowed.
#
# \throw ParseError if the string does not match the format
# \throw ValueError if an invalid format token is passed
#
# \return a tuple containing the parsed values

def sscanf(s, fmt, message):
    while fmt.endswith('\n'):
        if not s.endswith('\n'):
            raise ParseError, message
        fmt = fmt[:-1]
        s = s[:-1]

    if s.endswith('\n'):
        raise ParseError, message

    s = s.rstrip(' ') # whatever??

    stok = s.split(' ')
    fmttok = fmt.split(' ')

    if len(stok) != len(fmttok):
        raise ParseError, message

    return [parse_token(st, ft, message) for (st, ft) in zip(stok, fmttok)]

## Read a file in libgeda format.
#
# See \ref read_file for a description of the keyword arguments.
#
# \return The new revision.
#
# \throw ParseError if the file is not a valid gEDA schematic/symbol file

def read(filename, **kwds):
    f = codecs.open(filename, encoding = 'utf-8')
    try:
        return read_file(f, filename, **kwds)
    finally:
        f.close()

## Read a file in libgeda format from a file object.
#
# \param [in] f                   A file-like object from which to read.
# \param [in] name                The file name displayed in warning
#                                 and error messages.
# \param [in] override_net_color  Reset the color of nets do default?
# \param [in] override_bus_color  Reset the color of buses do default?
# \param [in] override_pin_color  Reset the color of pins do default?
# \param [in] force_boundingbox   <i>currently unused</i>
#
# \return The new revision.
#
# \throw ParseError if the file is not a valid gEDA schematic/symbol file

def read_file(f, name, override_net_color = None,
                       override_bus_color = None,
                       override_pin_color = None,
                       force_boundingbox = False):
    # "Stack" of outer contexts for embedded components
    object_lists_save = []

    # Last read object.  Attributes and embedded components attach to this.
    ob = None

    # This is where read objects end up.  Will be swapped for embedded comps.
    rev = xorn.storage.Revision()

    # origin for embedded components
    origin = 0, 0

    format = FileFormat(0, 0)  # no file format definition at all

    for line in f:
        if not line:
            continue
        objtype = line[0]

        if objtype == OBJ_LINE:
            ob = rev.add_object(read_line(line, origin, format))
        elif objtype == OBJ_NET:
            ob = rev.add_object(read_net(line, origin, format))
            if override_net_color is not None:
                ob.color = override_net_color
        elif objtype == OBJ_BUS:
            ob = rev.add_object(read_bus(line, origin, format))
            if override_bus_color is not None:
                ob.color = override_bus_color
        elif objtype == OBJ_BOX:
            ob = rev.add_object(read_box(line, origin, format))
        elif objtype == OBJ_PICTURE:
            ob = rev.add_object(read_picture(line, f, origin, format))
        elif objtype == OBJ_CIRCLE:
            ob = rev.add_object(read_circle(line, origin, format))
        elif objtype == OBJ_COMPLEX:
            ob = rev.add_object(read_complex(line, origin, format))
        elif objtype == OBJ_TEXT:
            ob = rev.add_object(read_text(line, f, origin, format))
        elif objtype == OBJ_PATH:
            ob = rev.add_object(read_path(line, f, origin, format))
        elif objtype == OBJ_PIN:
            ob = rev.add_object(read_pin(line, origin, format))
            if override_pin_color is not None:
                color = override_pin_color
        elif objtype == OBJ_ARC:
            ob = rev.add_object(read_arc(line, origin, format))
        elif objtype == STARTATTACH_ATTR:
            if ob is None:
                raise ParseError, \
                    _("Read unexpected attach symbol start marker in [%s] :\n"
                      ">>\n%s<<\n") % (name, line)

            while True:
                try:
                    line = f.next()
                except StopIteration:
                    raise ParseError, _("Unterminated attribute list")

                if not line:
                    continue

                if line[0] == ENDATTACH_ATTR:
                    break

                if line[0] != OBJ_TEXT:
                    raise ParseError, \
                        _("Tried to attach a non-text item as an attribute")

                attrib = read_text(line, f, origin, format)
                rev.relocate_object(rev.add_object(attrib), ob, None)

            ob = None
        elif objtype == START_EMBEDDED:
            if ob is None:
                raise ParseError, \
                    _("Read unexpected embedded symbol start marker in [%s] :\n"
                      ">>\n%s<<\n") % (name, line)
            component_data = rev.get_object_data(ob)
            if type(component_data) != xorn.storage.Component:
                raise ParseError, \
                    _("Read unexpected embedded symbol start marker in [%s] :\n"
                      ">>\n%s<<\n") % (name, line)
            if component_data.symbol.prim_objs is not None:
                raise ParseError
            object_lists_save.append((rev, ob, origin))
            rev = xorn.storage.Revision()
            component_data.symbol.prim_objs = rev
            origin = origin[0] + component_data.x, origin[1] + component_data.y
        elif objtype == END_EMBEDDED:
            if not object_lists_save:
                raise ParseError, \
                    _("Read unexpected embedded symbol end marker in [%s] :\n"
                      ">>\n%s<<\n") % (name, line)
            rev, ob, origin = object_lists_save.pop()
        elif objtype == ENDATTACH_ATTR:
            raise ParseError, "Unexpected '}'"
        elif objtype == INFO_FONT:
            # NOP
            pass
        elif objtype == COMMENT:
            # do nothing
            pass
        elif objtype == VERSION_CHAR:
            try:
                objtype, release_ver, fileformat_ver = \
                    sscanf(line, "%c %u %u\n", None)
            except ParseError:
                objtype, release_ver = \
                    sscanf(line, "%c %u\n",
                           _("Failed to parse version from buffer."))
                fileformat_ver = 0

            assert objtype == VERSION_CHAR

            # 20030921 was the last version which did not have a fileformat
            # version.
            if release_ver <= VERSION_20030921:
                fileformat_ver = 0

            if fileformat_ver == 0:
                print _("Read an old format sym/sch file!\n"
                        "Please run g[sym|sch]update on:\n[%s]\n") % name

            format = FileFormat(release_ver, fileformat_ver)
        else:
            raise ParseError, _("Read garbage in [%s] :\n"
                                ">>\n%s<<\n") % (name, line)

    if not format.enhanced_pinbus_format:
        pin_update_whichend(rev, force_boundingbox)

    return xorn.proxy.RevisionProxy(rev)

## Guess the orientation of pins.
#
# Calculates the bounding box of all pins in the revision.  The end of
# a pin that is closer to the boundary of the box is set as the active
# end.
#
# \return \c None.
#
# \warning This function is not implemented.  See Xorn bug #148.

def pin_update_whichend(rev, force_boundingbox):
    raise NotImplementedError


## Read a circle object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a circle object

def read_circle(buf, (origin_x, origin_y), format):
    if not format.supports_linefill_attributes:
        type, x1, y1, radius, color = sscanf(
            buf, "%c %d %d %d %d\n", _("Failed to parse circle object"))

        circle_width = 0
        circle_end = 0
        circle_type = 0
        circle_length = -1
        circle_space = -1

        circle_fill = 0
        fill_width = 0
        angle1 = -1
        pitch1 = -1
        angle2 = -1
        pitch2 = -1
    else:
        type, x1, y1, radius, color, \
        circle_width, circle_end, circle_type, circle_length, circle_space, \
        circle_fill, fill_width, angle1, pitch1, angle2, pitch2 = sscanf(
            buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
            _("Failed to parse circle object"))

    if type != OBJ_CIRCLE:
        raise ValueError

    if radius <= 0:
        print _("Found a zero or negative radius circle "
                "[ %c %d %d %d %d ]\n") % (type, x1, y1, radius, color)
        print _("Setting radius to 0\n")
        radius = 0

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    return xorn.storage.Circle(
        x = x1 - origin_x,
        y = y1 - origin_y,
        radius = radius,
        color = color,
        line = xorn.storage.LineAttr(
            width = circle_width,
            cap_style = circle_end,
            dash_style = circle_type,
            dash_length = circle_length,
            dash_space = circle_space),
        fill = xorn.storage.FillAttr(
            type = circle_fill,
            width = fill_width,
            angle0 = angle1,
            pitch0 = pitch1,
            angle1 = angle2,
            pitch1 = pitch2))

## Read an arc object from a string in gEDA format.
#
# A negative or null radius is not allowed.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a arc object

def read_arc(buf, (origin_x, origin_y), format):
    if not format.supports_linefill_attributes:
        type, x1, y1, radius, start_angle, end_angle, color = sscanf(
            buf, "%c %d %d %d %d %d %d\n", _("Failed to parse arc object"))

        arc_width = 0
        arc_end = 0
        arc_type = 0
        arc_space = -1
        arc_length = -1
    else:
        type, x1, y1, radius, start_angle, end_angle, color, \
        arc_width, arc_end, arc_type, arc_length, arc_space = sscanf(
            buf, "%c %d %d %d %d %d %d %d %d %d %d %d\n",
            _("Failed to parse arc object"))

    if type != OBJ_ARC:
        raise ValueError

    if radius <= 0:
        print _("Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n") % (
            type, x1, y1, radius, start_angle, end_angle, color)
        radius = 0

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    return xorn.storage.Arc(
        x = x1 - origin_x,
        y = y1 - origin_y,
        radius = radius,
        startangle = start_angle,
        sweepangle = end_angle,
        color = color,
        line = xorn.storage.LineAttr(
            width = arc_width,
            cap_style = arc_end,
            dash_style = arc_type,
            dash_length = arc_length,
            dash_space = arc_space))

## Read a box object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a box object

def read_box(buf, (origin_x, origin_y), format):
    if not format.supports_linefill_attributes:
        type, x1, y1, width, height, color = sscanf(
            buf, "%c %d %d %d %d %d\n", _("Failed to parse box object"))

        box_width = 0
        box_end = 0
        box_type = 0
        box_length = -1
        box_space = -1

        box_filling = 0
        fill_width = 0
        angle1 = -1
        pitch1 = -1
        angle2 = -1
        pitch2 = -1
    else:
        type, x1, y1, width, height, color, \
        box_width, box_end, box_type, box_length, box_space, \
        box_filling, fill_width, angle1, pitch1, angle2, pitch2 = sscanf(
            buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
            _("Failed to parse box object"))

    if type != OBJ_BOX:
        raise ValueError

    if width == 0 or height == 0:
        print _("Found a zero width/height box [ %c %d %d %d %d %d ]\n") % (
            type, x1, y1, width, height, color)

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    # In libgeda, a box is internally represented by its lower right
    # and upper left corner, whereas in the file format, it is
    # described as its lower left corner and its width and height.
    #
    # We don't care and just use the file format representation.

    return xorn.storage.Box(
        x = x1 - origin_x,
        y = y1 - origin_y,
        width = width,
        height = height,
        color = color,
        line = xorn.storage.LineAttr(
            width = box_width,
            cap_style = box_end,
            dash_style = box_type,
            dash_length = box_length,
            dash_space = box_space),
        fill = xorn.storage.FillAttr(
            type = box_filling,
            width = fill_width,
            angle0 = angle1,
            pitch0 = pitch1,
            angle1 = angle2,
            pitch1 = pitch2))

## Read a bus object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a bus object

def read_bus(buf, (origin_x, origin_y), format):
    if not format.enhanced_pinbus_format:
        type, x1, y1, x2, y2, color = sscanf(
            buf, "%c %d %d %d %d %d\n", _("Failed to parse bus object"))
        ripper_dir = 0
    else:
        type, x1, y1, x2, y2, color, ripper_dir = sscanf(
            buf, "%c %d %d %d %d %d %d\n", _("Failed to parse bus object"))

    if type != OBJ_BUS:
        raise ValueError

    if x1 == x2 and y1 == y2:
        print _("Found a zero length bus [ %c %d %d %d %d %d ]\n") % (
            type, x1, y1, x2, y2, color)

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    if ripper_dir < -1 or ripper_dir > 1:
        print _("Found an invalid bus ripper direction [ %s ]\n") % buf
        print _("Resetting direction to neutral (no direction)\n")
        ripper_dir = 0

    return xorn.storage.Net(
        x = x1 - origin_x,
        y = y1 - origin_y,
        width = x2 - x1,
        height = y2 - y1,
        color = color,
        is_bus = True,
        is_pin = False,
        is_inverted = False)

## Read a component object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a component object

def read_complex(buf, (origin_x, origin_y), format):
    type, x1, y1, selectable, angle, mirror, basename = sscanf(
        buf, "%c %d %d %d %d %d %s\n", _("Failed to parse complex object"))

    if type != OBJ_COMPLEX:
        raise ValueError

    if angle not in [0, 90, 180, 270]:
        print _("Found a component with an invalid rotation "
                "[ %c %d %d %d %d %d %s ]\n") % (
            type, x1, y1, selectable, angle, mirror, basename)
        print _("Setting angle to 0\n")
        angle = 0

    if mirror != 0 and mirror != 1:
        print _("Found a component with an invalid mirror flag "
                "[ %c %d %d %d %d %d %s ]\n") % (
            type, x1, y1, selectable, angle, mirror, basename)
        print _("Setting mirror to 0\n")
        mirror = 0

    # color = DEFAULT_COLOR

    if basename.startswith('EMBEDDED'):
        symbol = EmbeddedSymbol(basename = basename[8:])
    else:
        symbol = basename
        #clib = s_clib_get_symbol_by_name(basename)
        ## Delete or hide attributes eligible for promotion inside the complex
        #o_complex_remove_promotable_attribs(new_obj)

    return xorn.storage.Component(
        x = x1 - origin_x,
        y = y1 - origin_y,
        selectable = selectable,
        angle = angle,
        mirror = mirror,
        symbol = symbol)

## Read a line object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a line object

def read_line(buf, (origin_x, origin_y), format):
    if not format.supports_linefill_attributes:
        type, x1, y1, x2, y2, color = sscanf(
            buf, "%c %d %d %d %d %d\n", _("Failed to parse line object"))

        line_width = 0
        line_end = 0
        line_type = 0
        line_length = -1
        line_space = -1
    else:
        type, x1, y1, x2, y2, color, \
        line_width, line_end, line_type, line_length, line_space = sscanf(
            buf, "%c %d %d %d %d %d %d %d %d %d %d\n",
            _("Failed to parse line object"))

    if type != OBJ_LINE:
        raise ValueError

    # Null length line are not allowed.  If such a line is detected, a
    # message is issued.
    if x1 == x2 and y1 == y2:
        print _("Found a zero length line [ %c %d %d %d %d %d ]\n") % (
            type, x1, y1, x2, y2, color)

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    return xorn.storage.Line(
        x = x1 - origin_x,
        y = y1 - origin_y,
        width = x2 - x1,
        height = y2 - y1,
        color = color,
        line = xorn.storage.LineAttr(
            width = line_width,
            cap_style = line_end,
            dash_style = line_type,
            dash_length = line_length,
            dash_space = line_space))

## Read a net object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a net object

def read_net(buf, (origin_x, origin_y), format):
    type, x1, y1, x2, y2, color = sscanf(
        buf, "%c %d %d %d %d %d\n", _("Failed to parse net object"))

    if type != OBJ_NET:
        raise ValueError

    if x1 == x2 and y1 == y2:
        print _("Found a zero length net [ %c %d %d %d %d %d ]\n") % (
            type, x1, y1, x2, y2, color)

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    return xorn.storage.Net(
        x = x1 - origin_x,
        y = y1 - origin_y,
        width = x2 - x1,
        height = y2 - y1,
        color = color,
        is_bus = False,
        is_pin = False,
        is_inverted = False)

## Read a path object from a string and a file in gEDA format.
#
# Creates a path object from the string \a first_line and reads as
# many lines describing the path as specified there from \a f.
#
# \throw ParseError if the string could not be parsed
# \throw ParseError if not enough lines could be read from the file
# \throw ValueError if \a first_line doesn't describe a path object

def read_path(first_line, f, (origin_x, origin_y), format):
    type, color, \
    line_width, line_end, line_type, line_length, line_space, \
    fill_type, fill_width, angle1, pitch1, angle2, pitch2, \
    num_lines = sscanf(
        first_line, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
        _("Failed to parse path object"))

    if type != OBJ_PATH:
        raise ValueError

    # Checks if the required color is valid.
    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % first_line
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    pathstr = ''
    for i in xrange(0, num_lines):
        try:
            line = f.next()
        except StopIteration:
            raise ParseError, _("Unexpected end-of-file after %d lines "
                                "while reading path") % i

        pathstr += line

    if pathstr.endswith('\n'):
        pathstr = pathstr[:-1]

    return xorn.storage.Path(
        pathdata = pathstr.encode('utf-8'),
        color = color,
        line = xorn.storage.LineAttr(
            width = line_width,
            cap_style = line_end,
            dash_style = line_type,
            dash_length = line_length,
            dash_space = line_space),
        fill = xorn.storage.FillAttr(
            type = fill_type,
            width = fill_width,
            angle0 = angle1,
            pitch0 = pitch1,
            angle1 = angle2,
            pitch1 = pitch2))

## Read a picture object from a string and a file in gEDA format.
#
# Creates a picture object from the string \a first_line and, if the
# picture is specified as embedded, reads the picture data from \a f.
#
# \throw ParseError if the string could not be parsed
# \throw ParseError if the picture data could be read from the file
# \throw ValueError if \a first_line doesn't describe a picture object

def read_picture(first_line, f, (origin_x, origin_y), format):
    type, x1, y1, width, height, angle, mirrored, embedded = sscanf(
        first_line, "%c %d %d %d %d %d %d %d\n",
        _("Failed to parse picture definition"))

    if type != OBJ_PICTURE:
        raise ValueError

    if width == 0 or height == 0:
        print _("Found a zero width/height picture [ %c %d %d %d %d ]\n") % (
            type, x1, y1, width, height)

    if mirrored != 0 and mirrored != 1:
        print _("Found a picture with a wrong 'mirrored' parameter: "
                "%d.\n") % mirrored
        print _("Setting mirrored to 0\n")
        mirrored = 0

    if angle not in [0, 90, 180, 270]:
        print _("Found an unsupported picture angle [ %d ]\n") % angle
        print _("Setting angle to 0\n")
        angle = 0

    try:
        filename = f.next()
    except StopIteration:
        raise ParseError, _("Unexpected end-of-file "
                            "while reading picture file name")

    if filename.endswith('\n'):
        filename = filename[:-1]

    # Handle empty filenames
    if not filename:
        print _("Found an image with no filename.")
        filename = None

    pixmap = filename

    if embedded == 1:
        # Read the encoded picture
        try:
            file_content = xorn.base64.decode(f, delim = '.')
        except xorn.base64.DecodingError as e:
            print _("Failed to load image from embedded data [%s]: "
                    "%s\n") % (filename, e.message)
            # _("Base64 decoding failed.")
            print _("Falling back to file loading. Picture unembedded.\n")
        else:
            pixmap = EmbeddedPixmap(
                file_content = file_content,
                filename = filename)
    elif embedded != 0:
        print _("Found a picture with a wrong 'embedded' parameter: "
                "%d.\n") % embedded
        print _("Setting embedded to False\n")

    return xorn.storage.Picture(
        x = x1 - origin_x,
        y = y1 - origin_y,
        width = width,
        height = height,
        angle = angle,
        mirror = mirrored,
        pixmap = pixmap)

## Read a pin object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a pin object

def read_pin(buf, (origin_x, origin_y), format):
    if not format.enhanced_pinbus_format:
        type, x1, y1, x2, y2, color = sscanf(
            buf, "%c %d %d %d %d %d\n", _("Failed to parse pin object"))

        pin_type = 0
        whichend = -1
    else:
        type, x1, y1, x2, y2, color, pin_type, whichend = sscanf(
            buf, "%c %d %d %d %d %d %d %d\n", _("Failed to parse pin object"))

    if type != OBJ_PIN:
        raise ValueError

    if whichend == -1:
        print _("Found a pin which did not have the whichone field set.\n"
                "Verify and correct manually.\n")
    elif whichend < -1 or whichend > 1:
        print _("Found an invalid whichend on a pin (reseting to zero): "
                "%d\n") % whichend
        whichend = 0

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % buf
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    if pin_type == 0:
        is_bus = False
    elif pin_type == 1:
        is_bus = True
    else:
        raise ParseError

    if whichend == 0:
        is_inverted = False
    elif whichend == 1:
        x1, y1, x2, y2 = x2, y2, x1, y1
        is_inverted = True
    else:
        raise ParseError

    return xorn.storage.Net(
        x = x1 - origin_x,
        y = y1 - origin_y,
        width = x2 - x1,
        height = y2 - y1,
        color = color,
        is_bus = is_bus,
        is_pin = True,
        is_inverted = is_inverted)

## Read a text object from a string and a file in gEDA format.
#
# Creates a text object from the string \a first_line and reads as
# many lines of text as specified there from \a f.
#
# \throw ParseError if the string could not be parsed
# \throw ParseError if not enough lines could be read from the file
# \throw ValueError if \a first_line doesn't describe a text object

def read_text(first_line, f, (origin_x, origin_y), format):
    if format.supports_multiline_text:
        type, x, y, color, size, visibility, show_name_value, angle, \
        alignment, num_lines = sscanf(
            first_line, "%c %d %d %d %d %d %d %d %d %d\n",
            _("Failed to parse text object"))
    elif not format.supports_text_alignment:
        type, x, y, color, size, visibility, show_name_value, angle = sscanf(
            first_line, "%c %d %d %d %d %d %d %d\n",
            _("Failed to parse text object"))
        alignment = LOWER_LEFT  # older versions didn't have this
        num_lines = 1           # only support a single line
    else:
        type, x, y, color, size, visibility, show_name_value, angle, \
        alignment = sscanf(
            first_line, "%c %d %d %d %d %d %d %d %d\n",
            _("Failed to parse text object"))
        num_lines = 1           # only support a single line

    if type != OBJ_TEXT:
        raise ValueError

    if size == 0:
        print _("Found a zero size text string "
                "[ %c %d %d %d %d %d %d %d %d ]\n") % (
            type, x, y, color, size, visibility, show_name_value, angle,
            alignment)

    if angle not in [0, 90, 180, 270]:
        print _("Found an unsupported text angle "
                "[ %c %d %d %d %d %d %d %d %d ]\n") % (
            type, x, y, color, size, visibility, show_name_value, angle,
            alignment)
        print _("Setting angle to 0\n")
        angle = 0

    if alignment not in [LOWER_LEFT, MIDDLE_LEFT, UPPER_LEFT,
                         LOWER_MIDDLE, MIDDLE_MIDDLE, UPPER_MIDDLE,
                         LOWER_RIGHT, MIDDLE_RIGHT, UPPER_RIGHT]:
        print _("Found an unsupported text alignment "
                "[ %c %d %d %d %d %d %d %d %d ]\n") % (
            type, x, y, color, size, visibility, show_name_value, angle,
            alignment)
        print _("Setting alignment to LOWER_LEFT\n")
        alignment = LOWER_LEFT

    if color < 0 or color > MAX_COLORS:
        print _("Found an invalid color [ %s ]\n") % first_line
        print _("Setting color to default color\n")
        color = DEFAULT_COLOR

    if num_lines <= 0:
        raise ParseError

    text = ''
    for i in xrange(0, num_lines):
        try:
            line = f.next()
        except StopIteration:
            raise ParseError, _("Unexpected end-of-file after %d lines") % i

        text += line

    if text.endswith('\n'):
        text = text[:-1]

    return xorn.storage.Text(
        x = x - origin_x,
        y = y - origin_y,
        color = color,
        text_size = size,
        visibility = visibility,
        show_name_value = show_name_value,
        angle = angle,
        alignment = alignment,
        text = text.encode('utf-8'))
