# xorn.geda - Python library for manipulating gEDA files
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

## \namespace xorn.geda.read
## Reading gEDA schematic/symbol files.
#
# The gEDA file format is a space separated list of characters and
# numbers in plain ASCII.  The meaning of each item is described in
# the file format documentation which can be found in \c
# doc/geda/file_format_spec or
# [here](http://wiki.geda-project.org/geda:file_format_spec).

import codecs, sys
from gettext import gettext as _
import xorn.base64
import xorn.proxy
import xorn.storage
import xorn.geda.attrib
import xorn.geda.clib
import xorn.geda.ref
from xorn.geda.fileformat import *

## Raised when parsing a malformed file.

class ParseError(Exception):
    pass

## Default behavior for handling file read errors and warnings.
#
# When reading a file, a log object can be specified which handles any
# errors and warnings which occur while reading the file.  If no log
# object is specified, a new DefaultLog instance is used instead.
#
# The behavior of DefaultLog is to print any messages to \c sys.stderr
# along with the file name passed to the constructor, and to raise a
# ParseError exception on error.

class DefaultLog:
    def __init__(self, name):
        self.name = name
        self.lineno = 0

    def error(self, message):
        sys.stderr.write("%s:%d: error: %s\n" % (
            self.name, self.lineno + 1, message))
        raise ParseError

    def warn(self, message):
        sys.stderr.write("%s:%d: warning: %s\n" % (
            self.name, self.lineno + 1, message))

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

## Helper function for \ref sscanf.

def parse_token(s, fmt):
    if fmt == '%c':
        if len(s) != 1:
            raise ValueError
        return s

    if fmt == '%d':
        return int(s)

    if fmt == '%u':
        val = int(s)
        if val < 0:
            raise ValueError
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
# \throw ValueError if the string does not match the format
# \throw ValueError if an invalid format token is passed
#
# \return a tuple containing the parsed values

def sscanf(s, fmt):
    while fmt.endswith('\n'):
        if not s.endswith('\n'):
            raise ValueError
        fmt = fmt[:-1]
        s = s[:-1]

    if s.endswith('\n'):
        raise ValueError

    # gEDA/gaf ignores trailing spaces and, in some older versions,
    # wrote them for text objects
    s = s.rstrip(' ')

    stok = s.split(' ')
    fmttok = fmt.split(' ')

    if len(stok) != len(fmttok):
        raise ValueError

    return [parse_token(st, ft) for (st, ft) in zip(stok, fmttok)]

## Replace "\r\n" line endings with "\n" line endings.

def strip_carriage_return(f):
    for line in f:
        if line.endswith('\r\n'):
            yield line[:-2] + '\n'
        else:
            yield line

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
        return read_file(strip_carriage_return(f), filename, **kwds)
    finally:
        f.close()

## Read a file in libgeda format from a file object.
#
# \param [in] f                   A file-like object from which to read.
# \param [in] name                The file name displayed in warning
#                                 and error messages.
# \param [in] log                 An object to which errors are logged.
#                                 If this is \c None (the default), a
#                                 new DefaultLog instance is used
#                                 which raises a ParseError on error
#                                 and writes messages to \c sys.stderr.
# \param [in] load_symbols        Load referenced symbol files as well
# \param [in] override_net_color  Reset the color of nets do default?
# \param [in] override_bus_color  Reset the color of buses do default?
# \param [in] override_pin_color  Reset the color of pins do default?
# \param [in] force_boundingbox   <i>currently unused</i>
#
# \return The new revision.
#
# \throw ParseError if the file is not a valid gEDA schematic/symbol file

def read_file(f, name, log = None,
              load_symbols = False,
              override_net_color = None,
              override_bus_color = None,
              override_pin_color = None,
              force_boundingbox = False):
    if log is None:
        log = DefaultLog(name)

    def lineno_incrementor(f):
        for line in f:
            yield line
            log.lineno += 1
    f = lineno_incrementor(f)

    # Mock-ups for referenced symbols if we aren't loading them
    referenced_symbols = {}

    def load_symbol(basename):
        if load_symbols:
            # Look up the symbol from the component library, loading
            # it if necessary.
            return xorn.geda.clib.lookup_symbol(basename)

        try:
            return referenced_symbols[basename]
        except KeyError:
            symbol = xorn.geda.ref.Symbol(basename, None, False)
            referenced_symbols[basename] = symbol
            return symbol

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
            data = read_line(line, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_NET:
            data = read_net(line, origin, format, log)
            if data is not None:
                if override_net_color is not None:
                    data.color = override_net_color
                ob = rev.add_object(data)
        elif objtype == OBJ_BUS:
            data = read_bus(line, origin, format, log)
            if data is not None:
                if override_bus_color is not None:
                    data.color = override_bus_color
                ob = rev.add_object(data)
        elif objtype == OBJ_BOX:
            data = read_box(line, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_PICTURE:
            data = read_picture(line, f, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_CIRCLE:
            data = read_circle(line, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_COMPLEX:
            data = read_complex(line, origin, format, log, load_symbol)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_TEXT:
            data = read_text(line, f, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_PATH:
            data = read_path(line, f, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == OBJ_PIN:
            data = read_pin(line, origin, format, log)
            if data is not None:
                if override_pin_color is not None:
                    data.color = override_pin_color
                ob = rev.add_object(data)
        elif objtype == OBJ_ARC:
            data = read_arc(line, origin, format, log)
            if data is not None:
                ob = rev.add_object(data)
        elif objtype == STARTATTACH_ATTR:
            if ob is None:
                log.error(_("unexpected attribute list start marker"))
                continue
            if not isinstance(rev.get_object_data(ob), xorn.storage.Net) and \
               not isinstance(rev.get_object_data(ob), xorn.storage.Component):
                log.error(_("can't attach attributes to this object type"))
                continue

            while True:
                try:
                    line = f.next()
                except StopIteration:
                    log.error(_("unterminated attribute list"))
                    break

                if not line:
                    continue

                if line[0] == ENDATTACH_ATTR:
                    break

                if line[0] != OBJ_TEXT:
                    log.error(
                        _("tried to attach a non-text item as an attribute"))
                    continue

                attrib = read_text(line, f, origin, format, log)
                if attrib is not None:
                    rev.relocate_object(rev.add_object(attrib), ob, None)

            ob = None
        elif objtype == START_EMBEDDED:
            if ob is None:
                log.error(_("unexpected embedded symbol start marker"))
                continue
            component_data = rev.get_object_data(ob)
            if type(component_data) != xorn.storage.Component:
                log.error(_("embedded symbol start marker following "
                            "non-component object"))
                continue
            if not component_data.symbol.embedded:
                log.error(_("embedded symbol start marker following "
                            "component with non-embedded symbol"))
                continue
            if component_data.symbol.prim_objs is not None:
                log.error(_("embedded symbol start marker following "
                            "embedded symbol"))
                continue
            object_lists_save.append((rev, ob, origin))
            rev = xorn.storage.Revision()
            component_data.symbol.prim_objs = rev
            origin = origin[0] + component_data.x, origin[1] + component_data.y
        elif objtype == END_EMBEDDED:
            if not object_lists_save:
                log.error(_("unexpected embedded symbol end marker"))
                continue
            rev, ob, origin = object_lists_save.pop()
        elif objtype == ENDATTACH_ATTR:
            log.error(_("unexpected attribute list end marker"))
        elif objtype == INFO_FONT:
            # NOP
            pass
        elif objtype == COMMENT:
            # do nothing
            pass
        elif objtype == VERSION_CHAR:
            try:
                objtype, release_ver, fileformat_ver = \
                    sscanf(line, "%c %u %u\n")
            except ValueError:
                try:
                    objtype, release_ver = sscanf(line, "%c %u\n")
                except ValueError:
                    log.error(_("failed to parse version string"))
                    continue
                fileformat_ver = 0

            assert objtype == VERSION_CHAR

            # 20030921 was the last version which did not have a fileformat
            # version.
            if release_ver <= VERSION_20030921:
                fileformat_ver = 0

            if fileformat_ver == 0:
                log.warn(_("Read an old format sym/sch file! "
                           "Please run g[sym|sch]update on this file"))

            format = FileFormat(release_ver, fileformat_ver)
        else:
            log.error(_("read garbage"))

    for ob in rev.get_objects():
        data = rev.get_object_data(ob)
        if not isinstance(data, xorn.storage.Component) \
               or not data.symbol.embedded:
            continue
        if data.symbol.prim_objs is None:
            log.error(_("embedded symbol is missing"))
            continue

        # un-hide overwritten attributes in embedded symbol
        ob = xorn.proxy.ObjectProxy(rev, ob)
        visibility = {}
        for attached in xorn.geda.attrib.find_attached_attribs(ob):
            attached_name, attached_value = \
                xorn.geda.attrib.parse_string(attached.text)
            visibility[attached_name] = attached.visibility
        for inherited in xorn.geda.attrib.find_inherited_attribs(ob):
            inherited_name, inherited_value = \
                xorn.geda.attrib.parse_string(inherited.text)
            if inherited_name in visibility:
                inherited.visibility = visibility[inherited_name]

    if not format.enhanced_pinbus_format:
        pin_update_whichend(rev, force_boundingbox, log)

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

def pin_update_whichend(rev, force_boundingbox, log):
    log.error(_("file is lacking pin orientation information"))


## Read a circle object from a string in gEDA format.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a circle object

def read_circle(buf, (origin_x, origin_y), format, log):
    try:
        if not format.supports_linefill_attributes:
            type, x1, y1, radius, color = sscanf(buf, "%c %d %d %d %d\n")
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
            type, x1, y1, radius, color, circle_width, \
            circle_end, circle_type, circle_length, circle_space, \
            circle_fill, fill_width, angle1, pitch1, angle2, pitch2 = sscanf(
                buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse circle object"))
        return None

    if type != OBJ_CIRCLE:
        raise ValueError

    if radius == 0:
        log.warn(_("circle has radius zero"))
    elif radius < 0:
        log.warn(_("circle has negative radius (%d), setting to 0") % radius)
        radius = 0

    if color < 0 or color > MAX_COLORS:
        log.warn(_("circle has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
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

def read_arc(buf, (origin_x, origin_y), format, log):
    try:
        if not format.supports_linefill_attributes:
            type, x1, y1, radius, start_angle, sweep_angle, color = sscanf(
                buf, "%c %d %d %d %d %d %d\n")
            arc_width = 0
            arc_end = 0
            arc_type = 0
            arc_space = -1
            arc_length = -1
        else:
            type, x1, y1, radius, start_angle, sweep_angle, color, \
            arc_width, arc_end, arc_type, arc_length, arc_space = sscanf(
                buf, "%c %d %d %d %d %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse arc object"))
        return None

    if type != OBJ_ARC:
        raise ValueError

    if radius == 0:
        log.warn(_("arc has radius zero"))
    elif radius < 0:
        log.warn(_("arc has negative radius (%d), setting to 0") % radius)
        radius = 0

    if color < 0 or color > MAX_COLORS:
        log.warn(_("arc has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
        color = DEFAULT_COLOR

    return xorn.storage.Arc(
        x = x1 - origin_x,
        y = y1 - origin_y,
        radius = radius,
        startangle = start_angle,
        sweepangle = sweep_angle,
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

def read_box(buf, (origin_x, origin_y), format, log):
    try:
        if not format.supports_linefill_attributes:
            type, x1, y1, width, height, color = sscanf(
                buf, "%c %d %d %d %d %d\n")
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
                buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse box object"))
        return None

    if type != OBJ_BOX:
        raise ValueError

    if width == 0 or height == 0:
        log.warn(_("box has width/height zero"))

    if color < 0 or color > MAX_COLORS:
        log.warn(_("box has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
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

def read_bus(buf, (origin_x, origin_y), format, log):
    try:
        if not format.enhanced_pinbus_format:
            type, x1, y1, x2, y2, color = sscanf(
                buf, "%c %d %d %d %d %d\n")
            ripper_dir = 0
        else:
            type, x1, y1, x2, y2, color, ripper_dir = sscanf(
                buf, "%c %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse bus object"))
        return None

    if type != OBJ_BUS:
        raise ValueError

    if x1 == x2 and y1 == y2:
        log.warn(_("bus has length zero"))

    if color < 0 or color > MAX_COLORS:
        log.warn(_("bus has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
        color = DEFAULT_COLOR

    if ripper_dir < -1 or ripper_dir > 1:
        log.warn(_("bus has invalid ripper direction (%d)") % ripper_dir)
        ripper_dir = 0  # isn't used

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
# If the symbol is not embedded and \a load_symbol is \c True, try to
# load it from the component library.
#
# \throw ParseError if the string could not be parsed
# \throw ValueError if \a buf doesn't describe a component object

def read_complex(buf, (origin_x, origin_y), format, log, load_symbol):
    try:
        type, x1, y1, selectable, angle, mirror, basename = sscanf(
            buf, "%c %d %d %d %d %d %s\n")
    except ValueError:
        log.error(_("failed to parse complex object"))
        return None

    if type != OBJ_COMPLEX:
        raise ValueError

    if angle not in [0, 90, 180, 270]:
        log.warn(_("component has invalid angle (%d), setting to 0") % angle)
        angle = 0

    if mirror != 0 and mirror != 1:
        log.warn(_("component has invalid mirror flag (%d), "
                   "setting to 0") % mirror)
        mirror = 0

    # color = DEFAULT_COLOR

    if basename.startswith('EMBEDDED'):
        symbol = xorn.geda.ref.Symbol(basename[8:], None, True)
    else:
        symbol = load_symbol(basename)
        assert not symbol.embedded

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

def read_line(buf, (origin_x, origin_y), format, log):
    try:
        if not format.supports_linefill_attributes:
            type, x1, y1, x2, y2, color = sscanf(buf, "%c %d %d %d %d %d\n")
            line_width = 0
            line_end = 0
            line_type = 0
            line_length = -1
            line_space = -1
        else:
            type, x1, y1, x2, y2, color, \
            line_width, line_end, line_type, line_length, line_space = sscanf(
                buf, "%c %d %d %d %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse line object"))
        return None

    if type != OBJ_LINE:
        raise ValueError

    # Null length line are not allowed.  If such a line is detected, a
    # message is issued.
    if x1 == x2 and y1 == y2:
        log.warn(_("line has length zero"))

    if color < 0 or color > MAX_COLORS:
        log.warn(_("line has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
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

def read_net(buf, (origin_x, origin_y), format, log):
    try:
        type, x1, y1, x2, y2, color = sscanf(buf, "%c %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse net object"))
        return None

    if type != OBJ_NET:
        raise ValueError

    if x1 == x2 and y1 == y2:
        log.warn(_("net has length zero"))

    if color < 0 or color > MAX_COLORS:
        log.warn(_("net has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
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

def read_path(first_line, f, (origin_x, origin_y), format, log):
    try:
        type, color, \
        line_width, line_end, line_type, line_length, line_space, \
        fill_type, fill_width, angle1, pitch1, angle2, pitch2, \
        num_lines = sscanf(
            first_line, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse path object"))
        return None

    if type != OBJ_PATH:
        raise ValueError

    # Checks if the required color is valid.
    if color < 0 or color > MAX_COLORS:
        log.warn(_("path has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
        color = DEFAULT_COLOR

    pathstr = ''
    for i in xrange(0, num_lines):
        try:
            line = f.next()
        except StopIteration:
            log.error(_("unexpected end of file after %d lines "
                        "while reading path") % i)
            break

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

def read_picture(first_line, f, (origin_x, origin_y), format, log):
    try:
        type, x1, y1, width, height, angle, mirrored, embedded = sscanf(
            first_line, "%c %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse picture definition"))
        return None

    if type != OBJ_PICTURE:
        raise ValueError

    if width == 0 or height == 0:
        log.warn(_("picture has width/height zero"))

    if mirrored != 0 and mirrored != 1:
        log.warn(_("picture has wrong 'mirrored' parameter (%d), "
                   "setting to 0") % mirrored)
        mirrored = 0

    if angle not in [0, 90, 180, 270]:
        log.warn(_("picture has unsupported angle (%d), setting to 0") % angle)
        angle = 0

    try:
        filename = f.next()
    except StopIteration:
        log.error(_("unexpected end of file while reading picture file name"))
        filename = ''
    else:
        if filename.endswith('\n'):
            filename = filename[:-1]

        # Handle empty filenames
        if not filename:
            log.warn(_("image has no filename"))
            filename = None

    pixmap = xorn.geda.ref.Pixmap(filename, None, False)

    if embedded == 1:
        # Read the encoded picture
        try:
            pixmap.data = xorn.base64.decode(f, delim = '.')
        except xorn.base64.DecodingError as e:
            log.warn(_("Failed to load image \"%s\" from embedded data: %s. "
                       "Falling back to file loading. Picture unembedded")
                     % (filename, e.message))
            # _("Base64 decoding failed.")
        else:
            pixmap.embedded = True
    elif embedded != 0:
        log.warn(_("picture has wrong 'embedded' parameter (%d), "
                   "setting to not embedded") % embedded)

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

def read_pin(buf, (origin_x, origin_y), format, log):
    try:
        if not format.enhanced_pinbus_format:
            type, x1, y1, x2, y2, color = sscanf(buf, "%c %d %d %d %d %d\n")
            pin_type = 0
            whichend = -1
        else:
            type, x1, y1, x2, y2, color, pin_type, whichend = sscanf(
                buf, "%c %d %d %d %d %d %d %d\n")
    except ValueError:
        log.error(_("failed to parse pin object"))
        return None

    if type != OBJ_PIN:
        raise ValueError

    if whichend == -1:
        log.warn(_("pin does not have the whichone field set--"
                   "verify and correct manually!"))
    elif whichend < -1 or whichend > 1:
        log.warn(_("pin has invalid whichend (%d), "
                   "setting to first end") % whichend)
        whichend = 0

    if color < 0 or color > MAX_COLORS:
        log.warn(_("pin has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
        color = DEFAULT_COLOR

    if pin_type == 0:
        is_bus = False
    elif pin_type == 1:
        is_bus = True
    else:
        log.warn(_("pin has invalid type (%d), setting to 0") % pin_type)
        is_bus = False

    if whichend != 1:
        is_inverted = False
    else:
        x1, y1, x2, y2 = x2, y2, x1, y1
        is_inverted = True

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

def read_text(first_line, f, (origin_x, origin_y), format, log):
    try:
        if format.supports_multiline_text:
            type, x, y, color, size, visibility, show_name_value, angle, \
            alignment, num_lines = sscanf(
                first_line, "%c %d %d %d %d %d %d %d %d %d\n")
        elif not format.supports_text_alignment:
            type, x, y, color, size, visibility, show_name_value, angle = \
                sscanf(first_line, "%c %d %d %d %d %d %d %d\n")
            alignment = LOWER_LEFT  # older versions didn't have this
            num_lines = 1           # only support a single line
        else:
            type, x, y, color, size, visibility, show_name_value, angle, \
            alignment = sscanf(
                first_line, "%c %d %d %d %d %d %d %d %d\n")
            num_lines = 1           # only support a single line
    except ValueError:
        log.error(_("failed to parse text object"))
        return None

    if type != OBJ_TEXT:
        raise ValueError

    if size == 0:
        log.warn(_("text has size zero"))

    if angle not in [0, 90, 180, 270]:
        log.warn(_("text has unsupported angle (%d), setting to 0") % angle)
        angle = 0

    if alignment not in [LOWER_LEFT, MIDDLE_LEFT, UPPER_LEFT,
                         LOWER_MIDDLE, MIDDLE_MIDDLE, UPPER_MIDDLE,
                         LOWER_RIGHT, MIDDLE_RIGHT, UPPER_RIGHT]:
        log.warn(_("text has unsupported alignment (%d), "
                   "setting to LOWER_LEFT") % alignment)
        alignment = LOWER_LEFT

    if color < 0 or color > MAX_COLORS:
        log.warn(_("text has invalid color (%d), setting to %d")
                 % (color, DEFAULT_COLOR))
        color = DEFAULT_COLOR

    if num_lines <= 0:
        log.error(_("text has invalid number of lines (%d)") % num_lines)

    text = ''
    for i in xrange(0, num_lines):
        try:
            line = f.next()
        except StopIteration:
            log.error(_("unexpected end of file after %d lines of text") % i)
            break

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
