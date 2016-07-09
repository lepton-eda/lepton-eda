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
## Reading schematic/symbol files.

import os.path, sys
from gettext import gettext as _
import xorn.geda.clib
import xorn.geda.fileformat
import xorn.geda.plainread
import xorn.geda.ref
import xorn.geda.xmlread

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

## Read a symbol or schematic file.
#
# See \ref read_file for a description of the keyword arguments.
#
# \returns a transient xorn.proxy.RevisionProxy instance containing
#          the file's contents
#
# \throws ParseError if the file is not a valid schematic/symbol file
# \throws xorn.geda.fileformat.UnknownFormatError if \a format is not
#         specified and the format can't be guessed from \a path

def read(path, format = None, **kwds):
    if format is None:
        format = xorn.geda.fileformat.guess_format(path)

    f = open(path, 'rb')
    try:
        return read_file(
            f, path, format, pixmap_basepath = os.path.dirname(path), **kwds)
    finally:
        f.close()

## Read a symbol or schematic file from a file object.
#
# \param [in] f                A file-like object from which to read
# \param [in] name             The file name displayed in warning and
#                              error messages
# \param [in] format           The file format to be expected
# \param [in] log              An object to which errors are logged.
#                              If this is \c None (the default), a new
#                              DefaultLog instance is used which
#                              raises a ParseError exception on error
#                              and writes messages to \c sys.stderr
# \param [in] load_symbols     Whether to load referenced symbol files as well
# \param [in] load_pixmaps     Whether to load referenced pixmap files as well
# \param [in] pixmap_basepath  Base directory for relative pixmap paths
#
# \returns a transient xorn.proxy.RevisionProxy instance containing
#          the file's contents
#
# \throws ParseError if the file is not a valid schematic/symbol file

def read_file(f, name, format, log = None,
              load_symbols = False,
              load_pixmaps = False,
              pixmap_basepath = None, **kwds):
    if log is None:
        log = DefaultLog(name)

    # Mock-ups for referenced symbols if we aren't loading them
    referenced_symbols = {}
    # Mock-ups for or already loaded pixmaps
    referenced_pixmaps = {}

    def load_symbol(basename, fallback_available):
        if load_symbols:
            # Look up the symbol from the component library, loading
            # it if necessary.
            try:
                return xorn.geda.clib.lookup_symbol(basename)
            except ValueError:
                if fallback_available:
                    log.warn(
                        _("symbol \"%s\" not found in library") % basename)
                else:
                    log.error(
                        _("symbol \"%s\" not found in library") % basename)
                # fallthrough

        if fallback_available:
            return None

        try:
            return referenced_symbols[basename]
        except KeyError:
            symbol = xorn.geda.ref.Symbol(basename, None, False)
            referenced_symbols[basename] = symbol
            return symbol

    def load_pixmap(filename, fallback_available):
        try:
            pixmap = referenced_pixmaps[filename]
        except KeyError:
            pixmap = xorn.geda.ref.Pixmap(filename, None, False)
            referenced_pixmaps[filename] = pixmap

            if load_pixmaps:
                if pixmap_basepath is not None:
                    real_filename = os.path.join(pixmap_basepath, filename)
                else:
                    real_filename = filename
                try:
                    f = open(real_filename, 'rb')
                    try:
                        pixmap.data = f.read()
                    finally:
                        f.close()
                except IOError as e:
                    if fallback_available:
                        log.warn(_("can't read pixmap file \"%s\": %s")
                                 % (real_filename, e.strerror))
                    else:
                        log.error(_("can't read pixmap file \"%s\": %s")
                                  % (real_filename, e.strerror))

        if pixmap.data is None and fallback_available:
            return None
        return pixmap

    if format == xorn.geda.fileformat.FORMAT_SYM or \
       format == xorn.geda.fileformat.FORMAT_SCH:
        return xorn.geda.plainread.read_file(
            f, name, log, load_symbol, load_pixmap, **kwds)
    if format == xorn.geda.fileformat.FORMAT_SYM_XML or \
       format == xorn.geda.fileformat.FORMAT_SCH_XML:
        return xorn.geda.xmlread.read_file(
            f, name, log, load_symbol, load_pixmap, **kwds)
    raise ValueError
