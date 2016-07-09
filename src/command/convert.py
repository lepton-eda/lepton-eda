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

import codecs, getopt, os, sys
from gettext import gettext as _
import xorn.command
import xorn.fileutils
import xorn.geda.clib
import xorn.geda.read
import xorn.geda.write
import xorn.geda.xmlread
import xorn.geda.xmlwrite

def check_if_directory(path, option_name):
    if os.path.isdir(path):
        return
    sys.stderr.write(_("%s: \"%s\" is not a directory (passed to %s)\n")
                     % (xorn.command.program_short_name, path, option_name))
    sys.exit(1)

def main():
    try:
        options, args = getopt.getopt(
            xorn.command.args, 'o:I:O:', [
                'output-file=', 'input-format=', 'output-format=',
                'symbol-library=', 'symbol-library-search=',
                  'symbol-library-command=', 'reset-symbol-library',
                'omit-symbols', 'omit-pixmaps',
                'enable-hybridnum', 'disable-hybridnum',
                'help', 'version'])
    except getopt.GetoptError as e:
        xorn.command.invalid_arguments(e.msg)

    input_file = None
    output_file = None
    input_format = None
    output_format = None

    omit_symbols = False
    omit_pixmaps = False

    use_hybridnum = False

    for option, value in options:
        if option == '-o' or option == '--output-file':
            if output_file is not None:
                xorn.command.invalid_arguments(
                    "option `-o' can only be given once")
            output_file = value
        elif option == '-I' or option == '--input-format':
            if input_format is not None:
                xorn.command.invalid_arguments(
                    "option `-I' can only be given once")
            try:
                input_format = xorn.geda.fileformat.VALID_FORMATS[value]
            except KeyError:
                xorn.command.invalid_arguments(
                    "%s is not a valid format" % value)
        elif option == '-O' or option == '--output-format':
            if output_format is not None:
                xorn.command.invalid_arguments(
                    "option `-O' can only be given once")
            try:
                output_format = xorn.geda.fileformat.VALID_FORMATS[value]
            except KeyError:
                xorn.command.invalid_arguments(
                    "%s is not a valid format" % value)

        # symbol library

        elif option == '--symbol-library':
            check_if_directory(value, option)
            xorn.geda.clib.add_source(
                xorn.geda.clib.DirectorySource(value, False),
                xorn.geda.clib.uniquify_source_name(os.path.basename(value)))

        elif option == '--symbol-library-search':
            check_if_directory(value, option)
            xorn.geda.clib.add_source(
                xorn.geda.clib.DirectorySource(value, True),
                xorn.geda.clib.uniquify_source_name(os.path.basename(value)))

        elif option == '--symbol-library-command':
            tokens = value.split(':')
            if len(tokens) != 2:
                sys.stderr.write(_("Invalid library command argument: %s\n"
                                   "Must be 'list' and 'get' commands "
                                   "separated by colons\n") % value)
                sys.exit(1)
            listcmd, getcmd = tokens
            xorn.geda.clib.add_source(
                xorn.geda.clib.CommandSource(listcmd, getcmd),
                xorn.geda.clib.uniquify_source_name('command source'))

        elif option == '--reset-symbol-library':
            xorn.geda.clib.reset()

        # omit symbols/pixmaps

        elif option == '--omit-symbols':
            omit_symbols = True
        elif option == '--omit-pixmaps':
            omit_pixmaps = True

        # file format features

        elif option == '--enable-hybridnum':
            use_hybridnum = True
        elif option == '--disable-hybridnum':
            use_hybridnum = False

        elif option == '--help':
            sys.stdout.write(_(
"Usage: %s [OPTION]... INPUT-FILE [OUTPUT-FILE]\n"
"       %s [OPTION]... [-o OUTPUT-FILE] [INPUT-FILE]\n")
                             % ((xorn.command.program_name, ) * 2))
            sys.stdout.write(_(
"Convert files from one file format to another\n"))
            sys.stdout.write("\n")
            sys.stdout.write(_("""\
  -o, --output-file=FILE        write to FILE (can be `-' for stdout)
  -I, --input-format=FORMAT     input file format (optional if it can be
                                deduced from the input file name)
  -O, --output-format=FORMAT    output file format (optional if it can be
                                deduced from the output file name)

  --symbol-library=PATH         add PATH to the symbol library
  --symbol-library-search=PATH  add PATH and its subdirectories to the symbol
                                library
  --symbol-library-command=LISTCMD:GETCMD
                                add the source commands LISTCMD and GETCMD to
                                the symbol library
  --reset-symbol-library        clear all previous symbol library sources

  --omit-symbols                don't include referenced symbols in the output
  --omit-pixmaps                don't include referenced pixmaps in the output

  --enable-hybridnum            enable/disable use of hybrid number format
  --disable-hybridnum
"""))
            sys.stdout.write("\n")
            sys.stdout.write(_("Valid formats are: %s\n") %
                ', '.join(sorted(xorn.geda.fileformat.VALID_FORMATS)))
            sys.stdout.write("\n")
            sys.stdout.write(_(
"Both input and output filename can be `-' for the standard input or output,\n"
"respectively.  In this case, the corresponding format option is mandatory.\n"))
            sys.stdout.write("\n")
            sys.stdout.write(_("Report %s bugs to %s\n")
                             % (xorn.config.PACKAGE_NAME,
                                xorn.config.PACKAGE_BUGREPORT))
            sys.exit(0)
        elif option == '--version':
            xorn.command.core_version()

    if args:
        input_file = args[0]
    if len(args) > 1:
        if output_file is not None:
            xorn.command.invalid_arguments(
                "output file name specified more than once")
        output_file = args[1]
    if len(args) > 2:
        xorn.command.invalid_arguments("too many arguments")

    if input_file is None:
        input_file = '-'
    if output_file is None:
        output_file = '-'

    if input_format is None:
        if input_file == '-':
            xorn.command.invalid_arguments(
                "Input format must be specified when reading from stdin.")
        try:
            input_format = xorn.geda.fileformat.guess_format(input_file)
        except xorn.geda.fileformat.UnknownFormatError:
            sys.stderr.write(_(
                "%s: Input format could not be deduced from input file name.\n"
                "%s: Please specify an input format using `-I FORMAT'.\n")
                             % ((xorn.command.program_short_name, ) * 2))
            sys.exit(1)

    if output_format is None:
        if output_file == '-':
            xorn.command.invalid_arguments(
                "Output format must be specified when reading from stdin.")
        try:
            output_format = xorn.geda.fileformat.guess_format(output_file)
        except xorn.geda.fileformat.UnknownFormatError:
            sys.stderr.write(_(
              "%s: Output format could not be deduced from output file name.\n"
              "%s: Please specify an output format using `-O FORMAT'.\n")
                             % ((xorn.command.program_short_name, ) * 2))
            sys.exit(1)

    # read revision from input file

    output_format_is_xml = output_format in (
        xorn.geda.fileformat.FORMAT_SYM_XML,
        xorn.geda.fileformat.FORMAT_SCH_XML)
    load_symbols = output_format_is_xml and not omit_symbols
    load_pixmaps = output_format_is_xml and not omit_pixmaps
    xorn.geda.clib.load_pixmaps = load_pixmaps

    try:
        if input_file == '-':
            input_file = '<stdin>'
            rev = xorn.geda.read.read_file(sys.stdin, '<stdin>', input_format,
                                           load_symbols = load_symbols,
                                           load_pixmaps = load_pixmaps)
        else:
            rev = xorn.geda.read.read(input_file, input_format,
                                      load_symbols = load_symbols,
                                      load_pixmaps = load_pixmaps)
    except IOError as e:
        sys.stderr.write(_("%s: %s: %s\n")
                         % (xorn.command.program_short_name,
                            input_file, e.strerror))
        sys.exit(1)
    except xorn.geda.read.ParseError:
        sys.exit(1)

    # write revision to output file

    if output_format == xorn.geda.fileformat.FORMAT_SYM_XML or \
       output_format == xorn.geda.fileformat.FORMAT_SCH_XML:
        kwds = { 'use_hybridnum': use_hybridnum,
                 'omit_symbols': omit_symbols,
                 'omit_pixmaps': omit_pixmaps }
    else:
        kwds = {}

    try:
        if output_file == '-':
            output_file = '<stdout>'
            xorn.geda.write.write_file(sys.stdout, rev, output_format, **kwds)
        else:
            xorn.geda.write.write(rev, output_file, output_format,
                                  { 'backup': False, 'fsync': False }, **kwds)
    except IOError as e:
        sys.stderr.write(_("%s: %s: %s\n")
                         % (xorn.command.program_short_name,
                            output_file, e.strerror))
        sys.exit(1)
