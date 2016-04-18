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

## \namespace xorn.command
## Infrastructure for \c xorn command-line scripts.
#
# This module provides some common functionality used by \c xorn
# subcommands.  It is also where the \c xorn executable provides
# information to submodules.
#
# A typical \c xorn subcommand looks like this:
#
# \code{.py}
# #!/usr/bin/python
# import xorn.command
#
# xorn.command.bugreport = 'example@example.com'
#
# def main():
#     try:
#         options, args = getopt.getopt(
#             xorn.command.args, '', ['help', 'version'])
#     except getopt.GetoptError as e:
#         xorn.command.invalid_arguments(e.msg)
#
#     for option, value in options:
#         # ... process options ...
#
#     # ... do something ...
#
# if __name__ == '__main__':
#     main()
# \endcode

import os.path, sys
from gettext import gettext as _
import xorn.config

## Email address to which users should send bug reports.
#
# Set this to an appropriate value for your script.

bugreport = xorn.config.PACKAGE_BUGREPORT

## Return the value of <tt>argv[0]</tt>.
#
# Python replaces <tt>sys.argv[0]</tt> with an absolute path if the
# command was run from the search path.  This function tries to
# compensate for this by returning just the basename if
# <tt>sys.argv[0]</tt> is an absolute path.

def argv0():
    if sys.argv[0].startswith('/'):
        return os.path.basename(sys.argv[0])
    return sys.argv[0]

## Name that was used to invoke the script.
#
# If the script was run by \c xorn, this is <tt>argv[0]</tt> plus a
# space character plus the subcommand name.  Otherwise, it is the same
# as <tt>argv[0]</tt>.
#
# Typically used in the output of \c \--help.

program_name = argv0()

## Basename component of the name that was used to invoke the script.
#
# If the script was run by \c xorn, this is the basename of the
# executed script, i.e., \c 'xorn-<i>something</i>'.  Otherwise, it is
# the same as <tt>argv[0]</tt>, with all text up to and including the
# final slash (/), if any, removed.
#
# Typically used in error messages.

program_short_name = os.path.basename(argv0())

## List of command arguments.
#
# A list of unparsed arguments, starting with the first argument
# following the invocation name.  If the script was run by \c xorn,
# this is the right-hand part of \c argv with all \c xorn arguments
# and the command name stripped.  Otherwise, it is the same as
# <tt>argv[1:]</tt>.

args = sys.argv[1:]

## Print an argument error message to \c sys.stdout and exit.
#
# This command prints an error message of the form
# \verbatim
# xorn-frobnicate: missing file operand
# Try `xorn frobnicate --help' for more information.
# \endverbatim
# to \c sys.stderr and exits with status code \c 1.

def invalid_arguments(message):
    sys.stderr.write(_("%s: %s\n") % (program_short_name, message))
    sys.stderr.write(_("Try `%s --help' for more information.\n")
                     % program_name)
    sys.exit(1)

## Print core version.
#
# Prints an output appropriate for the \c \--version option of the core
# scripts to \c sys.stdout and exits with status code \c 0.

def core_version():
    sys.stdout.write("%s\n" % xorn.config.PACKAGE_STRING)
    sys.stdout.write(_("Copyright (C) 2016 Roland Lutz\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"This program is free software; you can redistribute it and/or\n"
"modify it under the terms of the GNU General Public License\n"
"as published by the Free Software Foundation; either version 2\n"
"of the License, or (at your option) any later version.\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"This program is distributed in the hope that it will be useful,\n"
"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"GNU General Public License for more details.\n"))
    sys.exit(0)
