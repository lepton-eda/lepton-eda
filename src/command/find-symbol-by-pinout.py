# Find symbols in a directory matching a given pinout
# Copyright (C) 2013, 2014 Roland Lutz
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

import getopt, os, sys
from gettext import gettext as _

import xorn.command
import xorn.config
import xorn.proxy
import xorn.geda.attrib
import xorn.geda.read

def find_pins_by_attribute(rev, name, value):
    found = []
    for pin in rev.toplevel_objects():
        data = pin.data()
        if isinstance(data, xorn.storage.Net) and data.is_pin and value in \
                xorn.geda.attrib.search_attached(pin, name):
            found += [pin]
    return found

def has_pin(rev, number, label):
    for pin in find_pins_by_attribute(rev, 'pinnumber', number):
        if label in xorn.geda.attrib.search_attached(pin, 'pinlabel'):
            return True
    return False

def find_symbols(root, pinout):
    for dirpath, dirnames, filenames in os.walk(root):
        for filename in filenames:
            if not filename.endswith('.sym'):
                continue
            path = os.path.join(dirpath, filename)
            try:
                rev = xorn.geda.read.read(path)
            except UnicodeDecodeError as e:
                sys.stderr.write(_("%s: can't read %s: %s\n")
                                 % (xorn.command.program_short_name,
                                    path, str(e)))
                continue
            except xorn.geda.read.ParseError:
                sys.stderr.write(_("%s: can't read %s: %s\n")
                                 % (xorn.command.program_short_name,
                                    path, _("parse error")))
                continue

            ok = True
            for pinnumber in pinout:
                if not has_pin(rev, pinnumber, pinout[pinnumber]):
                    ok = False
                    break
            if ok:
                print path

def main():
    try:
        options, args = getopt.getopt(
            xorn.command.args, '', ['help', 'version'])
    except getopt.GetoptError as e:
        xorn.command.invalid_arguments(e.msg)

    for option, value in options:
        if option == '--help':
            sys.stdout.write(_("Usage: %s PATH PINNUMBER:PINLABEL...\n")
                             % xorn.command.program_name)
            sys.stdout.write(_(
"Find symbols in a directory matching a given pinout\n"))
            sys.stdout.write("\n")
            sys.stdout.write(_(
"      --help            give this help\n"
"      --version         display version number\n"))
            sys.stdout.write("\n")
            sys.stdout.write(_("Report %s bugs to %s\n")
                             % (xorn.config.PACKAGE_NAME,
                                xorn.config.PACKAGE_BUGREPORT))
            sys.exit(0)

        if option == '--version':
            xorn.command.core_version()

    if len(args) < 2:
        xorn.command.invalid_arguments(_("too few arguments"))

    pinout = {}
    for arg in args[1:]:
        try:
            pos = arg.index(':')
        except ValueError:
            xorn.command.invalid_arguments(_("missing colon in parameter"))
        pinout[arg[:pos]] = arg[pos + 1:]

    find_symbols(args[0], pinout)
