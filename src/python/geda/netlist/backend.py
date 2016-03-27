# xorn.geda.netlist - gEDA Netlist Extraction and Generation
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

## \namespace xorn.geda.netlist.backend
## Handling netlist backends.
#
# After the netlist has been constructed, it is passed to a \a backend
# which creates some kind of output based on the information in the
# netlist.  The backend is selected by the user via the command-line
# option <tt>-g \a BACKEND</tt>; a list of available options can be
# displayed using the command-line option `--list-backends`.
#
# A backend is a Python file in the special backend load path whose
# name starts with the prefix \c "gnet_".  It should contain a special
# function \c run(f, netlist) as the main entry point.

import imp, os, sys
from gettext import gettext as _

## Module name prefix for netlister backends.
#
# Only Python modules whose name starts with this prefix are
# considered netlister backends.

BACKEND_PREFIX = 'gnet_'

## Backend load path.
#
# A list of directory names in which to search for netlister backends.

load_path = []

## Get a sorted list of available backends.
#
# Returns a list of available netlister backends by searching for
# files in each of the directories given in \ref load_path.  A module
# is considered to be a netlister backend if its name begins with
# \ref BACKEND_PREFIX ("gnet_").

def list_backends():
    backend_names = set()

    for dir_name in load_path:
        try:
            d_names = os.listdir(dir_name)
        except OSError as e:
            sys.stderr.write(_("Can't open directory %s: %s\n")
                             % (dir_name, e.strerror))
            continue

        for d_name in d_names:
            # Check that filename has the right format to be a backend
            if not d_name.startswith(BACKEND_PREFIX):
                continue

            for suffix, mode, type in imp.get_suffixes():
                if d_name.endswith(suffix, len(BACKEND_PREFIX)):
                    # Remove prefix & suffix.  Add to list of backend names.
                    backend_names.add(d_name[len(BACKEND_PREFIX):-len(suffix)])
                    break

    return sorted(backend_names)

## Load a specific netlister backend.
#
# Searches in the backend load path for a module called \c
# gnet_<em>backend_name</em>, loads it, and returns the module object.
#
# The backend module's path is added to the system load path.
#
# \throws ImportError if the module could not be loaded

def load(backend_name):
    # Search for backend module in load path
    f, pathname, description = imp.find_module(
        BACKEND_PREFIX + backend_name, load_path)
    if f is None:
        # module is a package
        raise ImportError

    # Load backend code.
    sys.path.insert(0, os.path.dirname(pathname))
    try:
        return imp.load_module(BACKEND_PREFIX + backend_name,
                               f, pathname, description)
    finally:
        f.close()
