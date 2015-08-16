# xorn.geda.netlist - gEDA Netlist Extraction and Generation
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

## \file util_alias.py
#
# Functions for dealing with naming requirements for different output
# netlist formats which may be more restrictive than gEDA's internals.

import sys
from gettext import gettext as _

## Build a hash table with the net name mappings.
#
# While doing so, check for any shorts which are created by modifying
# the netnames.  If a short occurs, error out with a descriptive
# message.

def build_net_aliases(alias_func, nets):
    net_hash_forward = {}  # mapping from net object to netlist net name
    net_hash_reverse = {}  # mapping from netlist net name to net object

    for net in nets:
        alias = alias_func(net)

        if alias in net_hash_reverse:
            net.error(_("net name collision: will be remapped to \"%s\" "
                        "which is already used by the net called \"%s\"")
                      % (alias, net_hash_reverse[alias].name))
            net.error(_(
                "This may be caused by netname attributes colliding with "
                "others due to truncation of the name, case insensitivity, "
                "or other limitations imposed by this netlist format."))

        net_hash_forward[net] = alias
        net_hash_reverse[alias] = net

    return net_hash_forward

## Build a hash table with the refdes mappings.
#
# While doing so, check for any name clashes which are created by
# modifying the refdes's.  If a name clash occurs, error out with a
# descriptive message.

def build_refdes_aliases(alias_func, packages):
    refdes_hash_forward = {}  # mapping from package object to netlist refdes
    refdes_hash_reverse = {}  # mapping from netlist refdes to package object

    for package in packages:
        alias = alias_func(package)

        if alias in refdes_hash_reverse:
            package.error(_("refdes name collision: will be mapped to \"%s\" "
                            "which is already used by \"%s\"")
                          % (alias, refdes_hash_reverse[alias].name))
            package.error(_(
                "This may be caused by refdes attributes colliding with "
                "others due to truncation of the refdes, case insensitivity, "
                "or other limitations imposed by this netlist format."))

        refdes_hash_forward[package] = alias
        refdes_hash_reverse[alias] = package

    return refdes_hash_forward
