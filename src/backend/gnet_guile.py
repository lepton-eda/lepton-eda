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

## \file gnet_guile.py
## Run a legacy backend written in Scheme.
#
# Backend options:
# load=FILE             load and execute FILE as Guile source
# eval=EXPR             evaluate EXPR as Guile expression
# add-to-load-path=DIR  add DIR to the Guile load path
# guile-proc=NAME       run NAME as the backend entry point (mandatory)
# verbosity=VALUE       set backend verbosity level (-1, 0 (default), or 1)
# use-spice-netnames    omit the "unknown_net" part from net names
# --                    pass all further options to the Scheme backend

import os.path
import xorn.geda.netlist.guile

def run(f, netlist, args):
    try:
        pos = args.index('--')
    except ValueError:
        passed_args = []
    else:
        passed_args = args[pos + 1:]
        del args[pos:]

    actions = []
    guile_proc = None
    verbosity = 0
    use_spice_netnames = False

    for arg in args:
        if arg == 'use-spice-netnames':
            use_spice_netnames = True
            continue

        try:
            pos = arg.index('=')
        except ValueError:
            netlist.error("invalid argument \"%s\" passed to guile backend"
                          % arg)
            return
        option = arg[:pos]
        value = arg[pos + 1:]

        if option == 'load' or option == 'eval' or \
           option == 'add-to-load-path':
            actions.append((option, value))
        elif option == 'guile-proc':
            guile_proc = value
        elif option == 'verbosity':
            if value in ['-1', '0', '1']:
                verbosity = int(value)
            else:
                netlist.error("invalid verbosity value \"%s\" passed to "
                              "guile backend" % value)
                return
        else:
            netlist.error("unknown option \"%s\" passed to guile backend"
                          % option)
            return

    if guile_proc is None:
        netlist.error("required option \"-O guile-proc=NAME\" not found")
        return

    xorn.geda.netlist.guile.the_netlist = netlist
    xorn.geda.netlist.guile.the_backend_arguments = passed_args
    xorn.geda.netlist.guile.the_verbosity = verbosity
    xorn.geda.netlist.guile.the_spice_mode = use_spice_netnames

    # Define the module "(geda deprecated)" so it can be included by
    # gnetlist.scm, but don't provide any actual functionality.
    xorn.guile.eval_string('(define-module (geda deprecated)) #f')

    for option, value in actions:
        if option == 'load':
            try:
                xorn.guile.load(value)
            except xorn.guile.GuileError:
                netlist.error("can't load \"%s\"" % value)
                return
        elif option == 'eval':
            try:
                xorn.guile.eval_string(value)
            except xorn.guile.GuileError:
                netlist.error("can't evaluate \"%s\"" % value)
                return
        elif option == 'add-to-load-path':
            if '"' in value or '\\' in value:
                netlist.error(
                    "invalid characters in load path: \"%s\"" % value)
                return
            try:
                xorn.guile.eval_string('(add-to-load-path "%s")' % value)
            except xorn.guile.GuileError:
                netlist.error(
                    "can't add \"%s\" to the Guile load path" % value)
                return

    try:
        xorn.guile.lookup(guile_proc)('/proc/self/fd/%d' % f.fileno())
    except xorn.guile.GuileError:
        # Guile has already printed an error message
        netlist.error("running the Guile backend failed")
