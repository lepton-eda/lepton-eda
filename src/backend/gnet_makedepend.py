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

# Copyright (C) 2011 Dan White <dan@whiteaudio.com>

# This backend does not work properly due to the way gnetlist handles
# source= attributes.

# --------------------------------------------------------------------------
#
# Backend to determine the dependencies of a given schematic.
#
# Output is Makefile lines relevant to the input schematic(s):
#
# foo.sch: subsheetA.sch subsheetB.sch
# foo.cir: foo.sch subsheetA.cir subsheetB.cir
#
# See the following for the intended usage:
# http://www.gnu.org/software/make/manual/make.html#Automatic-Prerequisites
#
# Backend example description
# ---------------------------
# A two-page schematic (idac-1.sch idac-2.sch) contains two sub-schematic
# symbols: inv1-1.sym and idac_8p-1.sym.  `inv1-1' is implemented with a
# single page indicated by the symbol's attribute `source=inv1-1.sch'.  The
# second block `idac_8p' is a three-page schematic whose symbol has a
# `source=idac_8p-1.sch,idac_8p-2.sch,idac_8p-3.sch' attribute.  Additionally,
# the top-level schematic pages include two `spice-include-1.sym' components
# with `file=' attribute values `dactune.pwl' and `idac.param'.
#
# Such a top-level schematic depends on the following files:
#      inv1-1.sch      - `inv1' sub-schematic
#      idac_8p-1.sch   - `idac_8p' sub-schematic, page 1
#      idac_8p-2.sch   - ... page 2
#      idac_8p-3.sch   - ... page 3
#
#      Note: The symbol files inv1-1.sym etc. are also dependencies and not
#            currently handled.
#
# For top-level SPICE simulation of `idac.cir' with hierarchical subckts, the
# additional files are dependencies:
#      inv1.cir    - from `gnetlist -g spice-sdb -o inv1.cir inv1-1.sch'
#      idac_8p.cir - `gnetlist -g spice-sdb -o idac_8p.cir idac_8p-*.sch'
#      dactune.pwl - additional SPICE include file
#      idac.param  - ...
#
# Calling this backend as:
#      gnetlist -g makedepend -o idac.d idac-*.sch
#
# generates the `idac.d' file contents as:
#
# idac-1.sch idac-2.sch: inv1-1.sch idac_8p-1.sch idac_8p-2.sch idac_8p-3.sch
# idac.cir: idac-1.sch idac-2.sch inv1.cir idac_8p.cir dactune.pwl idac.param
#
# Makefile snippet for use:
# ###
# modules=idac
# depends=$(addsuffix .d, $(modules))
#
# depend: $(depends)
# idac.d: $(wildcard idac-*.sch)
#     gnetlist -g makedepend -o $@ $^
#
# include $(depends)
# ###
# --------------------------------------------------------------------------

import re, sys

# Split a filename into 3 parts: base name, page number, and extension

MAKEDEPEND_SCHEME = re.compile('(\w+)-(\d+)\.(\w+)$')
# was: '([[:alnum:]_]+)-([[:digit:]]+).([[:alpha:]]+)$'

# Return a list of all values found for the given attribute name
# over all packages in the input files.

def get_all_attr_values(attribute, packages):
    l = []
    # collect values from all packages into a list
    # get all values for a given refdes
    for package in packages:
        for value in package.get_all_attributes(attribute):
            if value is not None:
                # split individual values
                # (gschem wants a single comma-sep list for source=)
                l += value.split(',')
            # ignore non-existent values
    return l

def run(f, netlist):
    input_files = [sheet.blueprint.filename
                   for sheet in netlist.toplevel_sheets]

    # lazy version, use first filename only for naming scheme
    match = MAKEDEPEND_SCHEME.match(input_files[0])
    if match is None:
        sys.stderr.write("ERROR: Schematic file name must take the form: "
                         "BASE-PAGENUM.EXT\n")
        sys.exit(1)
    base = match.group(1)
    # page = match.group(2)
    # ext = match.group(3)

    sources = get_all_attr_values('source', reversed(netlist.packages))
    files = get_all_attr_values('file', reversed(netlist.packages))

    # schematic deps
    f.write('%s: %s\n' % (' '.join(input_files), ' '.join(sources)))

    # netlist deps
    f.write('%s.cir: %s %s\n' % (base, ' '.join(input_files), ' '.join(files)))
