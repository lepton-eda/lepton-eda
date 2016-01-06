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

# Backend for propagating pin names from gschem to footprints in pcb
# Copyright (C) 2005-2010 Dan McMahill

# A comma or close parenthesis will cause problems with the pcb action
# script, so if one of the arguments to ChangePinName contains one it
# should be quoted.  Any quote characters within the argument are
# escaped.
#
# At present, this function only quotes if there is a comma or close
# parenthesis present in the string.

def quote_string(s):
    if ',' not in s and ')' not in s:
        return s
    return '"' + s.replace('"', '\"') + '"'

def run(f, netlist):
    f.write('# Pin name action command file\n')
    for package in reversed(netlist.packages):
        f.write('\n')
        f.write('# Start of element %s\n' % package.refdes)
        for pin in reversed(package.pins):
            pinnum = pin.get_attribute('pinnumber', 'unknown')
            label = pin.get_attribute('pinlabel', pinnum)
            f.write('ChangePinName(%s, %s, %s)\n' % (
                quote_string(package.refdes),
                quote_string(pinnum),
                quote_string(label)))
