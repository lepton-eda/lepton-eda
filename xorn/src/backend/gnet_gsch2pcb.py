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

# gsch2pcb format  (based on PCBboard format by JM Routoure & Stefan Petersen)
# Bill Wilson    billw@wt.net
# 6/17/2003

import re, subprocess, sys

use_m4 = False

# Let the user override the m4 command, the directory
# where pcb stores its m4 files and the pcb config directory.
pcb_m4_command = '@m4@'
pcb_m4_dir = '@pcbm4dir@'
m4_files = []

# Let the user override the m4 search path
pcb_m4_path = ['$HOME/.pcb', '.']

RX = re.compile('^[A-Za-z0-9_]*$')

# Return the footprint for a package.  If M4 footprints are enabled,
# writes in a format suitable for macro-expansion by M4.  Any
# footprint names that obviously can't be M4 footprints are protected
# from macro-expansion.

def value_footprint(package):
    value = package.get_attribute('value', 'unknown')
    footprint = [x for x in package.get_attribute(
                                'footprint', 'unknown').split(' ') if x]
    fp = footprint[0]
    fp_args = footprint[1:]
    nq = lambda x: x  # A non-quoting operator
    if use_m4:
        # Quote a string to protect from M4 macro expansion
        q = lambda x: "`%s'" % x  # A quoting operator
    else:
        q = nq

    # If the footprint is obviously not an M4 footprint,
    # protect it from macro-expansion.
    # Check if `str' contains only characters valid in an M4 function
    # name.  Note that this *doesn't* check that str is a valid M4
    # function name.
    if RX.match(fp) is not None:
        fp_ = 'PKG_' + fp
    else:
        fp_ = q('PKG_' + fp)

    return '%s(%s,%s,%s%s)\n' % (fp_,
                                 q('-'.join(footprint)),
                                 q(package.refdes),
                                 q(value),
                                 ''.join(',' + q(arg) for arg in fp_args))

def run(f, netlist):
    # Build list of the m4 argument vector
    m4_command_line_list = \
        [pcb_m4_command, '-d'] + \
        ['-I' + d for d in [pcb_m4_dir] + pcb_m4_path] + \
        [pcb_m4_dir + '/common.m4'] + m4_files + ['-']

    sys.stderr.write(MESSAGE % (pcb_m4_command,
                                pcb_m4_dir,
                                ', '.join('"%s"' % s for s in pcb_m4_path),
                                ', '.join('"%s"' % s for s in m4_files),
                                { True: 'yes', False: 'no' }[use_m4],
                                ' '.join(m4_command_line_list)))

    f.write(TOP_HEADER)

    # If we have defined use_m4 then run the footprints
    # through the pcb m4 setup.  Otherwise skip m4 entirely
    if use_m4:
        sys.stderr.write("Using the m4 processor for pcb footprints\n")
        f.flush()

        p = subprocess.Popen(m4_command_line_list,
                             stdin = subprocess.PIPE,
                             stdout = f.fileno(),
                             close_fds = True)

        for package in reversed(netlist.packages):
            p.stdin.write(value_footprint(package))
        p.stdin.close()

        status = p.wait()
        if status != 0:
            sys.stderr.write("%s returned exit status %d\n"
                             % (pcb_m4_command, status))
            sys.exit(1)
    else:
        sys.stderr.write("Skipping the m4 processor for pcb footprints\n")
        for package in reversed(netlist.packages):
            f.write(value_footprint(package))

    f.write(BOTTOM_FOOTER)

MESSAGE = """\
=====================================================
gsch2pcb backend configuration:

   ----------------------------------------
   Variables which may be changed in gafrc:
   ----------------------------------------
   pcb_m4_command:    %s
   pcb_m4_dir:        %s
   pcb_m4_path:       %s
   m4_files:          %s

   ---------------------------------------------------
   Variables which may be changed in the project file:
   ---------------------------------------------------
   use_m4:            %s

   ----------------
   M4 command line:
   ----------------
   %s

=====================================================
"""

TOP_HEADER = """\
# release: pcb 1.99x
# To read pcb files, the pcb version (or the cvs source date) must be >= the file version
FileVersion[20070407]
PCB["" 600000 500000]
Grid[10000.000000 0 0 0]
Cursor[0 0 0.000000]
PolyArea[200000000.000000]
Thermal[0.500000]
DRC[1000 1000 1000 1000 1500 1000]
Flags("nameonpcb,uniquename,clearnew,snappin")
Groups("1,c:2:3:4:5:6,s:7:8")
Styles["Signal,1000,3600,2000,1000:Power,2500,6000,3500,1000:Fat,4000,6000,3500,1000:Skinny,600,2402,1181,600"]
"""

BOTTOM_FOOTER = """\
Layer(1 "top")
(
)
Layer(2 "ground")
(
)
Layer(3 "signal2")
(
)
Layer(4 "signal3")
(
)
Layer(5 "power")
(
)
Layer(6 "bottom")
(
)
Layer(7 "outline")
(
)
Layer(8 "spare")
(
)
Layer(9 "silk")
(
)
Layer(10 "silk")
(
)
"""
