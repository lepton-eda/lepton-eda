divert(-1)
#
#                             COPYRIGHT
#
#   PCB, interactive printed circuit board design
#   Copyright (C) 1994,1995,1996 Thomas Nau
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
#   Contact addresses for paper mail and Email:
#   Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
#   Thomas.Nau@rz.uni-ulm.de
#
#
# common defines for packages
#
# -------------------------------------------------------------------
# create a single object
# $1: mask name
# $2: 'value' of the new object
# $3: package of the circuit
#
define(`CreateObject',
        `ifdef(`PinList_$1', `DefinePinList(PinList_$1)')'
        `PKG_$3(`Description_$1', ,``$2'', Param1_$1, Param2_$1)'
)

# this one is used to show the correct value for the footprint attribute
# in a gschem (www.geda.seul.org) schematic.  See QueryLibrary.sh
define(`QueryObject',
        `ifdef(`PinList_$1', `DefinePinList(PinList_$1)')'
`$3 ifdef(`Param1_$1', `Param1_$1') ifdef(`Param2_$1', `Param2_$1')'
)

# -------------------------------------------------------------------
# define for-loops like the manual tells us
#
define(`forloop',
        `pushdef(`$1', `$2')_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_forloop',
        `$4`'ifelse(eval($1 < `$3'), 1,
        `define(`$1', incr($1))_forloop(`$1', `$2', `$3', `$4')')')

# -------------------------------------------------------------------
# the following definitions evaluate the list of pin-names
# missing names will be defined as 'P_#'
#
# the first two arguments are skipped
#
define(`PIN', `Pin($1 $2 $3 $4 ifdef(`P_$5', "P_$5", "$5") ifelse($5, 1, 0x101, 0x01))')
define(`PAD', `Pad($1 $2 $3 $4 $5 ifdef(`P_$6', "P_$6", "$6") ifelse($6, 1, 0x00, 0x100))')

define(`EDGECONN', `Pad($1 $2 $3 $4 $5 ifdef(`P_$6', "P_$6", "$6") "$6" $7)')
define(`DEFPIN', `define(`count', incr(count))' `define(`P_'count, $1)')
define(`DefinePinList', `ifelse($#, 1, ,
        `pushdef(`count')'
        `define(`count', 0)'
        `_DEFPINLIST($@)'
        `popdef(`count')')')
define(`_DEFPINLIST', `ifelse($#, 0, , $#, 1, `DEFPIN(`$1')',
        `DEFPIN(`$1')'`
        _DEFPINLIST(shift($@))')')

define(`args',`
        ifelse($#, 0, , $#, 1,`define(`arg'cnt,`$1')',
        `define(`arg'cnt,`$1') define(`cnt',incr(cnt)) args(shift($@))')')

define(`PKG_CONNECTOR',
        `define(`MAXY', `eval(`$4' * 100)')
        define(`MAXX', `eval(`$5' * 100)')
Element(0x00 "$1" "`$2'" "$3" eval(MAXX + 60) 0 3 100 0x00)
(
        forloop(`row', 1, $4, `forloop(`col', 1, $5,
                `PIN(eval(col * 100 -50), eval(row * 100 -50), 60, 38, eval((row-1)*$5+col))
        ') ')
        ElementLine(0 0 0 MAXY 10)
        ElementLine(0 MAXY MAXX MAXY 10)
        ElementLine(MAXX MAXY MAXX 0 10)
        ElementLine(MAXX 0 0 0 10)
        ElementLine(0 100 100 100 10)
        ElementLine(100 100 100 0 10)
        Mark(50 50)
)')

# -------------------------------------------------------------------
# the definition of a dual-inline package N and similar types
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of pins
# $5: package size (300, 600, 900 + 100 for socket space)
# $6: pin spacing
# $7: pad size
# $8: drill size
#
define(`PKG_DIL',
        `
# retain backwards compatibility to older versions of PKG_DIL
# which did not have $6,$7,$8 args

        ifelse("`$6'","",
                `define(`PINSPACE', `100')'
        ,
                `define(`PINSPACE', eval(`$6'))'
        )
        ifelse("`$7'","",
                `define(`PADSIZE', `60')'
        ,
                `define(`PADSIZE', `$7')'
        )
        ifelse("`$8'","",
                `define(`DRILLSIZE', `28')'
        ,
                `define(`DRILLSIZE', `$8')'
        )
        define(`MAXY', `eval(`$4' / 2 * PINSPACE)')
        define(`MAXX', `eval(`$5' + 100)')
        define(`CENTERX', `eval(MAXX / 2)')
        define(`MARKY', `eval(PINSPACE / 2)')
Element(0x00 "$1" "`$2'" "$3" eval(CENTERX + 20) 100 3 100 0x00)
(
        forloop(`i', 1, eval($4 / 2),
                `PIN(50, eval((2*i-1) * PINSPACE/2),
                        eval(PADSIZE), eval(DRILLSIZE), i)
        ')
        forloop(`i', 1, eval($4 / 2),
                `PIN(eval(MAXX -50), eval(MAXY - (2*i-1) * PINSPACE/2),
                        eval(PADSIZE), eval(DRILLSIZE), eval(i + $4/2))
        ')
        ElementLine(0 0 0 MAXY 10)
        ElementLine(0 MAXY MAXX MAXY 10)
        ElementLine(MAXX MAXY MAXX 0 10)
        ElementLine(0 0 eval(CENTERX - 50) 0 10)
        ElementLine(eval(CENTERX + 50) 0 MAXX 0 10)
        ElementArc(CENTERX 0 50 50 0 180 10)
        Mark(50 MARKY)
)')

# -------------------------------------------------------------------
# the definition of a resistor (0.25W) package
# $1: canonical name
# $2: name on PCB
# $3: value
define(`PKG_R025',
`Element(0x00 "$1" "`$2'" "$3" 120 30 0 100 0x00)
(
        PIN(0, 50, 68, 38, 1)
        PIN(400, 50, 68, 38, 2)
        ElementLine(100 0 300 0 20)
        ElementLine(300 0 300 100 20)
        ElementLine(300 100 100 100 20)
        ElementLine(100 100 100 0 20)
        ElementLine(0 50 100 50 20)
        ElementLine(300 50 400 50 20)
        Mark(0 50)
)')

# -------------------------------------------------------------------
# a TO92 housing
#
# $1: canonical name
# $2: name on PCB
# $3: value
#
# by Volker Bosch (bosch@iema.e-technik.uni-stuttgart.de)
# lineare Anordnung der Pins
define(`PKG_TO92',
`Element(0x00 "$1" "`$2'" "$3" 60 70 0 100 0x00)
(

# The JEDEC drawing shows a pin diameter of 16-21 mils
#
#
#         _______
# TO92:  | 1 2 3 |   <-- bottom view
#         \_____/
#
# The pin to pin spacing is 100 mils.
        PIN(250, 200, 72, 42, 1)
        PIN(150, 200, 72, 42, 2)
        PIN(50, 200, 72, 42, 3)

        ElementArc(150 200 100 100 315 270 10)
        ElementLine( 80 130 220 130 10)

        Mark(50 200)
)')

divert(0)dnl
