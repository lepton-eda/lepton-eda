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

## \namespace xorn.geda.xmlformat
## Common definitions for XML file format.

NAMESPACE = 'https://hedmen.org/xorn/schematic/'

ENUM_BOOLEAN = ['no', 'yes']
ENUM_CAPSTYLE = ['none', 'square', 'round']
ENUM_DASHSTYLE = ['solid', 'dotted', 'dashed', 'center', 'phantom']
ENUM_FILLTYPE = ['hollow', 'fill', 'mesh', 'hatch', 'void']
ENUM_SHOW_NAME_VALUE = ['name-value', 'value', 'name']
ENUM_ALIGNMENT = ['lower-left', 'middle-left', 'upper-left',
                  'lower-middle', 'middle-middle', 'upper-middle',
                  'lower-right', 'middle-right', 'upper-right']
ENUM_NETTYPE = ['normal', 'bus']
ENUM_COLOR = ['background', 'pin', 'net-endpoint', 'graphic', 'net',
              'attribute', 'logic-bubble', 'dots-grid', 'detached-attribute',
              'text', 'bus', 'select', 'boundingbox', 'zoom-box', 'stroke',
              'lock', 'output-background',
              'freestyle1', 'freestyle2', 'freestyle3', 'freestyle4',
              'junction', 'mesh-grid-major', 'mesh-grid-minor']
