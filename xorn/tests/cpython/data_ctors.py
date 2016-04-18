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

import xorn.storage

la = xorn.storage.LineAttr(width = 11.,
                           cap_style = 12,
                           dash_style = 13,
                           dash_length = 14.,
                           dash_space = 15.)
assert la.width == 11.
assert la.cap_style == 12
assert la.dash_style == 13
assert la.dash_length == 14.
assert la.dash_space == 15.

fa = xorn.storage.FillAttr(type = 21,
                           width = 22.,
                           angle0 = 23,
                           pitch0 = 24.,
                           angle1 = 25,
                           pitch1 = 26.)
assert fa.type == 21
assert fa.width == 22.
assert fa.angle0 == 23
assert fa.pitch0 == 24.
assert fa.angle1 == 25
assert fa.pitch1 == 26.

ob = xorn.storage.Arc(x = 31.,
                      y = 32.,
                      radius = 33.,
                      startangle = 34,
                      sweepangle = 35,
                      color = 36,
                      line = la)

assert ob.x == 31.
assert ob.y == 32.
assert ob.radius == 33.
assert ob.startangle == 34
assert ob.sweepangle == 35
assert ob.color == 36
assert ob.line == la

ob = xorn.storage.Box(x = 41.,
                      y = 42.,
                      width = 43.,
                      height = 44.,
                      color = 45,
                      line = la,
                      fill = fa)

assert ob.x == 41.
assert ob.y == 42.
assert ob.width == 43.
assert ob.height == 44.
assert ob.color == 45
assert ob.line == la
assert ob.fill == fa

ob = xorn.storage.Circle(x = 51.,
                         y = 52.,
                         radius = 53.,
                         color = 54,
                         line = la,
                         fill = fa)

assert ob.x == 51.
assert ob.y == 52.
assert ob.radius == 53.
assert ob.color == 54
assert ob.line == la
assert ob.fill == fa

ob = xorn.storage.Component(x = 61.,
                            y = 62.,
                            selectable = True,
                            angle = 63,
                            mirror = True)

assert ob.x == 61.
assert ob.y == 62.
assert ob.selectable == True
assert ob.angle == 63
assert ob.mirror == True

ob = xorn.storage.Line(x = 71.,
                       y = 72.,
                       width = 73.,
                       height = 74.,
                       color = 75,
                       line = la)

assert ob.x == 71.
assert ob.y == 72.
assert ob.width == 73.
assert ob.height == 74.
assert ob.color == 75
assert ob.line == la

ob = xorn.storage.Net(x = 81.,
                      y = 82.,
                      width = 83.,
                      height = 84.,
                      color = 85,
                      is_bus = True,
                      is_pin = True,
                      is_inverted = True)

assert ob.x == 81.
assert ob.y == 82.
assert ob.width == 83.
assert ob.height == 84.
assert ob.color == 85
assert ob.is_bus == True
assert ob.is_pin == True
assert ob.is_inverted == True

ob = xorn.storage.Path(pathdata = "91",
                       color = 92,
                       line = la,
                       fill = fa)

assert ob.pathdata == "91"
assert ob.color == 92
assert ob.line == la
assert ob.fill == fa

ob = xorn.storage.Picture(x = 101.,
                          y = 102.,
                          width = 103.,
                          height = 104.,
                          angle = 105,
                          mirror = True)

assert ob.x == 101.
assert ob.y == 102.
assert ob.width == 103.
assert ob.height == 104.
assert ob.angle == 105
assert ob.mirror == True

ob = xorn.storage.Text(x = 111.,
                       y = 112.,
                       color = 113,
                       text_size = 114,
                       visibility = True,
                       show_name_value = 115,
                       angle = 116,
                       alignment = 117,
                       text = "118")

assert ob.x == 111.
assert ob.y == 112.
assert ob.color == 113
assert ob.text_size == 114
assert ob.visibility == True
assert ob.show_name_value == 115
assert ob.angle == 116
assert ob.alignment == 117
assert ob.text == "118"
