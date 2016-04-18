# Guile example: A Logo-like tortoise drawer
# Copyright (C) David Drysdale, Daniel Kraft
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

# This example shows how to embed Guile as a script interpreter into a
# Python application using the xorn.guile module.  It defines a set of
# functions for controlling a drawing tortoise, makes them available
# by calling xorn.guile.define which creates a top-level Scheme
# binding for each function, and then runs the example script file
# `guile-example.scm' using xorn.guile.load.
#
# In order to run this example, the built Xorn packages need to be in
# your PYTHONPATH.  This means you have to either
# - build Xorn and add the directory `built-packages' inside the build
#   tree to your PYTHONPATH,
# - build and install Xorn and add PREFIX/lib/python2.7/site-packages
#   to your PYTHONPATH, or
# - build and install Xorn to a default prefix (usually /usr/local).

import math, subprocess, sys
import xorn.guile

WIDTH = 10
HEIGHT = 10

pipe = None

x = 0.
y = 0.
direction = 0.
pendown = True

def draw_line(x1, y1, x2, y2):
    pipe.write("plot [0:1] %f + %f * t, %f + %f * t notitle\n" % (
        x1, x2 - x1, y1, y2 - y1))
    pipe.flush()

def tortoise_reset():
    global x, y, direction, pendown
    x = y = 0.
    direction = 0.
    pendown = True

    pipe.write("clear\n")
    pipe.flush()

def tortoise_pendown():
    global pendown
    result = pendown
    pendown = True
    return result

def tortoise_penup():
    global pendown
    result = pendown
    pendown = False
    return result

def tortoise_turn(degrees):
    global direction
    direction += math.pi / 180. * degrees
    return direction * 180. / math.pi

def tortoise_move(length):
    global x, y
    newX = x + length * math.cos(direction)
    newY = y + length * math.sin(direction)

    if pendown:
        draw_line(x, y, newX, newY)

    x = newX
    y = newY
    return x, y

if __name__ == '__main__':
    p = subprocess.Popen(['gnuplot'], stdin = subprocess.PIPE)
    pipe = p.stdin

    pipe.write("set multiplot\n")
    pipe.write("set parametric\n")
    pipe.write("set xrange [-%d:%d]\n" % (WIDTH, WIDTH))
    pipe.write("set yrange [-%d:%d]\n" % (HEIGHT, HEIGHT))
    pipe.write("set size ratio -1\n")
    pipe.write("unset xtics\n")
    pipe.write("unset ytics\n")
    pipe.flush()

    xorn.guile.define('tortoise-reset', tortoise_reset)
    xorn.guile.define('tortoise-penup', tortoise_penup)
    xorn.guile.define('tortoise-pendown', tortoise_pendown)
    xorn.guile.define('tortoise-turn', tortoise_turn)
    xorn.guile.define('tortoise-move', tortoise_move)

    xorn.guile.load('guile-example.scm')

    sys.stdin.readline()
