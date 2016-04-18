;;; Guile example: A Logo-like tortoise drawer
;;; Copyright (C) David Drysdale, Daniel Kraft
;;; Copyright (C) 2013-2016 Roland Lutz
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; This is the example script file run by `guile-example.py' in its
;;; embedded Guile interpreter.  It draws several polygons using the
;;; tortoise control functions defined by the Python application.

(define (draw-polygon! circumference vertices)
  (let ((side (/ circumference vertices))
        (angle (/ 360 vertices)))
    (let iterate ((i 1))
      (if (<= i vertices)
        (begin
          (tortoise-move side)
          (tortoise-turn angle)
          (iterate (1+ i)))))))

(draw-polygon! 16 4)

(tortoise-penup)
(tortoise-move 1)
(tortoise-turn 30)
(tortoise-pendown)
(draw-polygon! 12 3)

(tortoise-penup)
(tortoise-move -2)
(tortoise-turn -100)
(tortoise-pendown)
(draw-polygon! 10 64)
