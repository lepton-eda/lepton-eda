;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


(define-module (schematic viewport)
  #:use-module (schematic ffi)
  #:use-module (schematic viewport foreign)

  #:export (viewport-left
            viewport-right
            viewport-top
            viewport-bottom
            viewport-center))

(define (viewport-left viewport)
  "Returns the minimum left world X coordinate of VIEWPORT."
  (define *viewport (check-viewport viewport 1))
  (schematic_viewport_get_left *viewport))


(define (viewport-right viewport)
  "Returns the maximum right world X coordinate of VIEWPORT."
  (define *viewport (check-viewport viewport 1))
  (schematic_viewport_get_right *viewport))


(define (viewport-bottom viewport)
  "Returns the minimum bottom world Y coordinate of VIEWPORT."
  (define *viewport (check-viewport viewport 1))
  (schematic_viewport_get_bottom *viewport))


(define (viewport-top viewport)
  "Returns the maximum top world Y coordinate of VIEWPORT."
  (define *viewport (check-viewport viewport 1))
  (schematic_viewport_get_top *viewport))


(define (center min-coord max-coord)
  (round (/ (+ min-coord max-coord) 2)))


(define (viewport-center viewport)
  "Return coordinates of the VIEWPORT center point."
  (cons (center (viewport-left viewport)
                (viewport-right viewport))
        (center (viewport-top viewport)
                (viewport-bottom viewport))))
