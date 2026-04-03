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

;;; World geometry.

(define-module (schematic world-size)
  #:use-module (schematic ffi)

  #:export (world-left
            world-right
            world-top
            world-bottom))

(define %world-default-left (schematic_world_size_get_default_left))
(define %world-default-right (schematic_world_size_get_default_right))
(define %world-default-top (schematic_world_size_get_default_top))
(define %world-default-bottom (schematic_world_size_get_default_bottom))

(define (world-left)
  "Returns the minimum left world X coordinate."
  %world-default-left)

(define (world-right)
  "Returns the maximum right world X coordinate."
  %world-default-right)

(define (world-bottom)
  "Returns the minimum bottom world Y coordinate."
  %world-default-bottom)

(define (world-top)
  "Returns the maximum top world Y coordinate."
  %world-default-top)
