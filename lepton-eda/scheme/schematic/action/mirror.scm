;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2025-2026 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not write to the Free Software
;;; Foundation Inc. 51 Franklin Street Fifth Floor Boston MA 02110-1301 USA.


(define-module (schematic action mirror)
  #:use-module (system foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (mirror-objects))


(define (mirror-objects *window x y *objects)
  "Mirror *OBJECTS in *WINDOW around the center point (X . Y).  The
coords are in the world units."
  (if (null-pointer? *objects)
      (begin
        (i_action_stop *window)
        (set-action-mode! 'select-mode #:window (pointer->window *window)))
      (o_mirror_world_update *window x y *objects)))
