;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024 Lepton EDA Contributors
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


(define-module (schematic canvas)
  #:use-module (system foreign)

  #:use-module (lepton m4)

  #:use-module (schematic ffi)
  #:use-module (schematic canvas foreign)
  #:use-module (schematic viewport foreign)

  #:export (canvas-viewport
            invalidate-canvas
            *redraw-canvas))


(define (canvas-viewport canvas)
  "Return the <viewport> object of CANVAS."
  (define *canvas (check-canvas canvas 1))

  (pointer->viewport (schematic_canvas_get_viewport *canvas)))


(define (invalidate-canvas canvas)
  "Schedule redraw for the entire window CANVAS."
  (define *canvas (check-canvas canvas 1))

  (schematic_canvas_invalidate_all *canvas))


(define (redraw-canvas *widget *cairo-context-or-event *window)
  (if %m4-use-gtk3
      (x_event_draw *widget *cairo-context-or-event *window)
      (x_event_expose *widget *cairo-context-or-event *window)))

(define *redraw-canvas
  (procedure->pointer int redraw-canvas '(* * *)))
