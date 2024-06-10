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

  #:use-module (lepton ffi boolean)

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


;;; The function redraws the canvas *WIDGET in *WINDOW when an
;;; appropriate signal is received ("draw" for GTK3, and
;;; "expose-event" for GTK2).  The second argument,
;;; *CAIRO-CONTEXT-OR-EVENT, is cairo context of the canvas for
;;; GTK3, and a Gdk event instance for GTK2.  The function always
;;; returns FALSE to propagate the event further.
(define (redraw-canvas *widget *cairo-context-or-event *window)
  (schematic_canvas_redraw *widget *cairo-context-or-event *window)
  FALSE)

(define *redraw-canvas
  (procedure->pointer int redraw-canvas '(* * *)))
