;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024-2025 Lepton EDA Contributors
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
  #:use-module (lepton log)

  #:use-module (schematic ffi)
  #:use-module (schematic canvas foreign)
  #:use-module (schematic event)
  #:use-module (schematic viewport foreign)

  #:export (canvas-viewport
            invalidate-canvas
            *redraw-canvas
            scroll-canvas
            *scroll-canvas))


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


(define (scroll-canvas *widget *event *window)
  (define (state-contains? state mask)
    (if (logtest state mask) 1 0))

  (when (null-pointer? *window)
    (error "NULL window"))
  (when (null-pointer? *widget)
    (error "NULL canvas"))

  (let ((*page (schematic_canvas_get_page *widget)))
    (if (null-pointer? *page)
        ;; We cannot zoom or scroll a page if it doesn't exist :)
        FALSE
        (let ((alt-mask (schematic_event_alt_mask))
              (control-mask (schematic_event_control_mask))
              (shift-mask (schematic_event_shift_mask))
              (state (event-state *event)))
          ;; Update the state of the modifiers.
          (schematic_window_set_shift_key_pressed *window
                                                  (state-contains? state shift-mask))
          (schematic_window_set_control_key_pressed *window
                                                    (state-contains? state control-mask))
          (schematic_window_set_alt_key_pressed *window
                                                (state-contains? state alt-mask))
          ;; There are two scrolling types defined in gschem_defines.h:
          ;;   SCROLL_WHEEL_CLASSIC = 0
          ;;   SCROLL_WHEEL_GTK = 1
          (let ((scrolling-type (schematic_window_get_scroll_wheel *window))
                (scroll-direction (event-direction *event)))
            (x_event_scroll *widget
                            *event
                            *window
                            scrolling-type
                            (or scroll-direction 0)
                            (if scroll-direction TRUE FALSE)))))))

(define *scroll-canvas
  (procedure->pointer int scroll-canvas '(* * *)))
