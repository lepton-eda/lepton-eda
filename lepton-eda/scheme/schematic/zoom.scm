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


(define-module (schematic zoom)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)

  #:use-module (schematic ffi)

  #:export (zoom))

;;; Definitions from "schematic_defines.h".
(define DONTCARE 0)
(define MENU 1)
(define HOTKEY 2)
(define ZOOM_OUT 0)
(define ZOOM_IN 1)
(define ZOOM_FULL 2)
(define ZOOM_SAME 3)


(define (filter-out-scroll-events)
  (a_zoom))


;;; DIRECTION is either ZOOM_IN, ZOOM_OUT or ZOOM_FULL which are
;;; defined in globals.h.
(define (zoom *window *canvas direction selected-from)
  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (let ((*viewport (schematic_canvas_get_viewport *canvas))
        (zoom-gain (schematic_window_get_zoom_gain *window)))
    (when (null-pointer? *viewport)
      (error "NULL viewport"))

    ;; NB: zoom-gain is a percentage increase.
    (let ((relative-zoom-factor
           (cond
            ((= direction ZOOM_IN) (/ (+ 100.0 zoom-gain) 100.0))
            ((= direction ZOOM_OUT) (/ 100.0 (+ 100.0 zoom-gain)))
            ;; Indicate the zoom full with a negative zoomfactor.
            ((= direction ZOOM_FULL) -1)
            ;; Don't zoom.
            ((= direction ZOOM_SAME) 1)
            (else -1)))
          (hotkey-zoom-with-pan?
           (and (true? (schematic_window_get_zoom_with_pan *window))
                (= selected-from HOTKEY)))
          (start-x-bv (make-bytevector (sizeof int) 0))
          (start-y-bv (make-bytevector (sizeof int) 0))
          (warp-cursor (schematic_window_get_warp_cursor *window)))

      (unless (and hotkey-zoom-with-pan?
                   (false? (x_event_get_pointer_position
                            *window
                            FALSE
                            (bytevector->pointer start-x-bv)
                            (bytevector->pointer start-y-bv))))
        (let* ((start-x (bytevector-sint-ref start-x-bv
                                             0
                                             (native-endianness)
                                             (sizeof int)))
               (start-y (bytevector-sint-ref start-y-bv
                                             0
                                             (native-endianness)
                                             (sizeof int)))
               (viewport-left (schematic_viewport_get_left *viewport))
               (viewport-right (schematic_viewport_get_right *viewport))
               (viewport-top (schematic_viewport_get_top *viewport))
               (viewport-bottom (schematic_viewport_get_bottom *viewport))
               (viewport-center-x (/ (+ viewport-left viewport-right) 2))
               (viewport-center-y (/ (+ viewport-top viewport-bottom) 2))
               (new-pan-center-x
                (+ (/ (- viewport-center-x start-x) relative-zoom-factor)
                   start-x))
               (new-pan-center-y
                (+ (/ (- viewport-center-y start-y) relative-zoom-factor)
                   start-y))
               (warp-cursor? (true? warp-cursor))
               ;; Depending on the configuration settings, the new
               ;; viewport center is either the current mouse
               ;; position if the cursor should be warped, the
               ;; current center, or a new virtual center.
               (pan-center (if hotkey-zoom-with-pan?
                               (if warp-cursor?
                                   (cons start-x start-y)
                                   (cons new-pan-center-x new-pan-center-y))
                               (cons viewport-center-x viewport-center-y))))
          ;; Calculate new viewport and draw it.
          (schematic_canvas_pan_general
           *canvas
           (inexact->exact (round (car pan-center)))
           (inexact->exact (round (cdr pan-center)))
           relative-zoom-factor)

          (filter-out-scroll-events)

          ;; Warp the cursor to the right position.
          (when warp-cursor?
            (let ((x (schematic_viewport_pix_x
                      *viewport
                      (inexact->exact (round (car pan-center)))))
                  (y (schematic_viewport_pix_y
                      *viewport
                      (inexact->exact (round (cdr pan-center))))))
              (x_basic_warp_cursor *canvas x y))))))))
