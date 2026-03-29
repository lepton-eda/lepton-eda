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
                (= selected-from HOTKEY))))

      (a_zoom *window
              *canvas
              *viewport
              relative-zoom-factor
              (if hotkey-zoom-with-pan? TRUE FALSE)))))
