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

  #:use-module (schematic canvas foreign)
  #:use-module (schematic canvas)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic mouse-pointer)
  #:use-module (schematic viewport foreign)
  #:use-module (schematic viewport)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)
  #:use-module (schematic world-size)

  #:export (zoom))


(define (relative-zoom-factor zoom-gain direction)
  (case direction
    ((zoom-in) (/ (+ 100 zoom-gain) 100))
    ((zoom-out) (/ 100 (+ 100 zoom-gain)))
    ;; Indicate the zoom full with a negative zoomfactor.
    ((zoom-full) -1)
    (else #f)))


(define (pan *canvas position zoom-factor)
  "Pan the viewport of *CANVAS at POSITION zooming it with
ZOOM-FACTOR."
  (schematic_canvas_pan_general
   *canvas
   (round (car position))
   (round (cdr position))
   ;; The zoom factor is 'double' in C.
   (exact->inexact zoom-factor)))


(define (warp-pointer *canvas *viewport position)
  (let ((x (schematic_viewport_pix_x
            *viewport
            (round (car position))))
        (y (schematic_viewport_pix_y
            *viewport
            (round (cdr position)))))
    (x_basic_warp_cursor *canvas x y)))


(define* (zoom window canvas #:key (direction #f) (position #f))
  "Zoom CANVAS of WINDOW.  DIRECTION is a symbol which can be
'zoom-in, 'zoom-out, or 'zoom-full.  If the configuration key
\"zoom-with-pan\" in the \"schematic.gui\" group is true, and
POSITION is not #f, zooming with panning is enabled."
  (define (center min-coord max-coord)
    (/ (+ min-coord max-coord) 2))

  (define (pan-center-coord viewport-min
                            viewport-max
                            start-coord
                            relative-zoom-factor)
    (+ (/ (- (center viewport-min viewport-max) start-coord)
          relative-zoom-factor)
       start-coord))

  (define (pan-center viewport pan-position relative-zoom-factor)
    (cons (pan-center-coord (viewport-left viewport)
                            (viewport-right viewport)
                            (car pan-position)
                            relative-zoom-factor)
          (pan-center-coord (viewport-bottom viewport)
                            (viewport-top viewport)
                            (cdr pan-position)
                            relative-zoom-factor)))

  (define *window (check-window window 1))
  (define *canvas (check-canvas canvas 2))
  (define viewport (canvas-viewport canvas))
  (define *viewport (viewport->pointer viewport))
  (define zoom-gain (schematic_window_get_zoom_gain *window))
  (define zoom-with-pan?
    (true? (schematic_window_get_zoom_with_pan *window)))
  (define warp-cursor?
    (true? (schematic_window_get_warp_cursor *window)))

  ;; NB: zoom-gain is a percentage increase.
  (let ((zoom-factor (relative-zoom-factor zoom-gain direction)))

    ;; Depending on the configuration settings, the new viewport
    ;; center is either the current mouse position if the cursor
    ;; should be warped, the current center, or a new virtual
    ;; center.
    (let* ((zoom-center
            (cond
             ;; Display the world at its full size.
             ((eq? direction 'zoom-full)
              (world-center))
             ;; POSITION is undefined when the pointer is out of
             ;; the canvas.  If POSITION is defined, make it the
             ;; new viewport center and move the pointer there.
             ;; Otherwise use the current viewport center.
             (warp-cursor?
              (or position (viewport-center viewport)))
             ;; If the mouse pointer is over the canvas and the
             ;; "zoom-with-pan" configuration setting is set to
             ;; "true", do zooming with panning.
             ((and position zoom-with-pan?)
              (pan-center viewport position zoom-factor))
             ;; Zoom with no panning at the viewport center in all
             ;; other cases.
             (else (viewport-center viewport)))))
      ;; Calculate new viewport and draw it.
      (pan *canvas zoom-center zoom-factor)

      ;; Warp the cursor to the right position.
      (when warp-cursor?
        (warp-pointer *canvas *viewport zoom-center)))))
