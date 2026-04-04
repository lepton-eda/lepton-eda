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
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)

  #:use-module (schematic canvas foreign)
  #:use-module (schematic canvas)
  #:use-module (schematic ffi)
  #:use-module (schematic viewport foreign)
  #:use-module (schematic viewport)
  #:use-module (schematic window foreign)
  #:use-module (schematic world-size)

  #:export (zoom-in
            zoom-out
            zoom-world))


(define (pan canvas position zoom-factor)
  "Pan the viewport of CANVAS at POSITION zooming it with ZOOM-FACTOR.
If POSITION is #f, use the viewport center instead."
  (define *canvas (check-canvas canvas 1))
  (define viewport (canvas-viewport canvas))
  (define pan-position (or position (viewport-center viewport)))

  (schematic_canvas_pan_general
   *canvas
   (round (car pan-position))
   (round (cdr pan-position))
   ;; The zoom factor is 'double' in C.
   (exact->inexact zoom-factor)))


(define (warp-pointer-at-canvas-position canvas position)
  (define *canvas (check-canvas canvas 1))
  (define viewport (canvas-viewport canvas))
  (define *viewport (viewport->pointer viewport))

  (check-coord position 2)

  (let ((x (schematic_viewport_pix_x
            *viewport
            (round (car position))))
        (y (schematic_viewport_pix_y
            *viewport
            (round (cdr position)))))
    (x_basic_warp_cursor *canvas x y)))


;; Warp the mouse pointer.  If the pointer is out of the canvas, don't
;; warp it as moving it to an arbitrary position may be misleading.
(define (warp-pointer canvas position)
  (when position
    (warp-pointer-at-canvas-position canvas position)))


(define (zoom-pan-center viewport position zoom-factor)
  "Calculate new center for VIEWPORT when zooming with panning is
started at POSITION and the relative zoom factor is ZOOM-FACTOR.
The new center and POSITION are in world coordinates in the
form (X . Y)."
  (define (center min-coord max-coord)
    (/ (+ min-coord max-coord) 2))

  (define (pan-center-coord viewport-min
                            viewport-max
                            start-coord)
    (+ (/ (- (center viewport-min viewport-max) start-coord)
          zoom-factor)
       start-coord))

  (cons (pan-center-coord (viewport-left viewport)
                          (viewport-right viewport)
                          (car position))
        (pan-center-coord (viewport-bottom viewport)
                          (viewport-top viewport)
                          (cdr position))))


(define* (zoom window canvas zoom-factor #:key (position #f))
  "Zoom CANVAS of WINDOW.  DIRECTION is a symbol which can be
'zoom-in or 'zoom-out.  If the configuration key \"zoom-with-pan\" in
the \"schematic.gui\" group is true, and POSITION is not #f, zooming
with panning is enabled."
  (define *window (check-window window 1))
  (define viewport (canvas-viewport canvas))
  (define zoom-with-pan?
    (true? (schematic_window_get_zoom_with_pan *window)))
  (define warp-cursor?
    (true? (schematic_window_get_warp_cursor *window)))

  ;; Depending on the configuration settings, the new viewport center
  ;; is either the current mouse position if the cursor should be
  ;; warped, the current center, or a new virtual center.
  (if warp-cursor?
      (begin
        ;; POSITION is undefined when the pointer is out of the
        ;; canvas.  If POSITION is defined, make it the new viewport
        ;; center.  Otherwise, zoom with no panning at the current
        ;; viewport center.
        (pan canvas position zoom-factor)

        ;; Warp the cursor to the new center position.
        (warp-pointer canvas position))

      ;; If the mouse pointer is over the canvas and the
      ;; "zoom-with-pan" configuration setting is set to "true", do
      ;; zooming with panning.  Otherwise, zoom with no panning at the
      ;; current viewport center.
      (pan canvas
           (and zoom-with-pan?
                position
                (zoom-pan-center viewport position zoom-factor))
           zoom-factor)))

;;; NB: zoom-gain is a percentage increase.
(define (zoom-gain window)
  (define *window (check-window window 1))
  (schematic_window_get_zoom_gain *window))


(define (zoom-in window canvas position)
  "Zoom in CANVAS with settings from WINDOW at POSITION in world
coordinates."
  (define zoom-factor (/ (+ 100 (zoom-gain window)) 100))
  (zoom window canvas zoom-factor #:position position))


(define (zoom-out window canvas position)
  "Zoom out CANVAS with settings from WINDOW at POSITION in world
coordinates."
  (define zoom-factor (/ 100 (+ 100 (zoom-gain window))))
  (zoom window canvas zoom-factor #:position position))


(define (zoom-world canvas)
  "Zoom CANVAS to display all world coordinate space."
  (define *canvas (check-canvas canvas 1))

  ;; The negative zoom factor indicates that the world coordinate
  ;; space has to be displayed at its full size.
  (pan canvas (world-center) -1))
