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

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (zoom))


(define (filter-out-scroll-events)
  (let loop ((*event (gdk_event_get)))
    (unless (null-pointer? *event)
      (if (false? (schematic_event_is_scroll *event))
          (begin
            (gdk_event_put *event)
            (gdk_event_free *event))
          (begin
            (gdk_event_free *event)
            (loop (gdk_event_get)))))))


(define* (zoom *window *canvas #:key (direction #f) (position #t))
  "Zoom *CANVAS of *WINDOW.  DIRECTION is a symbol which can be 'zoom-in,
'zoom-out, 'zoom-full, or 'zoom-same.  If the configuration key
\"zoom-with-pan\" in the \"schematic.gui\" group is true, and POSITION
is not #f, zooming with panning is enabled."
  (define (center min-coord max-coord)
    (/ (+ min-coord max-coord) 2))

  (define (pan-center-coord viewport-min
                            viewport-max
                            start-coord
                            relative-zoom-factor)
    (+ (/ (- (center viewport-min viewport-max) start-coord)
          relative-zoom-factor)
       start-coord))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (let ((*viewport (schematic_canvas_get_viewport *canvas))
        (zoom-gain (schematic_window_get_zoom_gain *window))
        (zoom-with-pan?
         (true? (schematic_window_get_zoom_with_pan *window))))
    (when (null-pointer? *viewport)
      (error "NULL viewport"))

    ;; Skip calculations when no zoom change is requested.
    (unless (eq? direction 'zoom-same)

      ;; NB: zoom-gain is a percentage increase.
      (let ((relative-zoom-factor
             (case direction
               ((zoom-in) (/ (+ 100.0 zoom-gain) 100.0))
               ((zoom-out) (/ 100.0 (+ 100.0 zoom-gain)))
               ;; Indicate the zoom full with a negative zoomfactor.
               ((zoom-full) -1)
               (else -1)))
            (start-x-bv (make-bytevector (sizeof int) 0))
            (start-y-bv (make-bytevector (sizeof int) 0))
            (warp-cursor (schematic_window_get_warp_cursor *window)))

        (unless (and zoom-with-pan?
                     position
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
                 (warp-cursor? (true? warp-cursor))
                 ;; Depending on the configuration settings, the new
                 ;; viewport center is either the current mouse
                 ;; position if the cursor should be warped, the
                 ;; current center, or a new virtual center.
                 (pan-center (if (and zoom-with-pan? position)
                                 (if warp-cursor?
                                     (cons start-x start-y)
                                     (cons (pan-center-coord viewport-left
                                                             viewport-right
                                                             start-x
                                                             relative-zoom-factor)
                                           (pan-center-coord viewport-bottom
                                                             viewport-top
                                                             start-y
                                                             relative-zoom-factor)))
                                 (cons (center viewport-left viewport-right)
                                       (center viewport-top viewport-bottom)))))
            ;; Calculate new viewport and draw it.
            (schematic_canvas_pan_general
             *canvas
             (inexact->exact (round (car pan-center)))
             (inexact->exact (round (cdr pan-center)))
             relative-zoom-factor)

            ;; Before warping the cursor, filter out any consecutive
            ;; scroll events from the event queue.  If the program
            ;; receives more than one scroll event before it can process
            ;; the first one, then the globals mouse_x and mouse_y won't
            ;; contain the proper mouse position, because the handler
            ;; for the mouse moved event needs to run first to set these
            ;; values.
            (filter-out-scroll-events)

            ;; Warp the cursor to the right position.
            (when warp-cursor?
              (let ((x (schematic_viewport_pix_x
                        *viewport
                        (inexact->exact (round (car pan-center)))))
                    (y (schematic_viewport_pix_y
                        *viewport
                        (inexact->exact (round (cdr pan-center))))))
                (x_basic_warp_cursor *canvas x y)))))))))
