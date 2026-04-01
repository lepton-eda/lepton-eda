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
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

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


(define* (zoom window canvas #:key (direction #f) (position #f))
  "Zoom CANVAS of WINDOW.  DIRECTION is a symbol which can be 'zoom-in,
'zoom-out, 'zoom-full, or 'zoom-same.  If the configuration key
\"zoom-with-pan\" in the \"schematic.gui\" group is true, and POSITION
is not #f, zooming with panning is enabled."
  (define (center min-coord max-coord)
    (/ (+ min-coord max-coord) 2))

  (define (viewport-center *viewport)
    (cons (center (schematic_viewport_get_left *viewport)
                  (schematic_viewport_get_right *viewport))
          (center (schematic_viewport_get_top *viewport)
                  (schematic_viewport_get_bottom *viewport))))

  (define (pan-center-coord viewport-min
                            viewport-max
                            start-coord
                            relative-zoom-factor)
    (+ (/ (- (center viewport-min viewport-max) start-coord)
          relative-zoom-factor)
       start-coord))

  (define (pan-center *viewport pan-position relative-zoom-factor)
    (cons (pan-center-coord (schematic_viewport_get_left *viewport)
                            (schematic_viewport_get_right *viewport)
                            (car pan-position)
                            relative-zoom-factor)
          (pan-center-coord (schematic_viewport_get_bottom *viewport)
                            (schematic_viewport_get_top *viewport)
                            (cdr pan-position)
                            relative-zoom-factor)))

  (define *window (check-window window 1))
  (define *canvas (check-canvas canvas 2))
  (define *viewport (viewport->pointer (canvas-viewport canvas)))
  (define zoom-gain (schematic_window_get_zoom_gain *window))
  (define show-all? (eq? direction 'zoom-full))
  (define zoom-with-pan?
    (true? (schematic_window_get_zoom_with_pan *window)))
  (define warp-cursor?
    (true? (schematic_window_get_warp_cursor *window)))

  ;; Skip calculations when no zoom change is requested.
  (unless (eq? direction 'zoom-same)

    ;; NB: zoom-gain is a percentage increase.
    (let ((relative-zoom-factor
           (case direction
             ((zoom-in) (/ (+ 100.0 zoom-gain) 100.0))
             ((zoom-out) (/ 100.0 (+ 100.0 zoom-gain)))
             ;; Indicate the zoom full with a negative zoomfactor.
             ((zoom-full) -1)
             (else -1))))

      ;; Depending on the configuration settings, the new viewport
      ;; center is either the current mouse position if the cursor
      ;; should be warped, the current center, or a new virtual
      ;; center.
      (let* ((zoom-center
              (if (and zoom-with-pan?
                       ;; Position is undefined when the
                       ;; pointer is out of the canvas.  In
                       ;; such a case panning cannot be done.
                       position
                       ;; Panning cannot be used when the
                       ;; canvas should be displayed at its
                       ;; full size.
                       (not show-all?))
                  (if warp-cursor?
                      position
                      (pan-center *viewport
                                  position
                                  relative-zoom-factor))
                  (viewport-center *viewport))))
        ;; Calculate new viewport and draw it.
        (schematic_canvas_pan_general
         *canvas
         (inexact->exact (round (car zoom-center)))
         (inexact->exact (round (cdr zoom-center)))
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
                    (inexact->exact (round (car zoom-center)))))
                (y (schematic_viewport_pix_y
                    *viewport
                    (inexact->exact (round (cdr zoom-center))))))
            (x_basic_warp_cursor *canvas x y)))))))
