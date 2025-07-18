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
  #:use-module (lepton m4)

  #:use-module (schematic ffi gtk)
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


;;; Temporary definitions from "gschem_defines.h".
(define ZOOM_OUT 0)
(define ZOOM_IN 1)
(define ZOOM_FULL 2)
(define ZOOM_SAME 3)

(define DONTCARE 0)
(define MENU 1)
(define HOTKEY 2)

;;; The time of the last scroll event to check for duplicate
;;; scroll events.
(define %last-scroll-event-time 0)

(define (scroll-canvas *widget *event *window)
  "Process scroll *EVENT passed to the canvas *WIDGET from its
parent *WINDOW."
  (define (state-contains? state mask)
    (if (logtest state mask) 1 0))

  (define (scroll-direction->symbol scroll-direction smooth-scroll?)
    (and scroll-direction
         (not smooth-scroll?)
         (or (eq? (event-scroll-direction->symbol scroll-direction)
                  'gdk-scroll-left)
             (eq? (event-scroll-direction->symbol scroll-direction)
                  'gdk-scroll-right))))

  (define (event-scroll-direction->pan-direction scroll-direction)
    (case (event-scroll-direction->symbol scroll-direction)
      ((gdk-scroll-up gdk-scroll-left) -1)
      ((gdk-scroll-down gdk-scroll-right) 1)
      (else 0)))

  (define (event-scroll-direction->zoom-direction scroll-direction)
    (case (event-scroll-direction->symbol scroll-direction)
      ((gdk-scroll-up) ZOOM_IN)
      ((gdk-scroll-left) ZOOM_IN)
      ((gdk-scroll-down) ZOOM_OUT)
      ((gdk-scroll-right) ZOOM_OUT)
      (else ZOOM_SAME)))

  (define (update-adjustment *adjustment pan-direction)
    (gtk_adjustment_set_value
     *adjustment
     (min (+ (gtk_adjustment_get_value *adjustment)
             (* pan-direction
                (/ (gtk_adjustment_get_page_increment *adjustment)
                   (schematic_window_get_scrollpan_steps *window))))
          (- (gtk_adjustment_get_upper *adjustment)
             (gtk_adjustment_get_page_size *adjustment)))))

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
          (let* ((classic-scrolling? (= (schematic_window_get_scroll_wheel *window) 0))
                 (scroll-direction (event-direction *event))
                 (smooth-scroll? (pair? scroll-direction))
                 (left-or-right-direction? (scroll-direction->symbol scroll-direction
                                                                     smooth-scroll?))
                 (control-pressed?
                  (true? (schematic_window_get_control_key_pressed *window)))
                 (shift-pressed?
                  (true? (schematic_window_get_shift_key_pressed *window)))
                 (zoom-by-mods?
                  (if classic-scrolling?
                      ;; Classic gschem behaviour.
                      (and (not control-pressed?) (not shift-pressed?))
                      ;; GTK style behaviour.
                      (and control-pressed? (not shift-pressed?))))
                 (zoom
                  ;; If the user has a left/right scroll
                  ;; wheel, always scroll the y-axis.
                  (and (not left-or-right-direction?)
                       zoom-by-mods?))
                 (pan-y-by-mods
                  (if classic-scrolling?
                      ;; Classic gschem behaviour.
                      (and (not control-pressed?) shift-pressed?)
                      ;; GTK style behaviour.
                      (and (not control-pressed?) (not shift-pressed?))))
                 ;; You must have scrollbars enabled if you
                 ;; want to use the scroll wheel to pan.
                 (pan-y-axis (and (true? (schematic_window_get_scrollbars_flag *window))
                                  ;; If the user has a left/right
                                  ;; scroll wheel, never scroll
                                  ;; the Y axis.
                                  (and (not left-or-right-direction?)
                                       pan-y-by-mods)))
                 (pan-x-by-mods
                  (if classic-scrolling?
                      ;; Classic gschem behaviour.
                      (and control-pressed? (not shift-pressed?))
                      ;; GTK style behaviour.
                      (and (not control-pressed?) shift-pressed?)))
                 ;; You must have scrollbars enabled if you
                 ;; want to use the scroll wheel to pan.
                 (pan-x-axis (and (true? (schematic_window_get_scrollbars_flag *window))
                                  ;; If the user has a left/right
                                  ;; scroll wheel, always scroll
                                  ;; the X axis.
                                  (or left-or-right-direction?
                                      pan-x-by-mods))))

            ;; Check for duplicate legacy scroll event, see
            ;; GNOME bug 726878.
            (if (and %m4-use-gtk3
                     scroll-direction
                     (not smooth-scroll?)
                     (not (eq? (event-scroll-direction->symbol scroll-direction)
                               'gdk-scroll-smooth))
                     (= %last-scroll-event-time (event-time *event)))
                (begin (log! 'debug "[~A] duplicate legacy scroll event ~A"
                             (event-time *event)
                             (event-scroll-direction->symbol scroll-direction))
                       FALSE)
                (begin
                  (when (and %m4-use-gtk3
                             smooth-scroll?)
                    ;; As of GTK 3.4, all directional scroll
                    ;; events are provided by the
                    ;; GDK_SCROLL_SMOOTH direction on XInput2
                    ;; and Wayland devices.
                    (set! %last-scroll-event-time (event-time *event)))
                  (let ((pan-direction
                         (if %m4-use-gtk3
                             (if smooth-scroll?
                                 (inexact->exact (round (cdr scroll-direction)))
                                 (event-scroll-direction->pan-direction scroll-direction))
                             (event-scroll-direction->pan-direction scroll-direction)))
                        (zoom-direction
                         (if %m4-use-gtk3
                             (if smooth-scroll?
                                 ;; event->delta_x seems to be
                                 ;; unused on not touch
                                 ;; devices.
                                 (let ((direction (cdr scroll-direction)))
                                   (cond
                                    ((negative? direction) ZOOM_IN)
                                    ((positive? direction) ZOOM_OUT)
                                    ((zero? direction) ZOOM_SAME)))
                                 (event-scroll-direction->zoom-direction scroll-direction))
                             (event-scroll-direction->zoom-direction scroll-direction))))
                    (when zoom
                      (a_zoom *window *widget zoom-direction HOTKEY))

                    (let ((*horiz-adjustment (schematic_canvas_get_hadjustment *widget))
                          (*vert-adjustment (schematic_canvas_get_vadjustment *widget)))
                      (if (or (and pan-x-axis (null-pointer? *horiz-adjustment))
                              (and pan-y-axis (null-pointer? *vert-adjustment)))
                          (begin
                            (log! 'warning "scroll-canvas(): NULL horizontal or vertical adjustment.")
                            TRUE)
                          (begin
                            (when pan-x-axis
                              (update-adjustment *horiz-adjustment pan-direction))
                            (when pan-y-axis
                              (update-adjustment *vert-adjustment pan-direction))

                            (when (and (true? (schematic_window_get_undo_panzoom *window))
                                       (or zoom pan-x-axis pan-y-axis))
                              (o_undo_savestate_viewport *window))

                            (x_event_faked_motion *widget %null-pointer)
                            ;; Stop further processing of this signal.
                            TRUE)))))))))))

;;; Proxy C function for the scroll-canvas() procedure for using
;;; in C signal handlers.
(define *scroll-canvas
  (procedure->pointer int scroll-canvas '(* * *)))
