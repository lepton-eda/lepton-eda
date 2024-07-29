;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022-2024 Lepton EDA Contributors
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


(define-module (schematic preview-widget)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)

  #:use-module (lepton bounds)
  #:use-module (lepton ffi)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton gerror)
  #:use-module (lepton log)
  #:use-module (lepton m4)
  #:use-module (lepton object foreign)
  #:use-module (lepton object)

  #:use-module (schematic ffi)
  #:use-module (schematic gettext)

  #:export (init-preview-widget-signals))


(define %over-zoom-factor 0.1)


;;; Flags defined in struct.h.
(define F_OPEN_RC 1)
(define F_OPEN_CHECK_BACKUP 2)
(define F_OPEN_FORCE_BACKUP 4)
(define F_OPEN_RESTORE_CWD 8)


(define (update-preview *preview *user-data)
  "Update the preview widget *PREVIEW if it is 'active' and either a
filename or a buffer is associated with it.  The argument
*USER-DATA is currently unused.  In case of an error, when a
buffer should be displayed, the widget displays the error message."
  (define (report-gerror *error *page)
    (let ((*err (dereference-pointer *error)))
      (unless (null-pointer? *err)
        (let ((message (gerror-message *err)))
          (g_clear_error *error)
          (log! 'warning (G_ "Preview error: ~A") message)
          (lepton_page_append *page
                              (object->pointer
                               (make-text '(100 . 100)
                                          'lower-center
                                          0
                                          message
                                          10
                                          #t
                                          'both
                                          2)))))))

  (if (null-pointer? *preview)
      (log! 'warning "NULL preview widget")
      (let ((*page (schematic_canvas_get_page *preview)))
        (unless (null-pointer? *page)
          ;; Delete old preview.
          (lepton_page_delete_objects *page)
          (let ((*toplevel (lepton_page_get_toplevel *page))
                (preview_active (schematic_preview_get_active *preview))
                (*preview_filename (schematic_preview_get_filename *preview))
                (*preview_buffer (schematic_preview_get_buffer *preview)))
            (when (true? preview_active)
              (unless (or (null-pointer? *preview_filename)
                          (null-pointer? *preview_buffer))
                (error "Either preview filename or buffer has to be NULL!")))
            (when (true? preview_active)
              (unless (null-pointer? *preview_filename)
                ;; Open up file in current page.
                (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
                       (result
                        (f_open *toplevel
                                *page
                                *preview_filename
                                (logior F_OPEN_RC F_OPEN_RESTORE_CWD)
                                *error)))
                  (when (false? result)
                    (report-gerror *error *page))))
              (unless (null-pointer? *preview_buffer)
                (let* ((*error (bytevector->pointer (make-bytevector (sizeof '*) 0)))
                       ;; Load the data buffer.
                       (*objects (o_read_buffer *page
                                                %null-pointer
                                                *preview_buffer
                                                -1
                                                (string->pointer (G_ "Preview Buffer"))
                                                *error)))
                  (if (null-pointer? (dereference-pointer *error))
                      (lepton_page_append_list *page *objects)
                      (report-gerror *error *page)))))
            (let-values (((left top right bottom)
                          (object-list-bounds
                           (lepton_page_objects *page)
                           ;; Do not include hidden text.
                           FALSE)))
              (when left
                ;; Clamp the canvas size to the extents of the
                ;; page being previewed.
                (let ((width-add (inexact->exact (round (* %over-zoom-factor
                                                           (- right left)))))
                      (height-add (inexact->exact (round (* %over-zoom-factor
                                                            (- bottom top)))))
                      (*geometry (schematic_canvas_get_page_geometry *preview)))
                  (schematic_viewport_set_world_left *geometry
                                                     (- left width-add))
                  (gschem_page_geometry_set_world_right *geometry
                                                        (+ right width-add))
                  (gschem_page_geometry_set_world_top *geometry
                                                      (- top height-add))
                  (schematic_viewport_set_world_bottom *geometry
                                                       (+ bottom height-add)))))
            ;; Display current page (possibly empty).
            (schematic_canvas_zoom_extents *preview %null-pointer))))))


(define *update-preview
  (procedure->pointer void update-preview '(* *)))

(define (scroll-preview *preview *scroll-event *window)
  (if (true? (schematic_preview_get_active *preview))
      (x_event_scroll *preview
                      *scroll-event
                      (schematic_preview_get_window *preview))
      TRUE))

(define *scroll-preview
  (procedure->pointer int scroll-preview '(* * *)))

;;; The list of pairs (NAME . CALLBACK) for initialization of
;;; preview widgets.
(define %signal-callback-list
  (list
   (if %m4-use-gtk3
       `("draw" . ,*x_event_draw)
       `("expose-event" . ,*x_event_expose))
   `("realize" . ,*schematic_preview_callback_realize)
   `("button-press-event" . ,*schematic_preview_callback_button_press)
   `("configure-event" . ,*x_event_configure)
   `("scroll-event" . ,*scroll-preview)
   `("update-preview" . ,*update-preview)))


(define (init-preview-widget-signals *preview)
  "Initialize the preview widget *PREVIEW by connecting to it
appropriate signals.  Return *PREVIEW."
  (for-each
   (lambda (element)
     (schematic_signal_connect *preview
                               (string->pointer (car element))
                               (cdr element)
                               (schematic_preview_get_window *preview)))
   %signal-callback-list)
  *preview)
