;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023 Lepton EDA Contributors
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


(define-module (schematic buffer)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic hook)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (paste-buffer
            window-selection->buffer
            window-selection->buffer!))


;;; Defined in globals.h.
(define CLIPBOARD_BUFFER 0)
(define MAX_BUFFERS 5)


(define (check-buffer-number num)
  (when (or (< num 0)
            (> num MAX_BUFFERS))
    (error "Wrong buffer number.")))


(define (window-selection->buffer window buffer-number)
  "Copy the current WINDOW selection into a buffer with given
BUFFER-NUMBER."
  (define *window (check-window window 1))

  (check-buffer-number buffer-number)

  (schematic_buffer_from_selection *window buffer-number)

  (with-window *window
   (run-hook copy-objects-hook
             (glist->list (schematic_buffer_get_objects buffer-number)
                          pointer->object)))

  (when (= buffer-number CLIPBOARD_BUFFER)
    (x_clipboard_set *window
                     (schematic_buffer_get_objects buffer-number))))


(define (window-selection->buffer! window buffer-number)
  "Cut the current WINDOW selection into a buffer with given
BUFFER-NUMBER."
  (define *window (check-window window 1))

  (check-buffer-number buffer-number)

  (schematic_buffer_from_selection *window buffer-number)

  (o_delete_selected *window)

  (when (= buffer-number CLIPBOARD_BUFFER)
    (x_clipboard_set *window
                     (schematic_buffer_get_objects buffer-number))))


(define (paste-buffer window anchor buffer-number)
  "Place the contents of the buffer BUFFER-NUMBER in WINDOW into the
place list at the point ANCHOR."
  (define *window (check-window window 1))
  (define mouse-x (car anchor))
  (define mouse-y (cdr anchor))
  (define xleft (make-bytevector (sizeof int)))
  (define ytop (make-bytevector (sizeof int)))
  (define xright (make-bytevector (sizeof int)))
  (define ybottom (make-bytevector (sizeof int)))

  (check-coord anchor 2)
  (check-integer buffer-number 3)

  (when (= buffer-number CLIPBOARD_BUFFER)
    (schematic_buffer_from_clipboard *window buffer-number))

  ;; Cancel the action if there are no objects in the buffer.
  (or (null-pointer? (schematic_buffer_get_objects buffer-number))

      (let ((show-hidden-text? (gschem_toplevel_get_show_hidden_text *window)))
        ;; Remove the old place list if it exists.
        (schematic_window_delete_place_list *window)
        ;; Replace it with a list from buffer.
        (schematic_window_set_place_list *window
                                         (o_glist_copy_all (schematic_buffer_get_objects buffer-number)
                                                           %null-pointer))

        (let ((result (world_get_object_glist_bounds (schematic_window_get_place_list *window)
                                                     show-hidden-text?
                                                     (bytevector->pointer xleft)
                                                     (bytevector->pointer ytop)
                                                     (bytevector->pointer xright)
                                                     (bytevector->pointer ybottom))))
          ;; If the place buffer doesn't have any objects
          ;; to define its any bounds we drop out here.
          (or (false? result)
              (let ((left-x (bytevector-sint-ref xleft 0 (native-endianness) (sizeof int)))
                    (top-y (bytevector-sint-ref ytop 0 (native-endianness) (sizeof int))))

                ;; Place the objects into the buffer at the mouse
                ;; origin (mouse-x . mouse-y).
                (schematic_window_set_first_wx *window mouse-x)
                (schematic_window_set_first_wy *window mouse-y)

                ;; Snap x and y to the grid.
                (let ((x (snap_grid *window left-x))
                      (y (snap_grid *window top-y)))

                  (lepton_object_list_translate (schematic_window_get_place_list *window)
                                                (- mouse-x x)
                                                (- mouse-y y))

                  (i_set_state *window (symbol->action-mode 'paste-mode))
                  (o_place_start *window mouse-x mouse-y)

                  ;; The next paste operation will be a copy of
                  ;; these objects.
                  (with-window *window
                   (run-hook copy-objects-hook
                             (glist->list (schematic_buffer_get_objects buffer-number)
                                          pointer->object)))

                  (when (= buffer-number CLIPBOARD_BUFFER)
                    (x_clipboard_set *window (schematic_buffer_get_objects buffer-number)))

                  ;; Currently, the function returns #f if the
                  ;; buffer contains objects to paste, and #t
                  ;; otherwise.
                  #f)))))))
