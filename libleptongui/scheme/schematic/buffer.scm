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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)

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

  #:export (free-buffers
            paste-buffer
            window-selection->buffer
            window-selection->buffer!))


(define %schematic-buffer-list
  ;; 5 buffers plus clipboard buffer that has index 0.
  (make-list 6 %null-pointer))

(define (buffer-list-ref num)
  (define buffer
    (false-if-exception (list-ref %schematic-buffer-list num)))

  (or buffer
      (error "Wrong buffer number.")))


(define (buffer-list-set! num val)
  (list-set! %schematic-buffer-list num val))


(define (free-buffers)
  (define (delete-buffer *buffer)
    (lepton_object_list_delete *buffer))

  (for-each delete-buffer %schematic-buffer-list)
  (set! %schematic-buffer-list '()))


(define CLIPBOARD_BUFFER 0)


;;; Copy current selection to buffer.
(define (selection->buffer window buffer-number)
  (define *window (check-window window 1))

  (let ((*selection (schematic_window_get_selection_list *window)))

    (unless (null-pointer? (buffer-list-ref buffer-number))
      (lepton_object_list_delete (buffer-list-ref buffer-number))
      (buffer-list-set! buffer-number %null-pointer))

    (buffer-list-set! buffer-number
                      (o_glist_copy_all (lepton_list_get_glist *selection)
                                        %null-pointer))))


(define (window-selection->buffer window buffer-number)
  "Copy the current WINDOW selection into a buffer with given
BUFFER-NUMBER."
  (define *window (check-window window 1))

  (selection->buffer window buffer-number)

  (with-window *window
   (run-hook copy-objects-hook
             (glist->list (buffer-list-ref buffer-number)
                          pointer->object)))

  (when (= buffer-number CLIPBOARD_BUFFER)
    (x_clipboard_set *window
                     (buffer-list-ref buffer-number))))


(define (window-selection->buffer! window buffer-number)
  "Cut the current WINDOW selection into a buffer with given
BUFFER-NUMBER."
  (define *window (check-window window 1))

  (selection->buffer window buffer-number)

  (o_delete_selected *window)

  (when (= buffer-number CLIPBOARD_BUFFER)
    (x_clipboard_set *window
                     (buffer-list-ref buffer-number))))


;;; Copy the contents of the clipboard to buffer.
(define (clipboard->buffer window buffer-number)
  (define *window (check-window window 1))
  (define *objects (x_clipboard_get *window))

  (unless (null-pointer? (buffer-list-ref buffer-number))
    (lepton_object_list_delete (buffer-list-ref buffer-number)))

  (buffer-list-set! buffer-number *objects))


(define (object-list-bounds *objects show-hidden-text?)
  (define (get-int bv)
    (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))
  (define x1 (make-bytevector (sizeof int)))
  (define y1 (make-bytevector (sizeof int)))
  (define x2 (make-bytevector (sizeof int)))
  (define y2 (make-bytevector (sizeof int)))
  (define result
    (true? (world_get_object_glist_bounds *objects
                                          show-hidden-text?
                                          (bytevector->pointer x1)
                                          (bytevector->pointer y1)
                                          (bytevector->pointer x2)
                                          (bytevector->pointer y2))))
  (if result
      (values (get-int x1) (get-int y1) (get-int x2) (get-int y2))
      (values #f #f #f #f)))


(define (paste-buffer window anchor buffer-number)
  "Place the contents of the buffer BUFFER-NUMBER in WINDOW into the
place list at the point ANCHOR."
  (define *window (check-window window 1))
  (define mouse-coord (and (check-coord anchor 2) anchor))
  (define buffer-n
    (and (check-integer buffer-number 3) buffer-number))
  (define mouse-x (car mouse-coord))
  (define mouse-y (cdr mouse-coord))

  (define (buffer->place-list)
    ;; Remove the old place list if it exists.
    (schematic_window_delete_place_list *window)
    ;; Replace it with a list from buffer.
    (schematic_window_set_place_list *window
                                     (o_glist_copy_all (buffer-list-ref buffer-n)
                                                       %null-pointer))
    ;; Return the current place list.
    (schematic_window_get_place_list *window))

  (when (= buffer-n CLIPBOARD_BUFFER)
    (clipboard->buffer window buffer-n))

  ;; Cancel the action if there are no objects in the buffer.
  (or (null-pointer? (buffer-list-ref buffer-n))

      (let ((show-hidden-text? (gschem_toplevel_get_show_hidden_text *window))
            (*place-list (buffer->place-list)))
        (let-values (((objects-x1 objects-y1 objects-x2 objects-y2)
                      (object-list-bounds *place-list show-hidden-text?)))
          ;; If the place buffer doesn't have any objects
          ;; to define its any bounds we drop out here.
          (or (not objects-x1)
              (begin
                ;; Place the objects into the buffer at the mouse
                ;; origin (mouse-x . mouse-y).
                (schematic_window_set_first_wx *window mouse-x)
                (schematic_window_set_first_wy *window mouse-y)

                ;; Snap x and y to the grid.
                (let ((x (snap_grid *window objects-x1))
                      (y (snap_grid *window objects-y1)))

                  (lepton_object_list_translate *place-list
                                                (- mouse-x x)
                                                (- mouse-y y))

                  (i_set_state *window (symbol->action-mode 'paste-mode))
                  (o_place_start *window mouse-x mouse-y)

                  ;; The next paste operation will be a copy of
                  ;; these objects.
                  (with-window *window
                   (run-hook copy-objects-hook
                             (glist->list (buffer-list-ref buffer-n)
                                          pointer->object)))

                  ;; Currently, the function returns #f if the
                  ;; buffer contains objects to paste, and #t
                  ;; otherwise.
                  #f)))))))
