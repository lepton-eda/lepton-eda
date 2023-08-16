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

  #:use-module (schematic action delete)
  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic gettext)
  #:use-module (schematic hook)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (%clipboard-buffer-id
            free-buffers
            paste-buffer
            selection->buffer))


(define %schematic-buffer-list
  ;; 10 buffers.  Clipboard buffer has the index 0.
  (make-list 10 %null-pointer))

(define (buffer-list-ref num)
  (define buffer
    (false-if-exception (list-ref %schematic-buffer-list num)))

  (or buffer
      (error "Wrong buffer number.")))


(define (buffer-list-set! num val)
  (list-set! %schematic-buffer-list num val))


(define (free-buffers)
  "Remove and free objects stored in copy-paste buffers."
  (for-each lepton_object_list_delete %schematic-buffer-list)
  (set! %schematic-buffer-list '()))

;;; The ID of the clipboard buffer.
(define %clipboard-buffer-id 0)


(define (run-copy-objects-hook *window *objects)
  (with-window *window
   (run-hook copy-objects-hook
             (glist->list *objects pointer->object))))


;;; Copy current selection to buffer.
(define* (selection->buffer window buffer-number #:optional (cut? #f))
  "Copy the current WINDOW selection into a buffer with given
BUFFER-NUMBER running the copy-object-hook() on selected objects.
If the optional argument CUT? is set to non-#f value, the objects
selected will be cut, not copied.  That means that they will be
removed from the selection and the hook won't be run."
  (define *window (check-window window 1))
  (define *selection
    (lepton_list_get_glist (schematic_window_get_selection_list *window)))

  (unless (null-pointer? *selection)
    ;; On cutting, delete place list and invalidate canvas.
    (when cut? (o_redraw_cleanstates *window))

    ;; Remove previous buffer contents.
    (lepton_object_list_delete (buffer-list-ref buffer-number))

    ;; Make a copy of selected objects.
    (let ((*objects (o_glist_copy_all *selection %null-pointer)))
      (buffer-list-set! buffer-number *objects)
      (if cut?
          (delete-selection *window)
          (run-copy-objects-hook *window *objects))

      (when (= buffer-number %clipboard-buffer-id)
        (x_clipboard_set *window *objects)))

    (i_update_menus *window)))


;;; Copy the contents of the clipboard to buffer.
(define (clipboard->buffer window buffer-number)
  (define *window (check-window window 1))
  (define *objects (x_clipboard_get *window))

  (lepton_object_list_delete (buffer-list-ref buffer-number))
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
  ;; It may be (x . y) or #f.
  (define mouse-coord (and anchor
                           (check-coord anchor 2)
                           anchor))
  (define buffer-n
    (and (check-integer buffer-number 3) buffer-number))
  (define mouse-x (or (and=> mouse-coord car) 0))
  (define mouse-y (or (and=> mouse-coord cdr) 0))

  (define (buffer->place-list)
    ;; Remove the old place list if it exists.
    (schematic_window_delete_place_list *window)
    ;; Replace it with a list from buffer.
    (schematic_window_set_place_list *window
                                     (o_glist_copy_all (buffer-list-ref buffer-n)
                                                       %null-pointer))
    ;; Return the current place list.
    (schematic_window_get_place_list *window))

  (define (place-objects *objects x y)
    ;; Place the objects into the buffer at the mouse
    ;; origin (mouse-x . mouse-y).
    (schematic_window_set_first_wx *window mouse-x)
    (schematic_window_set_first_wy *window mouse-y)

    (lepton_object_list_translate *objects
                                  (- mouse-x x)
                                  (- mouse-y y))

    (i_set_state *window (symbol->action-mode 'paste-mode))
    (o_place_start *window mouse-x mouse-y)

    ;; The next paste operation will be a copy of
    ;; these objects.
    (run-copy-objects-hook *window (buffer-list-ref buffer-n)))

  (o_redraw_cleanstates *window)
  ;; Cancel current place or draw action if it is being
  ;; done.
  (when (in-action? window)
    (i_callback_cancel %null-pointer *window))

  (when (= buffer-n %clipboard-buffer-id)
    (clipboard->buffer window buffer-n))

  ;; Cancel the action if there are no objects in the buffer.
  (if (null-pointer? (buffer-list-ref buffer-n))
      ;; Report that the buffer is empty.
      (i_set_state_msg *window
                       (symbol->action-mode 'select-mode)
                       (string->pointer (G_ "Empty clipboard")))

      (let ((show-hidden-text? (gschem_toplevel_get_show_hidden_text *window))
            (*place-list (buffer->place-list)))
        (let-values (((x1 y1 x2 y2)
                      (object-list-bounds *place-list show-hidden-text?)))
          (if x1
              (place-objects *place-list
                             ;; Snap x and y to the grid.
                             (snap_grid *window x1)
                             (snap_grid *window y1))
              ;; If the place buffer doesn't have any objects to
              ;; define its any bounds we drop out here, reporting
              ;; this.
              (i_set_state_msg *window
                               (symbol->action-mode 'select-mode)
                               (string->pointer (G_ "Hit E N to insert invisible objects.")))
              )))))
