;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2025-2026 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not write to the Free Software
;;; Foundation Inc. 51 Franklin Street Fifth Floor Boston MA 02110-1301 USA.


(define-module (schematic action select)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (continue-box-selection
            continue-selection
            find-object
            finish-box-selection
            finish-selection
            start-box-selection
            start-selection))


(define (find-object *window x y)
  (define *canvas (schematic_window_get_current_canvas *window))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (let* ((slack (schematic_canvas_WORLDabs
                 *canvas
                 (schematic_window_get_select_slack_pixels *window)))
         (*objects (lepton_page_objects
                    (schematic_window_get_active_page *window)))
         (*last-found-object
          (schematic_window_get_object_lastplace *window))
         ;; Decide whether to iterate over all objects or start at
         ;; the last found object.  If there is more than one
         ;; object below the (X . Y) position, this will select
         ;; the next object below the position point.  You can
         ;; change the selected object by clicking at the same
         ;; place multiple times.
         (last-found-object-exists?
          (not (null-pointer? *last-found-object)))
         (*object-ls (glist->list *objects identity))
         (*object-ls-tail (and last-found-object-exists?
                               (member *last-found-object *object-ls)))
         (*rest-objects
          (if *object-ls-tail
              (cdr *object-ls-tail)
              '())))

    ;; Do first search (if we found any objects after the last
    ;; found object).
    (let loop ((ls *rest-objects))
      (if (null? ls)
          ;; Nothing found.
          (o_find_object *window
                         *objects
                         *last-found-object
                         x
                         y
                         slack)
          (if (true? (schematic_selection_find_single_object *window
                                                             (car ls)
                                                             x
                                                             y
                                                             slack))
              TRUE
              (loop (cdr ls)))))))


;;; Invalidate the area of the box selection in WINDOW.
(define (invalidate-selection-box window)
  (define *window (check-window window 1))
  (define *canvas (schematic_window_get_current_canvas *window))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (schematic_canvas_invalidate_world_rect
   *canvas
   (schematic_window_get_first_wx *window)
   (schematic_window_get_first_wy *window)
   (schematic_window_get_second_wx *window)
   (schematic_window_get_second_wy *window)))


(define (start-box-selection window x y)
  "Start the process of box selection in WINDOW.  (X . Y) is the
current coordinate."
  (define *window (check-window window 1))
  (define *canvas (schematic_window_get_current_canvas *window))

  (check-integer x 2)
  (check-integer y 3)

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  ;; If we are still close to the button press location, then
  ;; don't enter the selection box mode.
  (let* ((diff-x (abs (- (schematic_window_get_first_wx *window) x)))
         (diff-y (abs (- (schematic_window_get_first_wy *window) y)))
         (dist (schematic_canvas_SCREENabs *canvas (max diff-x diff-y))))
    (when (>= dist 10)
      (schematic_window_set_second_wx *window x)
      (schematic_window_set_second_wy *window y)

      (set-action-mode! 'box-select-mode #:window window)
      (i_action_start *window))))


(define (continue-box-selection window x y)
  "Continue box selection in WINDOW.  (X . Y) is the current
coordinate."
  (define *window (check-window window 1))

  (check-action-state window)

  (when (true? (schematic_window_get_rubber_visible *window))
    (invalidate-selection-box window))

  (schematic_window_set_second_wx *window x)
  (schematic_window_set_second_wy *window y)

  (invalidate-selection-box window)
  (schematic_window_set_rubber_visible *window 1))


;;; Definitions from schematic_defines.h.
(define SINGLE 0)
(define MULTIPLE 1)

(define (search-visible-objects window)
  (define *window (check-window window 1))

  (define shift-key-pressed?
    (true? (schematic_window_get_shift_key_pressed *window)))
  (define control-key-pressed?
    (true? (schematic_window_get_control_key_pressed *window)))

  (define show_hidden_text
    (schematic_window_get_show_hidden_text *window))

  (define wx1 (schematic_window_get_first_wx *window))
  (define wy1 (schematic_window_get_first_wy *window))
  (define wx2 (schematic_window_get_second_wx *window))
  (define wy2 (schematic_window_get_second_wy *window))

  (define left (min wx1 wx2))
  (define right (max wx1 wx2))
  (define top (min wy1 wy2))
  (define bottom (max wy1 wy2))

  (define *active-page (schematic_window_get_active_page *window))
  (define *objects (lepton_page_objects *active-page))

  (define object-bv-left (make-bytevector (sizeof int) 0))
  (define object-bv-right (make-bytevector (sizeof int) 0))
  (define object-bv-top (make-bytevector (sizeof int) 0))
  (define object-bv-bottom (make-bytevector (sizeof int) 0))

  (define (test-object-bounds *object count)
    ;; Only select visible objects.
    (let ((visible-object?
           (or (false? (lepton_object_is_text *object))
               (true? (lepton_text_object_is_visible *object))
               (true? show_hidden_text))))
      (if (and visible-object?
               (true? (lepton_object_calculate_visible_bounds
                       *object
                       show_hidden_text
                       (bytevector->pointer object-bv-left)
                       (bytevector->pointer object-bv-top)
                       (bytevector->pointer object-bv-right)
                       (bytevector->pointer object-bv-bottom)))
               (>= (bytevector-sint-ref object-bv-left
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   left)
               (<= (bytevector-sint-ref object-bv-right
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   right)
               (>= (bytevector-sint-ref object-bv-top
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   top)
               (<= (bytevector-sint-ref object-bv-bottom
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   bottom))
          (begin
            (o_select_object *window *object MULTIPLE count)
            (1+ count))
          count)))

  (define (select-objects)
    (let loop ((*object-ls (glist->list *objects identity))
               ;; Object count.
               (count 0))
      (if (null? *object-ls)
          count
          (loop (cdr *object-ls)
                (test-object-bounds (car *object-ls) count)))))

  (let ((count (select-objects)))
    ;; If there were no objects to be found in select box, count
    ;; will be zero, and you need to deselect anything remaining
    ;; (except when the Shift or Control keys are pressed).
    (when (and (zero? count)
               (not shift-key-pressed?)
               (not control-key-pressed?))
      (o_select_unselect_all *window)))

  (i_update_menus *window))


(define (finish-box-selection window x y)
  "Finish the process of box selection in WINDOW.  (X . Y) is the
unused value of the current world coordinate."
  (define *window (check-window window 1))

  (check-action-state window)

  (invalidate-selection-box window)
  (schematic_window_set_rubber_visible *window 0)

  (search-visible-objects window)

  (set-action-mode! 'select-mode #:window window)
  (i_action_stop *window))


(define (start-selection window x y)
  "Choose the way of how to start the selection process.  If no grip
was found at the given world coordinate (X . Y) the function
starts an action in WINDOW in order to force other function to
decide that.  Otherwise, it switches on the grips mode for working
with the grip found.  The function is intended to be called by
pressing the left mouse button."
  (define *window (check-window window 1))

  (check-integer x 2)
  (check-integer y 3)

  ;; Look for grips or fall through if not enabled.
  (o_grips_start *window x y)

  (unless (eq? (action-mode window) 'grips-mode)
    ;; Now go into normal select mode.
    (i_action_start *window)
    (schematic_window_set_first_wx *window x)
    (schematic_window_set_first_wy *window y)
    (schematic_window_set_second_wx *window x)
    (schematic_window_set_second_wy *window y)))


;;; Test if there is a selected object under cursor in *WINDOW at
;;; the coordinate (X . Y).  Return TRUE on success, otherwise
;;; return FALSE.
(define (find-selected-object *window x y)
  (define *canvas (schematic_window_get_current_canvas *window))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (let ((slack
         (schematic_canvas_WORLDabs *canvas
                                    (schematic_window_get_select_slack_pixels *window)))
        (*selection (schematic_window_get_selection_list *window)))

    (let loop ((*selected-objects (glist->list (lepton_list_get_glist *selection)
                                               identity)))
      (if (null? *selected-objects)
          FALSE
          (if (true? (schematic_selection_is_object_hit *window
                                                        (car *selected-objects)
                                                        x
                                                        y
                                                        slack))
              TRUE
              (loop (cdr *selected-objects)))))))


(define (continue-selection window x y)
  "Continue selection at world coordinate point (X . Y) in WINDOW.
The function determines whether objects have to be selected or
moved.  Checks if the Shift or Control keys are pressed, (that
means the user definitely wants to drag out a selection box), or
there are no selected objects under the cursor.  In that case the
function starts drawing the selection box.  Otherwise, it looks
for the objects that have been or could be selected and starts
moving them.  The function is intended to be called by motion of
the mouse while the left mouse button is pressed."
  (define *window (check-window window 1))
  (define wx1 (schematic_window_get_first_wx *window))
  (define wy1 (schematic_window_get_first_wy *window))

  (check-integer x 2)
  (check-integer y 3)
  (check-action-state window)

  ;; Check if a mod key is pressed or there is no selected object
  ;; under the cursor.
  (if (or (true? (schematic_window_get_shift_key_pressed *window))
          (true? (schematic_window_get_control_key_pressed *window))
          (and (false? (find-selected-object *window wx1 wy1))
               (or (false? (find-object *window wx1 wy1))
                   (false? (o_select_selected *window)))))
      ;; Start drawing a selection box to select objects.
      (start-box-selection window x y)

      ;; Start moving the selected object(s).
      (o_move_start *window wx1 wy1)))


(define (finish-selection window x y)
  "Finish the process of selection in WINDOW at the world
coordinate (X . Y) where the function tries to find an object and
select it.  The function is intended to be called by releasing the
left mouse button."
  (define *window (check-window window 1))

  (check-integer x 2)
  (check-integer y 3)
  (check-action-state window)

  ;; Look for objects to select.
  (find-object *window x y)
  (i_action_stop *window))
