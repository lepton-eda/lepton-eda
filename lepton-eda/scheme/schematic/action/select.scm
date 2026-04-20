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
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (continue-box-selection
            continue-selection
            finish-box-selection
            finish-selection
            start-box-selection
            start-selection))


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

  (let ((count (o_select_box_search *window
                                    show_hidden_text
                                    left
                                    top
                                    right
                                    bottom
                                    *objects)))
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
          (and (false? (o_find_selected_object *window wx1 wy1))
               (or (false? (o_find_object *window wx1 wy1 TRUE))
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
  (o_find_object *window x y TRUE)
  (i_action_stop *window))
