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
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (finish-selection
            start-selection))


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
