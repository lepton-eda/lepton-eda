;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2024-2026 Lepton EDA Contributors
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


(define-module (schematic callback cancel)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gui keymap)
  #:use-module (schematic window foreign)

  #:export (callback-cancel))


(define (callback-cancel *window)
  "Cancel all actions that may be in progress (e.g. move, component
placement, etc.) and return to default \"select\" mode."
  (define window (pointer->window *window))
  (define current-action-mode (action-mode window))
  (define *compselect (schematic_window_get_compselect_widget *window))

  (when (and (eq? current-action-mode 'component-mode)
             (not (null-pointer? *compselect)))
    ;; User hit escape key when placing components.

    ;; Undraw any outline of the place list.
    (o_place_invalidate_rubber *window FALSE)
    (schematic_window_set_rubber_visible *window 0)

    ;; De-select the lists in the component selector.
    (x_compselect_deselect *window)

    ;; Present the component selector again.
    (gtk_widget_set_visible *compselect TRUE))

  (when (in-action? window)
    ;; If we're cancelling from a move action, re-wind the page
    ;; contents back to their state before we started.
    (o_move_cancel *window))

  ;; If we're cancelling from a grip action, call the specific
  ;; cancel routine to reset the visibility of the object being
  ;; modified.
  (when (eq? current-action-mode 'grips-mode)
    (o_grips_cancel *window))

  ;; If we're cancelling from a net action, reset the visibility of
  ;; the net and all the variables used for net drawing.
  (when (eq? current-action-mode 'net-mode)
    (o_net_reset *window))

  ;; Free the place list and its contents. If we were in a move
  ;; action, the list (refering to objects on the page) would
  ;; already have been cleared in o_move_cancel(), so this is OK.
  (schematic_window_delete_place_list *window)

  ;; Set the 'select' mode.
  (set-action-mode! 'select-mode #:window window)

  ;; Reset the status bar.
  (schematic_window_set_keyaccel_string *window %null-pointer)
  (i_show_state *window %null-pointer)

  ;; If any prefix keys are stored in the current key sequence,
  ;; clear them.
  (reset-keys)

  (schematic_canvas_invalidate_all
   (schematic_window_get_current_canvas *window))

  (i_action_stop *window))
