;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2025-2026 Lepton EDA Contributors
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


(define-module (schematic dialog find-text-state)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)

  #:use-module (schematic dialog widget)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic widget)
  #:use-module (schematic window foreign)

  #:export (find-text-state-dialog))


(define (find-text-state-dialog window)
  "Create and/or show the Find text state dialog in *WINDOW."
  (define *window (check-window window 1))

  (define *find-text-state
    (schematic_window_get_find_text_state_widget *window))

  (if (eq? (widget-style) 'dock)
      (let ((*bottom-notebook
             (schematic_window_get_bottom_notebook *window)))
        (show-notebook-widget *bottom-notebook *find-text-state))

      (show-widget-dialog *window
                          *find-text-state
                          schematic_window_get_find_text_state_dialog
                          schematic_window_set_find_text_state_dialog
                          "Find Text"
                          "findtext")))
