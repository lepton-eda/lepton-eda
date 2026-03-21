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


(define-module (schematic dialog edit-text)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)

  #:use-module (schematic dialog widget)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic widget)
  #:use-module (schematic window foreign)

  #:export (text-edit-dialog))


(define (text-edit-dialog window)
  "Create and/or show the Edit text properties widget in *WINDOW."
  (define *window (check-window window 1))

  (define *text-properties-widget
    (schematic_window_get_text_properties_widget *window))

  (if (eq? (widget-style) 'dock)
      (let ((*right-notebook (schematic_window_get_right_notebook *window)))
        (show-notebook-widget *right-notebook *text-properties-widget))

      (show-widget-dialog *window
                          *text-properties-widget
                          schematic_window_get_text_properties_dialog
                          schematic_window_set_text_properties_dialog
                          "Edit Text"
                          "txtprops"))

  (schematic_text_properties_widget_adjust_focus *text-properties-widget))
