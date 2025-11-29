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

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (text-edit-dialog))


(define (text-edit-dialog *window)
  "Create and/or show the Edit text properties widget in *WINDOW."
  (when (null-pointer? *window)
    (error "NULL window."))

  (let ((*text-properties-widget
         (schematic_window_get_text_properties_widget *window)))

    (if (true? (x_widgets_use_docks))
        (let ((*right-notebook (schematic_window_get_right_notebook *window)))
          (x_widgets_show_in_dock *right-notebook *text-properties-widget))

        (let ((*dialog (schematic_window_get_text_properties_dialog *window)))
          (if (not (null-pointer? *dialog))
              (gtk_window_present *dialog)

              (let ((*new-dialog (x_widgets_dialog_new *window
                                                       *text-properties-widget
                                                       (string->pointer (G_ "Edit Text"))
                                                       (string->pointer "txtprops"))))
                (schematic_window_set_text_properties_dialog *window *new-dialog)))))

    (schematic_text_properties_widget_adjust_focus *text-properties-widget)))
