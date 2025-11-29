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


(define-module (schematic dialog object-properties)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (object-properties-dialog))


(define (object-properties-dialog *window)
  "Create and/or show the Object properties widget in *WINDOW."
  (when (null-pointer? *window)
    (error "NULL window."))

  (let ((*object-properties-widget
         (schematic_window_get_object_properties_widget *window)))

    (if (true? (x_widgets_use_docks))
        (let ((*right-notebook (schematic_window_get_right_notebook *window)))
          (x_widgets_show_in_dock *right-notebook *object-properties-widget))

        (let ((*dialog (schematic_window_get_object_properties_dialog *window)))
          (if (not (null-pointer? *dialog))
              (gtk_window_present *dialog)

              (let ((*new-dialog (x_widgets_dialog_new *window
                                                       *object-properties-widget
                                                       (string->pointer (G_ "Object Properties"))
                                                       (string->pointer "objprops"))))
                (schematic_window_set_object_properties_dialog *window *new-dialog)))))))
