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


(define-module (schematic dialog color-edit)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)

  #:use-module (schematic dialog widget)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (color-edit-dialog))


(define (color-edit-dialog *window)
  "Create and/or show the Color edit dialog in *WINDOW."
  (when (null-pointer? *window)
    (error "NULL window."))

  (let ((*color-edit-widget (schematic_window_get_color_edit_widget *window))
        (*dialog (schematic_window_get_color_edit_dialog *window)))

    (if (not (null-pointer? *dialog))
        (gtk_window_present *dialog)

        (let ((*new-dialog
               (make-widget-dialog *window
                                   *color-edit-widget
                                   (string->pointer (G_ "Color Scheme Editor"))
                                   (string->pointer "colored"))))
          (schematic_window_set_color_edit_dialog *window *new-dialog)))))
