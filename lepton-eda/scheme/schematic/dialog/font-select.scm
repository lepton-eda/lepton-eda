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


(define-module (schematic dialog font-select)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)

  #:use-module (schematic dialog widget)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (font-select-dialog))


(define (font-select-dialog *window)
  "Create and/or show the Font chooser dialog in *WINDOW."
  (when (null-pointer? *window)
    (error "NULL window."))

  (let ((*font-select-widget
         (schematic_window_get_font_select_widget *window))
        (*dialog (schematic_window_get_font_select_dialog *window)))
    (if (not (null-pointer? *dialog))
        (gtk_window_present *dialog)

        (let ((*new-dialog
               (make-widget-dialog *window
                                   *font-select-widget
                                   (string->pointer (G_ "Select Schematic Font"))
                                   (string->pointer "fontsel"))))
          (schematic_window_set_font_select_dialog *window *new-dialog)))))
