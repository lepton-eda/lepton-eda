;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024 Lepton EDA Contributors
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


(define-module (schematic dialog multiattrib)
  #:use-module (system foreign)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (multiattrib-dialog))


(define (multiattrib-dialog window)
  "Open multiple attribute editor dialog to edit selected objects in
WINDOW."
  (define *window (check-window window 1))
  (define *widget (schematic_window_get_multiattrib_widget *window))

  (if (null-pointer? *widget)
      (schematic_multiattrib_widget_open *window)
      (let ((*dialog-window
             (schematic_multiattrib_widget_get_gtk_window *widget)))
        (gtk_window_present *dialog-window))))
