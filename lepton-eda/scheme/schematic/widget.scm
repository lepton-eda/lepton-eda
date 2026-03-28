;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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


(define-module (schematic widget)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (show-notebook-widget))


(define (show-notebook-widget *notebook *widget)
  "Shows *WIDGET in *NOTEBOOK."
  (when (null-pointer? *notebook)
    (error "NULL notebook."))
  (when (null-pointer? *widget)
    (error "NULL widget."))

  (let ((page-number (gtk_notebook_page_num *notebook *widget)))
    (when (>= page-number 0)
      (gtk_notebook_set_current_page *notebook page-number)
      (gtk_widget_set_visible *notebook TRUE)))

  (gtk_widget_show_all *widget))
