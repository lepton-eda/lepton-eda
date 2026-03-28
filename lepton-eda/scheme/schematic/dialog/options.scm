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


(define-module (schematic dialog options)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)

  #:use-module (schematic dialog widget)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic widget)
  #:use-module (schematic window foreign)

  #:export (options-dialog))


(define (options-dialog window)
  "Create and/or show the Options dialog in *WINDOW."
  (show-widget window
               schematic_window_get_options_widget
               schematic_window_get_right_notebook
               schematic_window_get_options_widget_dialog
               schematic_window_set_options_widget_dialog
               "Options"
               "options"))
