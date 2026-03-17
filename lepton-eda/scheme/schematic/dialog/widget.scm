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


(define-module (schematic dialog widget)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (make-widget-dialog))


(define (make-widget-dialog *window *widget *title *settings-group)
  "Creates a new dialog box for *WIDGET in *WINDOW setting its title to
*TITLE and its group of settings to *SETTINGS-GROUP.  The dialog will
be a parent for the *WIDGET."
  (define *main-window (schematic_window_get_main_window *window))

  (define *dialog (x_widgets_dialog_new *window
                                        *main-window
                                        *widget
                                        *title
                                        *settings-group))

  (define *content-area (gtk_dialog_get_content_area *dialog))

  (gtk_container_add *content-area *widget)

  (gtk_widget_show_all *dialog)
  (gtk_window_present *dialog)

  *dialog)
