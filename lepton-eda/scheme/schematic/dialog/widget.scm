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
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton gettext)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)

  #:export (make-widget-dialog))

;;; Defined in gtkdialog.h.
(define GTK_DIALOG_DESTROY_WITH_PARENT 2)

(define (make-widget-dialog *window *widget *title *settings-group)
  "Creates a new dialog box for *WIDGET in *WINDOW setting its title to
*TITLE and its group of settings to *SETTINGS-GROUP.  The dialog will
be a parent for the *WIDGET."
  (when (null-pointer? *widget)
    (error "NULL widget."))

  (let* ((*main-window (schematic_window_get_main_window *window))
         (*dialog (schematic_dialog_new_empty *title
                                              *main-window
                                              GTK_DIALOG_DESTROY_WITH_PARENT
                                              *settings-group
                                              *window))
         (*content-area (gtk_dialog_get_content_area *dialog)))

    (gtk_dialog_add_button *dialog
                           (string->pointer (G_ "_Close"))
                           (symbol->gtk-response 'none))

    (when (true? (x_widgets_use_toplevel_windows))
      (gtk_window_set_transient_for *dialog %null-pointer)
      (gtk_window_set_type_hint *dialog
                                (gdk_string_to_window_type_hint
                                 (string->pointer "normal"))))

    (g_signal_connect *dialog
                      (string->pointer "response")
                      *gtk_widget_hide
                      %null-pointer)

    (g_signal_connect *dialog
                      (string->pointer "delete-event")
                      *gtk_widget_hide_on_delete
                      %null-pointer)

    (gtk_container_add *content-area *widget)

    (gtk_widget_show_all *dialog)
    (gtk_window_present *dialog)

    *dialog))
