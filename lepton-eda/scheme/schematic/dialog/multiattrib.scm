;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024-2025 Lepton EDA Contributors
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

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi gobject)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (multiattrib-dialog))

;;; Process the "response" signal returned by the Multiattrib
;;; widget.
(define (response-callback *widget response-id *window)
  (define response (gtk-response->symbol response-id))
  (when (or (eq? response 'close)
            (eq? response 'delete-event))
    (gtk_widget_hide *widget)))

(define *response-callback
  (procedure->pointer void response-callback (list '* int '*)))


(define (delete-event-callback *widget *event *window)
  (if (null-pointer? *window)
      (error "NULL window.")
      (gtk_widget_hide *widget))
  ;; Stop further propagation of the "delete-event" signal for the
  ;; widget.  Otherwise the widget will be destroyed in the code
  ;; of the parent GtkDialog structure.
  TRUE)

;;; The signal is emitted when you click the close button on the
;;; widget dialog window or hit Escape.
(define *delete-event-callback
  (procedure->pointer int delete-event-callback '(* * *)))


(define (multiattrib-dialog window)
  "Open multiple attribute editor dialog to edit selected objects in
WINDOW."
  (define *window (check-window window 1))
  (define *widget (schematic_window_get_multiattrib_widget *window))
  (define *selection (schematic_window_get_selection_list *window))
  (define *main-window (schematic_window_get_main_window *window))

  (if (null-pointer? *widget)
      (let ((*new-widget (schematic_multiattrib_widget_new *window
                                                           *selection)))
        (schematic_window_set_multiattrib_widget *window *new-widget)
        (gtk_window_set_transient_for (gtk_widget_get_gtk_window *new-widget)
                                      (gtk_widget_get_gtk_window *main-window))

        (g_signal_connect *new-widget
                          (string->pointer "delete-event")
                          *delete-event-callback
                          *window)

        (g_signal_connect *new-widget
                          (string->pointer "response")
                          *response-callback
                          *window)
        (gtk_widget_show *new-widget))
      (let ((*dialog-window (gtk_widget_get_gtk_window *widget)))
        (gtk_window_present *dialog-window))))
