;;; Lepton EDA Schematic Capture
;;; Scheme API
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


(define-module (schematic dialog attrib-edit)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton gettext)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)

  #:export (attrib-edit-dialog))

;;; liblepton/include/liblepton/defines.h
(define SHOW_NAME_VALUE 0)
(define SHOW_VALUE 1)
(define SHOW_NAME 2)
(define INVISIBLE 0)
(define VISIBLE 1)


;;; Handle the OK response of the Attribute add/edit dialog.
(define (ok-handler *widget *window)
  (define *value-entry
    (g_object_get_data *widget (string->pointer "value_entry")))
  (define *name-entry
    (g_object_get_data *widget (string->pointer "attrib_combo_entry")))
  (define *visibility-button
    (g_object_get_data *widget (string->pointer "visbutton")))
  (define *show-options
    (g_object_get_data *widget (string->pointer "show_options")))
  (define *value (gtk_entry_get_text *value-entry))
  (define *label (gtk_entry_get_text *name-entry))
  (define value (if (null-pointer? *value) "" (pointer->string *value)))
  (define label (if (null-pointer? *label) "" (pointer->string *label)))
  (define newtext (string-append label "=" value))
  (define *newtext (string->pointer newtext))

  (when (true? (x_dialog_validate_attribute *widget *newtext))
    (let ((visibility
           (if (true? (gtk_toggle_button_get_active *visibility-button))
               VISIBLE
               INVISIBLE))
          (show-mode
           (case (gtk_combo_box_get_active *show-options)
             ((0) SHOW_VALUE)
             ((1) SHOW_NAME)
             ((2) SHOW_NAME_VALUE)
             (else
              (format (current-error-port)
                      (G_ "Got invalid show option defaulting to show both\n"))
              SHOW_NAME_VALUE))))

      (attrib_edit_dialog_ok *widget *window *newtext visibility show-mode)
      (gtk_grab_remove *widget)
      (gtk_widget_destroy *widget)
      (schematic_window_set_attrib_edit_widget *window %null-pointer))))


;;; Response function for the Attribute add/edit dialog.
(define (response *widget response *window)
  (case (gtk-response->symbol response)
    ((apply)
     (ok-handler *widget *window))
    ((reject delete-event)
     (gtk_grab_remove *widget)
     (gtk_widget_destroy *widget)
     (schematic_window_set_attrib_edit_widget *window
                                              %null-pointer))
    (else #f)))

(define *response
  (procedure->pointer void response (list '* int '*)))


(define (attrib-edit-dialog *window *attrib invocation-flag)
  "Launch the single attrib edit dialog in *WINDOW for *ATTRIB.  The
INVOCATION-FLAG must be an integer set to a value defined in
libleptongui/include/schematic_defines.h, namely FROM_MENU (0) or
FROM_KEY (1)."
  (define *widget
    (schematic_window_get_attrib_edit_widget *window))

  (define (get-number-of-not-attached-objects *selection-list)
    (let loop ((*list *selection-list)
               (count 0))
      (if (null-pointer? *list)
          count
          (let ((*next (glist-next *list)))
            (loop *next
                  (if (null-pointer? (lepton_object_get_attached_to (glist-data *list)))
                      (1+ count)
                      count))))))

  ;; Return if the widget has already been created.
  (when (null-pointer? *widget)
    (let ((*selection-list (lepton_list_get_glist
                            (schematic_window_get_selection_list *window)))
          (*attrib-edit-dialog (schematic_attrib_edit_dialog_new *window)))

      (g_signal_connect *attrib-edit-dialog
                        (string->pointer "response")
                        *response
                        *window)

      (attrib_edit_dialog *window
                          *attrib-edit-dialog
                          *attrib
                          invocation-flag
                          (get-number-of-not-attached-objects *selection-list)))))
