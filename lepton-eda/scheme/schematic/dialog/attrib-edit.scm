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

  #:use-module (schematic ffi)

  #:export (attrib-edit-dialog))


(define (response *widget response *window)
  (attribute_edit_dialog_response *widget response *window))

(define *response
  (procedure->pointer void response (list '* int '*)))


(define (attrib-edit-dialog *window *attrib invocation-flag)
  "Launch the single attrib edit dialog in *WINDOW for *ATTRIB.  The
INVOCATION-FLAG must be an integer set to a value defined in
libleptongui/include/schematic_defines.h, namely FROM_MENU (0) or
FROM_KEY (1)."
  (define *attrib-edit-widget
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
  (when (null-pointer? *attrib-edit-widget)
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
