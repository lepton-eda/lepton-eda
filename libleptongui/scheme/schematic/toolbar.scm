;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022 Lepton EDA Contributors
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


(define-module (schematic toolbar)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton gettext)

  #:use-module (schematic ffi)

  #:export (make-toolbar))


(define (init-toolbar *button *toolbar)
  (schematic_toolbar_activate_button *button)
  *toolbar)

(define (make-toolbar-button *window *toolbar icon name tooltip callback position)
  (schematic_toolbar_button_new *window
                                *toolbar
                                (string->pointer icon)
                                (string->pointer (G_ name))
                                (string->pointer (G_ tooltip))
                                (procedure->pointer void callback '(* *))
                                position))

(define (make-toolbar-radio-button *group *window *toolbar icon name tooltip callback position)
  (schematic_toolbar_radio_button_new *group
                                      *window
                                      *toolbar
                                      (string->pointer icon)
                                      (string->pointer (G_ name))
                                      (string->pointer (G_ tooltip))
                                      (procedure->pointer void callback '(* *))
                                      position))


(define (make-toolbar *window *main-box)
  (let ((*toolbar (schematic_toolbar_new *window *main-box)))
    (make-toolbar-button *window
                         *toolbar
                         "document-new"
                         "New"
                         "New file"
                         i_callback_file_new
                         0)
    (make-toolbar-button *window
                         *toolbar
                         "document-open"
                         "Open"
                         "Open file"
                         i_callback_file_open
                         1)
    (make-toolbar-button *window *toolbar
                         "document-save"
                         "Save"
                         "Save file"
                         i_callback_file_save
                         2)
    (schematic_toolbar_insert_separator *toolbar 3)
    (make-toolbar-button *window
                         *toolbar
                         "edit-undo"
                         "Undo"
                         "Undo last operation"
                         i_callback_edit_undo
                         4)
    (make-toolbar-button *window
                         *toolbar
                         "edit-redo"
                         "Redo"
                         "Redo last undo"
                         i_callback_edit_redo
                         5)
    (schematic_toolbar_insert_separator *toolbar 6)
    (make-toolbar-button *window
                         *toolbar
                         "insert-symbol"
                         "Component"
                         "Add component...
Select library and component from list, move the mouse into main window, click to place.
Right mouse button to cancel"
                         i_callback_add_component
                         7)

    (let* ((*radio-button
            (make-toolbar-radio-button (bytevector->pointer (make-bytevector (sizeof '*) 0))
                                       *window
                                       *toolbar
                                       "insert-net"
                                       "Nets"
                                       "Add nets mode
Right mouse button to cancel"
                                       i_callback_toolbar_add_net
                                       8))
           (*radio-group (schematic_toolbar_radio_button_get_group *radio-button)))
      (schematic_window_set_toolbar_net *window *radio-button)

      (let* ((*radio-button
              (make-toolbar-radio-button (reference-pointer *radio-group)
                                         *window
                                         *toolbar
                                         "insert-bus"
                                         "Bus"
                                         "Add buses mode
Right mouse button to cancel"
                                         i_callback_toolbar_add_bus
                                         9))
             (*radio-group (schematic_toolbar_radio_button_get_group *radio-button)))
        (schematic_window_set_toolbar_bus *window *radio-button)

        (make-toolbar-button *window
                             *toolbar
                             "insert-text"
                             "Text"
                             "Add Text..."
                             i_callback_add_text
                             10)
        (schematic_toolbar_insert_separator *toolbar 11)


        (let ((*radio-button
               (make-toolbar-radio-button (reference-pointer *radio-group)
                                          *window
                                          *toolbar
                                          "select"
                                          "Select"
                                          "Select mode"
                                          i_callback_toolbar_edit_select
                                          12)))
          (schematic_window_set_toolbar_select *window *radio-button)

          (schematic_toolbar_insert_separator *toolbar 13)
          ;; Activate 'select' button at start-up.
          (init-toolbar *radio-button *toolbar))))))
