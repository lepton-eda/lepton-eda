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

  #:use-module (schematic callback)
  #:use-module (schematic ffi)
  #:use-module (schematic undo)

  #:export (make-toolbar))


(define %toolbar-button-callbacks '())

;;; Prevent garbage collection of callback procedures by storing
;;; them in the global variable.
(define (set-button-callback! *window *button signal callback)
  (let ((*callback (procedure->pointer void callback '(* *))))
    (set! %toolbar-button-callbacks
          (assq-set! %toolbar-button-callbacks *button *callback))
    (schematic_signal_connect *button
                              (string->pointer signal)
                              *callback
                              *window)))


(define (toolbar-insert! *toolbar *button position)
  "Insert *BUTTON into POSITION of *TOOLBAR.  Returns *TOOLBAR."
  (schematic_toolbar_insert_button *toolbar *button position)
  *toolbar)


;;; Sets the button icon for the button pointer *BUTTON to the
;;; pixmap from the file name ICON-NAME.
(define (set-button-icon! *button icon-name)
  (schematic_toolbar_button_set_icon_widget *button
                                            (string->pointer icon-name))
  *button)


;;; Sets the button label text of *BUTTON to the given LABEL text.
(define (set-button-label! *button label)
  (schematic_toolbar_button_set_label *button
                                      (string->pointer (G_ label)))
  *button)


;;; Sets the button tooltip text of *BUTTON to the given TOOLTIP
;;; text.
(define (set-button-tooltip! *button tooltip)
  (schematic_toolbar_button_set_tooltip_text *button
                                             (string->pointer (G_ tooltip)))
  *button)


;;; Makes and returns a new GTK toolbar button with given
;;; parameters.  The *WINDOW argument is necessary for setting a
;;; Scheme callback which may want to get any info from *WINDOW.
;;; The *TOOLBAR argument is a pointer to the toolbar instance to
;;; insert the button to.  ICON defines a filename for the icon
;;; pixmap.  LABEL defines a button text label.  TOOLTIP defines a
;;; tooltip that pops up when the mouse cursor is over the button.
;;; CALLBACK is a Scheme procedure to run when the user clicks the
;;; button.  POSITION is a number defining the place of the button
;;; in the toolbar it should be insert to.
(define (make-toolbar-button *window *toolbar icon label tooltip callback position)
  (let ((*button (schematic_toolbar_button_new)))
    (set-button-icon! *button icon)
    (set-button-label! *button label)
    (set-button-tooltip! *button tooltip)
    (set-button-callback! *window *button "clicked" callback)
    (toolbar-insert! *toolbar *button position)
    *button))


;;; Creates and returns a toolbar radio button.  *GROUP is a group
;;; to insert the radio button to.  Other parameters are the same
;;; with those for make-toolbar-button().
(define (make-toolbar-radio-button *group *window *toolbar icon label tooltip callback position)
  (let ((*button (schematic_toolbar_radio_button_new)))
    (schematic_toolbar_radio_button_set_group *button *group)
    (set-button-icon! *button icon)
    (set-button-label! *button label)
    (set-button-tooltip! *button tooltip)
    (set-button-callback! *window *button "toggled" callback)
    (toolbar-insert! *toolbar *button position)
    *button))


(define* (make-toolbar-separator *toolbar #:optional (position -1))
  (schematic_toolbar_insert_separator *toolbar position))


(define (make-toolbar *window *main-box)
  "Creates a new toolbar widget for *WINDOW, inserting it in the
*MAINBOX widget."
  (let ((*toolbar (schematic_toolbar_new *window *main-box)))
    (make-toolbar-button *window
                         *toolbar
                         "document-new"
                         "New"
                         "New file"
                         callback-file-new
                         0)
    (make-toolbar-button *window
                         *toolbar
                         "document-open"
                         "Open"
                         "Open file"
                         callback-file-open
                         1)
    (make-toolbar-button *window *toolbar
                         "document-save"
                         "Save"
                         "Save file"
                         i_callback_file_save
                         2)
    (make-toolbar-separator *toolbar)
    (make-toolbar-button *window
                         *toolbar
                         "edit-undo"
                         "Undo"
                         "Undo last operation"
                         callback-edit-undo
                         4)
    (make-toolbar-button *window
                         *toolbar
                         "edit-redo"
                         "Redo"
                         "Redo last undo"
                         callback-edit-redo
                         5)
    (make-toolbar-separator *toolbar)
    (make-toolbar-button *window
                         *toolbar
                         "insert-symbol"
                         "Component"
                         (format #f
                         "Add component...\n~
                          Select library and component from list, move\n~
                          the mouse into main window, click to place.\n~
                          Right mouse button to cancel.")
                         callback-add-component
                         7)

    (let* ((*radio-button
            (make-toolbar-radio-button %null-pointer
                                       *window
                                       *toolbar
                                       "insert-net"
                                       "Nets"
                                       (format #f
                                       "Add nets mode\n~
                                        Right mouse button to cancel")
                                       callback-toolbar-add-net
                                       8))
           (*radio-group (schematic_toolbar_radio_button_get_group *radio-button)))

      (let* ((*radio-button
              (make-toolbar-radio-button *radio-group
                                         *window
                                         *toolbar
                                         "insert-bus"
                                         "Bus"
                                         (format #f
                                         "Add buses mode\n~
                                          Right mouse button to cancel")
                                         callback-toolbar-add-bus
                                         9))
             (*radio-group (schematic_toolbar_radio_button_get_group *radio-button)))

        (make-toolbar-button *window
                             *toolbar
                             "insert-text"
                             "Text"
                             "Add Text..."
                             callback-add-text
                             10)
        (make-toolbar-separator *toolbar)


        (let ((*radio-button
               (make-toolbar-radio-button *radio-group
                                          *window
                                          *toolbar
                                          "select"
                                          "Select"
                                          "Select mode"
                                          callback-toolbar-edit-select
                                          12)))

          (make-toolbar-separator *toolbar)
          ;; Activate 'select' button at start-up.
          (schematic_toolbar_activate_button *radio-button)

          ;; Return pointer to the toolbar widget.
          *toolbar)))))
