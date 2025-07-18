;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2025 Lepton EDA Contributors
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


(define-module (schematic dialog autonumber)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (autonumber-dialog))


(define (run-autonumbering *autotext)
  (schematic_autonumber_run *autotext))


;;; Start autonumbering based on settings stored in the *AUTOTEXT
;;; object.
(define (start-autonumbering *autotext)
  (schematic_autonumber_dialog_save_state *autotext)
  (if (and (true? (schematic_autonumber_get_autotext_removenum *autotext))
           (false? (schematic_autonumber_get_autotext_scope_overwrite *autotext)))
      (begin
        ;; Temporarily set the overwrite flag.
        (schematic_autonumber_set_autotext_scope_overwrite *autotext TRUE)
        (run-autonumbering *autotext)
        (schematic_autonumber_set_autotext_scope_overwrite *autotext FALSE))

      (run-autonumbering *autotext)))


;;; Destroy the Autonumber dialog.
(define (destroy-autonumber-dialog! *autotext)
  (gtk_widget_destroy (schematic_autonumber_get_autotext_dialog *autotext))
  (schematic_autonumber_set_autotext_dialog *autotext %null-pointer))


;;; Get response signal from the autonumber dialog.  The function
;;; gets the autonumber dialog response ID and either starts
;;; autonumbering or destroys the dialog.
(define (autonumber-response *widget response *autotext)
  (if (eq? (gtk-response->symbol response) 'accept)
      ;; Triggering the apply button will call the autonumber
      ;; action functions.
      (start-autonumbering *autotext)
      ;; Close the dialog if the close button is pressed or the
      ;; user closes the dialog window.
      (destroy-autonumber-dialog! *autotext)))

;;; Response callback for the autonumber text dialog.
(define *autonumber-response-callback
  (procedure->pointer void autonumber-response (list '* int '*)))


;;; Activate or deactivate the "Overwrite existing numbers"
;;; checkbox depending on the state of the "Remove numbers"
;;; checkbox.
(define (autonumber-remove-numbers-checkbox-clicked-callback *widget *dialog)
  (gtk_widget_set_sensitive
   (schematic_autonumber_dialog_lookup_widget
    *dialog
    (string->pointer "scope_overwrite"))
   (if (true? (gtk_toggle_button_get_active *widget)) 0 1)))

(define *autonumber-remove-numbers-checkbox-clicked-callback
  (procedure->pointer void autonumber-remove-numbers-checkbox-clicked-callback '(* *)))


(define %gtk-response-accept (symbol->gtk-response 'accept))


;;; Create a structure for storing autonumber dialog state.
(define (make-autonumber-dialog-state)
  (define *autotext (schematic_autonumber_new))

  (schematic_autonumber_set_autotext_sort_order
   *autotext
   (schematic_autonumber_sort_order_from_string (string->pointer "sort-diagonal")))

  (schematic_autonumber_set_autotext_startnum *autotext 1)
  (schematic_autonumber_set_autotext_removenum *autotext FALSE)
  (schematic_autonumber_set_autotext_slotting *autotext FALSE)
  (schematic_autonumber_set_autotext_dialog *autotext %null-pointer)

  *autotext)

(define (autonumber-dialog window)
  "Opens autonumber dialog in WINDOW."
  (define *window (check-window window 1))
  (define *autotext
    (let ((*current-autotext (schematic_autonumber_get_autotext)))
      (if (null-pointer? *current-autotext)
          ;; If 'autotext' structure is NULL, let's init it.
          (let ((*new-autotext (make-autonumber-dialog-state)))
            (schematic_autonumber_set_autotext *new-autotext)
            *new-autotext)
          *current-autotext)))
  (define *current-dialog
    (schematic_autonumber_get_autotext_dialog *autotext))

  (define (make-autonumber-dialog)
    (let* ((*dialog (schematic_autonumber_dialog_new *window))
           (*remove-number-widget
            (schematic_autonumber_dialog_lookup_widget
             *dialog
             (string->pointer "opt_removenum")))
           (*sort-order-widget
            (schematic_autonumber_dialog_lookup_widget
             *dialog
             (string->pointer "sort_order"))))
      (schematic_autonumber_sort_order_widget_init *sort-order-widget)

      (gtk_dialog_set_default_response *dialog %gtk-response-accept)

      (schematic_signal_connect *dialog
                                (string->pointer "response")
                                *autonumber-response-callback
                                *autotext)
      (schematic_signal_connect *remove-number-widget
                                (string->pointer "clicked")
                                *autonumber-remove-numbers-checkbox-clicked-callback
                                *dialog)

      (schematic_autonumber_set_autotext_dialog *autotext *dialog)
      (schematic_autonumber_dialog_restore_state *autotext)
      (gtk_widget_show_all *dialog)
      *dialog))

  ;; If the function is called the first time the dialog is
  ;; created.  If the dialog is only in background it is moved to
  ;; the foreground.
  (let ((*dialog (if (null-pointer? *current-dialog)
                     ;; Create a new dialog.
                     (make-autonumber-dialog)
                     ;; Return existing dialog.
                     *current-dialog)))

    ;; Remember the parent window in *autotext.  To make the
    ;; widget dockable each window has to have an individual
    ;; autonumber widget, which is not yet implemented.
    (schematic_autonumber_set_autotext_window *autotext *window)

    (gtk_window_present (gtk_widget_get_gtk_window *dialog))))
