;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2025 Lepton EDA Contributors
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


(define-module (schematic dialog new-text)
  #:use-module (system foreign)

  #:use-module (lepton color-map)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton log)
  #:use-module (lepton object text)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (new-text-dialog))


;;; Callback function for the text entry dialog that handles user
;;; responses.
(define (newtext-dialog-response *dialog response *window)
  (define (log-warning)
    (log! 'warning
          "newtext-dialog-response(): strange signal: ~A" response)
    #f)

  (define (close-dialog!)
    (i_callback_cancel %null-pointer *window)
    (gtk_widget_destroy *dialog)
    (schematic_window_set_newtext_dialog *window %null-pointer))

  (define (text-caps->symbol caps)
    (string->symbol (pointer->string
                     (schematic_window_text_caps_to_string caps))))

  (define (prepare-text-placement *window *str color align angle size)
    (let ((*canvas (schematic_window_get_current_canvas *window)))
      (if (null-pointer? *canvas)
          (error "NULL canvas")
          (let ((*page (schematic_canvas_get_page *canvas)))
            (unless (null-pointer? *page)
              ;; Insert the new object into the buffer at world
              ;; coordinates (0,0).  It will be translated to the
              ;; mouse coordinates during placement.
              (schematic_window_set_first_wx *window 0)
              (schematic_window_set_first_wy *window 0)

              ;; Remove the old place list if it exists.
              (schematic_window_delete_place_list *window)

              ;; Add a new text object.
              (schematic_window_set_place_list
               *window
               (g_list_append
                (schematic_window_get_place_list *window)
                (lepton_text_object_new color
                                        0 ; x
                                        0 ; y
                                        align
                                        angle
                                        *str
                                        size
                                        ;; The object has to be
                                        ;; visible so it can be
                                        ;; placed.
                                        (text-visibility->integer #t)
                                        (symbol->text-attribute-show-mode 'both))))

              (i_action_start *window)
              (set-action-mode! 'text-mode #:window (pointer->window *window)))))))

  ;; Apply the text from the text entry dialog.
  (define (apply-changes!)
    (if (null-pointer? *dialog)
        (error "NULL dialog")
        (let ((*window (schematic_newtext_dialog_get_window *dialog)))
          (if (null-pointer? *window)
              (error "NULL window")
              (let* ((*text-view (schematic_newtext_dialog_get_text_view *dialog))
                     (*str (schematic_newtext_dialog_get_text *text-view))
                     (str (pointer->string *str)))
                ;; We own the string so have to free it.
                (g_free *str)
                (unless (string-null? str)
                  (let* ((text-str
                          (case (schematic_window_get_text_caps *window)
                            ((lower) (string-downcase str))
                            ((upper) (string-upcase str))
                            (else str)))
                         (dialog-color-value
                          (x_colorcb_get_index
                           (schematic_newtext_dialog_get_colorcb *dialog)))
                         (color (if (>= dialog-color-value 0)
                                    dialog-color-value
                                    (color-map-name-to-index 'text)))
                         (dialog-align-value
                          (schematic_alignment_combo_get_align
                           (schematic_newtext_dialog_get_aligncb *dialog)))
                         (align (if (>= dialog-align-value 0)
                                    dialog-align-value
                                    (symbol->text-alignment 'lower-left)))
                         (dialog-size-value
                          (schematic_integer_combo_box_get_value
                           (schematic_newtext_dialog_get_textsizecb *dialog)))
                         (size (if (> dialog-size-value 0)
                                   dialog-size-value
                                   (schematic_window_get_text_size *window)))
                         (dialog-angle-value
                          (schematic_rotation_combo_get_angle
                           (schematic_newtext_dialog_get_rotatecb *dialog)))
                         (angle (if (>= dialog-angle-value 0)
                                    dialog-angle-value
                                    0)))

                    ;; Select the text, so you can continue
                    ;; immediatly writing the next text.
                    (schematic_newtext_dialog_textview_select_all *text-view)
                    (gtk_widget_grab_focus *text-view)
                    (prepare-text-placement *window
                                            (string->pointer text-str)
                                            color
                                            align
                                            angle
                                            size))))))))

  (case (gtk-response->symbol response)
    ((apply) (apply-changes!))
    ((close delete-event) (close-dialog!))
    (else (log-warning))))


(define *newtext-dialog-response
  (procedure->pointer void newtext-dialog-response (list '* int '*)))


(define* (new-text-dialog #:optional (window (current-window)))
  "Run the New text dialog.  The optional argument WINDOW defines the
window the dialog should be run for.  By default it is the current
window."
  (define *window (check-window window 1))

  (define *newtext-widget
    (schematic_window_get_newtext_dialog *window))

  (define (get-newtext-widget)
    (if (null-pointer? *newtext-widget)
        ;; Widget not created yet, create it.
        (let ((*widget (schematic_newtext_dialog_new *window)))
          ;; Store pointer to the widget in the *window structure.
          (schematic_window_set_newtext_dialog *window *widget)
          ;; Connect callback to the widget's "response" signal.
          (schematic_signal_connect *widget
                                    (string->pointer "response")
                                    *newtext-dialog-response
                                    *window)
          *widget)
        ;; Otherwise just return the widget.
        *newtext-widget))

  (o_redraw_cleanstates *window)
  (o_invalidate_rubber *window)

  (i_action_stop *window)
  (set-action-mode! 'select-mode #:window window)

  (schematic_newtext_dialog_run (get-newtext-widget)))
