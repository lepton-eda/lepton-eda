;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2024-2026 Lepton EDA Contributors
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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton attrib)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton gettext)
  #:use-module (lepton m4)
  #:use-module (lepton object foreign)
  #:use-module (lepton object)

  #:use-module (schematic action)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic selection)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)
  #:use-module (schematic window)

  #:export (attrib-edit-dialog))

;;; liblepton/include/liblepton/defines.h
(define SHOW_NAME_VALUE 0)
(define SHOW_VALUE 1)
(define SHOW_NAME 2)
(define INVISIBLE 0)
(define VISIBLE 1)

;;; Definition from x_dialog.h.
;; #define DIALOG_BORDER_SPACING 5
(define %dialog-border-spacing 5)
;; #define DIALOG_V_SPACING 8
(define %dialog-vertical-spacing 8)
;; #define DIALOG_H_SPACING 10
(define %dialog-horizontal-spacing 10)
;; #define DIALOG_INDENTATION 10
(define %dialog-indentation 10)


;;; The following functions could be replaced with
;;; number->string() and string->number() with following
;;; conversion to/from pointers.

;; This is an analog of GPOINTER_TO_INT.
(define (pointer->int *p)
  (let ((bv (make-bytevector (sizeof '*) 0)))
    (bytevector-uint-set! bv 0 (pointer-address *p) (native-endianness) (sizeof '*))
    (bytevector-sint-ref bv 0 (native-endianness) (sizeof int))))


;; This is an analog of INT_TO_GPOINTER.
(define (int->pointer val)
  (define bv (make-bytevector (sizeof int) 0))
  (bytevector-sint-set! bv 0 val (native-endianness) (sizeof int))
  (make-pointer (bytevector-uint-ref bv 0 (native-endianness) (sizeof int))))


;;; Create a new empty attribute add/edit dialog widget with two
;;; buttons.
(define (make-attrib-edit-dialog *window existing-attrib?)
  (define title (if existing-attrib?
                    (G_ "Edit Attribute")
                    (G_ "Add Attribute")))
  (define *parent (schematic_window_get_main_window *window))
  (define flags GTK_DIALOG_MODAL)
  (define *settings-name (string->pointer "singleattrib"))
  (define *dialog (schematic_dialog_new_empty (string->pointer title)
                                              *parent
                                              flags
                                              *settings-name
                                              *window))

  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Cancel"))
                         GTK_RESPONSE_REJECT)
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_OK"))
                         GTK_RESPONSE_APPLY)
  (gtk_window_set_position *dialog
                           (symbol->gtk-window-position 'mouse))

  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_APPLY)

  *dialog)


(define (add-new-attrib! *widget *window *newtext visibility show-mode)
  ;; There is one or no selected object.
  (define *object (o_select_return_first_object *window))

  ;; We should set reasonable defaults for the coordinate of the
  ;; new attribute if it is floating, that is, no object to attach
  ;; to is selected.  We can do it only if the mouse pointer is
  ;; somewhere over the canvas.  For this, the 'position' property
  ;; must be set to TRUE.  In any other case, let's rely on
  ;; o_attrib_add_attrib() in choosing some reasonable strategy
  ;; for this.
  (define wx
    (pointer->int (g_object_get_data *widget
                                     (string->pointer "position-wx"))))
  (define wy
    (pointer->int (g_object_get_data *widget
                                     (string->pointer "position-wy"))))
  (define position
    (pointer->int (g_object_get_data *widget
                                     (string->pointer "position"))))
  (define propose-coord (if (null-pointer? *object) position FALSE))

  (o_attrib_add_attrib *window
                       *newtext
                       visibility
                       show-mode
                       *object
                       propose-coord
                       wx
                       wy)

  (schematic_window_active_page_changed *window)
  (o_undo_savestate_old *window))


(define (add-attrib-to-object! *widget
                               *window
                               *newtext
                               name
                               visibility
                               show-mode)
  (define *add-to-all-button
    (g_object_get_data *widget (string->pointer "add-to-all-button")))

  (define *add-to-components-button
    (g_object_get_data *widget (string->pointer "add-to-components-button")))

  (define *add-to-nets-button
    (g_object_get_data *widget (string->pointer "add-to-nets-button")))

  (define *overwrite-button
    (g_object_get_data *widget (string->pointer "overwrite-button")))

  (define replace?
    (true? (gtk_toggle_button_get_active *overwrite-button)))

  (define add-to
    (cond
     ((true? (gtk_toggle_button_get_active *add-to-all-button)) 7)
     ((true? (gtk_toggle_button_get_active *add-to-components-button)) 2)
     ((true? (gtk_toggle_button_get_active *add-to-nets-button)) 1)
     (else 0)))

  (let loop ((object-ls (page-selection (active-page))))
    (unless (null? object-ls)
      (let* ((object (car object-ls))
             (*object (object->pointer object)))
        (unless (text? object)
          (let ((add-mask (if (component? object)
                              2
                              (if (net? object)
                                  1
                                  ;; Default for other objects.
                                  4))))

            (when (logtest add-mask add-to)
              (let ((replaced
                     (and replace?
                          (let loop ((attrib-ls (object-attribs object))
                                     (replaced? #f))
                            (if (null? attrib-ls)
                                replaced?
                                (loop (cdr attrib-ls)
                                      (let ((attrib (car attrib-ls)))
                                        (if (string= (attrib-name attrib) name)
                                            (begin
                                              (o_text_change *window
                                                             (object->pointer attrib)
                                                             *newtext
                                                             visibility
                                                             show-mode)
                                              (schematic_window_active_page_changed *window)
                                              ;; The attribute has been replaced.
                                              #t)
                                            replaced?))))))))
                (unless replaced
                  (o_attrib_add_attrib *window
                                       *newtext
                                       visibility
                                       show-mode
                                       *object
                                       FALSE
                                       0
                                       0)))))))
      (loop (cdr object-ls))))
  (o_undo_savestate_old *window))


;;; Handle the OK response of the Attribute add/edit dialog.
(define (ok-handler *widget *window)
  (define *value-entry
    (g_object_get_data *widget (string->pointer "value-entry")))
  (define *name-entry
    (g_object_get_data *widget (string->pointer "attrib-combo-entry")))
  (define *visibility-button
    (g_object_get_data *widget (string->pointer "visibility-button")))
  (define *show-options
    (g_object_get_data *widget (string->pointer "show-options")))
  (define *value (gtk_entry_get_text *value-entry))
  (define *name (gtk_entry_get_text *name-entry))
  (define value (if (null-pointer? *value) "" (pointer->string *value)))
  (define name (if (null-pointer? *name) "" (pointer->string *name)))
  (define newtext (string-append name "=" value))
  (define *newtext (string->pointer newtext))
  (define non-attached-objects-number
    (pointer->int
     (g_object_get_data *widget
                        (string->pointer "number-of-objects"))))

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
                      "Got invalid attribute mode option")
              SHOW_NAME_VALUE))))

      (let ((*attrib
             (g_object_get_data *widget (string->pointer "attrib"))))
        (if (null-pointer? *attrib)
            (if (> non-attached-objects-number 1)
                (add-attrib-to-object! *widget
                                       *window
                                       *newtext
                                       name
                                       visibility
                                       show-mode)
                (add-new-attrib! *widget
                                 *window
                                 *newtext
                                 visibility
                                 show-mode))
            (begin
              (o_text_change *window
                             *attrib
                             *newtext
                             visibility
                             show-mode)
              (schematic_window_active_page_changed *window)
              (o_undo_savestate_old *window))))

      (gtk_grab_remove *widget)
      (gtk_widget_destroy *widget)
      (schematic_window_set_attrib_edit_widget *window %null-pointer))))


;;; Response function for the Attribute add/edit dialog.
(define (response *widget response *window)
  (with-window *window
    (case (gtk-response->symbol response)
      ((apply)
       (ok-handler *widget *window))
      ((reject delete-event)
       (gtk_grab_remove *widget)
       (gtk_widget_destroy *widget)
       (schematic_window_set_attrib_edit_widget *window
                                                %null-pointer))
      (else #f))))

(define *response
  (procedure->pointer void response (list '* int '*)))


(define (gobject-set-data-full *gobject *child child-name)
  (g_object_ref *child)
  (g_object_set_data_full *gobject
                          (string->pointer child-name)
                          *child
                          ;; GDestroyNotify
                          *g_object_unref))


(define* (attrib-edit-dialog window #:optional (attrib #f))
  "Launch the single attrib edit dialog in WINDOW for ATTRIB."
  (define *window (check-window window 1))
  (define *attrib (if attrib
                      (check-object attrib 2 attribute? 'attribute)
                      %null-pointer))
  (define (attached-attribute? object)
    (and (attribute? object)
         (attrib-attachment object)))

  (define selection (page-selection (active-page)))

  (define non-attached-objects-number
    (length (filter-map (negate attached-attribute?) selection)))

  (define first-object (and (not (null? selection)) (car selection)))

  ;; Return if the widget has already been created.
  (when (null-pointer? (schematic_window_get_attrib_edit_widget *window))
    (let ((*dialog (make-attrib-edit-dialog *window attrib)))

      (g_signal_connect *dialog
                        (string->pointer "response")
                        *response
                        *window)

      (let* ((*vbox (gtk_dialog_get_content_area *dialog))
             ;; Name selection.
             (*name-label (gtk_label_new_with_mnemonic
                           (string->pointer (G_ "N_ame:"))))
             ;; Value entry.
             (*value-label (gtk_label_new_with_mnemonic
                            (string->pointer (G_ "_Value:"))))
             (*attrib-combo-box-entry
              (gtk_combo_box_text_new_with_entry))
             (*attrib-combo-entry
              (gtk_bin_get_child *attrib-combo-box-entry))
             (*value-entry (gtk_entry_new))
             (*visibility-button
              (gtk_check_button_new_with_mnemonic
               (string->pointer (G_ "Vi_sible"))))
             (*show-options (gtk_combo_box_text_new))
             (*store (schematic_string_list_store_new))
             (visible (if (or (not attrib)
                              (text-visible? attrib))
                          TRUE
                          FALSE))
             (show-mode-combobox-number
              (case (and attrib (text-attribute-mode attrib))
                ((value) 0)
                ((name) 1)
                ((both) 2)
                ;; Show value.
                (else 0))))

        (gtk_container_set_border_width *dialog
                                        %dialog-border-spacing)
        (gtk_box_set_spacing *vbox %dialog-vertical-spacing)

        (g_object_set_data *dialog
                           (string->pointer "number-of-objects")
                           (int->pointer non-attached-objects-number))

        (gtk_label_set_mnemonic_widget *name-label
                                       *attrib-combo-box-entry)
        (gobject-set-data-full *dialog
                               *attrib-combo-entry
                               "attrib-combo-entry")
        (gtk_entry_set_activates_default *attrib-combo-entry TRUE)

        (gobject-set-data-full *dialog *value-entry "value-entry")
        (gtk_label_set_mnemonic_widget *value-label *value-entry)
        (gtk_entry_set_activates_default *value-entry TRUE)

        ;; Visibility.
        (gobject-set-data-full *dialog
                               *visibility-button
                               "visibility-button")
        (gtk_toggle_button_set_active *visibility-button visible)

        (gtk_combo_box_set_model *show-options *store)
        (gobject-set-data-full *dialog *show-options "show-options")

        (gtk_combo_box_text_append_text
         *show-options
         (string->pointer (G_ "Show Value Only")))
        (gtk_combo_box_text_append_text
         *show-options
         (string->pointer (G_ "Show Name Only")))
        (gtk_combo_box_text_append_text
         *show-options
         (string->pointer (G_ "Show Name & Value")))
        (gtk_combo_box_set_active *show-options
                                  show-mode-combobox-number)

        (if %m4-use-gtk3
            (let ((*grid (gtk_grid_new)))
              (gtk_grid_set_row_spacing *grid %dialog-vertical-spacing)
              (gtk_grid_set_column_spacing *grid %dialog-horizontal-spacing)
              (gtk_box_pack_start *vbox *grid TRUE TRUE 0)
              (gtk_label_set_xalign *name-label 0.0)
              (gtk_label_set_yalign *name-label 0.5)
              (gtk_grid_attach *grid *name-label 0 0 1 1)
              (gtk_widget_set_hexpand *attrib-combo-box-entry TRUE)
              (gtk_grid_attach *grid *attrib-combo-box-entry 1 0 1 1)
              (gtk_label_set_xalign *value-label 0.0)
              (gtk_label_set_yalign *value-label 0.5)
              (gtk_grid_attach *grid *value-label 0 1 1 1)
              (gtk_grid_attach *grid *value-entry 1 1 1 1)
              (gtk_grid_attach *grid *visibility-button 0 2 1 1)
              (gtk_grid_attach *grid *show-options 1 2 1 1))

            (let ((*alignment (gtk_alignment_new 0 0 1 1))
                  (*table (gtk_table_new 3 2 FALSE)))
              (gtk_alignment_set_padding *alignment 0 0 %dialog-indentation 0)
              (gtk_box_pack_start *vbox *alignment TRUE TRUE 0)
              (gtk_table_set_row_spacings *table %dialog-vertical-spacing)
              (gtk_table_set_col_spacings *table %dialog-horizontal-spacing)
              (gtk_container_add *alignment *table)
              (gtk_misc_set_alignment *name-label 0 0.5)
              (gtk_table_attach *table *name-label 0 1 0 1 GTK_FILL GTK_FILL 0 0)
              (gtk_table_attach *table *attrib-combo-box-entry 1 2 0 1 (logior GTK_EXPAND GTK_FILL) 0 0 0)
              (gtk_misc_set_alignment *value-label 0 0.5)
              (gtk_table_attach *table *value-label 0 1 1 2 GTK_FILL 0 0 0)
              (gtk_table_attach *table *value-entry 1 2 1 2 (logior GTK_EXPAND GTK_FILL) 0 0 0)
              (gtk_table_attach *table *visibility-button 0 1 2 3 GTK_FILL 0 0 0)
              (gtk_table_attach *table *show-options 1 2 2 3 (logior GTK_FILL GTK_EXPAND) 0 0 0)))

        (when (> non-attached-objects-number 1)
          (let* ((*attach-options-label
                  (gtk_label_new
                   (string->pointer (G_ "<b>Attach Options</b>"))))
                 (*add-to-all-button
                  (gtk_radio_button_new_with_label
                   %null-pointer
                   (string->pointer (G_ "All"))))
                 (*hbox2-group-0
                  (gtk_radio_button_get_group *add-to-all-button))
                 (*add-to-components-button
                  (gtk_radio_button_new_with_label
                   *hbox2-group-0
                   (string->pointer (G_ "Components"))))
                 (*hbox2-group-1
                  (gtk_radio_button_get_group *add-to-components-button))
                 (*add-to-nets-button
                  (gtk_radio_button_new_with_label
                   *hbox2-group-1
                   (string->pointer (G_ "Nets"))))
                 (*hbox2-group
                  (gtk_radio_button_get_group *add-to-nets-button))
                 (*overwrite-button
                  (gtk_check_button_new_with_label
                   (string->pointer (G_ "Replace existing attributes")))))

            (gtk_label_set_use_markup *attach-options-label TRUE)
            (if %m4-use-gtk3
                (begin
                  (gtk_label_set_xalign *attach-options-label 0.0)
                  (gtk_label_set_yalign *attach-options-label 0.0))
                (gtk_misc_set_alignment *attach-options-label 0 0))
            (gtk_box_pack_start *vbox *attach-options-label FALSE FALSE 0)

            (gobject-set-data-full *dialog
                                   *add-to-all-button
                                   "add-to-all-button")

            (gobject-set-data-full *dialog
                                   *add-to-components-button
                                   "add-to-components-button")

            (gobject-set-data-full *dialog
                                   *add-to-nets-button
                                   "add-to-nets-button")

            (gtk_toggle_button_set_active *overwrite-button TRUE)
            (gobject-set-data-full *dialog
                                   *overwrite-button
                                   "overwrite-button")
            (if %m4-use-gtk3
                (let ((*grid (gtk_grid_new)))
                  (gtk_grid_set_row_spacing *grid %dialog-vertical-spacing)
                  (gtk_grid_set_column_spacing *grid %dialog-horizontal-spacing)
                  (gtk_box_pack_start *vbox *grid TRUE TRUE 0)
                  (gtk_grid_attach *grid *add-to-all-button 0 0 1 1)
                  (gtk_grid_attach *grid *add-to-components-button 1 0 1 1)
                  (gtk_grid_attach *grid *add-to-nets-button 2 0 1 1)
                  (gtk_grid_attach *grid *overwrite-button 0 1 1 1))

                (let ((*alignment (gtk_alignment_new 0 0 1 1))
                      (*table (gtk_table_new 2 3 FALSE)))
                  (gtk_alignment_set_padding *alignment 0 0 %dialog-indentation 0)
                  (gtk_box_pack_start *vbox *alignment TRUE TRUE 0)
                  (gtk_table_set_row_spacings *table %dialog-vertical-spacing)
                  (gtk_table_set_col_spacings *table %dialog-horizontal-spacing)
                  (gtk_container_add *alignment *table)
                  (gtk_table_attach *table *add-to-all-button 0 1 0 1 GTK_FILL 0 0 0)
                  (gtk_table_attach *table *add-to-components-button 1 2 0 1 GTK_FILL 0 0 0)
                  (gtk_table_attach *table *add-to-nets-button 2 3 0 1 GTK_FILL 0 0 0)
                  (gtk_table_attach *table *overwrite-button 0 3 1 2 GTK_FILL 0 0 0)))))

        (let ((name (if attrib
                        (attrib-name attrib)
                        (and first-object
                             (net? first-object)
                             "netname")))
              (value (and=> attrib attrib-value)))

          (g_object_set_data *dialog
                             (string->pointer "attrib")
                             *attrib)
          (when name
            (gtk_entry_set_text *attrib-combo-entry
                                (string->pointer name)))
          (when value
            (gtk_entry_set_text *value-entry
                                (string->pointer value)))

          (let* ((position (and=> (action-position) snap-point))
                 (wx (if position (car position) -1))
                 (wy (if position (cdr position) -1)))

            (g_object_set_data *dialog
                               (string->pointer "position")
                               (make-pointer (if position TRUE FALSE)))
            (g_object_set_data *dialog
                               (string->pointer "position-wx")
                               (int->pointer wx))
            (g_object_set_data *dialog
                               (string->pointer "position-wy")
                               (int->pointer wy))

            (let loop ((i 0))
              (let ((*string (s_attrib_get i)))
                (unless (null-pointer? *string)
                  (gtk_combo_box_text_append_text *attrib-combo-box-entry
                                                  *string)
                  (loop (1+ i)))))


            ;; Add completion to attribute combo box entry.
            (let ((*attrib-combo-entry-completion
                   (gtk_entry_completion_new)))
              (gtk_entry_completion_set_model
               *attrib-combo-entry-completion
               (gtk_combo_box_get_model *attrib-combo-box-entry))
              (gtk_entry_completion_set_text_column
               *attrib-combo-entry-completion
               0)
              (gtk_entry_completion_set_inline_completion
               *attrib-combo-entry-completion
               TRUE)
              (gtk_entry_completion_set_popup_single_match
               *attrib-combo-entry-completion
               FALSE)
              (gtk_entry_set_completion
               *attrib-combo-entry
               *attrib-combo-entry-completion))

            (gtk_widget_show_all *dialog)
            (schematic_window_set_attrib_edit_widget *window *dialog)

            (gtk_grab_add *dialog)

            (gtk_widget_grab_focus (if name
                                       *value-entry
                                       *attrib-combo-entry))))))))
