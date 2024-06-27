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
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton ffi)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (autonumber-dialog))


(define (run-autonumbering *autotext)
  (define *window (schematic_autonumber_get_autotext_window *autotext))
  (define *active-page (schematic_window_get_active_page *window))

  (define scope-number
    (schematic_autonumber_get_autotext_scope_number *autotext))

  (define scope
    (string->symbol
     (pointer->string
      (schematic_autonumber_scope_to_string scope-number))))

  ;; Get all pages of the hierarchy.
  (define *pages (s_hierarchy_traversepages *window *active-page FALSE))
  (define *page-ls
    (if (eq? scope 'scope-hierarchy)
        (glist->list *pages identity)
        ;; The text will be searched for only in the current page.
        (list *active-page)))

  (define *scope-text
    (glist-data
     (g_list_first
      (schematic_autonumber_get_autotext_scope_text *autotext))))

  (define scope-text (pointer->string *scope-text))

  (define single-search? (string-suffix? "?" scope-text))
  (define multi-search? (string-suffix? "*" scope-text))

  (define search-text
    (and (or single-search? multi-search?)
         ;; Drop suffix "?" or "*".
         (string-drop-right scope-text 1)))

  (define *search-text
    (and search-text (string->pointer search-text)))

  (define (create-search-text-list *window *page *search-text *search-text-ls)
    (lepton_toplevel_goto_page (schematic_window_get_toplevel *window) *page)
    (schematic_window_page_changed *window)
    ;; Guard to check if the page has already got active.
    (unless (equal? *page (schematic_window_get_active_page *window))
      (error "Processing non-active page."))
    ;; Iterate over all objects and look for matching
    ;; search patterns.
    (let loop ((*objects
                (glist->list
                 (lepton_page_objects *page)
                 identity))
               (*ls *search-text-ls))
      (if (null? *objects)
          *ls
          (loop (cdr *objects)
                (let ((*object (car *objects)))
                  (if (and (true? (lepton_object_is_text *object))
                           (or (eq? scope 'scope-hierarchy)
                               (eq? scope 'scope-page)
                               (and (eq? scope 'scope-selected)
                                    (true? (lepton_object_get_selected *object)))))
                      ;; If the object is text then process it.
                      (let* ((*str (lepton_text_object_get_string *object))
                             ;; The beginning of the current text
                             ;; matches with the searchtext now.
                             ;; Strip of the trailing [0-9?] chars
                             ;; and add it to the searchtext.
                             (*new-search-text (schematic_autonumber_drop_string_suffix *str *search-text)))

                        (if (null-pointer? *new-search-text)
                            *ls
                            (let ((search-text-ls (glist->list *ls pointer->string)))
                              (if (member (pointer->string *new-search-text) search-text-ls)
                                  (begin
                                    (g_free *new-search-text)
                                    *ls)
                                  (g_list_append *ls *new-search-text)))))
                      ;; Otherwise return the list as is.
                      *ls))))))

  (schematic_autonumber_set_autotext_current_searchtext *autotext %null-pointer)
  (schematic_autonumber_set_autotext_root_page *autotext 1)
  (schematic_autonumber_set_autotext_used_numbers *autotext %null-pointer)
  (schematic_autonumber_set_autotext_free_slots *autotext %null-pointer)
  (schematic_autonumber_set_autotext_used_slots *autotext %null-pointer)

  ;; Step2: If the text to search has an asterisk at the end we
  ;; have to find all matching text templates.
  ;;
  ;; Example: "refdes=*" will match each text that starts with
  ;; "refdes=" and has a trailing "?" or a trailing number if the
  ;; "all"-option is set.  We get a list of possible prefixes:
  ;; refdes=R, refdes=C.
  ;;
  ;; If there is only one search pattern, it becomes a single item
  ;; in the template list.
  ;;
  ;; For example: "refdes=C?" will be transformed into a list
  ;; with only one template: refdes=C.
  (if (string-null? scope-text)
      (log! 'message (G_ "No search string given in autonumber text."))
      (if search-text
          (let ((*search-text-list
                 (if single-search?
                     (g_list_append %null-pointer *search-text)
                     (if multi-search?
                         ;; Collect all the possible searchtexts
                         ;; in all pages of the hierarchy.
                         (let loop ((ls *page-ls)
                                    (*search-text-ls %null-pointer))
                           (if (null? ls)
                               *search-text-ls
                               (loop (cdr ls)
                                     (create-search-text-list *window
                                                              (car ls)
                                                              *search-text
                                                              *search-text-ls))))
                         %null-pointer))))
            (schematic_autonumber_run *autotext
                                      *window
                                      *pages
                                      *scope-text
                                      *search-text
                                      *search-text-list
                                      scope-number)
            ;; Go back to the root page.
            (lepton_toplevel_goto_page (schematic_window_get_toplevel *window)
                                       *active-page)
            (schematic_window_page_changed *window)
            (schematic_canvas_invalidate_all (schematic_window_get_current_canvas *window))
            (g_list_free *pages)
            ;; FIXME: Undo information saving has to be done
            ;; for all changed pages in hierarchy, not only for
            ;; the current one.
            (o_undo_savestate_old *window))
          (log! 'message (G_ "No '*' or '?' given at the end of the autonumber text.")))))


;;; Return the widget of *DIALOG by its name which should be a
;;; symbol.
(define (lookup-dialog-widget *dialog name)
  (define *name (string->pointer (symbol->string name)))
  (schematic_autonumber_dialog_lookup_widget *dialog *name))


;;; Save the settings of the Autonumber text dialog in the
;;; *AUTOTEXT variable.
(define (save-autonumber-dialog-state *autotext)
  (define *dialog
    (schematic_autonumber_get_autotext_dialog *autotext))

  (define (combo-box-value name)
    (gtk_combo_box_get_active
     (lookup-dialog-widget *dialog name)))

  (define (toggle-button-active? name)
    (gtk_toggle_button_get_active
     (lookup-dialog-widget *dialog name)))

  (define (spin-button-value name)
    (gtk_spin_button_get_value_as_int
     (lookup-dialog-widget *dialog name)))

  ;; Obtain text from the widget NAME.
  (define (search-entry-text name)
    (g_strdup
     (gtk_entry_get_text
      (gtk_bin_get_child
       (lookup-dialog-widget *dialog name)))))

  (define (new-text-ls name)
    (schematic_autonumber_history_add
     (schematic_autonumber_get_autotext_scope_text *autotext)
     (search-entry-text name)))

  (define (set-data! element)
    (let ((name (first element))
          (getter (second element))
          (setter (third element)))
      (setter *autotext (getter name))))

  (define %funcs
    `((scope_text ,new-text-ls
                  ,schematic_autonumber_set_autotext_scope_text)
      (opt_startnum ,spin-button-value
                    ,schematic_autonumber_set_autotext_startnum)
      (scope_skip ,combo-box-value
                  ,schematic_autonumber_set_autotext_scope_skip)
      (scope_number ,combo-box-value
                    ,schematic_autonumber_set_autotext_scope_number)
      (scope_overwrite ,toggle-button-active?
                       ,schematic_autonumber_set_autotext_scope_overwrite)
      (sort_order ,combo-box-value
                  ,schematic_autonumber_set_autotext_sort_order)
      (opt_removenum ,toggle-button-active?
                     ,schematic_autonumber_set_autotext_removenum)
      (opt_slotting ,toggle-button-active?
                    ,schematic_autonumber_set_autotext_slotting)))

  (for-each set-data! %funcs))


;;; Start autonumbering based on settings stored in the *AUTOTEXT
;;; object.
(define (start-autonumbering *autotext)
  (save-autonumber-dialog-state *autotext)
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
   (lookup-dialog-widget *dialog 'scope_overwrite)
   (if (true? (gtk_toggle_button_get_active *widget)) 0 1)))

(define *autonumber-remove-numbers-checkbox-clicked-callback
  (procedure->pointer void autonumber-remove-numbers-checkbox-clicked-callback '(* *)))


(define %gtk-response-accept (symbol->gtk-response 'accept))


;;; Create a structure for storing autonumber dialog state.
(define (make-autonumber-dialog-state)
  ;; Default contents of the combo box history.
  (define default-text-ls
    '("refdes=*"
      "refdes=C?"
      "refdes=D?"
      "refdes=I?"
      "refdes=L?"
      "refdes=Q?"
      "refdes=R?"
      "refdes=T?"
      "refdes=U?"
      "refdes=X?"
      "netname=*"
      "netname=A?"
      "netname=D?"))

  (define *autotext (schematic_autonumber_new))

  (schematic_autonumber_set_autotext_scope_text *autotext %null-pointer)

  (let loop ((ls default-text-ls)
             (*gls %null-pointer))
    (if (null? ls)
        (schematic_autonumber_set_autotext_scope_text *autotext *gls)
        (loop (cdr ls)
              (g_list_append *gls
                             ;; Call for g_strdup() is necessary as
                             ;; after a while, the pointer created in
                             ;; Scheme will be garbage-collected.
                             (g_strdup (string->pointer (car ls)))))))

  (schematic_autonumber_set_autotext_sort_order
   *autotext
   (schematic_autonumber_sort_order_from_string (string->pointer "sort-diagonal")))

  (schematic_autonumber_set_autotext_scope_skip
   *autotext
   (schematic_autonumber_scope_from_string (string->pointer "scope-page")))
  (schematic_autonumber_set_autotext_scope_number
   *autotext
   (schematic_autonumber_scope_from_string (string->pointer "scope-selected")))
  (schematic_autonumber_set_autotext_scope_overwrite *autotext
                                                     FALSE)
  (schematic_autonumber_set_autotext_startnum *autotext 1)
  (schematic_autonumber_set_autotext_removenum *autotext FALSE)
  (schematic_autonumber_set_autotext_slotting *autotext FALSE)
  (schematic_autonumber_set_autotext_dialog *autotext %null-pointer)

  *autotext)


;;; Restore the Autonumber text dialog settings from the *AUTOTEXT
;;; variable.
(define (restore-autonumber-dialog-state *autotext)
  (define *dialog (schematic_autonumber_get_autotext_dialog *autotext))

  (define (update-text! name val)
    (let* ((*scope-text-widget (lookup-dialog-widget *dialog name))
           (*text-entry-widget
            (gtk_bin_get_child *scope-text-widget))
           (*model (gtk_combo_box_get_model *scope-text-widget)))

      ;; Simple way to clear the ComboBox. Owen from #gtk+ says:
      ;;
      ;; Yeah, it's just slightly "shady" ... if you want to stick
      ;; to fully advertised API, you need to remember how many
      ;; rows you added and use gtk_combo_box_remove_text().
      (gtk_list_store_clear *model)

      (for-each
       (lambda (*element)
         (gtk_combo_box_text_append_text *scope-text-widget
                                         *element))
       (glist->list val identity))

      (gtk_entry_set_text *text-entry-widget
                          (glist-data (g_list_first val)))))

  (define (set-spin-button-value! name val)
    (gtk_spin_button_set_value
     (lookup-dialog-widget *dialog name)
     val))

  (define (set-combo-box-value! name val)
    (gtk_combo_box_set_active
     (lookup-dialog-widget *dialog name)
     val))

  (define (set-toggle-button-state! name val)
    (gtk_toggle_button_set_active
     (lookup-dialog-widget *dialog name)
     val))

  (define (set-widget-state! element)
    (let ((name (first element))
          (setter (second element))
          (getter (third element)))
      (setter name (getter *autotext))))

  (define %funcs
    `((scope_text ,update-text!
                  ,schematic_autonumber_get_autotext_scope_text)
      (scope_skip ,set-combo-box-value!
                  ,schematic_autonumber_get_autotext_scope_skip)
      (scope_number ,set-combo-box-value!
                    ,schematic_autonumber_get_autotext_scope_number)
      (scope_overwrite ,set-toggle-button-state!
                       ,schematic_autonumber_get_autotext_scope_overwrite)
      (opt_startnum ,set-spin-button-value!
                    ,schematic_autonumber_get_autotext_startnum)
      (sort_order ,set-combo-box-value!
                  ,schematic_autonumber_get_autotext_sort_order)
      (opt_removenum ,set-toggle-button-state!
                     ,schematic_autonumber_get_autotext_removenum)
      (opt_slotting ,set-toggle-button-state!
                    ,schematic_autonumber_get_autotext_slotting)))

  (for-each set-widget-state! %funcs))


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
            (lookup-dialog-widget *dialog 'opt_removenum))
           (*sort-order-widget
            (lookup-dialog-widget *dialog 'sort_order)))
      (schematic_autonumber_sort_order_widget_init *sort-order-widget)

      (gtk_dialog_set_default_response *dialog %gtk-response-accept)

      (g_signal_connect *dialog
                        (string->pointer "response")
                        *autonumber-response-callback
                        *autotext)
      (g_signal_connect *remove-number-widget
                        (string->pointer "clicked")
                        *autonumber-remove-numbers-checkbox-clicked-callback
                        *dialog)

      (schematic_autonumber_set_autotext_dialog *autotext *dialog)
      (restore-autonumber-dialog-state *autotext)
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
