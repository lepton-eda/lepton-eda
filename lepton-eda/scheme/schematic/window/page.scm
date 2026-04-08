;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2026 Lepton EDA Contributors
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

(define-module (schematic window page)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi gobject)
  #:use-module (lepton ffi)
  #:use-module (lepton gerror)
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (file-select-save-page!
            window-save-page!
            window-set-toplevel-page!
            window-save-active-page!))

(define (window-set-toplevel-page! window page)
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define *toplevel (schematic_window_get_toplevel *window))

  (lepton_toplevel_goto_page *toplevel *page)
  (schematic_window_page_changed *window))


(define (window-save-page! *window *page *filename)
  (define **err
    (bytevector->pointer (make-bytevector (sizeof '*) 0)))
  (when (null-pointer? *window)
    (error "NULL window."))
  (when (null-pointer? *page)
    (error "NULL page."))
  (when (null-pointer? *filename)
    (error "NULL filename."))

  ;; Try saving page to filename.
  (let ((result (f_save *page *filename **err))
        (*err (dereference-pointer **err)))
    (when (and (false? result)
               (not (null-pointer? *err)))
      (let ((message (gerror-message *err)))
        (g_clear_error **err)

        (schematic_window_dialog_save_error
         *window
         (string->pointer message))))

    (let ((different-names?
           (not (string-ci= (pointer->string (lepton_page_get_filename *page))
                            (pointer->string *filename)))))
      (when (and (true? result)
                 different-names?)
        (lepton_page_set_filename *page *filename))
      (x_window_save_page *window
                          *page
                          *filename
                          result
                          (if different-names? TRUE FALSE))
      (when (true? result)
        ;; Reset page CHANGED flag.
        (lepton_page_set_changed *page 0)
        ;; Add to recent file list.
        (recent_manager_add *window *filename)
        ;; Update Page Manager.
        (page_select_widget_update *window))

      (i_set_state_msg *window
                       (symbol->action-mode 'select-mode)
                       (string->pointer
                        (if (false? result)
                            (G_ "Error while trying to save")
                            (G_ "Saved"))))
      result)))


;;; Opens a file chooser dialog in *WINDOW for *PAGE and waits for
;;; the user to select a file where the page will be saved.
;;; Returns #f on a save error, and #t in all other cases
;;; including cancelling the Save dialog or its child overwrite
;;; dialog.
;;;
;;; The function updates the user interface. (Actual UI update is
;;; performed in window-save-page!(), which is called by this
;;; function).
(define (file-select-save-page! *window *page)
  (define (file-chooser-filename *dialog)
    (let* ((*filename (gtk_file_chooser_get_filename *dialog))
           (filename (and (not (null-pointer? *filename))
                          (pointer->string *filename))))
      (g_free *filename)
      filename))

  (define (overwrite-dialog-response *parent-dialog filename)
    (let* ((*overwrite-dialog
            (schematic_file_select_dialog_overwrite_file
             *parent-dialog
             (string->pointer filename)))
           (result (gtk-response->symbol
                    (gtk_dialog_run *overwrite-dialog))))
      (gtk_widget_destroy *overwrite-dialog)

      result))

  (define (run-save-as-dialog *dialog)
    (define filename-accepted?
      (eq? (gtk-response->symbol (gtk_dialog_run *dialog)) 'accept))
    (define filename
      (and filename-accepted?
           (file-chooser-filename *dialog)))

    ;; It is OK if the user cancels the File select dialog.
    (or (not filename)
        ;; If the file already exists, display a dialog box to
        ;; check if the user really wants to overwrite it.
        (if (and (file-exists? filename)
                 (not (eq? (overwrite-dialog-response *dialog
                                                      filename)
                           'yes)))
            (begin
              (log! 'message (G_ "Save cancelled on user request"))
              #t)

            ;; Try saving the page to filename.
            (true? (window-save-page! *window
                                      *page
                                      (string->pointer filename))))))

  (when (null-pointer? *window)
    (error "NULL window."))
  (when (null-pointer? *page)
    (error "NULL page."))

  (let* ((*main-window (schematic_window_get_main_window *window))
         (*dialog
          (schematic_file_select_dialog_save_as *main-window))
         (*page-filename (lepton_page_get_filename *page)))
    ;; Add file filters to the dialog.
    (schematic_file_select_dialog_setup_filters *dialog)

    (if (true? (schematic_file_select_dialog_filename_sch *page-filename))
        (gtk_file_chooser_set_filter *dialog
                                     (schematic_file_select_dialog_get_filter_sch))

        (if (true? (schematic_file_select_dialog_filename_sym *page-filename))
            (gtk_file_chooser_set_filter *dialog
                                         (schematic_file_select_dialog_get_filter_sym))
            (gtk_file_chooser_set_filter *dialog
                                         (schematic_file_select_dialog_get_filter_all))))

    ;; Set the current filename or directory name for new documents.
    (if (file-exists? (pointer->string *page-filename))
        (gtk_file_chooser_set_filename *dialog *page-filename)

        (begin
          ;; Force save in the current working directory.
          (gtk_file_chooser_set_current_folder
           *dialog
           (string->pointer (getcwd)))

          ;; Set page file's basename as the current filename.
          (gtk_file_chooser_set_current_name
           *dialog
           (string->pointer
            (basename (pointer->string *page-filename))))))

    ;; Add handler for dialog's "filter" property change notification.
    (g_signal_connect *dialog
                      (string->pointer "notify::filter")
                      *schematic_file_select_dialog_filter_changed
                      %null-pointer)

    ;; Open "Save As.." dialog.
    (gtk_widget_show *dialog)

    (let ((save-result (run-save-as-dialog *dialog)))
      (gtk_widget_destroy *dialog)

      ;; Return the result of the save operation.
      save-result)))


(define (window-save-active-page! window)
  (define *window (check-window window 1))
  (define *page (schematic_window_get_active_page *window))

  (unless (null-pointer? *page)
    (if (true? (x_window_untitled_page *page))
        ;; Open "Save as..." dialog.
        (file-select-save-page! *window *page)
        ;; Save page.
        (window-save-page! *window
                           *page
                           (lepton_page_get_filename *page)))))
