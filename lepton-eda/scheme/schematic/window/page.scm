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
  #:use-module (lepton gettext)
  #:use-module (lepton log)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
  #:use-module (schematic gtk helper)
  #:use-module (schematic window foreign)

  #:export (file-select-save-page!
            window-set-toplevel-page!
            window-save-active-page!))

(define (window-set-toplevel-page! window page)
  (define *window (check-window window 1))
  (define *page (check-page page 2))
  (define *toplevel (schematic_window_get_toplevel *window))

  (lepton_toplevel_goto_page *toplevel *page)
  (schematic_window_page_changed *window))


(define (file-select-save-page! *window *page *result)
  ;; Get filename from the GtkFileChooser dialog.  As the value
  ;; must be freed anyway, the filename is transformed into a
  ;; Scheme string, and the original pointer is freed.  If the
  ;; original pointer is NULL, the function returns #f.
  (define (file-chooser-filename *dialog)
    (let* ((*filename (gtk_file_chooser_get_filename *dialog))
           (filename (and (not (null-pointer? *filename))
                          (pointer->string *filename))))
      (g_free *filename)
      filename))

  (define (run-save-as-dialog *dialog)
    (if (eq? (gtk-response->symbol (gtk_dialog_run *dialog)) 'accept)
        (let* ((filename (file-chooser-filename *dialog))
               (existing-file? (and filename (file-exists? filename)))
               ;; If the file already exists, display a dialog box to
               ;; check if the user really wants to overwrite it.
               (*overwrite-dialog
                (if existing-file?
                    (schematic_file_select_dialog_overwrite_file
                     *dialog
                     (string->pointer filename))
                    %null-pointer))
               (overwrite-cancelled?
                (and (not (null-pointer? *overwrite-dialog))
                     (not (eq? (gtk-response->symbol
                                (gtk_dialog_run *overwrite-dialog))
                               'yes)))))
          (when (not (null-pointer? *overwrite-dialog))
            (gtk_widget_destroy *overwrite-dialog))

          (when overwrite-cancelled?
            (log! 'message (G_ "Save cancelled on user request")))

          (if (and filename (not overwrite-cancelled?))
              ;; Try saving the page to filename.
              (begin
                (x_fileselect_save *window
                                   *page
                                   *result
                                   (if filename
                                       (string->pointer filename)
                                       %null-pointer))
                TRUE)
              FALSE))
        FALSE))

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

    (unless (null-pointer? *result)
      (bytevector-sint-set! (pointer->bytevector *result (sizeof int))
                            0
                            FALSE
                            (native-endianness)
                            (sizeof int)))

    (let ((accepted-filename?
           (run-save-as-dialog *dialog)))

      (gtk_widget_destroy *dialog)
      ;; Whether the filename to save was accepted by the user.
      accepted-filename?)))


(define (window-save-active-page! window)
  (define *window (check-window window 1))
  (define *page (schematic_window_get_active_page *window))

  (unless (null-pointer? *page)
    (if (true? (x_window_untitled_page *page))
        ;; Open "Save as..." dialog.
        (file-select-save-page! *window *page %null-pointer)
        ;; Save page.
        (x_window_save_page *window *page (lepton_page_get_filename *page)))))
