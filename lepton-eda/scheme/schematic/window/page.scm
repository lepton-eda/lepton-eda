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
  #:use-module (lepton ffi gobject)
  #:use-module (lepton ffi)
  #:use-module (lepton page foreign)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)
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
           (x_fileselect_save *window
                              *page
                              *result
                              *main-window
                              *dialog
                              *page-filename)))

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
