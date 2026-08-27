;;; Lepton EDA attribute editor
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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


(define-module (attrib dialog)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton gettext)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic gtk helper)

  #:export (file-chooser-dialog))


;;; Sets up file filters for the File chooser dialog *FILE-CHOOSER.
(define (setup-file-chooser-filters *file-chooser)
  ;; File filter for schematic files (*.sch).
  (let ((*filter (gtk_file_filter_new)))
    (gtk_file_filter_set_name *filter
                              (string->pointer (G_ "Schematics")))
    (gtk_file_filter_add_pattern *filter (string->pointer "*.sch"))
    (gtk_file_chooser_add_filter *file-chooser *filter))
  ;; File filter for symbol files (*.sym).
  (let ((*filter (gtk_file_filter_new)))
    (gtk_file_filter_set_name *filter
                              (string->pointer (G_ "Symbols")))
    (gtk_file_filter_add_pattern *filter (string->pointer "*.sym"))
    (gtk_file_chooser_add_filter *file-chooser *filter))
  ;; File filter for both symbol and schematic files (*.sym+*.sch).
  (let ((*filter (gtk_file_filter_new)))
    (gtk_file_filter_set_name *filter
                              (string->pointer (G_ "Schematics and symbols")))
    (gtk_file_filter_add_pattern *filter (string->pointer "*.sym"))
    (gtk_file_filter_add_pattern *filter (string->pointer "*.sch"))
    (gtk_file_chooser_add_filter *file-chooser *filter))
  ;; File filter that matches any file.
  (let ((*filter (gtk_file_filter_new)))
    (gtk_file_filter_set_name *filter
                              (string->pointer (G_ "All files")))
    (gtk_file_filter_add_pattern *filter (string->pointer "*"))
    (gtk_file_chooser_add_filter *file-chooser *filter)))


(define (file-chooser-dialog *window)
  "Opens a file chooser dialog in parent *WINDOW which may be NULL,
and waits for the user to select at least one file.  Returns the
list of selected files, or an empty list if the user cancelled the
dialog."
  (define *dialog
    (gtk_file_chooser_dialog_new (string->pointer (G_ "Open..."))
                                 *window
                                 (symbol->gtk-file-chooser-action 'open)
                                 %null-pointer))

  (gtk_file_chooser_set_select_multiple *dialog TRUE)

  ;; Add file filters to the dialog.
  (setup-file-chooser-filters *dialog)
  ;; Add buttons.
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Cancel"))
                         GTK_RESPONSE_CANCEL)
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Open"))
                         GTK_RESPONSE_ACCEPT)

  (gtk_widget_show *dialog)

  (let ((*filenames
         (if (= (gtk_dialog_run *dialog) GTK_RESPONSE_ACCEPT)
             (gtk_file_chooser_get_filenames *dialog)
             %null-pointer)))
    (gtk_widget_destroy *dialog)
    (gslist->list *filenames pointer->string 'free)))
