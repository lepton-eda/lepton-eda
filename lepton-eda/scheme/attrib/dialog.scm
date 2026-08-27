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
  #:use-module (lepton version)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic gtk helper)

  #:export (about-dialog
            add-attrib-dialog
            delete-attrib-dialog
            export-error-dialog
            export-file-dialog
            file-chooser-dialog
            missing-symbol-dialog
            unsaved-changes-dialog))


(define (about-dialog program-name)
  "Runs the About dialog."
  (define *dialog (gtk_about_dialog_new))

  (gtk_about_dialog_set_program_name
   *dialog
   (string->pointer program-name))

  (gtk_about_dialog_set_comments
   *dialog
   (string->pointer
    (G_ "Lepton Electronic Design Automation")))

  (let ((version-string
         (format #f
                 "~A (git: ~A)"
                 (lepton-version-ref 'dotted)
                 (lepton-version-ref 'git7))))
    (gtk_about_dialog_set_version *dialog
                                  (string->pointer version-string)))

  (gtk_about_dialog_set_copyright
   *dialog
   (string->pointer
    (G_ "Copyright © 2003-2006 Stuart D. Brorson
Copyright © 2003-2016 gEDA Contributors
Copyright © 2017-2026 Lepton EDA Contributors")))

  (gtk_about_dialog_set_license
   *dialog
   (string->pointer
    (G_ "Lepton EDA is freely distributable under the
GNU Public License (GPL) version 2.0 or (at your option) any later version.
See the COPYING file for the full text of the license.")))

  (gtk_about_dialog_set_website
   *dialog
   (string->pointer (lepton-version-ref 'url)))

  (gtk_widget_show_all *dialog)
  (gtk_dialog_run *dialog)

  (gtk_widget_destroy *dialog))


(define (gtk-entry-text *entry)
  (let ((*entry-text (gtk_entry_get_text *entry)))
    (and (not (null-pointer? *entry-text))
         (let ((entry-text (pointer->string *entry-text)))
           (and (not (string-null? entry-text))
                entry-text)))))


(define (add-attrib-dialog)
  "Runs an Add attribute dialog with an entry for a new
attrib name column.  Returns #t if the user hits the OK button to
insert the column, otherwise returns #f."
  ;; Create the dialog.
  (define *dialog (gtk_dialog_new))
  ;; Create a text label for the dialog window.
  (define *label
    (gtk_label_new (string->pointer (G_ "Enter new attribute name"))))
  ;; Create the attrib text entry area.
  (define *attrib-entry (gtk_entry_new))

  (gtk_window_set_title *dialog
                        (string->pointer (G_"Add new attribute")))
  (gtk_window_set_modal *dialog TRUE)

  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_OK"))
                         GTK_RESPONSE_OK)
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Cancel"))
                         GTK_RESPONSE_CANCEL)

  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_OK)
  (gtk_box_pack_start (gtk_dialog_get_content_area *dialog)
                      *label
                      FALSE
                      FALSE
                      0)

  (gtk_entry_set_max_length *attrib-entry 1024)
  (gtk_box_pack_start (gtk_dialog_get_content_area *dialog)
                      *attrib-entry
                      TRUE
                      TRUE
                      5)
  (gtk_widget_set_size_request *dialog 260 140)

  (gtk_widget_show_all *dialog)

  (let ((result
         (and (= (gtk_dialog_run *dialog) GTK_RESPONSE_OK)
              (gtk-entry-text *attrib-entry))))

    (gtk_widget_destroy *dialog)

    result))


(define (delete-attrib-dialog)
  "Runs the Delete attribute dialog.  Returns #t if the user hits the
Yes button, otherwise returns #f."
  (define *dialog
    (gtk_message_dialog_new
     %null-pointer
     GTK_DIALOG_MODAL
     (symbol->gtk-message-type 'question)
     (symbol->gtk-buttons-type 'yes-no)
     (string->pointer
      (G_ "Are you sure you want to delete this attribute?"))))

  (gtk_window_set_title *dialog
                        (string->pointer (G_ "Delete attribute")))
  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_NO)

  (let ((response (gtk_dialog_run *dialog)))
    (gtk_widget_destroy *dialog)
    (= response GTK_RESPONSE_YES)))


(define (confirm-overwrite-dialog filename)
  "Opens an Overwrite confirmation dialog for FILENAME.  Returns #t
if the the user pressed 'Yes' to overwrite the file.  Otherwise
returns #f."
  (define *dialog
    (gtk_message_dialog_new
     %null-pointer
     (logior GTK_DIALOG_MODAL
             GTK_DIALOG_DESTROY_WITH_PARENT)
     (symbol->gtk-message-type 'question)
     (symbol->gtk-buttons-type 'yes-no)
     (string->pointer
      (format #f
              (G_ "The selected file ~S already exists.

Would you like to overwrite it?")
              filename))))

  (gtk_window_set_title *dialog
                        (string->pointer (G_ "Overwrite file?")))
  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_NO)

  (let ((result (gtk_dialog_run *dialog)))
    (gtk_widget_destroy *dialog)

    (eq? result GTK_RESPONSE_YES)))


(define (export-error-dialog message)
  "Opens an Export error dialog reporting MESSAGE."
  (let ((*dialog (gtk_message_dialog_new %null-pointer
                                         GTK_DIALOG_MODAL
                                         (symbol->gtk-message-type 'error)
                                         (symbol->gtk-buttons-type 'ok)
                                         (string->pointer message))))
    (gtk_window_set_title *dialog
                          (string->pointer (G_ "Export error")))
    (gtk_dialog_run *dialog)
    (gtk_widget_destroy *dialog)))


(define (export-file-dialog)
  "Runs the Export CSV file dialog and returns the filename selected
by the user.  If no filename is selected, or the user pressed the
Cancel button, returns #f."
  (define *dialog
    (gtk_file_chooser_dialog_new
     (string->pointer (G_ "Export CSV"))
     %null-pointer
     (symbol->gtk-file-chooser-action 'save)
     %null-pointer))

  (define (file-chooser-filename *dialog)
    (let ((*filename (gtk_file_chooser_get_filename *dialog)))
      (and (not (null-pointer? *filename))
           (let ((filename (pointer->string *filename)))
             (g_free *filename)
             filename))))

  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Cancel"))
                         GTK_RESPONSE_CANCEL)

  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Save"))
                         GTK_RESPONSE_ACCEPT)

  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_ACCEPT)

  (let ((response (gtk_dialog_run *dialog))
        (filename (file-chooser-filename *dialog)))
    (gtk_widget_destroy *dialog)
    (and (= response GTK_RESPONSE_ACCEPT)
         filename
         (or (not (file-exists? filename))
             (confirm-overwrite-dialog filename))
         filename)))


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


(define (missing-symbol-dialog)
  "Runs the Missing symbol dialog.  It offers the user the chance to
close the project without saving because the program read a
schematic with a missing symbol file.  Returns #t if the user
pressed the button Forward to continue with the execution,
otherwise returns #f."
  (define message
    (G_ "One or more components have been found with missing symbol files!

This probably happened because lepton-attrib couldn't find your
component libraries, perhaps because your gafrc files are misconfigured.

Choose \"Quit\" to leave lepton-attrib and fix the problem, or
\"Forward\" to continue working with lepton-attrib."))

  ;; Create the *dialog.
  (define *dialog
    (gtk_message_dialog_new %null-pointer
                            GTK_DIALOG_MODAL
                            (symbol->gtk-message-type 'warning)
                            (symbol->gtk-buttons-type 'none)
                            (string->pointer message)))

  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Quit"))
                         GTK_RESPONSE_REJECT)
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Forward"))
                         GTK_RESPONSE_ACCEPT)
  (gtk_window_set_title
   *dialog
   (string->pointer (G_ "Missing symbol file found for component!")))
  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_REJECT)

  (let ((response (gtk_dialog_run *dialog)))
    (gtk_widget_destroy *dialog)
    (= response GTK_RESPONSE_ACCEPT)))


;;; The dialog is thrown up before the user quits if there are
;;; unsaved project data.
(define (unsaved-changes-dialog *window title)
  "Creates a new Unsaved changes dialog with TITLE in parent *WINDOW
and returns the response value."
  (define msg1 (G_ "Save the changes before closing?"))
  (define msg2
    (G_ "If you don't save all your changes will be permanently lost."))
  (define markup
    (string-append "<big><b>" msg1 "</b></big>" "\n\n" msg2))

  (define *dialog
    (gtk_message_dialog_new *window
                            (logior GTK_DIALOG_MODAL
                                    GTK_DIALOG_DESTROY_WITH_PARENT)
                            (symbol->gtk-message-type 'warning)
                            (symbol->gtk-buttons-type 'none)
                            %null-pointer))
  (gtk_message_dialog_set_markup *dialog (string->pointer markup))

  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "Close without saving"))
                         GTK_RESPONSE_NO)
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Cancel"))
                         GTK_RESPONSE_CANCEL)
  (gtk_dialog_add_button *dialog
                         (string->pointer (G_ "_Save"))
                         GTK_RESPONSE_YES)

  (gtk_window_set_title *dialog (string->pointer title))

  (gtk_dialog_set_default_response *dialog GTK_RESPONSE_YES)

  (let ((response (gtk_dialog_run *dialog)))
    (gtk_widget_destroy *dialog)
    (cond
     ((= response GTK_RESPONSE_NO) 'quit)
     ((= response GTK_RESPONSE_YES) 'save)
     ((= response GTK_RESPONSE_CANCEL) 'cancel)
     (else #f))))
